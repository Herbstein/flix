/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, Definition}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *
  * (a) Appears in the global namespaces, takes zero arguments, and is not marked as synthetic.
  * (b) Appears in a fact or a rule as a filter/transfer function.
  * (c) Appears in a lattice declaration.
  * (d) Appears in a function which itself is reachable.
  */

object TreeShaker extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Performs tree shaking on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    /**
      * A queue of function definitions to be processed recursively.
      *
      * For example, if the queue contains the entry:
      *
      * -   f
      *
      * it means that the function definition f should be considered to determine new reachable functions.
      */
    val queue: mutable.Queue[Definition.Constant] = mutable.Queue.empty

    /**
      * A map used to collect reachable functions (symbols and the corresponding definitions).
      */
    val reachableFunctions: mutable.Map[Symbol.DefnSym, Definition.Constant] = mutable.Map.empty

    /**
      * Returns true iff the function definition `defn` is initially reachable by (a).
      *
      * That is, returns true iff `defn` satisfies:
      *
      *   (a) Appears in the global namespaces, takes zero arguments, and is not marked as synthetic.
      */
    def isReachableRoot(defn: Definition.Constant): Boolean = {
      defn.sym.namespace.isEmpty && defn.formals.isEmpty && !defn.isSynthetic
    }

    /**
      * Searches the given SimplifiedAst.Term.Head `head` for reachable functions.
      */
    def visitTermHead(head: SimplifiedAst.Term.Head): Unit = {
      head match {
        case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) => visitExp(lit)
        case SimplifiedAst.Term.Head.App(sym, args, tpe, loc) => newDefinitionSymbol(sym)
        case _ =>
      }
    }

    /**
      * Searches the given SimplifiedAst.Term.Body `body` for reachable functions.
      */
    def visitTermBody(body: SimplifiedAst.Term.Body): Unit = {
      body match {
        case SimplifiedAst.Term.Body.Lit(exp, tpe, loc) => visitExp(exp)
        case _ =>
      }
    }

    /**
      * Searches the given Expression `e0` for reachable functions.
      */
    def visitExp(e0: Expression): Unit =  e0 match {
      case Expression.Ref(sym, tpe, loc) => newDefinitionSymbol(sym)
      case Expression.Lambda(args, body, tpe, loc) => visitExp(body)
      case Expression.Hook(hook, tpe, loc) => newDefinitionSymbol(hook.sym)
      case Expression.MkClosure(lambda, freeVars, tpe, loc) => visitExp(lambda)
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => visitExp(ref)
      case Expression.ApplyRef(sym, args, tpe, loc) =>
        newDefinitionSymbol(sym)
        args.foreach(visitExp)
      case Expression.ApplyTail(sym, formals, actuals, tpe, loc) =>
        newDefinitionSymbol(sym)
        actuals.foreach(visitExp)
      case Expression.Apply(exp, args, tpe, loc) =>
        visitExp(exp)
        args.foreach(visitExp)
      case Expression.Unary(op, exp, tpe, loc) => visitExp(exp)
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        visitExp(exp1)
        visitExp(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        visitExp(exp1)
        visitExp(exp2)
        visitExp(exp3)
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        visitExp(exp1)
        visitExp(exp2)
      case Expression.Is(exp, tag, loc) => visitExp(exp)
      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp)
      case Expression.Untag(tag, exp, tpe, loc) => visitExp(exp)
      case Expression.Index(base, offset, tpe, loc) => visitExp(base)
      case Expression.Tuple(elms, tpe, loc) => elms.foreach(visitExp)
      case Expression.Existential(fparam, exp, loc) => visitExp(exp)
      case Expression.Universal(fparam, exp, loc) => visitExp(exp)
      case _ =>
    }

    /**
      * Adds the function `sym` to the map of reachable functions.
      */
    def newDefinitionSymbol(sym: Symbol.DefnSym): Unit = {
      // If `sym` has not already been determined reachable, look up its definition in `root`.
      if (!reachableFunctions.contains(sym)) {
        root.definitions.get(sym) match {
          case Some(defn) =>
            reachableFunctions.put(sym, defn)
            queue.enqueue(defn)
          // If `sym` is not defined in `root`, leave this for error checking later.
          case _ =>
        }
      }
    }

    /*
     * We can now use these helper functions to perform tree shaking.
     */

    // Start the timer.
    val t = System.nanoTime()

    /*
     * Find reachable functions that:
     *
     * (a) Appear in the global namespaces, take zero arguments, and are not marked as synthetic.
     */
    for ((sym, defn) <- root.definitions) {
      if (isReachableRoot(defn)) {
        queue.enqueue(defn)
        reachableFunctions.put(sym, defn)
      }
    }

    /*
     * Find reachable functions that:
     *
     * (b) Appear in a fact or a rule as a filter/transfer function.
     */
    for (stratum <- root.strata) {
      for (constraint <- stratum.constraints) {
        constraint.head match {
          case SimplifiedAst.Predicate.Head.Positive(sym, terms, loc) => terms.foreach(visitTermHead)
          case SimplifiedAst.Predicate.Head.Negative(sym, terms, loc) => terms.foreach(visitTermHead)
          case _ =>
        }

        constraint.body.foreach {
          case SimplifiedAst.Predicate.Body.Positive(sym, terms, loc) => terms.foreach(visitTermBody)
          case SimplifiedAst.Predicate.Body.Negative(sym, terms, loc) => terms.foreach(visitTermBody)
          case SimplifiedAst.Predicate.Body.Filter(sym, terms, loc) => newDefinitionSymbol(sym)
          case SimplifiedAst.Predicate.Body.Loop(sym, term, loc) => visitTermHead(term)
        }
      }
    }

    /*
     * Find reachable functions that:
     *
     * (c) Appear in a lattice declaration.
     */
    root.lattices.values.foreach({
      case SimplifiedAst.Definition.Lattice(tpe, bot, top, leq, lub, glb, loc) =>
        visitExp(bot)
        visitExp(top)
        visitExp(leq)
        visitExp(lub)
        visitExp(glb)
    })

    /*
     * Find reachable functions that:
     *
     * (d) Appear in a function which itself is reachable.
     */
    while (queue.nonEmpty) {
      // Extract a function body from the queue and search for other reachable functions.
      visitExp(queue.dequeue().exp)
    }

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      definitions = reachableFunctions.toMap,
      time = root.time.copy(treeshaker = e)
    ).toSuccess
  }

}
