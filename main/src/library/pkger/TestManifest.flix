use SemVer.SemVer;
use Manifest.LockedPackage;

def _packageRepository(package: String, version: SemVer): Map[SemVer, Map[String, SemVer]] = {
    use Map.{empty, filterWithKey, getWithDefault, insert};
    use SemVer.isCompatible;

    let package_one_versions = empty()
        |> insert(SemVer.SemVer(0, 1, 0), empty())
        |> insert(SemVer.SemVer(0, 1, 1), empty())
        |> insert(SemVer.SemVer(0, 2, 0), empty())
        |> insert(SemVer.SemVer(0, 2, 1), empty() |> insert("package_two", SemVer.SemVer(0, 1, 0)));

    let package_two_versions = empty()
        |> insert(SemVer.SemVer(0, 1, 0), empty())
        |> insert(SemVer.SemVer(0, 1, 1), empty())
        |> insert(SemVer.SemVer(0, 2, 0), empty() |> insert("package_one", SemVer.SemVer(0, 1, 0)));

    let all_packages = empty()
        |> insert("package_one", package_one_versions)
        |> insert("package_two", package_two_versions);

    let versions = getWithDefault(package, empty(), all_packages);
    let compat_versions = filterWithKey((k, _) -> isCompatible(version, k), versions);

    compat_versions
}

def transitiveContains(mx: Map[String, SemVer.SemVer], m: (String, SemVer.SemVer)): Bool = {
    Map.exists((k, v) -> match m { case (p, s) => p == k && s == v }, mx)
}

def containedIn(xs: List[LockedPackage], package: LockedPackage): Bool = {
    let transitive = (p) -> Map.forall((k, v) -> transitiveContains(p.transitive, (k, v)), package.transitive);

    List.exists(p -> {
        p.name == package.name
            && p.version == package.version
            && transitive(p)
    }, xs)
}

pub def checkLock(locked: List[LockedPackage], check: List[LockedPackage]): Bool = List.forall(containedIn(locked), check)

@test
def xx(): Bool = true

/*
namespace Test {
        def testLocking(): Bool & Impure = {
            use Test.test;
            use Manifest.{LockedPackage, lockPackage, newLockedPackage};
            use Map.{empty, insert};
            use SemVer.SemVer;

            let package = { name = "some_package"
                          , version = SemVer.SemVer(0, 1, 0)
                          , authors = {name = "Some Guy", email = "test@test.com"} :: Nil
                          , description = "Some package"
                          // This could be mandatory
                          , license = "MIT"
                          // Mapping package dependency names to the semver pattern
                          , depends = empty() |> insert("package_one", SemVer.SemVer(0, 1, 0))
                          , home = None
                          };

            let locked = lockPackage(package);

            let check = newLockedPackage("some_package",  SemVer.SemVer(0,1,0), empty() |> insert("package_one", SemVer.SemVer(0, 1, 1)))
                :: newLockedPackage("package_one", SemVer.SemVer(0,1,1), empty())
                :: Nil;

            let first = test("lockingIsExpected1", () -> List.forall(containedIn(locked), check));

            let package2 = {
                name = "another_package",
                version = SemVer.SemVer(0, 1, 0),
                authors = {name = "Some Guy", email = "test@test.com"} :: Nil,
                description = "Some package",
                license = "MIT",
                depends = empty() |> insert("package_one", SemVer.SemVer(0, 2, 0)),
                home = None
            };

            let locked2 = lockPackage(package2);

            let check2 = newLockedPackage("another_package", SemVer.SemVer(0, 1, 0), empty() |> insert("package_one", SemVer.SemVer(0, 2, 1)) |> insert("package_two", SemVer.SemVer(0, 1, 1)))
                :: newLockedPackage("package_one", SemVer.SemVer(0, 2, 1), empty() |> insert("package_two", SemVer.SemVer(0, 1, 1)))
                :: newLockedPackage("package_two", SemVer.SemVer(0, 1, 1), empty())
                :: Nil;

            let second = test("lockingIsExpected2", () -> List.forall(containedIn(locked2), check2));

            first && second
        }

        pub def allTests(): Bool & Impure = {
            testLocking()
        }
    }
    */
