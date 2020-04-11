pub def main(): Result[Path, Manifest.InitError] & Impure = Manifest.initPackage("package_test")

    case Some(_) => true
    case None => false
}

pub def main(): Bool & Impure = {
    SemVer/Test.allTests() &&
    ParseToml/Test.allTests()
}
