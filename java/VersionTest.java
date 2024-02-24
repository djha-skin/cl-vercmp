import java.util.*;
import java.lang.module.ModuleDescriptor.Version;

// We are in the middle of creating a java version comparison tool in lisp.
// However, we need to ensure that all the versions we test on our tool have the
// same version comparison outcomes in Java as theyu do in lisp.
// We're moving lisp code into java code so we can check our work.
// 
// Here is the lisp code we are moving to java:
/*
(deftest maven-cases
  (testing "Numeric Comparison"
           (ok (= (vc:maven-vercmp "1.0.0" "1.0.0") 0))
           (ok (< (vc:maven-vercmp "1.0.0" "1.0") 0))
           (ok (> (vc:maven-vercmp "1.0.1" "1.0") 0))
           (ok (< (vc:maven-vercmp "1.0.0" "1.0.1") 0))
           (ok (< (vc:maven-vercmp "1.0.0" "1.0.0-1") 0))
           (ok (> (vc:maven-vercmp "1.0.0" "0.9.2") 0))
           (ok (< (vc:maven-vercmp "0.9.2" "0.9.3") 0))
           (ok (> (vc:maven-vercmp "0.9.2" "0.9.1") 0))
           (ok (< (vc:maven-vercmp "0.9.5" "0.9.13") 0))
           (ok (< (vc:maven-vercmp "10.2.0.3.0" "11.2.0.3.0") 0))
           (ok (> (vc:maven-vercmp "10.2.0.3.0" "5.2.0.3.0") 0))
           (ok (< (vc:maven-vercmp "1.0.0-SNAPSHOT" "1.0.1-SNAPSHOT") 0))
           (ok (< (vc:maven-vercmp "1.0.0-alpha" "1.0.1-beta") 0))
           (ok (< (vc:maven-vercmp "1.1-dolphin" "1.1.1-cobra") 0)))
  (testing "Lexical Comparison"
           (ok (< (vc:maven-vercmp "1.0-alpaca" "1.0-bermuda") 0))
           (ok (< (vc:maven-vercmp "1.0-alpaca" "1.0-alpaci") 0))
           (ok (> (vc:maven-vercmp "1.0-dolphin" "1.0-cobra") 0)))
  (testing "Qualifier Comparison"
           (ok (< (vc:maven-vercmp "1.0.0-alpha" "1.0.0-beta") 0))
           (ok (> (vc:maven-vercmp "1.0.0-beta" "1.0.0-alpha") 0))
           (ok (< (vc:maven-vercmp "1.0.0-alpaca" "1.0.0-beta") 0))
           (ok (> (vc:maven-vercmp "1.0.0-final" "1.0.0-milestone") 0)))
  (testing "Qualifier/Numeric Comparison"
           (ok (< (vc:maven-vercmp "1.0.0-alpha1" "1.0.0-alpha2") 0))
           (ok (< (vc:maven-vercmp "1.0.0-alpha5" "1.0.0-alpha23") 0))
           (ok (< (vc:maven-vercmp "1.0-RC5" "1.0-RC20") 0))
           (ok (> (vc:maven-vercmp "1.0-RC11" "1.0-RC6") 0)))
  (testing "Releases are newer than SNAPSHOTs"
           (ok (> (vc:maven-vercmp "1.0.0" "1.0.0-SNAPSHOT") 0))
           (ok (= (vc:maven-vercmp "1.0.0-SNAPSHOT" "1.0.0-SNAPSHOT") 0))
           (ok (< (vc:maven-vercmp "1.0.0-SNAPSHOT" "1.0.0") 0)))
  (testing "Releases are newer than qualified versions"
           (ok (> (vc:maven-vercmp "1.0.0" "1.0.0-alpha5") 0))
           (ok (< (vc:maven-vercmp "1.0.0-alpha5" "1.0.0") 0)))
  (testing "SNAPSHOTS are newer than qualified versions"
           (ok (> (vc:maven-vercmp "1.0.0-SNAPSHOT" "1.0.0-RC1") 0))
           (ok (< (vc:maven-vercmp "1.0.0-SNAPSHOT" "1.0.1-RC1") 0)))
  (testing "Some other Formats"
           (ok (> (vc:maven-vercmp "9.1-901.jdbc4" "9.1-901.jdbc3") 0))
           (ok (> (vc:maven-vercmp "9.1-901-1.jdbc4" "9.1-901.jdbc4") 0)))
  (testing "Some more zero-extension Tests"
           (ok (= (vc:maven-vercmp "1-SNAPSHOT" "1.0-SNAPSHOT") 0))
           (ok (= (vc:maven-vercmp "1-alpha" "1-alpha0") 0))))
*/

// And here is the java code we are using to test our lisp code:

class VersionTest {
    public static void main(String[] args) {
        List<List<Version> > comparisons = Arrays.asList(
           Arrays.asList(Version.parse("1.0.0"), Version.parse("1.0.0")),
           Arrays.asList(Version.parse("1.0.0"), Version.parse("1.0")),
           Arrays.asList(Version.parse("1.0.1"), Version.parse("1.0")),
           Arrays.asList(Version.parse("1.0.0"), Version.parse("1.0.1")),
           Arrays.asList(Version.parse("1.0.0"), Version.parse("1.0.0-1")),
           Arrays.asList(Version.parse("1.0.0"), Version.parse("0.9.2")),
           Arrays.asList(Version.parse("0.9.2"), Version.parse("0.9.3")),
           Arrays.asList(Version.parse("0.9.2"), Version.parse("0.9.1")),
           Arrays.asList(Version.parse("0.9.5"), Version.parse("0.9.13")),
           Arrays.asList(Version.parse("10.2.0.3.0"), Version.parse("11.2.0.3.0")),
           Arrays.asList(Version.parse("10.2.0.3.0"), Version.parse("5.2.0.3.0")),
           Arrays.asList(Version.parse("1.0.0-SNAPSHOT"), Version.parse("1.0.1-SNAPSHOT")),
           Arrays.asList(Version.parse("1.0.0-alpha"), Version.parse("1.0.1-beta")),
           Arrays.asList(Version.parse("1.1-dolphin"), Version.parse("1.1.1-cobra")),
           Arrays.asList(Version.parse("1.0-alpaca"), Version.parse("1.0-bermuda")),
           Arrays.asList(Version.parse("1.0-alpaca"), Version.parse("1.0-alpaci")),
           Arrays.asList(Version.parse("1.0-dolphin"), Version.parse("1.0-cobra")),
           Arrays.asList(Version.parse("1.0.0-alpha"), Version.parse("1.0.0-beta")),
           Arrays.asList(Version.parse("1.0.0-beta"), Version.parse("1.0.0-alpha")),
           Arrays.asList(Version.parse("1.0.0-alpaca"), Version.parse("1.0.0-beta")),
           Arrays.asList(Version.parse("1.0.0-final"), Version.parse("1.0.0-milestone")),
           Arrays.asList(Version.parse("1.0.0-alpha1"), Version.parse("1.0.0-alpha2")),
           Arrays.asList(Version.parse("1.0.0-alpha5"), Version.parse("1.0.0-alpha23")),
           Arrays.asList(Version.parse("1.0-RC5"), Version.parse("1.0-RC20")),
           Arrays.asList(Version.parse("1.0-RC11"), Version.parse("1.0-RC6")),
           Arrays.asList(Version.parse("1.0.0"), Version.parse("1.0.0-SNAPSHOT")),
           Arrays.asList(Version.parse("1.0.0-SNAPSHOT"), Version.parse("1.0.0-SNAPSHOT")),
           Arrays.asList(Version.parse("1.0.0-SNAPSHOT"), Version.parse("1.0.0")),
           Arrays.asList(Version.parse("1.0.0"), Version.parse("1.0.0-alpha5")),
           Arrays.asList(Version.parse("1.0.0-alpha5"), Version.parse("1.0.0")),
           Arrays.asList(Version.parse("1.0.0-SNAPSHOT"), Version.parse("1.0.0-RC1")),
           Arrays.asList(Version.parse("1.0.0-SNAPSHOT"), Version.parse("1.0.1-RC1")),
           Arrays.asList(Version.parse("9.1-901.jdbc4"), Version.parse("9.1-901.jdbc3")),
           Arrays.asList(Version.parse("9.1-901-1.jdbc4"), Version.parse("9.1-901.jdbc4")),
           Arrays.asList(Version.parse("1-SNAPSHOT"), Version.parse("1.0-SNAPSHOT")),
           Arrays.asList(Version.parse("1-alpha"), Version.parse("1-alpha0")));
        for (List<Version> pair : comparisons) {
            System.out.println(pair.get(0) + " vs. " + pair.get(1) +
                    ": " + pair.get(0).compareTo(pair.get(1)));
        }
    }
}

