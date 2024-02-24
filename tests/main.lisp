(defpackage #:skin.djha.vercmp/tests/main
  (:use :cl
        :rove)
  (:import-from #:skin.djha.vercmp)
  (:local-nicknames (:vc #:skin.djha.vercmp)))
(in-package #:skin.djha.vercmp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-vercmp)' in your Lisp.


#+(or)
(rove:run-test *)

(deftest test-nonnumeric-part-compare
  (testing "empty cases"
           (ok (= (vc::nonnumeric-part-compare "" "") 0))
           (ok (> (vc::nonnumeric-part-compare " " "") 0))
           (ok (< (vc::nonnumeric-part-compare "" " ") 0))
           (ok (= (vc::nonnumeric-part-compare (coerce '(#\Nul) 'string) "") 0)))
  (testing "non-empty cases"
           (ok (> (vc::nonnumeric-part-compare ".alpha" "alpha") 0))
           (ok (< (vc::nonnumeric-part-compare "alpha" ".alpha") 0))
           (ok (> (vc:debian-vercmp "a" "") 0)))
  (testing "trump cases"
           (ok (< (vc::nonnumeric-part-compare "~alpha" "") 0))

           (ok (< (vc::nonnumeric-part-compare "~~" "~a") 0))
           (ok (< (vc::nonnumeric-part-compare "~~~a" "~~~b") 0))
           (ok (> (vc::nonnumeric-part-compare "~~~b" "~~~a") 0))
           (let ((vc:*trumpc* #\-))
             (ok (< (vc::nonnumeric-part-compare "-a" "") 0))
             (ok (> (vc::nonnumeric-part-compare "" "-a") 0))
             (ok (< (vc::nonnumeric-part-compare "-a" "!a") 0))
             (ok (> (vc::nonnumeric-part-compare "!a" "-a") 0)))))

(deftest test-numeric-part-compare
  (testing "zero cases"
           (ok (= (vc::numeric-part-compare "" "") 0))
           (ok (= (vc::numeric-part-compare "" "0") 0))
           (ok (= (vc::numeric-part-compare "0" "") 0))
           (ok (= (vc::numeric-part-compare "0" "0") 0))
           (ok (= (vc::numeric-part-compare "0" "000000") 0)))
  (testing "basic cases"
           (ok (< (vc::numeric-part-compare "" "000017") 0))
           (ok (> (vc::numeric-part-compare "000017" "") 0))
           (ok (= (vc::numeric-part-compare "000017" "000017") 0))
           (ok (= (vc::numeric-part-compare "000017" "017") 0))
           (ok (< (vc::numeric-part-compare "000017" "18") 0))
           (ok (> (vc::numeric-part-compare "675" "332") 0))
           (ok (< (vc::numeric-part-compare "675" "67555555555555555555555") 0))
           (ok (> (vc::numeric-part-compare "5558675309" "5552333892") 0))))

(deftest version-parts-split
  (testing
    "The empty case. This case doesn't really make sense, but it's here for
    completeness."
    (ok (equalp (vc::version-parts-split "") #())))
  (testing
    "Some simple cases"
    (ok (equalp (vc::version-parts-split " ") #(" ")))
    (ok (equalp (vc::version-parts-split "5") #("" "5")))
    (ok (equalp (vc::version-parts-split "1.0")
               #("" "1" "." "0")))
    (ok (equalp (vc::version-parts-split "1.0.0")
               #("" "1" "." "0" "." "0"))))
  (testing
    "A case with a numeric suffix"
    (ok (equalp (vc::version-parts-split "5.3.alpha17")
        #("" "5" "." "3" ".alpha" "17"))))
  (testing
    "Concerning dots"
    (ok (equalp (vc::version-parts-split ".a1")
        #(".a" "1")))
    (ok (equalp (vc::version-parts-split "5.a1")
        #("" "5" ".a" "1"))))
  (testing
    "A case with a tilde"
    (ok (equalp (vc::version-parts-split "1.0~alpha")
               #("" "1" "." "0" "~alpha")))))

(deftest debian-vercmp
  (testing "Non-epoch Cases"
    (ok (= (vc:debian-vercmp "" "") 0))
    (ok (< (vc:debian-vercmp "~~" "~~a") 0))
    (ok (> (vc:debian-vercmp "~" "~~a") 0))
    (ok (< (vc:debian-vercmp "~" "") 0))
    (ok (> (vc:debian-vercmp "a" "") 0))
    (ok (< (vc:debian-vercmp "1.2.3~rc1" "1.2.3") 0))
    (ok (= (vc:debian-vercmp "1.2" "1.2") 0))
    (ok (< (vc:debian-vercmp "1.2" "a1.2") 0))
    (ok (> (vc:debian-vercmp "1.2.3" "1.2-3") 0))
    (ok (> (vc:debian-vercmp "1.2.3~rc1" "1.2.3~~rc1") 0))
    (ok (< (vc:debian-vercmp "1.2.3" "2") 0))
    (ok (> (vc:debian-vercmp "2.0.0" "2.0") 0))
    (ok (> (vc:debian-vercmp "1.2.a" "1.2.3") 0))
    (ok (> (vc:debian-vercmp "1.2.a" "1.2a") 0))
    (ok (< (vc:debian-vercmp "1" "1.2.3.4") 0))
    (ok (= (vc:debian-vercmp "1:2.3.4" "1:2.3.4") 0)))
  (testing "Scary Cases"
    (ok (= (vc:debian-vercmp "a" "a0"))))
  (testing "Epoch Cases"
    (ok (= (vc:debian-vercmp "0:" "0:") 0))
    (ok (= (vc:debian-vercmp "0:" "" ) 0))
    (ok (= (vc:debian-vercmp "0:1.2" "1.2") 0))
    (ok (< (vc:debian-vercmp "0:" "1:") 0))
    (ok (< (vc:debian-vercmp "0:1.2.3" "2:0.4.5") 0))))
1.0.1 vs. 1.0: 1
1.0.0 vs. 1.0.1: -1
1.0.0 vs. 1.0.0-1: 1
1.0.0 vs. 0.9.2: 1
0.9.2 vs. 0.9.3: -1
0.9.2 vs. 0.9.1: 1
0.9.5 vs. 0.9.13: -1
10.2.0.3.0 vs. 11.2.0.3.0: -1
10.2.0.3.0 vs. 5.2.0.3.0: 1
1.0.0-SNAPSHOT vs. 1.0.1-SNAPSHOT: -1
1.0.0-alpha vs. 1.0.1-beta: -1
1.1-dolphin vs. 1.1.1-cobra: -1
1.0-alpaca vs. 1.0-bermuda: -1
1.0-alpaca vs. 1.0-alpaci: -8
1.0-dolphin vs. 1.0-cobra: 1
1.0.0-alpha vs. 1.0.0-beta: -1
1.0.0-beta vs. 1.0.0-alpha: 1
1.0.0-alpaca vs. 1.0.0-beta: -1
1.0.0-final vs. 1.0.0-milestone: -7
1.0.0-alpha1 vs. 1.0.0-alpha2: -1
1.0.0-alpha5 vs. 1.0.0-alpha23: -1
1.0-RC5 vs. 1.0-RC20: -1
1.0-RC11 vs. 1.0-RC6: 1
1.0.0 vs. 1.0.0-SNAPSHOT: 1
1.0.0-SNAPSHOT vs. 1.0.0-SNAPSHOT: 0
1.0.0-SNAPSHOT vs. 1.0.0: -1
1.0.0 vs. 1.0.0-alpha5: 1
1.0.0-alpha5 vs. 1.0.0: -1
1.0.0-SNAPSHOT vs. 1.0.0-RC1: 1
1.0.0-SNAPSHOT vs. 1.0.1-RC1: -1
9.1-901.jdbc4 vs. 9.1-901.jdbc3: 1
9.1-901-1.jdbc4 vs. 9.1-901.jdbc4: -57
1-SNAPSHOT vs. 1.0-SNAPSHOT: 0
1-alpha vs. 1-alpha0: 0

(deftest maven-cases
  (testing "Numeric Comparison"
           (ok (= (vc:maven-vercmp "1.0.0" "1.0.0") 0))
           (ok (= (vc:maven-vercmp "1.0.0" "1.0") 0))
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


#+(or)
(rove:run-test *)



;(deftest
;  ^:version-comparison naive-cases
;  (testing "Naive comparison cases"
;    (are [v0 v1 r] (r ^java.lang.Integer (naive-vercmp v0 v1) 0)
;      ""             ""              .equals
;      "abc"          "abc"           .equals
;      "a.b.c"        "a_b-c"         .equals
;      "1.2.0"        "1.2"           >
;      "1.a"          "1.2"           <
;      "0100"         "100"           .equals
;      "0100.al0100"  "100.al100"     <
;      "1000.9.17"    "100.9.18"      >)))
;
;; The following test case versions list is taken from the file
;; "tests/test_version.py" of the `pypa/packaging` git repository, found here:
;; https://github.com/pypa/packaging The top of that file carries this copyright
;; notice:
;;
;; This file is dual licensed under the terms of the Apache License, Version
;; 2.0, and the BSD License. See the LICENSE file in the root of this repository
;; for complete details.
;;
;; The License associated with this list of version numbers can be found in
;; the file `licenses/LICENSE.python_test` at the root of this repository.
;
;(def python-versions [; Implicit epoch of 0
;                      "1.0.dev456", "1.0a1", "1.0a2.dev456", "1.0a12.dev456", "1.0a12",
;                      "1.0b1.dev456", "1.0b2", "1.0b2.post345.dev456", "1.0b2.post345",
;                      "1.0b2-346", "1.0c1.dev456", "1.0c1", "1.0rc2", "1.0c3", "1.0",
;                      "1.0.post456.dev34", "1.0.post456", "1.1.dev1", "1.2", "1.2+123abc",
;                      "1.2+123abc456", "1.2+abc", "1.2+abc123", "1.2+abc123def", "1.2+1234.abc",
;                      "1.2+123456", "1.2.r32+123456", "1.2.rev33+123456",
;
;    ; Explicit epoch of 1
;                      "1!1.0.dev456", "1!1.0a1", "1!1.0a2.dev456", "1!1.0a12.dev456", "1!1.0a12",
;                      "1!1.0b1.dev456", "1!1.0b2", "1!1.0b2.post345.dev456", "1!1.0b2.post345",
;                      "1!1.0b2-346", "1!1.0c1.dev456", "1!1.0c1", "1!1.0rc2", "1!1.0c3", "1!1.0",
;                      "1!1.0.post456.dev34", "1!1.0.post456", "1!1.1.dev1", "1!1.2+123abc",
;                      "1!1.2+123abc456", "1!1.2+abc", "1!1.2+abc123", "1!1.2+abc123def",
;                      "1!1.2+1234.abc", "1!1.2+123456", "1!1.2.r32+123456", "1!1.2.rev33+123456"])
;
;(defn test-python-pair [a b]
;  (testing (str
;            "python version `"
;            a
;            "` is less than `"
;            b
;            "`")
;    (is (< (python-vercmp a b) 0))
;    (is (> (python-vercmp b a) 0))
;    (is (= (python-vercmp a a) 0))))
;
;(deftest
;  ^:version-comparison python-cases
;  (testing "Python edge cases"
;    (are [v0 v1 r] (r ^java.lang.Integer (python-vercmp v0 v1) 0)
;      ""             ""              .equals
;      "1.2.3rc1"     "1.2.3RC1"      .equals
;      "1.0+foo0100"  "1.0+foo100"    <
;      "1.0+0100foo"  "1.0+100foo"    <
;      "1.0.a1"       "1.0a1"         .equals
;      "1.0.rc1"      "1.0rc1"        .equals
;      "1.0.b1"       "1.0b1"         .equals))
;  (test-python-pair (first python-versions)
;                    (peek python-versions))
;  (doseq [[a b] (map
;                 #(do [%1 %2])
;                 (pop python-versions)
;                 (rest python-versions))]
;    (test-python-pair a b)))
;
;; https://ruby-doc.org/stdlib-2.0.0/libdoc/rubygems/rdoc/Gem/Version.html
;(deftest
;  ^:version-comparison gem-cases
;  (testing "Gem Cases"
;    (are [v0 v1 r] (r ^java.lang.Integer (rubygem-vercmp v0 v1) 0)
;      ""                ""              .equals
;      "1.0.0"           "0.1.0"         >
;      "1.0.0"           "2.0.0"         <
;      "1.1.1"           "1.1.1"         .equals
;      "2.0.0"           "2.1.0"         <
;      "2.1.1"           "2.1.0"         >
;      "1.0.1"           "1.0"           >
;      "1.0.0"           "1.0.1"         <
;      "1.0.0"           "0.9.2"         >
;      "0.9.2"           "0.9.3"         <
;      "0.9.2"           "0.9.1"         >
;      "0.9.5"           "0.9.13"        <
;      "10.2.0.3.0"      "11.2.0.3.0"    <
;      "10.2.0.3.0"      "5.2.0.3.0"     >
;      "1.0"             "1"             >
;      "1.2.a"           "1.2"           <
;      "1.2.z"           "1.2"           <
;      "1.1.z"           "1.0"           >
;      "1.0.a10"         "1.0.a9"        >
;      "1.0"             "1.0.b1"        >
;      "1.0.a2"          "1.0.b1"        <
;      "1.0.a2"          "0.9"           >
;      "1.0.a.10"        "1.0.a10"       .equals
;      "1.0.a10"         "1.0.a.10"      .equals)))
;
;(deftest
;  ^:version-comparison semver-cases
;  (testing "Semver Cases"
;    (are [v0 v1 r] (r ^java.lang.Integer (semver-vercmp v0 v1) 0)
;      ""            ""                      .equals
;      "1.0.0+001"   "1.0.0+20130313144700"  .equals
;      "1.0.0+exp.sha.5114f85" "1.0.0"       .equals
;      "1.0.0-alpha" "1.0.0-alpha.1"         <
;      "1.0.0-alpha.1" "1.0.0-alpha.beta"    <
;      "1.0.0-alpha.beta" "1.0.0-beta"       <
;      "1.0.0-beta.2" "1.0.0-beta"           >
;      "1.0.0-beta.2" "1.0.0-beta.11"        <
;      "1.0.0-beta.11" "1.0.0-rc.1"          <
;      "1.0.0-rc.1"   "1.0.0"                <
;      "1.0.0"        "0.1.0"                >
;      "1.2.10"       "1.2.9"                >
;      "9.8.7"        "9.8.7+burarum"        .equals
;      "1.0.0"        "2.0.0"                <
;      "2.0.0"        "2.1.0"                <
;      "2.1.1"        "2.1.0"                >
;      "1.0.0"        "1.0.0-alpha"          >)))
;
;(deftest
;  ^:version-comparison rpm-cases
;  ; https://fedoraproject.org/wiki/Archive:Tools/RPM/VersionComparison
;  (testing "Random cases"
;    (are [v0 v1 r] (r ^java.lang.Integer (rpm-vercmp v0 v1) 0)
;      ""            ""          .equals
;      "1.2.3"       "1.2-3"     .equals
;      "1.0010"      "1.9"       >
;      "1.05"        "1.5"       .equals
;      "1.0"         "1"         >
;      "2.50"        "2.5"       >
;      "fc4"         "fc.4"      .equals
;      "FC5"         "fc4"       <
;      "2a"          "2.0"       <
;      "2.a"         "2a"        .equals
;      "1.0"         "1.fc4"     >
;      "3.0.0_fc"    "3.0.0.fc"  .equals
;      "~~"          "~~a"       <
;      "~"           "~~a"       >
;      "~"           ""          <
;      "a"           ""          >)))
;
;
