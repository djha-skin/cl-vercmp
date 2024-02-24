(defpackage #:skin.djha.vercmp
  (:use :cl
        :arrows)
  (:import-from #:uiop)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:import-from #:cl-ppcre-unicode)
  (:export
    #:*trumpc*
    #:debian-vercmp
    #:maven-normalize
    #:maven-vercmp)
  (:documentation
    "Version comparison library"))

(deftype version ()
  string)

(in-package #:skin.djha.vercmp)

(defparameter *trumpc* #\-)
(defparameter *ignore-after* #\+)

(defparameter *fillerc* (code-char 0)
  "
  Character to use when comparing non-numeric parts of the version strings.
  If one part is longer than the other, the shorter part will be logically
  extended with this character until they are both the same size.
  ")
(defparameter *fillers*
  (make-string 1 :initial-element *fillerc*))

(defun nonnumeric-part-compare
    (a b)
  "
  Compare two version parts which both consist entirely of
  non-digits (or are empty).
  "
  (declare (type string a b))
  (loop with maxlength = (max (length a) (length b))
        for i from 0 below maxlength
        for ca = (if (< i (length a))
                     (elt a i)
                     *fillerc*)
        for cb = (if (< i (length b))
                     (elt b i)
                     *fillerc*)
        do
        (let ((diff
                (cond ((char= ca cb) 0)
                      ((char= ca *trumpc*) -1)
                      ((char= cb *trumpc*) 1)
                      ((char= ca *fillerc*) -1)
                      ((char= cb *fillerc*) 1)
                      ((and (alpha-char-p ca)
                            (not (alpha-char-p cb))) -1)
                      ((and (alpha-char-p cb)
                            (not (alpha-char-p ca))) 1)
                      (:else
                       (- (char-code ca) (char-code cb))))))
          (when (not (zerop diff))
            (return diff)))
        finally (return 0)))

(defun numeric-part-compare
    (a b)
  "
  Compares two version parts, which both consist
  entirely of digits (or are empty).
  "
  (declare (type string a b))
  (let* ((trimmed-a (string-left-trim '(#\0) a))
        (trimmed-b (string-left-trim '(#\0) b))
        (ldiff (- (length trimmed-a)
                  (length trimmed-b))))
    (if (zerop ldiff)
        (cond ((string= trimmed-a trimmed-b) 0)
              ((eq trimmed-a *fillers*) -1)
              ((eq trimmed-b *fillers*) 1)
              ((string< trimmed-a trimmed-b) -1)
              (:else 1))
        ldiff)))

(defun version-part-compare (i a b)
  "
  Compare the `i`th part of the parts of version strings `a` and `b`, as found
  by `version-parts-split`.
  "
  (declare (type fixnum i)
           (type string a b))
  (let ((comparator (if (zerop (mod i 2))
                        #'nonnumeric-part-compare
                        #'numeric-part-compare)))
    (funcall comparator a b)))

(defun version-parts-split
    (version)
  "
  Split a version string up into its component parts, with digits in the `0,2,4,...`th
  spots and non-digits in the `1,3,5,...`th spots.
  "
  (declare (type string version))
  (loop
    with parts = (make-array 10
                             :element-type 'string
                             :initial-element ""
                             :adjustable t
                             :fill-pointer 0)
    for i = 0 then (1+ i)
    for check = (if (zerop (mod i 2))
                    #'digit-char-p
                    (complement #'digit-char-p))
    for scratch = version then (subseq scratch next-checked)
    for next-checked = (or
                         (position-if check scratch)
                         (length scratch))
    while (not (zerop (length scratch)))
    do
    (vector-push-extend (subseq scratch 0 next-checked) parts)
    finally (return parts)))

(defun epochless-vercmp (a b)
  "
  Compare two version numbers, assuming that neither has an epoch.
  "
  (declare (type string a b))
  (if (equal a b)
      0
      (let ((split-a (version-parts-split a))
            (split-b (version-parts-split b)))
          (loop with maxlength = (max (length split-a) (length split-b))
                for i from 0 below maxlength
                for sa = (if (< i (length split-a))
                            (elt split-a i)
                            *fillers*)
                for sb = (if (< i (length split-b))
                            (elt split-b i)
                            *fillers*)
                for cmp = (version-part-compare i sa sb)
                while (zerop cmp)
                finally (return cmp)))))


(defun epoch
    (a)
  "
  Extract the epoch from a version string.
  "
  (let ((found (position #\: a)))
    (if found
        (values (parse-integer (subseq a 0 found))
                (subseq a (1+ found)))
        (values 0 a))))

(defun without-build-info
    (a)
  "
  Extract build information from a version string.
  "
  (let ((found (position *ignore-after* a)))
    (if found
        (subseq a 0 found)
        a)))


(declaim (inline vercmp))
(defun vercmp
    (a b)
  "
  Compares two version numbers according to the rules laid out in the [Debian
  Policy
  Manual](https://www.debian.org/doc/gpolicy/ch-controlfields.html#s-f-Version),
  retrieved 2024-02-15.

  Epoch numbers, upstream versions, and revision version parts are fully
  supported.
  "
  (declare (type string a b))
  (multiple-value-bind (a-epoch a-vers) (epoch a)
    (multiple-value-bind (b-epoch b-vers) (epoch b)
      (if (= a-epoch b-epoch)
          (epochless-vercmp
            (without-build-info a-vers)
            (without-build-info b-vers))
          (- a-epoch b-epoch)))))


