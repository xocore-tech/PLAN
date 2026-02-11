(require "balance")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

;;;; Structs
;;;; =======
;;;;
;;;; This defines basic Wisp macros for declaring structs, which generates
;;;; the construct, accessors, and updaters.

(defun (sexp-check-step inp[ok err])
  (Or ok (Error inp)))

(defun (sexp-check checks val)
  (DeepSeq (map sexp-check-step checks)
    val))

(defun (is-caps-symbol x)
  (And (Nat x) ; is a non-zero number
    (strAll isUpper x)))

(defun (multi-bind e bs)
  (lfoldl {e [k v] -> (putenv k v 0 e)} e bs))

(defun (mkix i)
  (Case5 i Ix0 Ix1 Ix2 Ix3 (Ix i)))

(tests
  (Ix0    (mkix 0))
  (Ix1    (mkix 1))
  (Ix2    (mkix 2))
  (Ix3    (mkix 3))
  ((Ix 4) (mkix 4)))

(defun (field-getter i [nm ty])
  [(strWeld "." nm) (mkix i)])

(defun (field-setter i [nm ty])
  [(strWeld nm "=") (Up i)])

(defun (struct-getters fields)
  (lmapi field-getter (stream fields)))

(defun (struct-setters fields)
  (lmapi field-setter (stream fields)))

(defun (valid-field field[nm type])
  (And (Eq 2 (Sz field))
    (And (And (Nat nm) (Nat type))
      (strAll isLower nm))))

(defun (all-valid-fields fields)
  (And fields
    (And (Nil (Hd fields))
      (And (lall valid-field (stream fields))))))

(defmacro (defstruct e x[_ nm cnstr])
  (@ fields (drop 3 x))
  (sexp-check [[(is-caps-symbol cnstr) ("bad-cnstr" cnstr)]
               [(all-valid-fields fields) ("bad-fields" fields)]]
    [ (multi-bind e
        (lcons [cnstr 0]
          (lweld (struct-getters fields)
                 (struct-setters fields))))
     0]))


;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Bind BIND         ; Global Binding
  (key  Nat)                 ;   Name
  (val  Any)                 ;   Value
  (exp Expr))                ;   The expression which created the binding.

assert(planEq BIND 0)
assert(planEq .key Ix0)
assert(planEq .exp Ix2)
assert(planEq exp= (Up 2))

(def bindEx
  (BIND 5 "Hello World!" (1 "Hello World!")))

(tests
  5(.key bindEx)
  9(.key (key= 9 bindEx)))
