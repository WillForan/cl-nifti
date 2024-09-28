(defpackage :pdl-io-nifti
  (:use :common-lisp :pdl :pdl-io-flexraw))

(in-package :pdl-io-nifti)

(defvar *template* "ic10c18isCCs8fffssssf8fffsccffffiic80c24ssfffffff4f4f4c16c4")

(defvar *byte-order* "")

(defvar *version* "0.73")

(defvar *force* 0)

(defvar *sizes*
  '((c . 1)
    (C . 1)
    (l . 4)
    (S . 2)
    (s . 2)
    (L . 4)
    (f . 4)
    (d . 8)
    (q . 8)
    (a . 1)
    (A . 1)))

(defvar *map-pdltypes*
  '((byte . 2)
    (ushort . 256)
    (short . 4)
    (long . 8)
    (float . 16)
    (double . 64)
    (longlong . 1024)))

(defvar *bitsize*
  '((0 . 8)
    (2 . 16)
    (1 . 16)
    (3 . 32)
    (5 . 32)
    (6 . 64)
    (4 . 64)))

(defvar *map-pdltypes-complex*
  '((5 . 32)
    (6 . 1792)))

(defvar *maptypes*
  '((16 . (float 1 0 f))
    (32 . (float 2 0 f))
    (64 . (double 1 0 d))
    (2 . (byte 1 0 C))
    (4 . (short 1 0 s))
    (8 . (long 1))
    (128 . (byte 3))
    (256 . (short 1 1 s c))
    (512 . (ushort 1))
    (768 . (longlong 1 1 q L))
    (1024 . (longlong 1))
    (1280 . (double 1 2 d Q))
    (1536 . (double 1 2 d D))
    (1792 . (double 2))
    (2048 . (double 2 2 d D))
    (2304 . (byte 4))))

(defvar *farray* (make-array 0 :element-type 'float))

(defvar *fields*
  '((sizeof-hdr . ((nr . 0) (type . l) (val . 348)))
    (data-type . ((nr . 1) (type . c) (length . 10) (val . "")))
    (db-name . ((nr . 2) (type . c) (length . 18) (val . "")))
    (extents . ((nr . 3) (type . l) (val . 0)))
    (session-error . ((nr . 4) (type . s) (val . 0)))
    (regular . ((nr . 5) (type . c) (val . "r")))
    (dim-info . ((nr . 6) (type . C)))))
