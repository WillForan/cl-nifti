(defun as-keyword (sym) "symbol to keyword"
  (intern (string sym) :keyword))
(defun ->class-slot (spec)
  "from gitamonkeys: quickly write class slot w/intarg and accessor"
    (let ((name (first spec)))
      `(,name :initarg ,(as-keyword name) :accessor ,name)))

;; class
(defmacro binclass (name slots)
 `(defclass ,name () ,(mapcar #'->class-slot slots)))

;; reading
(defgeneric read-value (type stream &key)
  (:documentation "read off a stream by given type"))
(defmethod read-value ((type (eql 'u1) in &key)) (print "hi u1"))



(defun read-u2-elt (in)
  "less generic form of below."
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (elt in 0))
    (setf (ldb (byte 8 8) u2) (elt in 1))
    u2))

(defun read-bytes-elt (inseq)
  "Bytes from array into numeric values."
  (let ((u 0)
        (n (length inseq)))
    (dotimes (i n)
      (setf (ldb (byte 8 (* i 8)) u)
            (elt inseq i)))
    u))


(defun read-part (inseq start bytesize n)
  "read directly from stream using bytesize intervals"
  (let ((fullarr (make-array n :initial-element nil)))
    (dotimes (i n)
      (let ((starton (+ start (* i bytesize))))
        (setf (elt fullarr i)
              (read-bytes-elt (subseq inseq starton (+ starton bytesize))))))
    fullarr))

(defun read-bytes-part (s bytesize n)
  "read directly from stream using bytesize intervals"
  (let ((fullarr (make-array n :initial-element nil)))
    (dotimes (i n)
      (let ((arr (make-array bytesize :initial-element nil)))
        (read-sequence arr s)
        (setf (elt fullarr i) (read-bytes-elt arr))))
    fullarr))

;; (read-bytes-elt (subseq *niihead* 0 7)) == 348 ; vox_offset (start) is 352
;; (read-bytes-elt (subseq *niihead* 40 42)) == 3
(defvar *niihead* (make-array 352 :initial-element nil) "nifti header. first 348 bytes")
(defvar *data* nil "vector of matrix data")
(with-open-file (s "./test.nii" :element-type 'unsigned-byte)
  ;; (read-byte s nil 'eof )
  (read-sequence *niihead* s)           ; read up to 352
  (read-part *niihead* 0 4 1)           ; header size is 348.
  (read-bytes-elt (subseq *niihead* 0 4)) ; same thing
  ;; offset -- should be 352
  (read-bytes-elt (subseq *niihead* 114 118))
  (read-part *niihead* 106  1 20)           ; header size is 348


  ;; if this is > 8 (max dims) the enconding needs to be flipped
  (read-bytes-elt (subseq *niihead* 40 42)) ; 3

  ;; dims
  (read-part *niihead* 42 2 8)              ; 50 50 50 1 1 1 1 0


  ;; total elements in matrix
  (let ((dims (read-part *niihead* 42 2 8))
        (ndim (read-bytes-elt (subseq *niihead* 40 42))))
    (reduce #'* (subseq dims 0 ndim)))  ;125000

  ;; datatype
  (read-bytes-elt (subseq *niihead* 70 72)) ; 4
  (read-part *niihead* 70 2 1)              ; #(4)

  ;; read in rest of data
  (print (file-position s))
  ;; doesn't work!? offset is wrong?
  (setq *data*  (read-bytes-part s 1 125000)))

;; 8*s f f f s s
;; 16  4 4 4 2 2


;vox_offset: 42 8*2 4 4 4 2 2 2 2 4*8 => 114 118
; 'dim'           =>      {nr=>7,type=>'s',count=>8},
;        'intent_p1'     =>      {nr=>8,type=>'f'},
;        'intent_p2'     =>      {nr=>9,type=>'f'},
;        'intent_p3'     =>      {nr=>10,type=>'f'},
;        'intent_code'   =>      {nr=>11,type=>'s',val=>'4001'},
;        'datatype'      =>      {nr=>12,type=>'s'},
;        'bitpix'        =>      {nr=>13,type=>'s'},
;        'slice_start'   =>      {nr=>14,type=>'s',val=>0},
;        'pixdim'        =>      {nr=>15,type=>'f',count=>8,
;                key=>['nifti_dims','x','y','z','t','te','chs',''],
;                val=>[8,1,1,1,1,1,1,1,1,],
;                },
;        'vox_offset'    =>      {nr=>16,type=>'f',val=>352}
