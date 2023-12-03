#|

datatype desc: https://nifti.nimh.nih.gov/nifti-1/documentation/faq#Q19
   unsigned char      = 8-bit byte     [     0..  255]
   signed char        = 8-bit byte     [  -128..  127]
   signed short       = 16-bit integer [-32768..32767]
   unsigned short     = 16-bit integer [     0..65535]
   signed int         = 32-bit integer
   unsigned int       = 32-bit integer
   signed long long   = 64-bit integer
   unsigned long long = 64-bit integer

   float              = 32-bit floating point value
   double             = 64-bit floating point value
   long double        = 128-bit floating point value

#define NIFTI_TYPE_UINT8           2 /! unsigned char. /
#define NIFTI_TYPE_INT16           4 /! signed short. /
#define NIFTI_TYPE_INT32           8 /! signed int. /
#define NIFTI_TYPE_FLOAT32        16 /! 32 bit float. /
#define NIFTI_TYPE_COMPLEX64      32 /! 64 bit complex = 2 32 bit floats. /
#define NIFTI_TYPE_FLOAT64        64 /! 64 bit float = double. /
#define NIFTI_TYPE_RGB24         128 /! 3 8 bit bytes. /
#define NIFTI_TYPE_INT8          256 /! signed char. /
#define NIFTI_TYPE_UINT16        512 /! unsigned short. /
#define NIFTI_TYPE_UINT32        768 /! unsigned int. /
#define NIFTI_TYPE_INT64        1024 /! signed long long. /
#define NIFTI_TYPE_UINT64       1280 /! unsigned long long. /
#define NIFTI_TYPE_FLOAT128     1536 /! 128 bit float = long double. /
#define NIFTI_TYPE_COMPLEX128   1792 /! 128 bit complex = 2 64 bit floats. /
#define NIFTI_TYPE_COMPLEX256   2048 /! 256 bit complex = 2 128 bit floats /


	'a'=>1,  8
	'c'=>1,  8
	's'=>2, 16
	'l'=>4, 32
	'f'=>4, 32

cf.
  nifti_tool -disp_hdr -infile test.nii
  fslhd test.nii
  nifti_tool -disp_exts  -input test.nii

sizeof_hdr     0    0  4 l,val=>348},
data_type      1    4 10 c,length=>10
db_name        2   14 18 c,length=>18
extents        3   32  4 l
session_error  4   36  2 s
regular        5   38  1 c
dim_info       6   39  1 c
dim            7   40 16 s,count=>8
intent_p1      8   56  4 f
intent_p2      9   60  4 f
intent_p3      10  64  4 f
intent_code    11  68  2 s,val=>4001
datatype       12  70  2 s
bitpix         13  72  2 s
slice_start    14  74  2 s
pixdim         15  76 32 f,count=>8,;nifti_dims,x,y,z,t,te,chs,],val=>[8,1,1,1,1,1,1,1,1,],
vox_offset     16 108  4 f,val=>352 ;; start of image data
scl_slope      17 112  4 f
scl_inter      18 116  4 f
slice_end      19 120  2 s,key=>z
slice_code     20 122  1 c
xyzt_units     21 123  1 c,val=>10}, # 8 = sec, 2=mm
cal_max        22 124  4 f
cal_min        23 128  4 f
slice_duration 24 132  4 f
toffset        25 136  4 f
glmax          26 140  4 l
glmin          27 144  4 l
descrip        28 148 80 a,length=>80,
aux_file       29 228 24 a,length=>24
qform_code     30 252  2 s
sform_code     31 254  2 s
quatern_b      32 256  4 f
quatern_c      33 260  4 f
quatern_d      34 264  4 f
quatern_x      35 268  4 f
quatern_y      36 272  4 f
quatern_z      37 276  4 f
srow_x         38 280 16 f,count=>4
srow_y         39 296 16 f,count=>4
srow_z         40 312 16 f,count=>4
intent_name    41 328 16 a,length=>16
magic          42 344  4 a,length=>4,val=>"n+1"
extension      43 348  4 c,count=>4
#imag        undef, # image data

|#

#|
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
|#

(ql:quickload 'ieee-floats)
(ql:quickload 'lisp-binary)

(defun read-bytes-elt (inseq)
  "Bytes from array into numeric values (uint8? little-end?)."
  (if (not (elt inseq 0)) (return-from read-bytes-elt nil))
  (let ((u 0)
        (n (length inseq)))
    (dotimes (i n)
      (setf (ldb (byte 8 (* i 8)) u)
            (elt inseq i)))
    u))


(defun read-part (inseq start bytesize n)
  "read octet inseq using bytesize intervals"
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
(defvar *niihead* (make-array 352 :initial-element nil)
  "nifti header. first 348+4 bytes")
(defvar *data* nil "vector of matrix data")
;; (with-open-file (s "test.nii") (file-length s)) ; 252640
(with-open-file (s "./test.nii" :element-type 'unsigned-byte)
  ;; (read-byte s nil 'eof )
  (read-sequence *niihead* s)             ; read up to 352
  (read-part *niihead* 0 4 1)             ; header size is 348.
  (read-bytes-elt (subseq *niihead* 0 4)) ; same thing

  (read-part *niihead* 108  1 4)


  ;; if this is > 8 (max dims) the enconding needs to be flipped
  (read-bytes-elt (subseq *niihead* 40 42)) ; 3

  ;; dims
  (read-part *niihead* 42 2 8)          ; 50 50 50 1 1 1 1 0

  ;; total size


  ;; total elements in matrix
  (let ((dims (read-part *niihead* 42 2 8))
        (ndim (read-bytes-elt (subseq *niihead* 40 42))))
    (reduce #'* (subseq dims 0 ndim)))  ;125000

  ;; datatype
  (read-bytes-elt (subseq *niihead* 70 72)) ; 4
  (read-part *niihead* 70 2 1)              ; #(4)


  ;; vox offset -- must be 352 or greater (if magic=n+1 [img in ssame file as hdr] )
  ;;  vox_offset should be an integer multiple of 16
  (setq voxoffset (floor (ieee-floats:decode-float32
                          (read-bytes-elt
                           (subseq *niihead* 108 112))))) ; 2640

  (read-bytes-elt (subseq *niihead* 108 112))

  ;; read in rest of data
  (let*
      ((len (file-length s))
       (voxoffset (floor
                   (ieee-floats:decode-float32
                    (read-bytes-elt
                     (subseq *niihead* 108 112)))))
        (total-dims (read-bytes-elt (subseq *niihead* 40 42)))
        (dims (read-part *niihead* 42 2 8))
        (image-size (reduce '* (subseq dims 0 total-dims))))
    (file-position s voxoffset)

    ;; TODO: rewrite to read whole file using lisp-binary
    (lisp-binary:defbinary image-part
        (:export t :byte-order :little-endian)
        (image #() :type (simple-array (unsigned-byte 8) (image-size))))
    (setq *data* (lisp-binary:read-binary 'image-part s))
    ;; (setq *data* (loop for int = (read-bytes-part s 2 2)
    ;;                    while int
    ;;                    collect int))
    t

    ;; (setq *data*  (read-bytes-part s  (- voxoffset (file-position s)) 200))
    ))

(reduce '+ (slot-value *data* 'image))


(setq *td* nil)
(with-open-file (s "./test.nii" :element-type 'unsigned-byte)
  (setq *td* (lisp-binary:read-binary 'nifti s))
)

;(with-open-file (s "../../work/rosetta-nii/wf-mp2rage-7t_2017087.nii.gz" :element-type 'unsigned-byte) (setq *td* (lisp-binary:read-binary 'nifti s)))
(lisp-binary:defbinary nifti
    (:export t :byte-order :little-endian)
    (sizeof_hdr 0 :type (unsigned-byte 32)) ; must be 348 (end is 352)
    (data_type #() :type (simple-array (unsigned-byte 8) (10)))
    (db_name   #() :type (simple-array (unsigned-byte 8) (18)))
    (extents 0 :type (unsigned-byte 32))
    (session_error 0 :type (unsigned-byte 16))
    (regular 0 :type (unsigned-byte 8))
    (dim_info 0 :type (unsigned-byte 8))
    (dim 0  :type (simple-array (unsigned-byte 16) (8)))
    (intent_p1 0 :type (unsigned-byte 32))
    (intent_p2 0 :type (unsigned-byte 32))
    (intent_p3 0 :type (unsigned-byte 32))
    (intent_code 0 :type (unsigned-byte 16))
    (datatype  0 :type (unsigned-byte 16))
    ; bits per voxel; total bytes = prod(dim)* bitpix / 8
    (bitpix 0 :type (unsigned-byte 16))
    (slice_start 0 :type (unsigned-byte 16))
    ;; something wrong here?
    (pixdim #() :type (simple-array float (8)))
    ;; start of image data (>= 352) ;; 2640 in test.nii
    (vox_offset 0 :type float)
)

; here
    (scl_slope)      17 112  4 f
    (scl_inter)      18 116  4 f
    (slice_end)      19 120  2 s,key=>z
    () slice_code     20 122  1 c
    () xyzt_units     21 123  1 c,val=>10}, # 8 = sec, 2=mm
    () cal_max        22 124  4 f
    () cal_min        23 128  4 f
    () slice_duration 24 132  4 f
    () toffset        25 136  4 f
    () glmax          26 140  4 l
    () glmin          27 144  4 l
    () descrip        28 148 80 a,length=>80,
    () aux_file       29 228 24 a,length=>24
    () qform_code     30 252  2 s
    () sform_code     31 254  2 s
    () quatern_b      32 256  4 f
    () quatern_c      33 260  4 f
    () quatern_d      34 264  4 f
    () quatern_x      35 268  4 f
    () quatern_y      36 272  4 f
    () quatern_z      37 276  4 f
    () srow_x         38 280 16 f,count=>4
    () srow_y         39 296 16 f,count=>4
    () srow_z         40 312 16 f,count=>4
    () intent_name    41 328 16 a,length=>16
    () magic          42 344  4 a,length=>4,val=>"n+1"
    () extension      43 348  4 c,count=>4
    (#imag undef, # image data)

(image #() :type (simple-array (unsigned-byte 8) (image-size))))
