(in-package :trivial-glue-stream-asd)

(defpackage glue-stream
  (:use #:cl #:trivial-gray-streams)
  (:nicknames #:glue)
  (:export
    #:glue-stream #:make-glue-stream
    #:stream-read-byte #:stream-read-byte-sequence
    #:stream-read-char #:stream-read-char-sequence
    #:stream-read-sequence #:stream-read-string
    #:stream-read-special
    #:stream-write-byte #:stream-write-byte-sequence
    #:stream-write-char #:stream-write-char-sequence
    #:stream-write-sequence #:stream-write-string
    #:stream-write-special ))

(in-package :glue-stream)

(defclass glue-stream (fundamental-stream)
  ((in :initarg :input :accessor input)
   (input-input-type :initarg :i-i-type :accessor i-i-type)
   (input-output-type :initarg :i-o-type :accessor i-o-type)
   (out :initarg :output :accessor output)
   (output-input-type :initarg :o-i-type :accessor o-i-type)
   (output-output-type :initarg :o-o-type :accessor o-o-type) )
  (:default-initargs
    :i-i-type :binary
    :i-o-type :binary
    :o-i-type :binary
    :o-o-type :character ))

(defun make-glue-stream
    (input-stream output-stream
     &rest keys &key i-i-type i-o-type o-i-type o-o-type )
  (declare (ignore i-i-type i-o-type o-i-type o-o-type))
  (apply #'make-instance 'glue-stream
         :input input-stream :output output-stream
         keys ))

(defmethod pass ((stream glue-stream)
                 (o-i-type (eql :binary))
                 (i-o-type (eql :binary)) )
  (let ((elm (stream-read-byte (output stream))))
    (unless (eq elm :EOF)
      (stream-write-byte (input stream) elm) )))

(defmethod stream-write-char ((stream glue-stream) (c character))
  (unless (eq (o-o-type stream) :character)
    (error "stream-write-char not applicable") )
  (stream-write-char (output stream) c)
  (pass stream (o-i-type stream) (i-o-type stream)) )

(defmethod stream-write-char-sequence
    ((stream glue-stream) (sequence sequence)
     &optional (start 0) (end (length sequence)) )
  (let ((seq (subseq sequence start end)))
    (loop for i from start below end
          do (stream-write-char stream (elt seq i)) )))

(defmethod stream-write-sequence
    ((stream glue-stream) (sequence sequence) start end
     &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (ecase (o-o-type stream)
    (:binary (glue:stream-write-byte-sequence stream sequence start end))
    (:character (glue:stream-write-char-sequence stream sequence start end)) ))

(defmethod stream-write-string
    ((stream glue-stream) (string string) &optional start end)
  (stream-write-char-sequence stream string
    (or start 0) (or end (length string)) ))

(defmethod stream-write-byte ((stream glue-stream) (byte integer))
  (unless (eq (o-o-type stream) :binary)
    (error "stream-write-byte not applicable") )
  (stream-write-char (output stream) byte)
  (pass stream (o-i-type stream) (i-o-type stream)) )

(defmethod stream-write-byte-sequence
    ((stream glue-stream) (sequence sequence)
     &optional (start 0) (end (length sequence)) no-hang interactive)
  (declare (ignore no-hang interactive))
  (let* ((seq (subseq sequence start end))
         (len (length seq)) )
    (loop for i from 0 below len
          do (stream-write-byte stream (elt seq (+ i start))) )))

(defmethod stream-write-special
    ((stream glue-stream) (method function) &rest args)
  (apply method (output stream) args) )

(defmethod stream-read-char ((stream glue-stream))
  (unless (eq (i-i-type stream) :character)
    (error "stream-read-char not applicable") )
  (pass stream (o-i-type stream) (i-o-type stream))
  (stream-read-char (input stream)) )

(defmethod stream-read-byte ((stream glue-stream))
  (unless (eq (i-i-type stream) :binary)
    (error "stream-read-byte not applicable") )
  (pass stream (o-i-type stream) (i-o-type stream))
  (stream-read-byte (input stream)) )

(defmethod stream-read-special
    ((stream glue-stream) (method function) &rest args)
  (apply method (input stream) args) )

