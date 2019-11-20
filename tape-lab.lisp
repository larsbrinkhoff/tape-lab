;;;; tape-lab.lisp

(in-package #:tape-lab)

;; 9-track, 7-track
;; parity
;; SIMH, E11 image
;; big, little endian

(defvar *image* nil
  "A vector of octets.")

(defvar *tape* nil
  "A list of tape records, marks, and errors.")

(defvar *endian* nil
  "Little or big endian record length.")

(defvar *tracks* nil
  "Seven or nine track tape.")

(defvar *source*)
(defvar *index*)

(defun read-tape (file)
  (with-open-file (stream file :direction :input :element-type 'unsigned-byte)
    (let ((image (make-array (file-length stream)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      (loop for octet = (read-byte stream nil nil)
         for length from 0
         when (> length (* 100 1024 1024)) do (error "Size!")
         until (null octet)
         do (setf (aref image length) octet))
      (setq *image* image))))

(defun seventrackp (frame)
  (<= frame #o177))

(defun guess (&optional (image *image*))
  (let ((*source* image)
        (*index* 0))
    (setq *endian* :little)
    (let ((l1 (read-length)))
      (cond
        ((< l1 10000)
         (format t "~&Little endian~%"))
        (t
         (setq *endian* :big
               *index* 0)
         (setq l1 (read-length))
         (if (< l1 10000)
             (format t "~&Big endian~%")
             (error "Unknown endian"))))
      (let ((record (read-data l1)))
        (if (every #'seventrackp record)
            (progn (setq *tracks* 7) (format t "~&7-track~%"))
            (progn (setq *tracks* 9) (format t "~&9-track~%"))))
      (ecase *tracks*
        (7 (when (eq l1 (* 7 1024))
             (format t "~&ITS DUMP~%")))
        (9 (when (eq l1 (* 5 1024))
             (format t "~&ITS DUMP~%")))))))
               
(defun files (&optional (tape *tape*))
  (let ((file nil)
        (files nil))
    (loop for record in tape
       when (vectorp record) do (push record file)
       else do (push (nreverse file) files))
    (nreverse files)))

(defun records (&optional (image *image*))
  (let ((*source* image)
        (*index* 0))
    (setq *tape*
          (loop for record = (read-record)
             until (null record)
             collect record))))

(defun print-records (&optional (tape *tape*))
  (loop for item in tape
     do (cond ((vectorp item)
               (format t "~&Record: ~D frames~%" (length item)))
              ((eq item :mark)
               (format t "~&Tape mark~%"))
              ((eq item :error)
               (format t "~&Read error~%"))
              (t
               (error "Unknown tape item")))))

(defun read-frame ()
  (prog1 (aref *source* *index*)
    (incf *index*)))

(defun read-data (length)
  (if (zerop length)
      :mark
      (prog1 (make-array length
                         :element-type '(unsigned-byte 8)
                         :displaced-to *source*
                         :displaced-index-offset *index*)
        (incf *index* length))))

(defun read-length ()
  (let ((l (list (read-frame)
                 (read-frame)
                 (read-frame)
                 (read-frame))))
    (ecase *endian*
      (:little  (+ (ash (fourth l) 24)
                   (ash (third l) 16)
                   (ash (second l) 8)
                   (first l)))
      (:big     (+ (ash (first l) 24)
                   (ash (second l) 16)
                   (ash (third l) 8)
                   (fourth l))))))

(defun read-record ()
  (if (eql *index* (length *source*))
      nil
      (let* ((l1 (read-length))
             (x1 (unless (zerop (logand #x80000000 l1))
                   (return-from read-record :error)))
             (d (read-data l1))
             #|(x (unless (zerop (logand l1 1))
                  (read-frame)))|#
             (l2 (if (zerop l1)
                     0
                     (read-length))))
        (if (= l1 l2)
            d
            (error "Record length ~A ~A" l1 l2)))))

(setq *print-length* 100)
