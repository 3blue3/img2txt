;; #!/usr/bin/sbcl --script
;; Uncomment to run source as script


;; Ensure Quicklisp is available
(let ((quicklisp-init (merge-pathnames #p"quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "png" :silent t)
;(ql:quickload "cl-colors" :silent t)
;;
;; (require :uiop)
;; (require :cl-colors)
;; (require :png)

(defpackage :img2txt
  (:use :cl :png)
  (:export :image-to-ascii :img-to-txt :main))

(in-package :img2txt)

(defparameter *ascii-chars* " .,:;-¬~=±+*#%@"
  "List of ascii chars corresponding to a value of image luminance
in [0.0, 1.0] going from lightest to darkest.")

(defun map-to-ascii (luminance &key (chars *ascii-chars*))
  "Map a grayscale value (0.0-1.0) to an ASCII character from :CHARS."
  (let* ((len (length chars))
         (index  (min (1- len)
                      (floor (* len luminance)))))
    (char chars index)))

(defmacro pixel-to-float (value bit-depth)
  "Return a value in [0.0, 1.0] given VALUE of from 0 to 2^BIT-DEPTH-1."
  `(/ ,value (expt 2 ,bit-depth)))

(defmacro gsref (image x y)
  "Index pixel at X Y in a grayscale IMAGE"
  `(aref ,image ,y ,x 0))


(defmacro in-bounds (x y width height)
  `(and (< ,x ,width)  (> ,x 0)
       (< ,y ,height) (> ,y 0)))

(defun sample-pixel (x y width height)
  "Return all valid points around (X, Y) in a image of IMAGE x HEIGHT."
  (let* ((perms '((1 . 1) (1 . -1) (-1 . 1)
                 (0 . 1) (0 . -1)
                 (1 . 0) (-1 . 0)
                 (0 . 0)))
         (valid '()))

    (dolist (p perms)
      (when (in-bounds (car p) (cdr p) width height)
        (push p valid)))

    (cons `(,x . ,y) valid)))

(defun sum (l)
  (if (equal l '())
      0
      (+ (car l) (sum (cdr l)))))

(defun pixel-average (image x y)
  (let* ((width  (png:image-width image))
         (height (png:image-height image))
         (valid-points (sample-pixel x y width height))
         (pixels (mapcar (lambda (p)
                           (gsref image (car p) (cdr p)))
                         valid-points)))
    (/ (sum pixels) (length pixels))))

(defun image-to-ascii (file &key (average t) (width 10) (scale-y 0.53) (chars *ascii-chars*))
  "Convert an image to ASCII representation.

 :WIDTH is the character width out the output text.
 :SCALE-Y is a scaling factor applied to the y axis of the image
        to compensate for character height, preserving the image proportions.
 :CHARS is a list of characters which correspond to a value of luminance in [0.0,1.0].
        from the lowest value to largest.
"

  (unless chars
    (setq chars *ascii-chars*))

  (let* ((image        (png:decode-file file))
         (gsimage      (png:grayscale-image image))
         (bit-depth    (png:image-bit-depth gsimage))
         (image-width  (png:image-width gsimage))
         (image-height (png:image-height gsimage))
         (scale         (/ image-width width))
         (height (floor (/ (* scale-y image-height) scale) ))
         (output '()))

    (dotimes (y height)
      (let* ((row (make-string width)))
        (dotimes (x width)
          (let* ((src-x (floor (* x scale)))
                 (src-y (floor (* y scale))))
            ;; Should never happen
            (assert (and (< src-x image-width)
                         (< src-y image-height)))

            (let* ((p-val (if average
                              (pixel-average gsimage src-x src-y)
                              (gsref gsimage src-x src-y)))

                   (luminance (pixel-to-float p-val bit-depth))
                   (char      (map-to-ascii luminance :chars chars)))

              (setf (aref row x) char))))
        (push row output)))
     (nreverse output)))



(defun img-to-txt (file &key (width 80) (average t) (chars nil))
  "Given a at image at filepath FILE, return a string where each line
is WIDTH chars wide."
  (format nil "~a"
          (apply #'concatenate 'string
                 (mapcar (lambda (row)
                           (format nil "~a~%" row))
                         (image-to-ascii file
                                         :width width
                                         :chars chars
                                         :average average)))))




(defun parse-arg (key args &key (isbool nil))
  (let ((res (member key args :test #'equal)))
    (if res (if isbool t (nth 1 res))
        nil)))

(defun show-help (bin-name)
  (format t "~a~a"
           (format nil "Usage: ~a [ <filename> ] [ OPTION ... ]~%"  bin-name)
           (format nil
                   (concatenate
                    'string
                    "Options:~%"
                    " -a, --alphabet  String of characters used for luminance~%"
                    "                 from light -> dark.~%"
                    " -s, --sample    Sample method, one of: average~%"
                    " -f, --file      Path to image file, must exist if <filename>~%"
                    "                 isn't the 1st arg.~%"
                    " -c, --columns   The width of the text image (default 80)~%"
                    " -h, --help, --usage Show this message~%"))))

(defun main ()
  "Main function to process command-line arguments and generate ASCII art."
  (let* ((argv (uiop:raw-command-line-arguments))
         (argc (length argv))

         (arg-alpha (or (parse-arg "--alphabet" argv :isbool nil)
                      (parse-arg "-a" argv :isbool nil)))

         (arg-avg (or (parse-arg "--sample" argv :isbool nil)
                      (parse-arg "-s" argv :isbool nil)))

         (arg-file (or (parse-arg "--file" argv :isbool nil)
                       (parse-arg "-f" argv :isbool nil)))

         (arg-cols (or (parse-arg "--columns" argv :isbool nil)
                       (parse-arg "-c" argv :isbool nil))))



    (when (< argc 2)
      (format t "Not enough arguments, need a filename.")
      (show-help (car argv))
      (uiop:quit 1))

    (when (or (parse-arg "--help" argv :isbool t)
              (parse-arg "-h" argv :isbool t)
              (parse-arg "--usage" argv :isbool t))
      (show-help (car argv))
      (uiop:quit 0))

    (let ((image-path (or arg-file
                          (and (not (equal (aref (second argv) 0) #\-))
                               (second argv))
                          (progn  (format t "No filename given, exiting...")
                                  (show-help (car argv))
                                  (uiop:quit 1))))
          (width (if (> argc 2)
                     (or (and arg-cols (parse-integer arg-cols :junk-allowed t))
                         80))))
      (format t "~a"
              (img-to-txt image-path
                          :width width
                          :chars arg-alpha
                          :average (or (equal arg-avg nil)
                                       (equal "average" arg-avg))))
      (uiop:quit 0))))


(defun build-exec (&key (filepath "./img2txt"))
  "Build an executable for the script."
  (sb-ext:save-lisp-and-die filepath :toplevel 'main :executable t))

;; Run main function if executed as a script

;; Uncomment to build:
(build-exec)
;; Uncomment to run source (main)
