;; Uncomment to run source as script
;; #!/usr/bin/sbcl --script

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
  (:export :image-to-ascii :img-to-txt :main :build-exec))

(in-package :img2txt)

(defparameter *compiling?* uiop:*compile-check*)

(defparameter *ascii-chars* " .,-~¬;:÷=»+*{}[]?£#€§%@¶"
  "List of ascii chars corresponding to a value of image luminance
in [0.0, 1.0] going from lightest to darkest.")

(defparameter *default-column-width* 80
  "The default column width.")

(defun map-to-ascii (luminance &key (chars *ascii-chars*))
  "Map a grayscale `LUMINANCE' (0.0 - 1.0) to an ASCII character from :`CHARS'."
  (let* ((len (length chars))
         (index  (min (1- len)
                      (floor (* len luminance)))))
    (char chars index)))

(defmacro pixel-to-float (value bit-depth)
  "Return a value in [0.0, 1.0] given `VALUE' of from 0 to 2^`BIT-DEPTH'-1."
  `(/ ,value (expt 2 ,bit-depth)))

(defmacro gsref (image x y)
  "Index pixel at `X' `Y' in a grayscale `IMAGE'"
  `(aref ,image ,y ,x 0))


(defmacro in-bounds (x y width height)
  "Check if pixel at (`X', `Y') is in bounds in an image of `WIDTH' x `HEIGHT'."
  `(and (< ,x ,width)  (> ,x 0)
        (< ,y ,height) (> ,y 0)))

(defun pixel-average (image x y width height)
  "Compute the average in `IMAGE' of size `WIDTH' x `HEIGHT' of the pixels around
and on (`X', `Y')."

  (let* ((sum 0)
         (len 0)
         ;; Pixel positions relative to (x, y)
         (perms '((1 . 1)  (1 . -1) (-1 . 1)
                  (0 . 1)  (0 . -1)  (1 . 0)
                  (-1 . 0) (0 . 0))))

    ;; Compute average pixel luminance
    (dolist (p perms)
      (let ((x_ (+ x (car p)))
            (y_ (+ y (cdr p))))
        ;; (x, y) must always be in bounds, otherwise
        ;; assert is triggered previously.
        (when (in-bounds  x_ y_ width height)
          (incf sum (gsref image x_ y_))
          (incf len 1))))

    (/ sum len)))


(defun image-to-ascii (file &key (average t) (width 10) (scale-y 0.53) (chars *ascii-chars*))
  "Convert an image at path `FILE' to ASCII representation.

 :`WIDTH' is the character width out the output text.

 :`SCALE-Y' is a scaling factor applied to the y axis of the image. Used to
            compensate for character height, preserving the image proportions.

 :`CHARS' is a list of characters which correspond to a value of luminance
          in [0.0,1.0]. Chars are sorted such that the left most char
          represent the lowest value of the luminance, increasing towards
          the right.

 :`AVERAGE' if non-nil, compute the luminance of pixel through averaging."

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

            ;; This should never happen
            (assert (and (< src-x image-width)
                         (< src-y image-height)))

            (let* ((p-val (cond (average (pixel-average gsimage
                                  src-x src-y
                                  image-width image-height))

                                (t
                                 (gsref gsimage src-x src-y))))

                   (luminance (pixel-to-float p-val bit-depth))
                   (char      (map-to-ascii luminance :chars chars)))

              (setf (aref row x) char))))
        (push row output)))
     (nreverse output)))



(defun img-to-txt (file &key (width 80) (average t) (chars nil))
  "Given a at image at filepath `FILE', return a string where each line
is `WIDTH' chars wide.


 :`CHARS' is a list of characters which correspond to a value of luminance
          in [0.0,1.0]. Chars are sorted such that the left most char
          represent the lowest value of the luminance, increasing towards
          the right.

 :`AVERAGE' if non-nil, compute the luminance of pixel through averaging."

  (format nil "~a"

   (apply #'concatenate 'string
          (mapcar (lambda (row)
                    (format nil "~a~%" row))
                  (image-to-ascii file
                                  :width width
                                  :chars chars
                                  :average average)))))




(defun parse-arg (key args &key (isbool nil))
  "Parse a parameter `KEY' in `ARGS'.

 :`ISBOOL': If non-nil, then the parameter takes no argument.

Returns the parameters given value if `KEY' has been supplied,
otherwise nil. If `ISBOOL' is non-nil, then return t when it is
present."

  (let ((res (member key args :test #'equal)))
    (if res
        (if isbool
            t
            (nth 1 res))
        nil)))

(defun show-help (bin-name)
  "Show a help message for the binary executable with filename `BIN-NAME'."

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

    ;; A path cannot have been provided. Show help message
    (when (< argc 2)
      (format t "Not enough arguments, need a filename.")
      (show-help (car argv))
      (uiop:quit 1))

    ;; The user has specified that the help message is shown
    (when (or (parse-arg "--help" argv :isbool t)
              (parse-arg "-h" argv :isbool t)
              (parse-arg "--usage" argv :isbool t))
      (show-help (car argv))
      (uiop:quit 0))


    (let* ((image-path
             ;; Get the image path, either the first argument or explicitly
             ;; given using the '-f' or '--file' parameter.
             (or arg-file
                 (and (not (equal (aref (second argv) 0) #\-))
                      (second argv))
                 ;; If not show help message.
                 (progn  (format t "No filename given, exiting...")
                         (show-help (car argv))
                         (uiop:quit 1))))
           ;; Get the column width
           (width (if (> argc 2)
                      (or (and arg-cols
                               (parse-integer arg-cols :junk-allowed t))
                          *default-column-width*)))
           ;; Save image to a file?
           (save-image-path (or (parse-arg "-o" argv :isbool nil)
                                (parse-arg "--output" argv :isbool nil)))

           ;; Compute the text image
           (text-image
             (if (probe-file image-path)
                 (img-to-txt image-path
                             :width width
                             :chars arg-alpha
                             :average (equal "average" arg-avg))

                 (progn (format t "File does not exist, exiting...")
                        (uiop:quit 1)))))

      ;; Print the text image
      (if save-image-path
          (let ((file-path (merge-pathnames (pathname save-image-path))))
            (with-open-file (stream file-path
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
              (format stream "~a" text-image)))
          (format t "~a" text-image))

      (uiop:quit 0))))


(defun build-exec (&key (filepath "./img2txt"))
  "Build an executable for the script."
  (sb-ext:save-lisp-and-die filepath :toplevel 'main :executable t))

;; Uncomment to run source as script
;; (main)
