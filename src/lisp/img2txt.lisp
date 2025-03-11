;; Uncomment to run source as script
;; #!/usr/bin/sbcl --script

;; Ensure Quicklisp is available
(let ((quicklisp-init (merge-pathnames #p"quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)

      ;; Install Quicklisp automatically if it doesn't exist.
      (progn
        (format t "Quicklisp not found. Installing...~%")

        (let ((quicklisp-url "https://beta.quicklisp.org/quicklisp.lisp"))
          ;; Install quicklisp
          (uiop:run-program `("curl" "-O" ,quicklisp-url))
          ;; Initialize and load it
          (load "quicklisp.lisp")
          (funcall (intern "INSTALL" "QUICKLISP"))
          (funcall (intern "QL:ADD-TO-INIT-FILE" "QUICKLISP"))
          (load quicklisp-init)))))

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
(defparameter *default-sample-method* 'normal)

(defparameter *default-scale* 53 "Default Y-scale")

(defparameter *ascii-chars* " .,-~¬;:÷=»+*}]?£#€§%@"
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
  `(and (< ,x ,width)  (>= ,x 0)
        (< ,y ,height) (>= ,y 0)))

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



(defparameter *relative-lines*
  '(((-1 . 1)  (0 . 1)  (1 . 1)) ;; Top
   ((-1 . 0)  (0 . 0)  (1 . 0)) ;; Middle
   ((-1 . -1) (0 . -1) (1 . -1));; Bottom
   ;; Diagonal lines
   ((1 . -1)  (0 . 0) (-1 . 1)) ;; [\]
   ((-1 . 1)  (0 . 0) (1 . -1)) ;; [/]
   ;; Vertical lines
   ((1 . -1)  (1 . 0)  (1 . 1))    ;; Left
   ((0 . -1)  (0 . 0)  (0 . 1))    ;; Middle
   ((-1 . -1) (-1 . 0) (-1 . 1)))) ;; Right

(defparameter *3x3-points*
  '((1 . 1)  (1 . -1) (-1 . 1)
    (0 . 1)  (0 . -1)  (1 . 0)
    (-1 . 0) (0 . 0)))

(defun pixel-average (image x y width height)
  "Compute the average in `IMAGE' of size `WIDTH' x `HEIGHT' of the pixels around
and on (`X', `Y')."

  (let* ((sum 0)
         (len 0)
         ;; Pixel positions relative to (x, y)
         (perms '((1 . 1)  (1 . -1) (-1 . 1)
                  (0 . 1)  (0 . -1)  (1 . 0)
                  (-1 . 0) (0 . 0)))
         (valid-pixels '())
         (color-len 0)
         (line-sum 0))

    ;; Compute average pixel luminance
    (dolist (p perms)
      (let ((x_ (+ x (car p)))
            (y_ (+ y (cdr p))))
        ;; (x, y) must always be in bounds, otherwise
        ;; assert is triggered previously.
        (when (in-bounds  x_ y_ width height)
          (push (cons p (gsref image x_ y_)) valid-pixels)
          (incf sum   (gsref image x_ y_))
          (incf len 1))))


    (dolist (line *relative-lines*)
      (let* ((cline
               (catch 'back
                 (loop :for pixel :in valid-pixels
                       :if (not (member (car pixel)
                                        line :test #'equal))
                       :do (throw 'back nil)
                     :else
                       :collect (cdr pixel)))))

        (when (= 3 (length cline))
          (incf line-sum
                (/ (+ (car cline)
                      (cadr cline)
                      (caddr cline))
                   3))
          (incf color-len 1))))

    (/ line-sum color-len)))


;; Checks if two points form a diagonal line
(defmacro check-diagonal (a b)
  "Return T if points A and B form a diagonal line."
  `(or (and (equal ,a '(1 . 1)) (equal ,b '(-1 . -1)))
       (and (equal ,b '(1 . 1)) (equal ,a '(-1 . -1)))
       (and (equal ,a '(1 . -1)) (equal ,b '(-1 . 1)))
       (and (equal ,b '(1 . -1)) (equal ,a '(-1 . 1)))))

;; Checks if three points form a horizontal line
(defmacro check-horizontal (a b c)
  "Return T if A, B, and C form a horizontal line."
  `(and (equal (car ,a) (car ,b))
        (equal (car ,b) (car ,c))))

;; Checks if three points form a vertical line
(defmacro check-vertical (a b c)
  "Return T if A, B, and C form a vertical line."
  `(and (equal (cdr ,a) (cdr ,b))
        (equal (cdr ,b) (cdr ,c))))

(defun check-line (a b c)
  "Check if A, B, C form a horizontal, vertical, or diagonal line."
  (let ((not-origo '())
        (has-origo nil))
    (or
     (check-vertical a b c)
     (check-horizontal a b c)
     (progn
       (dolist (p `(,a ,b ,c))
         (if (not (equal p '(0 . 0)))
             (push p not-origo)
             (setq has-origo t)))
       (if (and has-origo (= (length not-origo) 2))
           (check-diagonal (nth 0 not-origo) (nth 1 not-origo))
           nil)))))

;; Computes absolute difference in pixel intensity
(defun color-distance (c1 c2)
  (abs (- c1 c2)))

;; Checks if the three points in a line have similar colors
(defun similar-color (line &key (similarity 20))
  "Return T if all pixels in LINE have similar color."
  (let ((a (nth 0 line))
        (b (nth 1 line))
        (c (nth 2 line)))

    (and (< 0 (color-distance (nth 1 a) (nth 1 b)) similarity)
         (< 0 (color-distance (nth 1 a) (nth 1 c)) similarity)
         (< 0 (color-distance (nth 1 b) (nth 1 c)) similarity))))

;; Wrapper for `similar-color`
(defun similar-colorp (line &key (similarity 20))
  "Checks if three pixel values A, B, and C are similar."
  (similar-color line :similarity similarity))

(defun pixel-line (image x y width height &key  (intensify 0.8) (simularity 3))
  "Check if three pixels around (`X', `Y`) form a straight line.
   Returns the average intensity if they form a line, otherwise the original pixel."
  (let* ((valid-points '())
         (valid-lines '())
         (valid-pixels '()))

    ;; Gather valid pixels
    (dolist (p *3x3-points*)
      (let ((x_ (+ x (car p)))
            (y_ (+ y (cdr p))))
        (when (in-bounds x_ y_ width height)
          (push p valid-points)
          (setq valid-pixels (append (list p (gsref image x_ y_))
                                     valid-pixels)))))

          ;; Search for valid lines

    (dolist (line *relative-lines*)
      (let ((is-valid? t)
            (line-w-color '()))

        (catch 'no-line
          ;; Go through each point in the line, check if its in the list
          ;; of valid points.
          (dolist (point line)
            (let ((pixel (member point valid-pixels :test #'equal)))
              (if (and pixel (nth 0 pixel))
                  (progn
                    ;; If it is, push the luminanceit to `line-w-color'
                    (push (list (nth 0 pixel) (nth 1 pixel)) line-w-color))
                  (progn (setq is-valid? nil)
                         (throw 'no-line t))))))

        ;; If a valid line is found and colors match, return the average intensity
        (when (and is-valid?
                   ;;                         (apply #'check-line (mapcar #'car line-w-color))
                   (similar-colorp line-w-color :similarity simularity))
          (push line-w-color valid-lines))))

    (if (= 1 (length valid-lines))
        ;; Only one line should be able to exist, since
        ;; a plain dark square could otherwise be counted
        ;; as a line
        (let ((sum (reduce #'+ (mapcar #'(lambda (p) (nth 1 p)) (car valid-lines)))))
          (max 255 (* intensify (floor (/ sum 3)))))

        ;; If no valid line is found
        ;; return the original pixel value
        (gsref image x y))))

(defun image-to-ascii (file &key (sample *default-sample-method*) (width 10) (scale-y 0.53) (chars *ascii-chars*))
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
         (height (floor (/ (* scale-y image-height) scale)))
         (output '()))

    (dotimes (y height)
      (let* ((row (make-string width))
             (src-y (floor (* y scale))))

        (when  (< src-y image-height)
          (dotimes (x width)
            ;; Compute position for the sampling of characters
            ;; spaced out width the distance of `SCALE'.
            (let ((src-x (floor (* x scale))))
              (when (< src-x image-width)
                ;; This should never happen

                (let* ((p-val (case sample
                                (average (pixel-average gsimage src-x src-y
                                                        image-width image-height))

                                (normal (gsref gsimage src-x src-y))

                                (lines (pixel-line gsimage src-x src-y
                                                   image-width image-height))

                                (_      (gsref gsimage src-x src-y))))

                       (luminance (pixel-to-float p-val bit-depth))
                       (char      (map-to-ascii luminance :chars chars)))

                  (setf (aref row x) char)))))
          (push row output))))
    (nreverse output)))



(defun img-to-txt
    (file &key
            (width *default-column-width*)
            (sample *default-sample-method*)
            (scale-y *default-scale*)
            (chars *ascii-chars*))
  "Given a at image at filepath `FILE', return a string where each line
is `WIDTH' chars wide.


 :`CHARS' is a list of characters which correspond to a value of luminance
          in [0.0,1.0]. Chars are sorted such that the left most char
          represent the lowest value of the luminance, increasing towards
          the right.

 :`AVERAGE' if non-nil, compute the luminance of pixel through averaging."


  (apply #'concatenate 'string
         (mapcar (lambda (row)
                   (format nil "~a~%" row))
                 (image-to-ascii file
                                 :scale-y scale-y
                                 :width width
                                 :chars chars
                                 :sample sample))))




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
                    (format
                     nil
                     " -s, --sample    Sample method, one of: average, normal (default ~a)~%"
                     *default-sample-method*)
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


         (arg-intfy (or (parse-arg "--intensify" argv :isbool nil)
                        (parse-arg "-i" argv :isbool nil)))

         (arg-avg (or (parse-arg "--sample" argv :isbool nil)
                      (parse-arg "-s" argv :isbool nil)))

         (arg-file (or (parse-arg "--file" argv :isbool nil)
                       (parse-arg "-f" argv :isbool nil)))

         (arg-scale (or (parse-arg "--scale" argv :isbool nil)
                       (parse-arg "-S" argv :isbool nil)))

         (arg-cols (or (parse-arg "--columns" argv :isbool nil)
                       (parse-arg "-c" argv :isbool nil))))

    ;; A path cannot have been provided. Show help message
    (when (< argc 2)
      (format t "Not enough arguments, need a filename.~%")
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
                 (progn  (format t "No filename given, exiting...~%")
                         (show-help (car argv))
                         (uiop:quit 1))))
           ;; Get the column width
           (width (if (> argc 2)
                      (or (and arg-cols
                               (parse-integer arg-cols :junk-allowed t))
                          *default-column-width*)))
           (scale-y (/  (or (and arg-scale
                                 (parse-integer arg-scale))
                            *default-scale*)
                        100))
           ;; Save image to a file?
           (save-image-path (or (parse-arg "-o" argv :isbool nil)
                                (parse-arg "--output" argv :isbool nil)))

           ;; Compute the text image
           (text-image
             (if (probe-file image-path)
                 (progn

                   (when (or (<= scale-y 0) (> scale-y 100))
                     (format t "Scale must be between 0 and 100~%")
                     (uiop:quit 1))

                   (img-to-txt image-path
                             :width width
                             :scale-y scale-y
                             :chars arg-alpha
                             :sample (or (and (equal "average" arg-avg) 'average)
                                         (and (equal "normal" arg-avg) 'normal)
                                         (and (equal "lines" arg-avg) 'lines)
                                         *default-sample-method*))
                 )

                 (progn (format t "File does not exist, exiting...~%")
                        (uiop:quit 1)))))

      ;; Print the text image
      (if save-image-path
          (let ((file-path (merge-pathnames (pathname save-image-path))))
            (with-open-file
                (stream file-path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
              (format stream "~a" text-image)))
          (format t "~a" text-image))

      (uiop:quit 0))))


(defun build-exec (&key (filepath "./img2txt"))
  "Build an executable for the script."
  (sb-ext:save-lisp-and-die filepath
     :toplevel 'main
     :executable t))

;; Uncomment to run source as script
;; (main)
