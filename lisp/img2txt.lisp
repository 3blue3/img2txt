;; Uncomment to run source as script
;; #!/usr/bin/sbcl --script

;;; Code:

;;;; ###########################################################################
;;;; #                   LOAD AND OR INSTALL QUICKLISP                         #
;;;; ###########################################################################



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




;;;; ###########################################################################
;;;; #                                  IMG2TXT                                #
;;;; ###########################################################################

(defpackage :img2txt
  (:use :cl :png)
  (:export :image-to-ascii :img-to-txt :main :build-exec))

(in-package :img2txt)

(defparameter *default-intensify* 0.8
  "In `pixel-line' when a line has been detected, multiply the lines average by this factor.")

(defparameter *default-simularity* 3
  "In `pixel-line', this variable defines the difference in grayscale pixel
 value which all pixels of a line must share.")

(defparameter *default-sample-method* 'normal)

(defparameter *default-scale* (/ 100 57)
  "Default scale factor applied to the y-axis.")

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



;;;;; ########## SAMPLING METHOD: LINES ##########



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
(defun similar-color (line &key (similarity *default-simularity*))
  "Return T if all pixels in LINE have similar color."

  (let ((a (nth 0 line))
        (b (nth 1 line))
        (c (nth 2 line)))

    (and (< 0 (color-distance (nth 1 a) (nth 1 b)) similarity)
         (< 0 (color-distance (nth 1 a) (nth 1 c)) similarity)
         (< 0 (color-distance (nth 1 b) (nth 1 c)) similarity))))

;; Wrapper for `similar-color`
(defun similar-colorp (line &key (similarity *default-simularity*))
  "Checks if three pixel values A, B, and C are similar."
  (similar-color line :similarity similarity))

(defun pixel-line
    (image x y width height
     &key
       (intensify *default-intensify*)
       (simularity *default-simularity*))

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
                   ;; Check if
                   (similar-colorp line-w-color :similarity simularity))

          (push line-w-color valid-lines))))

    (if (= 1 (length valid-lines))
        ;; Only one line should be able to exist, since
        ;; a plain dark square could otherwise be counted
        ;; as a line
        (let ((sum (reduce #'+ (mapcar #'(lambda (p) (nth 1 p)) (car valid-lines)))))
;;          (format t "~a ~a ~a ~%" sum intensify           (* intensify (/ sum 3)))
          (min 255 (floor (* intensify (/ sum 3)))))

        ;; If no valid line is found
        ;; return the original pixel value
        (gsref image x y))))




;;;;; ########## SAMPLING METHOD: AVERAGE ##########


(defun pixel-average (image x y width height)
  "Compute the average of the pixels around point (`X', `Y'), in `IMAGE'
of size `WIDTH' x `HEIGHT'"

    (let* ((sum 0) (len 0))

      ;; Compute average pixel luminance.
      (dolist (p *3x3-points*)
        (let ((x_ (+ x (car p)))
              (y_ (+ y (cdr p))))

          ;; Add all points that are in bounds.
          (when (in-bounds  x_ y_ width height)
            (incf sum   (gsref image x_ y_))
            (incf len 1))))

      ;; Compute average.
      (/ sum len)
      ))




(defun image-to-ascii
    (file &key
            (width *default-column-width*)
            (sample *default-sample-method*)
            (scale-y *default-scale*)
            (intensify *default-intensify*)
            (simularity *default-simularity*)
            (chars *ascii-chars*))

  "Convert an image at path FILE to ASCII representation.

 :WIDTH    - The character width of the output text.
 :SCALE-Y  - Vertical scaling factor to preserve aspect ratio.
 :CHARS    - List of ASCII characters sorted by luminance.
 :SAMPLE   - Sampling method: 'normal, 'average, or 'lines.
 :INTENSIFY - Scale lines with this factor when :SAMPLE is lines.
 :SIMULARITY - Used with :SAMPLE lines to determine the maximum.
               allowed differences of pixels in a line."

  (unless chars (setq chars *ascii-chars*))

  (let* ((image        (png:decode-file file))
         (gsimage      (png:grayscale-image image))
         (image-width  (png:image-width gsimage))
         (image-height (png:image-height gsimage))
         (bit-depth    (png:image-bit-depth gsimage)))

    (let* ((scale        (/ image-width width))
           (height       (floor (/ image-height scale) scale-y)))

    (let ((output-string
            (make-array
             0 :element-type 'character
               :adjustable t
               :fill-pointer 0)))
      (declare (type string output-string))


      (dotimes (y height)
        (let* ((row (make-string width))
               (src-y (min (floor (* y scale scale-y))
                           (1- image-height))))

          (when  (>= src-y image-height)
            (format *error-output* "Y-Axis: ~a>=~aOut of bounds~%" src-y image-height))

          (dotimes (x width)
            ;; Compute position for the sampling of characters
            ;; spaced out width the distance of `SCALE'.
            (let ((src-x (min (floor (* x scale)) (1- image-width))))

              (when (< src-x image-width)

                ;; This should never happen
                (when (not (in-bounds src-x src-y image-width image-height))
                  (format *error-output* "X-Axis: Position (~a, ~a) is out of bounds~%" src-x src-y))

                (let* ((pixel
                         (case sample
                           ;; Average sampling method
                           (average (pixel-average gsimage src-x src-y
                                                   image-width image-height))

                           ;; Explicitly specify no sampling algorithm
                           (normal (gsref gsimage src-x src-y))

                           ;; Line detection algorithm
                           (lines (pixel-line gsimage src-x src-y
                                              image-width image-height
                                              :intensify intensify
                                              :simularity simularity))

                           ;; Default sampling
                           (_ (gsref gsimage src-x src-y))))

                       ;; Convert luminance from 0-255 to 0.0-1.0
                       (luminance (pixel-to-float pixel bit-depth))
                       ;; Get character mapped to `luminance' value
                       (char      (map-to-ascii luminance :chars chars)))

                  ;; Set column char
                  (setf (aref row x) char)))))

          ;; Append row
          (format output-string "~a~%" row)))

      (the string output-string)))))



(defun img-to-txt
    (file &key
            (width *default-column-width*)
            (sample *default-sample-method*)
            (intensify *default-intensify*)
            (simularity *default-simularity*)
            (scale-y *default-scale*)
            (chars *ascii-chars*))
  "Given a at image at filepath `FILE', compute a text image `WIDTH' chars wide.

 :CHARS - is a list of characters which correspond to a value of luminance
          in [0.0,1.0]. Chars are sorted such that the left most char
          represent the lowest value of the luminance, increasing towards
          the right.
 :SAMPLE   - Sampling method: 'normal, 'average, or 'lines.

 :AVERAGE - if non-nil, compute the luminance of pixel through averaging.

 :INTENSIFY - Scale lines with this factor when :SAMPLE is lines.
 :SIMULARITY - Used with :SAMPLE lines to determine the maximum.
               allowed differences of pixels in a line."

  (image-to-ascii
   file :intensify intensify
        :simularity simularity
        :scale-y scale-y
        :width width
        :chars chars
        :sample sample))



;;;; ##### Argument handling, Startup and Compilation ####


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

                    (format nil "~a~a (default ~a)~%" " -s, --sample    "
                            "Sample method, one of: average, normal, lines"
                            *default-sample-method*)

                    " -f, --file      Path to image file, must exist if <filename>~%"
                    "                 isn't the 1st arg.~%"
                    " -c, --columns   The width of the text image (default 80)~%"

                    (format nil "~a~a (default ~a)~%"
                            " -y, --scale-y   "
                            "Scalingfactor applied to the y-axis, 0-100"
                            *default-scale*)
                    (format nil "~a~a (default ~a)~%"
                            " -i, --intensify "
                            "Intensify detected lines in sampling mode 'lines' by this factor 0-100"
                            *default-intensify*)

                    (format nil "~a~a (default ~a)~%"
                            " -S, --simularity "
                            "The difference in luminance allowed between characters 0-255"
                            *default-simularity*)
                    "~%"
                    " -h, --help, --usage~%"
                    "                 Show this message~%"))))


(defun main ()
  "Main function to process command-line arguments and generate ASCII art."

  (let* ((argv (uiop:raw-command-line-arguments))
         (argc (length argv))

         (arg-alpha (or (parse-arg "--alphabet" argv :isbool nil)
                        (parse-arg "-a" argv :isbool nil)))


         (arg-intensify
           (or (parse-arg "--intensify" argv :isbool nil)
               (parse-arg "-i" argv :isbool nil)))

         (arg-simularity
           (or (parse-arg "--simularity" argv :isbool nil)
               (parse-arg "-S" argv :isbool nil)))

         (arg-avg (or (parse-arg "--sample" argv :isbool nil)
                      (parse-arg "-s" argv :isbool nil)))

         (arg-file (or (parse-arg "--file" argv :isbool nil)
                       (parse-arg "-f" argv :isbool nil)))

         (arg-scale (or (parse-arg "--scale-y" argv :isbool nil)
                        (parse-arg "-y" argv :isbool nil)))

         (arg-cols (or (parse-arg "--columns" argv :isbool nil)
                       (parse-arg "-c" argv :isbool nil))))

    ;; A path cannot have been provided. Show help message
    (when (< argc 2)
      (format *error-output* "Not enough arguments, need a filename.~%")
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
                 (progn  (format *error-output* "No filename given, exiting...~%")
                         (show-help (car argv))
                         (uiop:quit 1))))
           ;; Get the column width
           (width (if (> argc 2)
                      (or (and arg-cols
                               (parse-integer arg-cols :junk-allowed t))
                          *default-column-width*)))
           ;; Scaling factor for y-axis
           (scale-y (or (and arg-scale
                             (/ 100 (parse-integer arg-scale)))
                        *default-scale*))

           ;; Save image to a file?
           (save-image-path (or (parse-arg "-o" argv :isbool nil)
                                (parse-arg "--output" argv :isbool nil)))

           ;; Compute the text image
           (text-image
             (if (probe-file image-path)
                 (progn


                   (img-to-txt image-path
                               :intensify (or (and arg-intensify
                                                   (/ (parse-integer arg-intensify) 100))
                                              *default-intensify*)

                               :simularity  (or (and arg-simularity
                                                     (parse-integer arg-simularity))
                                                *default-simularity*)
                             :width width
                             :scale-y scale-y
                             :chars arg-alpha
                             :sample (or (and (equal "average" arg-avg) 'average)
                                         (and (equal "normal" arg-avg) 'normal)
                                         (and (equal "lines" arg-avg) 'lines)
                                         *default-sample-method*))
                 )

                 (progn (format *error-output* "File does not exist, exiting...~%")
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
