`img2txt`
=======

Simple Image to Text Converters. Contains two different implementations of image to text conversions, 
one in python and one in Common Lisp.





Implementations
---------------

 - [lisp/](lisp): Common Lisp image to text. Slow but supports many formats including gifs.
   
 - [python/](python): Python image to text program. Fast but only supports png's, but other
   formats can easily be used by writing a script with `magick convert <image> <output>.png`.
