

`img2txt`
=======

```
usage: img2txt.py [-h] --file IMGFILE [--scale SCALE] [--quiet] [--luminance LUMINANCE] [--brightness BRIGHTNESS] [--contrast CONTRAST] [--background BACKGROUND]
                  [--sharpness SHARPNESS] [--output OUTFILE] [--cols COLS] [--rotate ROT_DEG] [--spread SPREAD] [--dither COLORS] [--blur] [--sharpen] [--edge-detect]
                  [--enhance-edge] [--emboss] [--contour] [--threshold THRESHOLD] [--posterize BITS] [--mirror] [--invert] [--banner] [--list-styles]
                  [--custom-chars CHARS] [--style CHARSET]

Tool used to render images as text

options:
  -h, --help            show this help message and exit
  --file IMGFILE, -f IMGFILE
  --scale SCALE, -S SCALE
  --quiet, -q
  --luminance LUMINANCE, -l LUMINANCE
  --brightness BRIGHTNESS, -b BRIGHTNESS
  --contrast CONTRAST, -c CONTRAST
  --background BACKGROUND, -bg BACKGROUND
  --sharpness SHARPNESS, -s SHARPNESS
  --output OUTFILE, -o OUTFILE
  --cols COLS, -C COLS
  --rotate ROT_DEG, -r ROT_DEG
  --spread SPREAD
  --dither COLORS, -d COLORS
  --blur
  --sharpen
  --edge-detect
  --enhance-edge
  --emboss
  --contour
  --threshold THRESHOLD, -t THRESHOLD
  --posterize BITS, -p BITS
  --mirror, -m
  --invert, -i
  --banner
  --list-styles, -L
  --custom-chars CHARS, -cc CHARS
  --style CHARSET
```
















Example
-------

```
$ ./img2txt.py  -f black-mamba.png  --contrast 1.2 --brightness 1.2
generating ASCII art...
input image dims: 753 x 435
cols: 80, rows: 19
tile dims: 9 x 21
````````````````````````````````````````````````````ˇˇˇˇˇˇˇˇˇˇˇˇˇˇ``ˇ```````````
``````````````````````ˇ```ˇˇˇˇˇ`ˇ`ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ`ˇˇˇˇˇ``
`````ˇ````ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ,,+»«+=⇛∴≡❖⇶⇶┼‖╬╬‖‖‖‖╋╋╬|∹+ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ…~+≖∹❖‖‖:±-·¬¬⇛≣‖‖┼❖┼‖‖╳‖‖╳╳╳╳╳╳+ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ-┄+»∴⇶⇶∴∴❖‖↟≖└´¨·+∹‖⇶╬╬‖∴⇶‖╬‖‖‖‖≣∴∴‖=ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ,+└↟∴|‖‖⇶∴∴∴|≣⇶∹=≖↟∹∹≣╬╬┼╬╋╬‖↟∹-+·´¨-±+¬ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ~⇛‖╬╋‖‖‖┼|⯀∴∴∴∴∴∴∴↡↡↟∴╬╋‖╬‖⇛±¬-ˇ,,,,-,-ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ=⇶╋╬╬≣❖∴∴≡∴∴↟∴↡∹⇛---⇛≖+==+,¨¨ˇˇ,,┄.·ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇˇˇˇ,=⯀⯀‖∴↡∹∹-≖++:+¬~-ˇˇ´¨¨¨ˇˇ…-.~¬┄.-ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇˇ…=≡∴∹∹∴∴∴∴∴≡∴└~¨        ¨,--·…ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇˇ,∴‖‖⯀↟∹∹∹∹∴∴∴┼╋‖≣+ˇ     ¨ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇˇˇ=⇶‖╬|∴∴↟↡⇛∹∹∴∴|≣┼‖╳╳∴+¨   ¨ˇ-…ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇ…└‖╋≣❖≡∴≡∴∴∴∴∴∹↟↟∴∴≡|‖‖╳╬-┄¨ ¨ˇ·~-ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇˇ=╬‖⯀∴∹∹↟⇛--└┘┘┘≖⇛-∹↟↡∴∴≡‖╬‖‖❖⇛=,ˇ+┄┄~,,ˇˇ,,¬…ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇˇ=‖‖≡↟∴↡∹--≖└└≖⇛≖⇛-⇛-└┘└└-⇛∹↟∴⯀┼╬╋‖╳╋┼≡∹↟==+⇛❖❖⇛:=ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇˇ+|❖∹|∴∹∹∹↟↟∹∹∹∹∹↟∴-¬,ˇˇˇˇˇˇˇ,·.~+==-∴≡❖⯀⯀≣≣┼╬‖‖‖⇶=ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇˇ.∴∴∹∴∴∹∹∹∹↡↡↡∴↡↡∴∴±ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ-,¬+±=«=«±-ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇˇ…∹┼∴∴≣⇶⯀❖≡∴∴∴∴↡∴∴∴~ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
ˇˇ,≖∴↡╬‖╬‖‖‖‖‖⇶∴❖|⇶+,ˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇˇ
```
