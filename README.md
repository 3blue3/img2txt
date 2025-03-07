`img2txt`
=======

Simple Image to Text Converter

Directory Structure
-------------------

 - `src/`: Source code used for or related to the executable `img2txt.py`.

   - `src/charsets.py`: Python files for the data-structure `CHARSETS` which is
     used for the ASCII images.

   - `src/img2txt.py`: Source code for `img2txt`, also an executable file.

   - `src/braille_shades.py`: Python files for the data-structure `CHARSETS`
     which is used for the ASCII images.

  - `scripts`: Scripts related to the project.

  - `test`: Testing

Usage
-----

```
$ ./img2txt.py --help
usage: img2txt.py [-h] --file IMGFILE [--scale SCALE] [--quiet] [--luminance LUMINANCE]
                  [--brightness BRIGHTNESS] [--contrast CONTRAST] [--background BACKGROUND]
                  [--sharpness SHARPNESS] [--output OUTFILE] [--cols COLS] [--rotate ROT_DEG]
                  [--spread SPREAD] [--dither COLORS] [--blur] [--sharpen] [--edge-detect] [--enhance-edge]
                  [--emboss] [--contour] [--threshold THRESHOLD] [--posterize BITS] [--mirror] [--invert]
                  [--banner] [--list-styles] [--custom-chars CHARS] [--style CHARSET]

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
$ ./img2txt.py -f ../test/ZombieSpider.jpg --contrast 10.0\
    --sharpness 2.0 --luminance 1.18 --brightness 1.2

<der.jpg --contrast 10.0 --sharpness 2.0 --luminance 1.18 --brightness 1.2
generating ASCII art...
Image size: 642x534     Char size: 80x37        Tile size: 8.025x14.203539823008851
████████████████████████████████████████████████████████████████████████████████
████████████████████████████████████████████████████████████████████████████████
0$HXw██████████████████████1&K███████████████████████M██████████████████████████
k▒ '=Vx6pW███████████████░BÿbWP█████████████████████▒█a▒████████████████████████
bbbbbH&Xw"i%aBK▒████░▒████▒ÿ3X$░███████████████████████K████████████████████████
bbbbbbbbbbbb&RXK-"_  *}iêbbS█8░7░████████████████M█████k████████████████████████
bbbbbbbbbbbbbbbbbbbbbAis=X<V█}AAW87Rk▒████W███████████░██░░█████████████████████
bbbbbbbbbbbbbbbbbbbbb(&█░Yw_MW2█░]Qp7=}iÿ%Pk░████░███████████░██████████████████
bbbbbbbbbbbbbbbbbbbbb▒KBH▒4)h!O██HX██YXbHhAw█"[ÿK███&░███████b██████████████████
bbbbbbbbbbbbbbbbbbbbbb{W7AAHR'W░███░█SMHbbbbbb&YK██&_üp▒█MqK▒WP█████████████████
bbbbbbbbbbbbbbbbbbbbbb,█████b#R#██%B█vXbbbbbbk05K██z1███akhW,]}▒p51HM▒██████████
bbbbbbbbbbbbbbbbbbbbb&x█#H7█MA7l██7▒█~bbhbbbbêêl███K██▒█A&bbbbk9H*AR█~<{iÿ92bKM░
bbbbbbbbbbbbbbbbbbbbbKÿPMÿx/XM██h█M█aKhHHK▒&k'.M█████K>3▒.Abbbb,█^bbbbbbbbbHhAM,
bbbbbbbbbbbbbbbbbbbbbb5ä&&By+X█MA61O,M- =VlLMA_P████q▒M\█+bbbbbbk8bbbbbbbbbbbbbb
bbbbbbbbbbbbbbbbbbbbbwW~hb▒AV1hBHR8Iv.&~Q8H4S~b)████R████%kbbbbbê)bbbbbbbbbbbbbb
bbbbbbbbbbbbbbbbbbbbbO3bbbb-MR;/kKx>0sb{xx2mZwbm██wrü<_█0A=bbbbbmHbbbbbbbbbbbbR▒
bbbbbbbbbbbbbbbbbbbbb2êhbbbZ█VA&<:ä<c\&H \&$t'i12{hb█Kbb(█HMbbbHLbbbbbbbbbbbh;}>
bbbbbbbbbbbbbbbbbbbb░█Ibbb>█I,;bbH.W-s:HH'}{^Wi\bbbbbbbbX██ükbb*.bbbbbbbbbbbbbbb
bbbbbbbbbbbbbbbbbbbb'░.bbb:6bb_>bbbb>#VAH-/~(-&bbbbbbbbb>kVä0▒bObbbbbbbbbbbbbbbb
bbbbbbbbbbbbbbbbbbbbbLIbbb;Vbbb:Mbbh^q&█hA^<WWbbbbbbbb.äQAXHä$>!bbbbbbbbbbbbbbbb
bbbbbbbbbbbbbbbbbbbbbb1-bb*\bbbb/Wb-">aR9Mmö'bbbbbbbXIT&bbbbbzR░bbbbbbbbbbbbbbbb
bbbbbbbbbbbbbbbbbb&&Hb)VbbO*bbbbA=^hbb_wmc}░&bbbbbH)l.bbbbbbb.?P▒bbbbbbbbbbbbbbb
bbbbbbbbbbbbbbbb&&&AA&X7Rbs▒bbbbMo:wbbbbbbbbbbbbb█â-bbbbbbbbb)wh6Mbbbbbbbbbbbbbb
bbbbbbb&HbbH-=v=HHbhbbb:ibrbHbH"█Xb█Mbbbbbbbbbb▒L)bbbbbbbbbbK;b"R#bbbbbbbbbbbbbb
bHbb&▒bA>+^;X&w~-"!;X&&b7.übbbKXb_sbWXbbbbbbbb Qbbbbbbbbbbbb}bbb_$bbbbb&bbAbbbbb
bbbbb░YbxQ,>;{=,<,Kw░█A&VQüHbbbbbHy(bbbbbbbbbbbbbbbbbbbbbbbh_bbbb6Hbbbbbbbbbbbbb
bbbb)IOyhW?àYto/*(,HR&bbXB#\}.h*àBêkbbbbbbbbbbb&░*);:>-Xbbbbbbbbby,bbbbbbbbbbbbb
bbbbbbbbbh :)vyüü72BR█RbH▒░██K1&███?,bw'=*QisO;MbbbbbbbbbbbbbbHbb]tbbbbbbb▒>b&▒H
░hRbbH;^!*>=))O[TLO="tW█████████████ö(O}O!) Rbbbbbbbbbb&XbbbbbHhbwâbbbbAbHbbbbbb
^░w_Xw-KkW,▒)Kw▒[â78p██▒░█W██████████s;]}=,--v),HbbH&XX&Abbbbbb&b&5AXHhkbMX&RHhH
_wWRbhAbbbRX█m&@Q.AM▒,\icä$▒██████H9kM=XHkWkw.,hbHbbbbbHhbbbb&bb&bC*bbhbbb&bHHW&
bbH░▒WWR-~XMR>Q]"O}!~K:WAkHK█,,MbXA&&OB3o-h&HhbbXWbbKbbbbHbbbbbbkb>LAb&hbbkbHHHA
b&bbbb&░,&K&&hHW/Abbwhb&HhbbbbbHbbbbbbbbA![OlI=WMwHbbbbbbbbbbbbbbbK?HHbbbbbHbbbb
HbHbbHhH-^░/=I*>-░ bbbbHbbhbbHbKA&W░,+.kbhHbH&M~,/TöV~KbMAbbbbbbbbb0bbbbbbbbbbbb
Abb█-&bhRhM~&b&AbbhXHbbbbbbbbbbbhHbbA_&bbbbbbbA▒XhH~(Zÿ~>tO\;KbbbbHÿ"bbbbb&bbHHb
bRbR]Mb&)i&HRbbbbbbbb&bbbbbbbbbbbbAbXbh-bbbbbbbbbb&&VI( bbbW\>])!:h]Qbbbbbbb&Rk(
bbbbbbbbbbhbbbbbbbhHbbbbbHHb░):,&&hbbbR&bbHbbbbbbbHbbbX /,!,Whbbbb█v~bA<▒>*<),{i
```
