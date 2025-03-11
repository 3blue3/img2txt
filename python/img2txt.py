#!/usr/bin/python

# The file 'img2txt.py' from project 'img2txt' is licensed under GPLv3.

# Desc: This script transforms images into ASCII art
# File: img2txt.sh
# Date: 2502282948
# Author: blu3, OCCU

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Todo:
#   - [x] Add option to specify background to add under transparent images

# Code:

import argparse
from charsets import CHARSETS
import numpy as np
from PIL import Image, ImageFilter, ImageEnhance, ImageOps, ImageSequence
import io

############################################################
#                        Variables                         #
############################################################

DEFAULT_QUIET = False

DEFAULT_BACKGROUND = (0, 0, 0) # Black
DEFAULT_COLOR_COUNT = 256

# Since chars aren't cubes, but rectangles, the height of the image can be
# scaled to better fit into chars (recommended 0.43) Computed in Emacs using:
# '(/ (float (window-font-width)) (window-font-height))' It may be needed to
# recompute it for different fonts and with font rendering
DEFAULT_SCALE   = 0.565
DEFAULT_COLUMNS = 80
DEFAULT_OUTFILE = 'out.txt'
DEFAULT_STYLE = "thonky"

DEFAULT_LUMINANCE_SCALE = 1.0
DEFAULT_SPREAD_RADIUS   = 1


DEFAULT_SHARPNESS = None
DEFAULT_CONTRAST = 1.2
DEFAULT_BRIGHTNESS = 1.2
DEFAULT_SOLARIZED_THRESHOLD = None
DEFAULT_POSTERIZE_BITS = 2
DEFAULT_LUMINANCE = 1.0
DEFAULT_THRESHOLD = None
DEFAULT_POSTERIZE = None
DEFAULT_MIRROR    = None

############################################################
#                        Functions                         #
############################################################


def create_bg(size: tuple[int, int], color) -> Image:
    """Create a new blank, colored image."""
    return Image.new('RGB', size, color)


def get_average(image) -> float:
    """Given PIL Image, return average grayscale value between all pixels."""
    im = np.array(image)
    (w, h) = (im.shape)
    return np.average(im.reshape(w*h))


def remove_transparency(img, bg_color=DEFAULT_BACKGROUND) -> Image:
    """ Replace the alpha layer of image with a color """

    if img.mode == 'RGBA':
        background = create_bg(img.size, bg_color)

        # Paste image onto background
        background.paste(img, (0, 0), img)
        return background.convert("RGB")
    else:
        return img.convert("RGB")


def ungifify(img):
    """Convert image to png if its a gif."""
    # Open the GIF image
    if img.format == 'GIF':
        first_frame = ImageSequence.all_frames(img)[0]
        return first_frame
    else:
        return img


def convert_img2txt(
        filename: str,
        cols_count=DEFAULT_COLUMNS,
        scale=DEFAULT_SCALE,
        luminance=DEFAULT_LUMINANCE,
        dither=None,
        invert=None,
        filters=[],
        brightness=DEFAULT_BRIGHTNESS,
        contrast=DEFAULT_CONTRAST,
        rotate=None,
        spread_radius=DEFAULT_SPREAD_RADIUS,
        sharpness=DEFAULT_SHARPNESS,
        posterize_bits=DEFAULT_POSTERIZE_BITS,
        solarized_threshold=DEFAULT_SOLARIZED_THRESHOLD,
        mirror=DEFAULT_MIRROR,
        charset_style=DEFAULT_STYLE,
        custom_charset=None,
        background=DEFAULT_BACKGROUND,
        quiet=True
):
    """
    Given Image and dims (rows, cols) returns an m*n list of Images
    """

    image = Image.open(filename)
    # Replace GIFs with their first frame.
    image = ungifify(image)
    # Replace transparency with a background color.
    image = remove_transparency(image, background)

    if solarized_threshold:   image = ImageOps.solarize(image, threshold=solarized_threshold)
    if posterize_bits: image = ImageOps.posterize(image, bits=posterize_bits)
    if brightness:     image = ImageEnhance.Brightness(image).enhance(brightness)
    if contrast:       image = ImageEnhance.Contrast(image).enhance(contrast)
    if sharpness:      image = ImageEnhance.Sharpness(image).enhance(sharpness)
    if dither:      image = image.convert(dither=dither, colors=dither)

    # Apply filters on image
    for f in filters:
        match f:
            case "sharpen":      image = image.filter(ImageFilter.SHARPEN)
            case "find-edges":   image = image.filter(ImageFilter.FIND_EDGES)
            case "enhance-edge": image = image.filter(ImageFilter.EDGE_ENHANCE)\
                                              .filter(ImageFilter.EDGE_ENHANCE_MORE)
            case "blur":         image = image.filter(ImageFilter.BLUR)
            case "emboss":       image = image.filter(ImageFilter.EMBOSS)
            case "contour":      image = image.filter(ImageFilter.CONTOUR)

            case _:
                if not quiet:
                    print(f"Unknown filter {filter}")

    # Convert to grayscale
    image = image.convert('L')

    # Invert image colors
    if invert: image = ImageOps.invert(image)
    # Mirror image across vertical axis
    if mirror: image = ImageOps.mirror(image)
    if rotate:    image = image.rotate(int(rotate), expand=True)

    # Original image dimensions
    (original_width, original_height) = (
        int(image.size[0]),
        int(image.size[1])
    )

    # Tile height is based on aspect ratio and scale,
    # width is not affected by scale,
    (w, h) = (
        (original_width / cols_count),
        (original_width / cols_count) / scale
    )

    # Number of rows
    rows_count = int(original_height/h)

    # check if image size is too small
    if cols_count > original_width or rows_count > original_height:
        if not quiet:
            print("Image too small for specified cols!")
            exit(0)

    if not quiet:
        print(f"Image size: {original_width}x{original_height}\t" +
              f"Char size: {cols_count}x{rows_count}\t" +
              f"Tile size: {w}x{h}")

    nth_tile = lambda n, size: (int(n * size), int((n + spread_radius) * size))


    #   Compute text image
    #   ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

    # Charset to use
    if custom_charset:
        cset = custom_charset
    elif charset_style:
        cset = CHARSETS[charset_style]
    else:
        cset = CHARSETS[DEFAULT_STYLE]


    # The ascii image
    ascii_rows = [""] * rows_count

    # generate list of dimensions
    for j in range(rows_count):
        (y1, y2) = nth_tile(j, h)

        # FIXME: Remove this; (correct last tile)
        if j == rows_count - 1:
            y2 = original_height

        # crop tiles
        for i in range(cols_count):
            (x1, x2) = nth_tile(i, w)

            # FIXME: Remove this; (correct last tile)?
            if i == cols_count - 1:
                x2 = original_width

            # crop image to extract tile
            img = image.crop((x1, y1, x2, y2))
            img = img.effect_spread(int(spread_radius))

            # get average luminance
            avg = int(get_average(img) * luminance)

            # FIXME: Wrote the charsets wrong direction,
            # i.e light ==> dark, so i have to subtract
            # from the length here.
            char = cset[min(
                len(cset) - 1,
                len(cset) - int((avg/255.0) * len(cset))
            )]
            ascii_rows[j] += char

        # Row finished.

    # All rows of the text image are finished.
    return ascii_rows


############################################################
#            USER INTERFACE / ARGUMENT HANDLING            #
############################################################

header=" [48;5;52m║     meow                                           ║[0m\n"
header += " [48;5;52m║|\\_/|                                          |\\_/|║[0m\n"
header += " [48;5;52m║│ì\"í│  [1m[38;5;198m[5m__  _  _   ___  ____  ____  _  _  ____[0m[48;5;52m  │ò\"ó│║[0m\n"
header += ",[48;5;52m║`¬ª⌐' [1m[38;5;198m[5m(  )( \\/ ) / __)(___ \\(_  _)( \\/ )(_  _)[0m[48;5;52m `¬ª⌐'║[0m\n"
header += "\\[48;5;52m║/'Σ\"\\  [1m[38;5;198m[5m)( / \\/ \\( (_ \\ / __/  )(   )  (   )([0m[48;5;52m   /\"Σ\"\║[0m\n"
header += " [48;5;52m║/| |\\\\[1m[38;5;198m[5m(__)\\_)(_/ \\___/(____) (__) (_/\\_) (__)[0m[48;5;52m //| |\µ[0m;\n"
header += " [48;5;52m║ │|\\\\[31m░░░▒▒░░▒▒░▒▒░░▒▒▒▒░▒▒▒▒░░▒▒▒▒░▒▒░▒▒▒░▒▒▒▒░░[0m[48;5;52m|\"│ ║[0m\n"
header += " [48;5;52m║ ││ \\\\  \"Tool used to render images as text\"    ││| ║[0m\n"
header += " [48;5;52m║ ^\"  \"^  (c) OCCU 2025                          ^\"' ║[0m\n"


def main():

    # create parser
    desc_str = "Tool used to render images as text"
    parser = argparse.ArgumentParser(description=desc_str)

    # add expected arguments
    parser.add_argument('--file',  '-f',         dest='imgFile',    required=True)
    parser.add_argument('--scale', '-S',         dest='scale',      required=False)
    parser.add_argument('--quiet', '-q',         dest='quietPrint',          action="store_true")
    parser.add_argument('--luminance', '-l',     dest='luminance',  required=False)

    parser.add_argument('--brightness', '-b',    dest='brightness', required=False)
    parser.add_argument('--contrast', '-c',      dest='contrast',   required=False)
    parser.add_argument('--background', '-bg',   dest='background', required=False)

    parser.add_argument('--sharpness', '-s',     dest='sharpness',  required=False)

    parser.add_argument('--output', '-o',        dest='outFile',    required=False)
    parser.add_argument('--cols',   '-C',        dest='cols',       required=False)
    parser.add_argument('--rotate',   '-r',      dest='rot_deg',    required=False)
    parser.add_argument('--spread',              dest='spread',     required=False)
    parser.add_argument('--dither', '-d',        dest='colors',     required=False)
    parser.add_argument('--blur',                dest='blur_filter',         action="store_true")
    parser.add_argument('--sharpen',             dest='sharpen_filter',      action="store_true")
    parser.add_argument('--edge-detect',         dest='edge_detect_filter',  action="store_true")
    parser.add_argument('--enhance-edge',        dest='enhance_edge_filter', action="store_true")
    parser.add_argument('--emboss',              dest='emboss_filter',       action="store_true")
    parser.add_argument('--contour',             dest='contour_filter',      action="store_true")

    parser.add_argument('--threshold', '-t',     dest='threshold',  required=False)
    parser.add_argument('--posterize', '-p',     dest='bits',       required=False)
    parser.add_argument('--mirror',  '-m',       dest='mirror',       action="store_true")

    parser.add_argument('--invert', '-i',        dest='invert',       action="store_true")
    parser.add_argument('--banner',        dest='show_banner',       action="store_true")
    parser.add_argument('--list-styles', '-L',   dest='listCharsets', action='store_true')
    parser.add_argument('--custom-chars', '-cc', dest='chars',      required=False)
    parser.add_argument('--style',               dest='charset',    required=False)

    # parse args
    args = parser.parse_args()

    # Parse filters
    filters = []

    if args.show_banner:
        print(header)

    if args.sharpen_filter:
        filters.append("sharpen")

    if args.blur_filter:
        filters.append("blur")

    if args.emboss_filter:
        filters.append("emboss")

    spread_radius = args.spread or 1

    if args.contour_filter:
        filters.append("contour")

    if args.enhance_edge_filter:
        filters.append("enhance-edge")

    if args.edge_detect_filter:
        filters.append("edge-detect")

    is_quiet = args.quietPrint

    # List charsets
    if args.listCharsets:
        print("name:\t\tcharset")
        for lvl in CHARSETS.keys():
            print(f"{lvl}:\t\t{CHARSETS[lvl]}")
        exit(0)

    charset=args.charset

    dither = args.colors
    invert = args.invert

    solarized_threshold = int(args.threshold) if args.threshold \
        else DEFAULT_THRESHOLD

    posterize_bits = int(args.bits) if args.bits \
        else DEFAULT_POSTERIZE

    mirror = int(args.mirror) if args.mirror \
        else DEFAULT_MIRROR

    brightness = float(args.brightness) if args.brightness\
        else DEFAULT_BRIGHTNESS

    contrast = float(args.contrast) if args.contrast\
        else DEFAULT_CONTRAST

    sharpness = float(args.sharpness) if args.sharpness\
        else DEFAULT_SHARPNESS


    if charset and not charset in CHARSETS.keys():
        print("Invalid charset")
        exit(1)

    do_rotate = args.rot_deg

    # Set luminance scaler
    luminance = ( args.luminance and float(args.luminance )) or DEFAULT_LUMINANCE_SCALE
    # Set input file
    imgfile = args.imgFile

    if imgfile == "":
        print("Empty input file")
        exit(1)

    # Set output file
    outfile = args.outFile if args.outFile else None
    # Set scale default as 0.43 which suits a Courier font
    scale = float(args.scale) if args.scale else DEFAULT_SCALE
    # Set column width
    cols = int(args.cols) if args.cols else DEFAULT_COLUMNS

    if not is_quiet:
        print('generating ASCII art...')

    # convert image to ascii txt
    ascii_rows = convert_img2txt(
        imgfile,
        cols_count=cols,
        scale=scale,
        luminance=luminance,
        dither=dither,
        invert=invert,
        filters=filters,
        brightness=brightness,
        contrast=contrast,
        sharpness=sharpness,
        background=args.background,
        mirror=mirror,
        posterize_bits=posterize_bits,
        solarized_threshold=solarized_threshold,
        rotate=do_rotate,
        quiet=is_quiet,
        spread_radius=int(spread_radius),
        custom_charset=args.chars,
        charset_style=charset
    )

    # No output file, print to stdout
    if not args.outFile:
        for row in ascii_rows:
            print(row)
        exit(0)

    # Write output to file
    with open(outfile, 'w') as f:
        for row in ascii_rows:
            f.write(row + '\n')

    if not DEFAULT_QUIET:
        print("ASCII art written to %s" % outfile)

# call main
if __name__ == '__main__':
    main()
