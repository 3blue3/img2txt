#!/bin/sh


# The file 'asciify.sh' from project 'img2txt' is licensed under GPLv3.

# Usage: $0 <path to image> [ <width> (default ${COLUMNS})]
# Desc: This script converts an image to text
# Date: 2503060111

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

# Code:

img2txt=../src/img2txt.py
COLUMNS=80
STYLE="large"
STYLE2="lines"

function asciify
{
    if [ -f "$1" ]
    then

        local cols=${2}
        if [ -z "$cols" ]; then

            cols=$COLUMNS
        fi

        local img_path="${1:?}"

        ./$img2txt -f $img_path     \
                   --contrast 1.5   \
                   --brightness 1.4 \
                   --scale 0.5      \
                   --sharpness 1.1  \
                   --cols $cols     \
                   --style $STYLE2  \
                   --quiet          \
                   --sharpen        \
                   --edge-detect    \
                   --enhance-edge   \
                   --spread 1       \
                   --luminance 1.0  \
                   --background "white"
    fi
}

if [ $# -lt 1 ]; then
    echo "Usage: $0 <path to image> [ <width> (default ${COLUMNS})]"
    exit 1
else
    asciify $@
fi
