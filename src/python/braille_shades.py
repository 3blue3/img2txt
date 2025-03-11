#!/bin/python3

# The file 'braille_shades.py' from project 'img2txt' is licensed under GPLv3.

# Desc: Generate all possible Braille dot patterns (0-8 filled dots) print them
# sorted by the amount of (pixel) space the characters takes upby assuming more
# dots => more space.

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

from itertools import combinations

braille_chars = []


# Braille start char in unicode
BRAILLE_START = 0x2800

# Iterate list: 8,7,...,0
for num_dots in range(8, -1, -1):
    # Iterate each 8 num (and less) tuple combinations of numbers 0-7.
    for dots in combinations(range(8), num_dots):
        # Generate each braille char unicode position.
        unicode_val = BRAILLE_START

        # Set the corresponding bits for each dot.
        for dot in dots:
            unicode_val |= (1 << dot)

        # Store character with dot count
        braille_chars.append((chr(unicode_val), num_dots))


# roughly sort each char by the space they take up (in pixels),
# in descending order, by assuming more dots => more space
braille_sorted = list(sorted(
    braille_chars,
    reverse=False,
    key=lambda x: x[1]

))

# Extract only the characters for output
braille_chars_sorted = [char for (char, _) in braille_sorted]

# Print characters in sorted order
print("".join(braille_chars_sorted))
