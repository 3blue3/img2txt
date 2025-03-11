# The file 'charsets.py' from project 'img2txt' is licensed under GPLv3.

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

CHARSETS =\
{
    # 'large' is taken from  http://paulbourke.net/dataformats/asciiart/ but reversed
      "large":   ' `^",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$',
    "thonky": " ,.-~,_'\";:+*<>=!^/\\)(}{][QOoVvTLlIitCcSszZyYxmÿüöäâêà#r%3987654021aq?$@PpBbH&hRXAkKWwM░▒█",
      "lines":   " .,-~¬;:÷=»+*{}[]?£#€§%@¶",
      "curls":   ' ·.:/?%8&$@#"',
      "slash":   ".-:=/\\%$#",
     "blocks":   "░▝▌▐▒▚▙▜▓▟█",
       "nums":   "9876543210",
    "minimal":   "·:=÷×%#",
     "simple":   ".:-=+*#%@",
    # This one is not monospaced, 'unicode-fonts' in Emacs may make it seem that way though.
    "simple1":   " ¨¨¨´`ˇˇˇ…,,·-~.,┄~¬++++±±==:«»+=┘└≖⇛-⇛∹∹∹↟↡∴∴∴∴≡❖⯀|≣⇶┼‖‖‖╬╬╬╋‖‖‖╳╳",
    "simple2":   "'\",.<>?!",
     "short3":   " .,'`´/|;:-+\"=~^*()[]{}!?£§€%&$@¶#",
     "short2":   "`/|;:.,-+=~^*()[]{}!?@#$%&`",
     "short1":   "`/|;:.,-+=~^*()[]{}!?",
     "squigg":   '_~^;/|()[]{}',
    "heights":   "▁▂▃▄▅▆▇█",
     "shades":   " ░░░░░░░░▒░▒░▒░▒░▒▒▒▒▒▒▒▒▒▒▒▒▓▒▓▒▓▒▓▓▓▓▓▓▓▓▓▓▓▓▓█▓█▓█▓██",
      "cross":   "🞨🞩🞪🞫🞬🞭🞮",
        "sqr":   "🞎🞖🞔🞐🞑🞒🞕🞓",
     "domino":   "🁣🁤🁥🁦🁧🁨🁩🁪🁫🁬🁭🁮🁯🁰🁱🁲🁳🁴🁵🁶🁷🁸🁹🁺🁻🁼🁽🁾🁿🂀🂁🂂🂃🂄🂅🂆🂇🂈🂉🂊🂋🂌🂍🂎🂏🂐o🂑🂒🂓🁢",
      "cards":   "🂱🂲🂳🂴🂵🂶🂷🂸🂹🂺🂻🂼🂽🂾🃏",
    # Computed using braille_shades.py
    "braille":   '⠀⠁⠂⠄⠈⠐⠠⡀⢀⠃⠅⠉⠑⠡⡁⢁⠆⠊⠒⠢⡂⢂⠌⠔⠤⡄⢄⠘⠨⡈⢈⠰⡐⢐⡠⢠⣀⠇⠋⠓⠣⡃⢃⠍⠕⠥⡅⢅⠙⠩⡉⢉⠱⡑⢑⡡⢡⣁⠎⠖⠦⡆⢆⠚⠪⡊⢊⠲⡒⢒⡢⢢⣂⠜⠬⡌⢌⠴⡔⢔⡤⢤⣄⠸⡘⢘⡨⢨⣈⡰⢰⣐⣠⠏⠗⠧⡇⢇⠛⠫⡋⢋⠳⡓⢓⡣⢣⣃⠝⠭⡍⢍⠵⡕⢕⡥⢥⣅⠹⡙⢙⡩⢩⣉⡱⢱⣑⣡⠞⠮⡎⢎⠶⡖⢖⡦⢦⣆⠺⡚⢚⡪⢪⣊⡲⢲⣒⣢⠼⡜⢜⡬⢬⣌⡴⢴⣔⣤⡸⢸⣘⣨⣰⠟⠯⡏⢏⠷⡗⢗⡧⢧⣇⠻⡛⢛⡫⢫⣋⡳⢳⣓⣣⠽⡝⢝⡭⢭⣍⡵⢵⣕⣥⡹⢹⣙⣩⣱⠾⡞⢞⡮⢮⣎⡶⢶⣖⣦⡺⢺⣚⣪⣲⡼⢼⣜⣬⣴⣸⠿⡟⢟⡯⢯⣏⡷⢷⣗⣧⡻⢻⣛⣫⣳⡽⢽⣝⣭⣵⣹⡾⢾⣞⣮⣶⣺⣼⡿⢿⣟⣯⣷⣻⣽⣾⣿',
 "center box": "□▤▥▧▨▦▩▣■"
}
