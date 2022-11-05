mkdir frames
seq 0 23 | parallel --keep-order --group "./main {} frames/{}.tga"
ffmpeg -r 24 -i frames/%d.tga -filter_complex "split[p1][p2];[p1]palettegen[palette];[p2][palette]paletteuse" frames/output.gif