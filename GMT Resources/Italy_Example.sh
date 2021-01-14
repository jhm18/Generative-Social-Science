#! /usr/bin/env bash
gmt begin italy_example png
    gmt coast -R5/20/35/50 -Wthin -Gbeige -Df -Sazure -B -N1/thick,red -JM15c
    gmt inset begin -DjTR+w4c+o0.2c -M0 -F+gwhite+pthick
        gmt coast -Rg -JG10/40/4c -Gbeige -Bg -EIT+gred
    gmt inset end
gmt end show