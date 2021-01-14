#Egyt Contour Map: Airship Campaign
#Jonathan H. Morgan
#8 August 2020

#! /usr/bin/env bash

gmt begin contour_egypt png
     gmt basemap -REG -JM15c -B

    # Restrict to the positive contours by passing -Lp (P would include 0)
     gmt grdcontour @earth_relief_06m -Lp \
        -C500 -Wcthinnest,black \
        -A2000+f6p -Wathin,black

     # Restrict to the negative contours by passing -Ln (N would include 0)
    gmt grdcontour @earth_relief_06m -Ln \
        -C500 -Wcthinnest,lightblue \
        -A2000+f6p -Wathin,lightblue

    #Adding Inset to Clarify Global Position
    gmt inset begin -DjTL+w4c+o0.2c -M0 -F+gwhite+pthick
        gmt coast -Rg -JG10/40/4c -Gbeige -Bg -EEG+gred
    gmt inset end

    # Plot the coastlines using a thick black line
    gmt coast -Wthick,black

    #Adding Points
    gmt plot egypt_locations.csv -REG -JX15c/10c -Sc0.35c -G#58C73A -W0.03c,black 

gmt end show
