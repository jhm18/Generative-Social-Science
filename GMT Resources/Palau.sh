#! /usr/bin/env bash

#Palau Regional Map
#Jonathan H. Morgan
#17 January 2022

#Palau Bound Box Coordinates
#sw.lng         sw.lat      ne.lng          ne.lat 
#131.011500     2.639400    135.076900      8.238674

gmt begin contour_palau pdf
    #   Creating Base Map
        gmt set FONT_ANNOT_PRIMARY 10p FORMAT_GEO_MAP ddd:mm:ssF
        gmt basemap -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c -B

    #   Plot Administrative regions
        gmt plot -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c PLW_adm1.gmt -Wthin,black -Gbeige 

    #   Plot Waterways
        gmt plot -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c PLW_water_lines_dcw.gmt -Wthin,blue

    #   Restrict to the negative contours by passing -Ln (N would include 0)
        gmt grdcontour N00E090.earth_relief_03m_p.nc  -Ln \
            -C500 -Wcthinnest,lightblue \
            -A2000+f6p -Wathin,lightblue

    #   Adding the Capital Star
        echo 134.624167 7.500556 > t.dat
        gmt plot t.dat -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c -Skstar/0.5c -Gplum2 -W0.01c,black

    #   Adding Region Names
        echo '134.38,7.342222,Koror' > region_text.txt
        echo '134.72,7.55,Babelthuap' >> region_text.txt
        echo '134.21,6.9092,Angaur' >> region_text.txt
        echo '134.32,7,Peleliu' >> region_text.txt
        echo '134.8,8.066667,Kayangel' >> region_text.txt
        echo '134.73,7.495,Ngerulmud' >> region_text.txt
        gmt text -R133.5/6.75/135/8.5+r region_text.txt -F+f9p,Helvetica,black

    #   Adding Sea Names
        echo '134,7.2,Philippine Sea' > sea_text.txt 
        echo '134.7,7.2,Pacific Ocean' >> sea_text.txt
        gmt text -R133.5/6.75/135/8.5+r sea_text.txt -F+f11p,Helvetica,blue

    #   Adding Map Inset of Entire Region of Palau
        gmt inset begin -DjTL+w1.5i+o0.15i/0.1i -F+gwhite+p1p+c0.1c
			gmt coast -Rg -JG133/5.5N/? -Da -Ggrey -A5000 -Bg -Wfaint -EDE+gbrown
		gmt inset end

    #   Adding Scale
        gmt basemap --FONT_ANNOT_PRIMARY=9p -LjLB+c19:23N+f+w50k+lKm.+u+o0.5c --FONT_LABEL=10p

    #   Adding Compass Rose
        gmt basemap -TdjCL+w3+f3+l,,,N --FONT_TITLE=15p
gmt end show

#Palau Population Density Map
gmt begin palau_population pdf
    #   Creating Base Map
        gmt set FONT_ANNOT_PRIMARY 10p FORMAT_GEO_MAP ddd:mm:ssF
        gmt basemap -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c -B

    #   Converting 2015 Population Density .tif file to a .grd file
    #   .tif file retrieved from: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/
    #   gmt grdconvert gpw_v4_population_density_rev11_2015_30_sec.tif -Ggpw_v4_population_density_rev11_2015_30_sec.grd -V

    #   Plot Grid Image: -I controlls illumnination, with a controlling the angle of the illumination, N indicates that a cumulative Laplace distribution was used to normalize the intensity.
    #   The following commands plot the coast, overlay the image, cut what does not fit in the polygon, redraws the polygon.
        gmt coast -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c  -Gc
		gmt grdimage gpw_v4_population_density_rev11_2015_30_sec.grd -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c -Q -B -Clajolla -I+a15+ne0.75 
		gmt clip -C
		gmt coast -R133.5/6.75/135/8.5+r -JC134.25/7.5/15c -Wthin -N1/thinner,darkred

    #   Restrict to the negative contours by passing -Ln (N would include 0)
        gmt grdcontour N00E090.earth_relief_03m_p.nc  -Ln \
            -C500 -Wcthinnest,lightblue \
            -A2000+f6p -Wathin,lightblue

    #   Adding Region Names
        echo '134.38,7.342222,Koror' > region_text.txt
        echo '134.72,7.55,Babelthuap' >> region_text.txt
        echo '134.21,6.9092,Angaur' >> region_text.txt
        echo '134.32,7,Peleliu' >> region_text.txt
        echo '134.8,8.066667,Kayangel' >> region_text.txt
        gmt text -R133.5/6.75/135/8.5+r region_text.txt -F+f9p,Helvetica,black

    #   Adding Sea Names
        echo '134,7.2,Philippine Sea' > sea_text.txt 
        echo '134.7,7.2,Pacific Ocean' >> sea_text.txt
        gmt text -R133.5/6.75/135/8.5+r sea_text.txt -F+f11p,Helvetica,blue

   #   Adding Map Inset of Entire Region of Palau
        gmt inset begin -DjTL+w1.5i+o0.15i/0.1i -F+gwhite+p1p+c0.1c
			gmt coast -Rg -JG133/5.5N/? -Da -Ggrey -A5000 -Bg -Wfaint -EDE+gbrown
		gmt inset end

    #   Adding Colorbar
        gmt colorbar -DJRM+o1c/0+e+mc -Bx25 -By+l"Per Sq. Km."

    #   Adding Scale
        gmt basemap --FONT_ANNOT_PRIMARY=9p -LjLB+c19:23N+f+w50k+lKm.+u+o0.5c --FONT_LABEL=10p

    #   Adding Compass Rose
        gmt basemap -TdjCL+w3+f3+l,,,N --FONT_TITLE=15p
gmt end show