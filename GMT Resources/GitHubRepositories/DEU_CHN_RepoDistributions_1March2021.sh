#! /usr/bin/env bash

#   Assigning Size Variables
    germany_p_size = $(awk -F "\"*,\"*" '{print $3}' germany_city_points.txt)
    germany_b_size = $(awk -F "\"*,\"*" '{print $4}' germany_city_points.txt)
    china_p_size = $(awk -F "\"*,\"*" '{print $3}' china_city_points.txt)
    china_b_size = $(awk -F "\"*,\"*" '{print $4}' china_city_points.txt)

#   Germany: Repository Owner Densities
    gmt begin DEU_Distributions_11Feb2021 pdf
        gmt set FONT_ANNOT_PRIMARY 10p FONT_TITLE 18p FORMAT_GEO_MAP ddd:mm:ssF
        gmt coast -RDE+r1 -Wthin -Ggrey -EDE+gbeige -I1/blue -I2/blue  -Df -Sazure -B -BWSne -N1/thin,red -JM15c 
        gmt plot -RDE+r1 german_owner_densities.gmt -Wthick,brown
        gmt plot -RDE+r1 germany_city_points.txt -Sc$germany_p_sizec -Gplum2 -W$germany_b_sizec,black
        gmt text  -RDE+r1 germany_city_text.txt -F+f8p,Helvetica,black 
        gmt inset begin -DjTL+w1.5i+o0.15i/0.1i -F+gwhite+p1p+c0.1c
			gmt coast -Rg -JG12/52N/? -Da -Ggrey -A5000 -Bg -Wfaint -EDE+gbrown
		gmt inset end
        gmt basemap --FONT_ANNOT_PRIMARY=9p -LjLB+c19:23N+f+w500k+lkm+u+o0.5c --FONT_LABEL=10p
        gmt basemap --FONT_ANNOT_PRIMARY=9p -TdjTR+w2+f3 --FONT_LABEL=5p
    gmt end show

#   China: Repository Owner Densities
    gmt begin CHN_Distributions_11Feb2021 pdf
        gmt set FONT_ANNOT_PRIMARY 10p FONT_TITLE 18p FORMAT_GEO_MAP ddd:mm:ssF
        gmt coast -RCN+r1 -Wthin -Ggrey -ECN+gbeige -I1/blue -I2/blue  -Df -Sazure -B  -BWSne -N1/thin,red -JM15c  
        gmt plot -RCN+r1 chinese_owner_densities.gmt -Wthick,brown
        gmt plot -RCN+r1 china_city_points.txt -Sc$china_p_sizec -Gplum2 -W$china_b_sizec,black
        gmt text  -RCN+r1 china_city_text.txt -F+f8p,Helvetica,black -B -BWSne
        gmt inset begin -DjTL+w1i+o0.1i/0.1i -F+gwhite+p1p+c0.1c
			gmt coast -Rg -JG105/35N/? -Da -Ggrey -A5000 -Bg -Wfaint -ECN+gbrown
		gmt inset end
        gmt basemap --FONT_ANNOT_PRIMARY=9p -LjLB+c19:23N+f+w1000k+lkm+u+o0.5c --FONT_LABEL=10p
        gmt basemap --FONT_ANNOT_PRIMARY=9p -TdjTR+w2+f3 --FONT_LABEL=5p
    gmt end show

#   To Do
    #Add Margin Text: Contour intervals indicate the density of GitHub repository owners who reported thier locations,
    #                 with intervals indicating 25th, 50th, 75th, and 90th percentiles respectively.

    #Add small size key