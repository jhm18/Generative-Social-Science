#Open Street MapX Demonstration: https://github.com/pszufe/OpenStreetMapXPlot.jl
#Jonathan H. Morgan
#30 July 2020

################
#   PACKAGES   #
################

using Plots
using PyPlot
using OpenStreetMapX
using OpenStreetMapXPlot

######################
#   DEMONSTRATIONS   #
######################

#Creating Map Data
pth = joinpath(dirname(pathof(OpenStreetMapX)),"..","test","data","reno_east3.osm")
m =  get_map_data(pth,use_cache = false);
import Random
Random.seed!(0);
pointA = point_to_nodes(generate_point_in_bounds(m), m)
pointB = point_to_nodes(generate_point_in_bounds(m), m)
sr = shortest_route(m, pointA, pointB)[1]

#Creating Simple Street Map
Plots.gr()
p = OpenStreetMapXPlot.plotmap(m,width=600,height=400);
addroute!(p,m,sr;route_color="red");
plot_nodes!(p,m,[sr[1],sr[end]],start_numbering_from=nothing,fontsize=13,color="pink");
p
