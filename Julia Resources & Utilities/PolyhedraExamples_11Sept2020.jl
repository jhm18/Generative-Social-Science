#Polyhedra Examples
#Jonathan H. Morgan
#11 September 2020

################
#   PACKAGES   #
################

using Plots
using Polyhedra
using GeometryTypes
using CDDLib
using GLPK
using MeshCat
using Makie

######################################
#   EXAMPLES FROM THE DOCUMENTATION  #
######################################

#Example to Follow-Up On: https://github.com/JuliaPolyhedra/Polyhedra.jl/blob/master/examples/Polyhedral%20Function.ipynb

#2D Example

#Creating HalfSpace
h = HalfSpace([-1, 0], 1) ∩ HalfSpace([1, 0], 1) ∩ HalfSpace([-4, -1], 1) ∩ HalfSpace([2, -1], 1) ∩
    HalfSpace([-1/4, -1], 0) ∩ HalfSpace([1/2, -1], 0)

#Creating Polyhedron
solver = GLPK.Optimizer
lib = DefaultLibrary{Float64}(solver)
p = polyhedron(h, CDDLib.Library())

#Getting Extreme Points
vrep(p)
p

#Plotting
plot(p ∩ HalfSpace([0, 1], 4))
scatter!([x[1] for x in points(p)], [x[2] for x in points(p)])

#Come Back: Plot Doesn't Seem Work with 2D Examples

#3D Example

#Creating Convexhull
v = convexhull([0, 0, 0]) + conichull([1, 0, 0], [0, 1, 0], [0, 0, 1])

#Creating Polyhedron
p = polyhedron(v)

#Plotting
m = Polyhedra.Mesh(p)

struct Mesh{N, T, PT <: Polyhedron{T}} <: GeometryTypes.GeometryPrimitive{N, T}
    polyhedron::PT
end

vis = Visualizer()

setobject!(vis, m)

open(vis)
