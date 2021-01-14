#CogSci 2020 Survey Analysis
#Jonathan H. Morgan
#25 July 2020

#Setting Work Directory: Home
cd("/Users/jonathan.h.morgan/Desktop/Personal/ACS")

#Package Mode:
#activate "/Users/Desktop/Personal/ACS/ACS_Julia/Project.toml"

################
#   PACKAGES   #
################

using Glob              #Useful Package for String Manipulation
using CSV               #Export Files as CSV
using NamedArrays       #Replicating R's Named Lists Functionality
using DataFrames        #Generates Julia Style DataFrames and DataFrame Manipulation
using DataFramesMeta    #Facilitates DataFrame Manipulation
using Distributions     #Used to get moments of the Big 5 Distributions
using RCall             #Supports Calling R Functions
using Plots             #Nice for Basic Plotting
using GMT               #Generic Mapping Tools

#################
#   FUNCTIONS   #
#################

include("/Users/jonathan.h.morgan/Julia Resources/Julia_Utilities_28May2020.jl")

######################
#   IMPORTING DATA   #
######################
cd("/Users/jonathan.h.morgan/Desktop/Personal/ACS")

cogsci_2020 = DataFrame!(CSV.File("CogSci2020_Survey_25July2020.csv"))
cogsci_2020_numeric = DataFrame!(CSV.File("CogSci2020_Numeric_25July2020.csv"))
names(cogsci_2020)

#Location Data
cogsci_locations = DataFrame!(CSV.File("CogSci2020_Locations.csv"))
maximum(cogsci_locations.Long_DMS)
minimum(cogsci_locations.Long_DMS)
maximum(cogsci_locations.Lat_DMS)
minimum(cogsci_locations.Lat_DMS)

#######################
#   SUBSETTING DATA   #
#######################

cogsci_2020 = cogsci_2020[:,[9,14,15, 18, 19, 20, 21, 22, 23, 24]]
cogsci_2020_numeric = cogsci_2020_numeric[:,[9,14,15, 18, 19, 20, 21, 22, 23, 24]]
names(cogsci_2020)

#Experiences/Aspirations
experiences = cogsci_2020_numeric[1:15, 4]
e_elements = Int64[]

for i in 1:length(experiences)
    e_element = collect(experiences[i,1])
    filter!(x->x!=',', e_element)
    e_element = parse.(Int, e_element)
    append!(e_elements, e_element)
end

e_elements = sort(e_elements)

#Getting Counts per Experience Category
e_cat = unique(e_elements)
e_counts = [sum(e_elements .== i) for i in unique(e_elements)]
e_labels = ["Human-Computer Interaction Studies", "Reaction Time Experiments",
            "Psychophysical Responses", "Mixed Methods", "Other"]
e_tab = hcat(e_cat, e_labels, e_counts)

#Roles
roles = cogsci_2020_numeric[1:15, 6]
r_elements = Int64[]

for i in 1:length(roles)
    r_element = collect(roles[i,1])
    filter!(x->x!=',', r_element)
    r_element = parse.(Int, r_element)
    append!(r_elements, r_element)
end

r_elements = sort(r_elements)

#Getting Counts per Role Category
r_cat = unique(r_elements)
r_counts = [sum(r_elements .== i) for i in unique(r_elements)]
r_labels = ["Participant/Subject", "Experimenter",
            "Primary Investigator", "IRB Member"]
r_tab = hcat(r_cat, r_labels, r_counts)

#Add Row
r_other = hcat(5, "Other", 0)
r_tab = vcat(r_tab, r_other)

#Number of Studies
num_studies = cogsci_2020_numeric[1:15, 8]

num_cat = unique(num_studies)
num_counts = [sum(num_studies .== i) for i in unique(num_studies)]
num_labels = ["1-4", "5-10", "More than 10"]
num_tab = hcat(num_cat, num_labels, num_counts)

#Topics
topics = cogsci_2020_numeric[1:15, 9]
t_elements = Int64[]

for i in 1:length(topics)
    t_element = collect(topics[i,1])
    filter!(x->x!=',', t_element)
    t_element = parse.(Int, t_element)
    append!(t_elements, t_element)
end

t_elements = sort(t_elements)

#Getting Counts per Role Category
t_cat = unique(t_elements)
t_counts = [sum(t_elements .== i) for i in unique(t_elements)]
t_labels = ["Experimental Preparation and Setup", "Conducting an Experiment",
            "Assessment & Piloting", "Ensuring Repeatability", "Managing Ethical and Safety Challenges",
            "Working with IRB", "Other Concerns"]
t_tab = hcat(t_cat, t_labels, t_counts)

#########################
#   PLOTTING WITH GMT   #
#########################

#Institutional Homes
coast(region=[-127 135 16 59], land=:beige, water=:azure, shore=:thinnest,
      rivers=(type=0), borders=(type=1, pen=("thin", "black")),
      figsize=(14.3, 5.2), frame=:ag, resolution=:low)


GMT.plot!(cogsci_locations.Long_DMS, cogsci_locations.Lat_DMS, marker=:square,
          markeredgecolor=0, size=0.2, markerfacecolor=:blue,  markersize=0.15,
          fmt=:png, show=true)

#############
#   TESTS   #
#############

#U.S. MAP
coast(region=[-130 -70 24 52], proj=(name=:lambertConic, center=[-100 35], parallels=[33 45]),
      frame=:ag, res=:low, borders=((type=1, pen=("thick","red")), (type=2, pen=("thinner",))),
      area=500, land=:tan, water=:blue, shore=(:thinnest,:white), show=true)

coast(region=[110 140 20 35],                                   # The Map limits
      proj=(name=:Albers, center=[125 20], parallels=[25 45]),  # The projection parameters
      frame=:ag,          # Tell it to set annotations and grid lines automatically
      resolution=:low,    # Use the low resolution coastlines
      area=250,           # Do not plot polygons with areas < 250 km^2
      land=:green,        # Paint land with green
      shore=:thinnest,    # Coastlines are drwan with a 0.1 pt thickness
      show=true)

#Example Plot
using Libdl
push!(Libdl.DL_LOAD_PATH, "/opt/local/lib")

coast(region=:global, proj=:Winkel, frame=:g, area=10000,
      land=:burlywood4, water=:wheat1, figsize=12, show=true)

#Hello World Example
GMT.plot(1:10, rand(10), lw=1, lc=:blue, fmt=:png, marker=:square,
          markeredgecolor=0, size=0.2, markerfacecolor=:red, title="Hello World",
          xlabel="Spoons", ylabel="Forks", show=true)


#Points on a Globe
x = range(0, stop=2pi, length=180);       seno = sin.(x/0.2)*45;

coast(region=[0 360 -90 90], proj=(name=:laea, center=(300,30)), frame=:g,
                    res=:crude, land=:navy, figsize=6, show=false)

GMT.plot!(collect(x)*60, seno, lw=0.5, lc=:red, fmt=:png, marker=:circle,
            markeredgecolor=0, size=0.05, markerfacecolor=:cyan, show=true)


#Iso Code Map of the Italian Map with Borders
GMT.coast(region=:IT, pen=:thin, show=true)
