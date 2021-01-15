#Jonathan H. Morgan
#Processing Dataplot .DAT Files
#17 March 2019

#Setting Work Directory: Home
cd("/Users/jonathan.h.morgan/dataplot/Analysis_Scripts/GS")
pwd()

################
#   PACKAGES   #
################

using JLD               #Saving Data in Julia Format
using CSV               #Export Files as CSV
using DataFrames        #Generates Julia Style DataFrames and DataFrame Manipulation
using DataFramesMeta    #Facilitates DataFrame Manipulation
using Glob              #Useful Package for String Manipulation
using Statistics        #Necessary for even basic stats
using NamedArrays       #Replicating R's Named Lists Functionality
using LabelledArrays    #More Flexible in Terms of Types (Supports DataFrames), less Flexibile in Labeling

#################
#   FUNCTIONS   #
#################

function remove!(a, item)
    deleteat!(a, findall(x->x==item, a))
end

#######################
#   IMPORTING FILES   #
#######################

#Get .DAT files from the Directory
path = "/Users/jonathan.h.morgan/dataplot/Analysis_Scripts/GS"

#Creating and Writing Out Files List for Iterative Processing in Dataplot
dat_files = glob("*.DAT",path)
dat_files = dat_files[1:4]

#Creating Names for DataFrames Array
dat_names = Array{String}(undef, length(dat_files))
for i in 1:length(dat_files)
  last_slash = findlast("/", dat_files[i])
  last_slash = last_slash[1]
  dat_names[i] = dat_files[i][(last_slash + 1):length(dat_files[i])]
end
dat_names = Symbol.(dat_names)

#Creating Empty Labelled List
dataplot_files = @LArray map(_ -> DataFrames.DataFrame(), 1:length(dat_files)) (dat_names[1], dat_names[2], dat_names[3], dat_names[4])

#Reading-In Files
for i in 1:length(dat_files)

  #Reading in File
  file = readlines(dat_files[i], keep=true)

  #Isolating Variable Names from DAT Files
  v_names = split(file[3])

  #Isolating Data Contents, Stripping, and Splitting
  file = file[5:(length(file)-1)]
  file = strip.(file)
  file = split.(file)

  #Convert each item in array x[1] to a Symbol by broadcasting Symbol()
  #across the array with dot syntax#
  v_names = Symbol.(v_names)

  #Construct an empty DataFrame
  file_df  = DataFrames.DataFrame()

  #Loop through the namelist array, create a column in the DataFrame entitled namelist[i]
  #Assign its values by using an array comprehension to build an array with the appropriate values,
  #Starting at the second array in array
  for (j, name) in enumerate(v_names)
        file_df[:,name] = [file[l][j] for l in 1:length(file)]
  end

  #Adding Obs_ID for Joining
  file_df = insertcols!(file_df, 1, :Obs_ID => collect(1:1:nrow(file_df)))

  #Transforming String into Proper Variables by Exporting
  CSV.write("file_df.csv", file_df, header=names(file_df))
  file_df = CSV.read("file_df.csv")
  rm("file_df.csv", force=true)

  global dataplot_files[i] =  file_df
end

#####################################
#   JOINING DATAFRAMES & EXPORTING  #
#####################################

#Creating Base DataFrame with which to Merge
results  = dataplot_files[1]

#Assuming that all files are related and all the same size here
for i in 2:length(dat_files)
 global results = join(results, dataplot_files[i], on =:Obs_ID, kind = :outer)
end

#Writing Out Results
CSV.write("e_ag_event_deflection.csv", results, header=names(results))
#CSV.write("h_ag_event_deflection.csv", results, header=names(results))
#CSV.write("e_rec_event_deflection.csv", results, header=names(results))
#CSV.write("h_rec_event_deflection.csv", results, header=names(results))

########################
#    CHECKING RANGES   #
########################

#Address-the-Group Rate Comparison
results = CSV.read("e_ag_event_deflection.csv")
results = CSV.read("h_ag_event_deflection.csv")

#Reciprocity Comparison
results = CSV.read("e_rec_event_deflection.csv")
results = CSV.read("h_rec_event_deflection.csv")

results = results[:,2:ncol(results)]

min_values = NamedArray(rand(ncol(results),1))
setnames!(min_values, String.(names(results)), 1)

max_values = NamedArray(rand(ncol(results),1))
setnames!(max_values, String.(names(results)), 1)

#Calculating Minimum Values
for i in 1:ncol(results)
    min_value = minimum(results[:,i])
    global min_values[i] = min_value
end

#Calculating Maximum Values
for i in 1:ncol(results)
    max_value = maximum(results[:,i])
    global max_values[i] = max_value
end

################################################################
#   WRITING SMALL PROGRAM TO READ IN ALL FILES INTO DATAPLOT   #
################################################################

outfile = "dat_files_list.DP"
open(outfile, "w") do f
  for i in 1:length(dat_files)
    var_names = string(names(dataplot_files[i])[2], " ", names(dataplot_files[i])[3])
    command_file = string("READ", " ", dat_files[i], " ", var_names)
    println(f, command_file)
  end
end

#############
#   TESTS   #
#############

#median.(eachcol(dataplot_files[1]))

#Creating Named Lists (Problem with Named DataFrame Lists)
#dataplot_nfiles = NamedArray(map(_ -> DataFrames.DataFrame(), 1:length(dat_files)))

#Creating Names for DataFrames Array
#dat_names = Array{String}(undef, length(dat_files))
#for i in 1:length(dat_files)
#  last_slash = findlast("/", dat_files[i])
#  last_slash = last_slash[1]
#  dat_names[i] = dat_files[i][(last_slash + 1):length(dat_files[i])]
#end
#dat_names = Symbol.(dat_names)
#setnames!(dataplot_nfiles , dat_names, 1)

#@LArray Float64 (2,2) (:a,:b,:c,:d)
#test = @LArray map(_ -> DataFrames.DataFrame(), 1:length(dat_files)) (collect(data_names))
#test[1] = dataplot_files[1]
