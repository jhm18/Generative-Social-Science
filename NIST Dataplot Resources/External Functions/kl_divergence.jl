#   KL-Divergence Utility to Enable Cacluating Dirvergences in Dataplot
#   Jonathan H. Morgan
#   11 June 2021

#   Activating the EnvironmentJS
    using Pkg
    Pkg.activate("/Users/jonathan.h.morgan/Dataplot_Resources/Dataplot_Utilities")
    Pkg.status()

########################
#   LOADING PACKAGES   #
########################
    using Glob               #Useful Package for String Manipulation
    using DataFrames         #Generates Julia Style DataFrames and DataFrame Manipulation
    using DataFramesMeta     #Facilitates DataFrame Manipulation
    using StatsBase          #Supports basic statistical operations

######################
#   IMPORTING DATA   #
######################
    #   Reading-In Directory information
        directory = readlines("/Users/jonathan.h.morgan/Desktop/directory.dat", keep=true)
        directory = split.(directory, ":")
        directory = convert(String,directory[1][2])
        directory = replace(directory, "\n" => "")
        directory = replace(directory, " " => "")
        directory = replace(directory,"Dropbox/MyMac(Jonathan’sMacBookPro)/" => "")
    
    #   Setting Directory
        cd(directory)
        pwd()

    #   Pulling-In Data
        file = readlines("datafile.dat", keep=true)

    #   Isolating Variable Names from DAT Files
        v_names = Symbol.(split(file[3]))

    #   Isolating Data Contents, Stripping, and Splitting
        file = file[5:(length(file)-1)]
        file = strip.(file)
        file = split.(file)

    #   Construct an empty DataFrame
        file_df  = DataFrames.DataFrame()

    #   Populating DataFrame
        for (j, name) in enumerate(v_names)
                file_df[:,name] = [file[l][j] for l in 1:length(file)]
        end

    #   Parsing File
        for j in 1:size(file_df)[2]
            var_j = parse.(Float64,file_df[:,j])
            if (sum(isinteger.(var_j)) == size(file_df)[1])
                file_df[!,j] = convert.(Int64, parse.(Float64,file_df[:,j]))
            else
                file_df[!,j] = parse.(Float64,file_df[:,j])
            end
        end

################################
#   PERFORMING KL-DIVERGENCE   #
################################

    #   Normalizing vectors
        for i in 1:size(file_df)[2]
          file_df[:,i] =  file_df[:,i]/sum(file_df[:,i])
        end

    #   Calculating the KL-Divergence
        kl_divergence = StatsBase.kldivergence(file_df[:,1], file_df[:,2])

########################
#   WRITING-OUT DATA   #
########################

outfile = string("kl_divergence", ".dat")
open(outfile, "w") do f
    println(f, kl_divergence)
end