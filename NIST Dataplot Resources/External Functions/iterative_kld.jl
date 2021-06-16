#   KL-Divergence Utility to Enable Cacluating Dirvergences from Random Samples in Dataplot
#   Jonathan H. Morgan
#   15 June 2021

#   Activating the Environment
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
    using Distributions      #Supports sampling from a variety of probability distributions
    using Random             #Used to shuffle values
    using RCall              #Used to move objects and functions in and out of R

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

    #   Looping through Random Permutations
        kld_scores = vec(fill(0.0, 1, 1000))
        for i in 1:length(kld_scores)
            #   Creating random Permutation
                sample_b = Random.shuffle(file_df[:,1])

            #   Calculating the Divergence
                kld_score = StatsBase.kldivergence(file_df[:,1], sample_b)

            #   Populating vector
                kld_scores[i] = kld_score
        end

########################
#   WRITING-OUT DATA   #
########################

outfile = string("kl_divergence", ".dat")
open(outfile, "w") do f
    for i in eachindex(kld_scores)
        println(f, kld_scores[i])
    end
end

#############
#   NOTES   #
#############

#   KL-Divergence is not Symmetric: DKL[P(X)∥Q(X)] ≠ DKL[Q(X)∥P(X)]

#   Generalized KL-Divergence
#    function gkl_div(a::AbstractArray{T}, b::AbstractArray{T}) where T<:AbstractFloat
#        n = length(a)
#        r = 0.0
#        for i = 1:n
#            @inbounds ai = a[i]
#            @inbounds bi = b[i]
#            if ai > 0
#                r += (ai * log(ai / bi) - ai + bi)
#            else
#                r += bi
#            end
#        end
#        return r::Float64
#    end

#   Generating Random Variable and Permuting for the Purpose of Testing
#    sample_a = true_data = [0.02, 0.03, 0.05, 0.14, 0.16, 0.15, 0.12, 0.08, 0.1, 0.08, 0.07]
#    sample_b = Random.shuffle(sample_a)

#    sample_a = sample_a/sum(sample_a)
#    sample_b = sample_b/sum(sample_b)

#   Comparing gkldiv to kldivergence and to R's_condition
#    gkl_div(sample_a, sample_b)
#    StatsBase.kldivergence(sample_a, sample_b)

#    @rput sample_a
#    @rput sample_b

#    R"""
#       library(philentropy)
#
#        x <- rbind(sample_a, sample_b)
#        philentropy::KL(x,  unit = "log")
#    """