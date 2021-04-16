#Cog.Exchange Exploaratory Analyses
#Jonathan H. Morgan
#25 February 2021

#   Pulling-In Utilities
    include("/Users/jonathan.h.morgan/Julia Resources/Julia_Utilities_7April2021.jl")

#   Activating the EnvironmentJS
    using Pkg
    Pkg.activate("/Users/jonathan.h.morgan/Desktop/DNAC/Cog.Exchange/Data_Scripts/Julia_Scripts")
    Pkg.status()

#   Defining my Package Versions for Reproducibility in the Future
    cd(mktempdir()) do
        Pkg.activate(".")
        Pkg.add(PackageSpec(name="CSV", version="0.8.3"))
        Pkg.add(PackageSpec(name="DataFrames", version="0.22.5"))
        Pkg.add(PackageSpec(name="DataFramesMeta", version="0.6.0"))
        Pkg.add(PackageSpec(name="Glob", version="1.3.0"))
        Pkg.add(PackageSpec(name="NamedArrays", version="0.9.4"))
        Pkg.add(PackageSpec(name="Plots", version="1.10.6"))
        Pkg.add(PackageSpec(name="RCall", version="0.13.10"))
        Pkg.add(PackageSpec(name="StatsBase", version="0.33.3"))
        Pkg.add(PackageSpec(name="StatsPlots", version="0.14.19"),

        preserve=Pkg.PRESERVE_DIRECT)
        Pkg.status()
    end

################
#   PACKAGES   #
################
  using Glob               #Useful Package for String Manipulation
  using CSV                #Export Files as CSV
  using NamedArrays        #Replicating R's Named Lists Functionality
  using DataFrames         #Generates Julia Style DataFrames and DataFrame Manipulation
  using DataFramesMeta     #Facilitates DataFrame Manipulation
  using Plots              #Nice for Basic Plotting
  using StatsBase          #Supports basic statistical operations
  using Statistics         #Supports Statistical Operations
  using StatsPlots         #Plotting packags with more support for statistical plotsegraph
  using RCall              #Used to move objects and functions in and out of R

################################
#   CLEANING-UP DATA & TYPES   #
################################

#   Setting Input Directory
    cd("/Users/jonathan.h.morgan/Desktop/DNAC/Cog.Exchange/Data_Scripts")
    pwd()

#   Pulling in R Datasets
    R_objects = R_import(pwd())
    R_keys = String.(keys(R_objects))
    R_objects =  R_objects[R_keys[1]]
    R_keys = Symbol.(keys(R_objects))

#   Pulling-In Spring 2010 Embeddedness Experiment
    embeddedness_2010 = DataFrame!(CSV.File("Spring 2010 Embeddedness Experiment.csv"))

#   Making Combined List to Clean-Up Data Structure
    molm_2013 = Dict("Spring2009" => R_objects[R_keys[2]], "Study_2_Fall2009" => R_objects[R_keys[1]], "Embeddedness_Spring2010" => embeddedness_2010)

#   Exporting & Importing to Clean-Up Types
    molm_keys = String.(keys(molm_2013_combined))
    for i in eachindex(molm_keys)
        #   Exporting R Data.DataFrame
            R_export(molm_2013[molm_keys[i]], pwd(), molm_keys[i], molm_keys[i])
    end

#   Re-Importing
    R_objects = R_import(pwd())
    R_keys = String.(keys(R_objects))

#   Making New Clean Dictionary   
    molm_2013_combined = Dict("Spring2009" => R_objects[R_keys[3]], "Study_2_Fall2009" => R_objects[R_keys[1]], "Embeddedness_Spring2010" =>  R_objects[R_keys[4]])
    molm_keys = String.(keys(molm_2013_combined))
    
#   Exporting Combined List
    R_export(molm_2013_combined, pwd(), "molm_2013", "molm_2013_combined")

############################
#   IMPORTING CLEAN DATA   #
############################

#   Importing molm_2013
    R_objects = Julia_Utilities.R_import(pwd())
    R_keys = String.(keys(R_objects))

#   Pulling-Out List
    molm_2013_combined = deepcopy(R_objects[R_keys[1]])
    molm_keys = Symbol.(keys(molm_2013_combined))

#   Importing Networks, Types, and Positions Dataset
    exchange_counts = DataFrame!(CSV.File("/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/DNAC/Cog.Exchange/Data_Scripts/Summary_PointsAnalysis_30March2021/ExchangeCounts_31March2021.csv"))

##################################
#   IDENTIFYING DATA STRUCTURE   #
##################################

#   Isolating the variable names from each dataset for the purposes of comparison
#       Spring 2009: 323 variables
#       Fall 2009: 349 variables
#       Spring 2010: 393 variables

    variable_names =  vec(fill(String[], length(molm_keys), 1))
    var_lengths = vec(fill(0, 1, length(molm_keys)))
    for i in eachindex(molm_keys)
        var_names = names(molm_2013_combined[molm_keys[i]])
        variable_names[i] = var_names
        var_lengths[i] = length(var_names)
    end

    molm_2013_combined[molm_keys[3]]

#   Breaking Data-Up by condition
    molm_conditions = Dict(molm_keys[i] => Dict() for i = 1:1:length(molm_keys))
    for i in eachindex(molm_keys)
        #   Identifying the conditions in each study
            conditions = sort(unique(molm_2013_combined[molm_keys[i]][:,1]))

        #   Subsetting by Condition
            conditions_list = Dict(string.(conditions)[i] => DataFrames.DataFrame() for i = 1:1:length(conditions))
            c_keys = String.(keys(conditions_list))
            for j in eachindex(conditions)
                conditions_list[c_keys[j]] = molm_2013_combined[molm_keys[i]][(molm_2013_combined[molm_keys[i]][:,1] .== conditions[j]), :]
            end

        #   Identifying the Number Missing in Each Column and Eliminating Columns that 100% Missing
            for j in eachindex(c_keys)
                c_names = Symbol.(names(conditions_list[c_keys[j]]))
                missing_vars = vec(fill(0, 1, length(c_names)))
                for k in eachindex(c_names)
                    if ( sum(ismissing.(conditions_list[c_keys[j]][:,k])) == size(conditions_list[c_keys[j]])[1]   )
                        missing_vars[k] = k
                    else
                        missing_vars = missing_vars
                    end
                end

                filter!(x->x!=0, missing_vars)
                conditions_list[c_keys[j]] = conditions_list[c_keys[j]][:, DataFrames.Not(missing_vars)]
            end
        
        #   Populating Dictionary
            molm_conditions[molm_keys[i]] =  conditions_list
    end

#   Saving Dictionary with Condition-Level Datasets
    dir = "/Users/jonathan.h.morgan/Desktop"
    Julia_Utilities.R_export(molm_conditions,dir, "molm_conditions", "molm_conditions_2March2021")

########################################
#   CREATING NTRIAL & NPOINT DATASET   #
########################################

#   Setting Working Directory
    cd("/Users/jonathan.h.morgan/Desktop/DNAC/Cog.Exchange/Data_Scripts")

#   Extracting Conditions Dataset
    R_objects = Julia_Utilities.R_import(pwd())
    R_keys = String.(keys(R_objects))

    molm_combined = deepcopy(R_objects[R_keys[1]])
    molm_keys = Symbol.(keys(molm_combined))

#   Isolating NPOINTS & NTRIAL COLUMNS
    output_dictionary = Dict(molm_keys[i] => DataFrames.DataFrame() for i = 1:1:length(molm_keys))
    for i in eachindex(molm_keys)
        semester = molm_combined[molm_keys[i]]
        if (i == 2)
            semester_data = [select(semester, ["cond", "case", "sex", "exp" ,"form"]) select(semester, occursin.("ntrial", names(semester))) select(semester, occursin.("npoints", names(semester))) select(semester, occursin.("earn", names(semester)))]
            semester_data =  [vec(fill(string(molm_keys[i]), 1, size(semester_data)[1])) semester_data]
            rename!(semester_data, Dict(:x1 => string("semester")))
            output_dictionary[molm_keys[i]] = semester_data
        else
            semester_data = [select(semester, ["COND", "CASE", "SEX", "EXP", "FORM"]) select(semester, occursin.("NTRIAL", names(semester))) select(semester, occursin.("NPOINTS", names(semester))) select(semester, occursin.("EARN", names(semester)))]
            semester_data =  [vec(fill(string(molm_keys[i]), 1, size(semester_data)[1])) semester_data]
            rename!(semester_data, Dict(:x1 => string("semester")))
            output_dictionary[molm_keys[i]] = semester_data
        end
    end

#   Outputting Summary Wide Datasets for References
    cd("/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/DNAC/Cog.Exchange/Data_Scripts/Summary_PointsAnalysis_30March2021")
    for i in eachindex(molm_keys)
        output_dictionary[molm_keys[i]] = sort(output_dictionary[molm_keys[i]], [Symbol(names(output_dictionary[molm_keys[i]])[2]),  Symbol(names(output_dictionary[molm_keys[i]])[3])])
        CSV.write(string(string(molm_keys[i]),".csv"), output_dictionary[molm_keys[i]], header=true)
    end

#   Creating Long Dataset by Stacking Stages
    stages_list = vec(fill(DataFrames.DataFrame(), 1, length(molm_keys)))
    for i in eachindex(stages_list)
        if (i == 1)
                semester_data = output_dictionary[molm_keys[i]]
                stages = vec(fill(DataFrames.DataFrame(), 1, 2))
                for j in eachindex(stages)
                    stages[j] = [select(semester_data, ["COND", "CASE", "SEX", "EXP", "FORM", "NTRIAL"]) select(semester_data, occursin.(string("NTRIAL",j), names(semester_data))) select(semester_data, occursin.(string("NPOINTS", j), names(semester_data))) select(semester_data, occursin.(string("EARN", j), names(semester_data)))]
                    stages[j] = [vec(fill(j, 1, size(stages[j])[1])) stages[j]]
                    rename!(stages[j], Dict(:x1 => string("stage")))
                    stages[j] = [select(semester_data, "semester") stages[j]]
                end

                for j in eachindex(stages)
                    DataFrames.rename!(stages[j], Symbol.(["semester","stage","condition","case","gender","exp", "form", "ntrial_total", "ntrial","a1_point_total","a2_point_total","b1_point_total","b2_point_total", "a1_earn_total","a2_earn_total","b1_earn_total","b2_earn_total"]))
                end

                stages_data = [stages[1]; stages[2]]
                stages_list[i] = stages_data
        elseif (i == 2)
                semester_data = output_dictionary[molm_keys[i]]
                stages = vec(fill(DataFrames.DataFrame(), 1, 2))
                for j in eachindex(stages)
                    stages[j] = [select(semester_data, ["cond", "case", "sex", "exp","form", "ntrial"]) select(semester_data, occursin.(string("ntrial",j), names(semester_data))) select(semester_data, occursin.(string("npoints", j), names(semester_data))) select(semester_data, occursin.(string("earn", j), names(semester_data)))]
                    stages[j] = [vec(fill(j, 1, size(stages[j])[1])) stages[j]]
                    rename!(stages[j], Dict(:x1 => string("stage")))
                    stages[j] = [select(semester_data, "semester") stages[j]]
                end

                for j in eachindex(stages)
                    DataFrames.rename!(stages[j], Symbol.(["semester","stage","condition","case","gender","exp", "form", "ntrial_total", "ntrial","a1_point_total","a2_point_total","b1_point_total","b2_point_total","a1_earn_total","a2_earn_total","b1_earn_total","b2_earn_total"]))
                end

                stages_data = [stages[1]; stages[2]]
                stages_list[i] = stages_data
        else
                semester_data = output_dictionary[molm_keys[i]]
                stages = vec(fill(DataFrames.DataFrame(), 1, 3))
                for j in eachindex(stages)
                    index = [0, 1, 2]
                    stages[j] = [select(semester_data, ["COND", "CASE", "SEX", "EXP", "FORM", "NTRIAL"]) select(semester_data, occursin.(string("NTRIAL",index[j]), names(semester_data))) select(semester_data, occursin.(string("NPOINTS", index[j]), names(semester_data))) select(semester_data, occursin.(string("EARN", index[j]), names(semester_data)))]
                    stages[j] = [vec(fill(j, 1, size(stages[j])[1])) stages[j]]
                    rename!(stages[j], Dict(:x1 => string("stage")))
                    stages[j] = [select(semester_data, "semester") stages[j]]
                end

                for j in eachindex(stages)
                    DataFrames.rename!(stages[j], Symbol.(["semester","stage","condition","case","gender","exp", "form","ntrial_total", "ntrial","a1_point_total","a2_point_total","b1_point_total","b2_point_total", "a1_earn_total","a2_earn_total","b1_earn_total","b2_earn_total"]))
                end

                stages_data = [stages[1]; stages[2]; stages[3]]
                stages_list[i] = stages_data
        end
    end

#   Formatting & Sorting
    stages_data = [stages_list[2]; stages_list[1]; stages_list[3]]
    
    semester_id = DataFrames.DataFrame([[1: 1: 3;] ["Spring2009", "Study_2_Fall2009", "Embeddedness_Spring2010"]])
    semester_id[!,1] = convert(Array{Int64}, semester_id[:,1])
    semester_id[!,2] = convert(Array{String}, semester_id[:,2])
    DataFrames.rename!(semester_id, Symbol.(["semester_id", "semester"]))
    stages_data = rightjoin(semester_id, stages_data, on = :semester)

    stages_data = sort(stages_data, [:semester_id, :stage, :condition, :case, :exp, :form])

#   Getting Position Totals for Each Position and Comparing to Condition Data
    condition_id = unique(stages_data.condition)
    conditions_list = vec(fill(DataFrames.DataFrame(), 1, length(condition_id)))
    for i in eachindex(conditions_list)
        condition_data = stages_data[(stages_data.condition .== condition_id[i]), :]
        groups_data = vec(fill(DataFrames.DataFrame(), 1, length(unique(condition_data.case))))
        case_id = unique(condition_data.case)
        for j in eachindex(groups_data)
            #   Isolating cases
                case_data = condition_data[(condition_data.case .== case_id[j]), :]

            #   Isolating ntrial meta-data
                ntrials = case_data.ntrial
                ntrials = string(ntrials)

            #   Isolating Points
                a1_points = case_data.a1_point_total
                a1_points = string(a1_points)
                a2_points = vec(fill("a", 1, size(case_data)[1]))
                for k in 1:length(a2_points)
                    if(typeof(case_data.a2_point_total[k])== Int64 )
                        a2_points[k] = string(case_data.a2_point_total[k])
                    else
                        a2_points[k] = "missing"
                    end
                end
                sep = string(", ")
                if (length(a2_points) == 2)
                    a2_points = vec(fill(a2_points[1] * sep * a2_points[2], 1, 1))
                else
                    a2_points = vec(fill(a2_points[1] * sep * a2_points[2] * sep * a2_points[3], 1, 1))
                end

                b1_points = case_data.b1_point_total
                b1_points = string(b1_points)
                b2_points = case_data.b2_point_total
                b2_points = string(b2_points)

            #   Isolating Earned Points
                a1_earn = case_data.a1_earn_total
                a1_earn = string(a1_earn)
                a2_earn = vec(fill("a", 1, length(case_data.a2_earn_total)))
                for k in 1:size(case_data)[1]
                    if(typeof(case_data.a2_earn_total[k])== Int64 )
                        a2_earn[k] = string(case_data.a2_earn_total[k])
                    else
                        a2_earn[k] = "missing"
                    end
                end
                sep = string(", ")
                if (length(a2_earn) == 2)
                    a2_earn = vec(fill(a2_earn[1] * sep * a2_earn[2], 1, 1))
                else
                    a2_earn = vec(fill(a2_earn[1] * sep * a2_earn[2] * sep * a2_earn[3], 1, 1))
                end

                b1_earn = case_data.b1_earn_total
                b1_earn = string(b1_earn)
                b2_earn = case_data.b2_earn_total
                b2_earn = string(b2_earn)

            #   Identifying Numeric Variables & Summing Rows
                numcols = names(case_data[:,(11:18)], findall(x -> eltype(x) <: Union{Missing,Number}, eachcol(case_data[:,(11:18)])))
                col_matrix = Matrix(case_data[:,numcols])
                col_sums = sum(col_matrix, dims=1)
                col_sums = map(c -> collect(Missings.replace(c, 0)), eachcol(col_sums))
                col_sums = DataFrames.DataFrame(col_sums)
                DataFrames.rename!(col_sums, Symbol.(numcols))

            #   Adding Back Metadata
                meta_data = DataFrames.DataFrame(case_data[1,(1:8)])
                meta_data.ntrials = vec(fill(ntrials, 1, 1))
                meta_data.ntrials_total = vec(fill(select(case_data,"ntrial_total")[1,1], 1, 1))
                meta_data.a1_points = vec(fill(a1_points, 1, 1))
                meta_data.a1_earn = vec(fill(a1_earn, 1, 1))
                meta_data.a2_points = a2_points
                meta_data.a2_earn = a2_earn
                meta_data.b1_points = vec(fill(b1_points, 1, 1))
                meta_data.b1_earn = vec(fill(b1_earn, 1, 1))
                meta_data.b2_points = vec(fill(b2_points, 1, 1))
                meta_data.b2_earn = vec(fill(b2_earn, 1, 1))
                group_data = [meta_data col_sums]
                group_data = group_data[:,[1,2,3,4,5,6,7,8,9,10,11,19,12,23,13,20,14,24,15,21,16,25,17,22,18,26]]

            #   Populating Groups Data
                groups_data[j] = group_data
        end
        base_data = groups_data[1]
        for j in 2:length(groups_data)
            base_data = [base_data; groups_data[j]]
        end
        conditions_list[i] = base_data
    end
    
    condition_counts = conditions_list[1]
    for i in 2:length(conditions_list)
        condition_counts = [condition_counts; conditions_list[i]]
    end
    condition_counts = condition_counts[:,[1,2,4,5,6,7,8,9,10,11,12,13,14, 15, 16, 17, 18,19,20,21,22,23,24,25,26]]

#   Adding Network Types
    network_id = vec(fill(0, 1, size(condition_counts)[1]))
    for i in eachindex(network_id)
        if(condition_counts[i,3] >= 5) & (condition_counts[i,3] < 11)
            network_id[i] = 3
        else
            network_id[i] = 4
        end
    end
    DataFrames.insertcols!(condition_counts, 8, :network_size => network_id)

#   Adding Condition Identifier
    condition = ["Negotiated, Reciprocated", "Reciprocated, Negotiated", "Reciprocated, Reciprocated", "Negotiated, Negotiated", "Negotiated, Reciprocated", "Reciprocated, Negotiated", "Reciprocated, Reciprocated", 
                 "Negotiated, Negotiated", "PN, REN, REN", "PR, NER, NER", "PN, REN, REN", "PR, NER, NER"]

    condition_id = DataFrames.DataFrame([unique(condition_counts[:,3]) condition])
    DataFrames.rename!(condition_id, Symbol.(["condition","condition_label"]))
    condition_counts = rightjoin(condition_id, condition_counts, on = :condition)
    condition_counts = condition_counts[:, [3, 4, 1, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ,27]]
    
#   Writing-Out Data to Compare with Component Datasets
    CSV.write("PositionCounts_30March2021.csv", condition_counts, header=true)

###################################
#   POINTS by CONDITION by NODE   #  
###################################

#   Setting Working Directory
    cd("/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/DNAC/Cog.Exchange/Data_Scripts/Summary_PointsAnalysis_30March2021")

#   Importing Data
    condition_counts = DataFrame!(CSV.File("PositionCounts_30March2021.csv"))

#   Subsetting Data
    position_counts = condition_counts[:, [1,2,3,4,5, 9,10,12,16,20,24]]

#   Separating Group Types, Trials, & Points
    exchange_list = vec(fill(DataFrames.DataFrame(), 1, size(position_counts)[1]))
    for i in eachindex(exchange_list)
        row_data = position_counts[i,:]
        condition = split.(row_data[4], ',')
        condition = filter.(x -> !isspace(x), condition) 
        ntrials = split.(row_data[7], ',')
        ntrials = replace.(ntrials, "[" => " ")
        ntrials = replace.(ntrials, "]" => " ")
        ntrials = filter.(x -> !isspace(x), ntrials) 
        ntrials = parse.(Int64, ntrials)

        a1_points = split.(row_data[8], ',')
        a1_points = replace.(a1_points, "[" => " ")
        a1_points = replace.(a1_points, "]" => " ")
        a1_points = filter.(x -> !isspace(x), a1_points) 
        a1_points = parse.(Int64, a1_points)

        a2_points = split.(row_data[9], ',')
        a2_points = replace.(a2_points, "[" => " ")
        a2_points = replace.(a2_points, "]" => " ")
        a2_points = filter.(x -> !isspace(x), a2_points) 
        if(a2_points[1] == "missing")
            a2_points = a2_points
        else
            a2_points = parse.(Int64, a2_points)
        end

        b1_points = split.(row_data[10], ',')
        b1_points = replace.(b1_points, "[" => " ")
        b1_points = replace.(b1_points, "]" => " ")
        b1_points = filter.(x -> !isspace(x), b1_points) 
        b1_points = parse.(Int64, b1_points)
    
        b2_points = split.(row_data[11], ',')
        b2_points = replace.(b2_points, "[" => " ")
        b2_points = replace.(b2_points, "]" => " ")
        b2_points = filter.(x -> !isspace(x), b2_points) 
        b2_points = parse.(Int64, b2_points)

        row_matrix = [condition ntrials a1_points a2_points b1_points b2_points]
        row_matrix = [vec(fill(row_data[1], 1, size(row_matrix)[1])) vec(fill(row_data[2], 1, size(row_matrix)[1])) vec(fill(row_data[6], 1, size(row_matrix)[1]))  vec(fill(row_data[5], 1, size(row_matrix)[1])) vec(fill(row_data[3], 1, size(row_matrix)[1])) row_matrix]
        row_matrix = DataFrames.DataFrame(row_matrix)
        DataFrames.rename!(row_matrix, Symbol.(["semester_id", "semester", "network_size", "case", "condition", "exchange_type", "ntrials", "a1_points", "a2_points", "b1_points", "b2_points"]))

        exchange_list[i] = row_matrix
    end

#   Stacking DataFrame List
    exchange_counts = exchange_list[1]
    for i in 2:length(exchange_list)
        exchange_counts = [exchange_counts; exchange_list[i]]
    end

#   Exporting Dataset
#   CSV.write("ExchangeCounts_31March2021.csv", exchange_counts, header=true)

#   Creating a List of Subset by Network for the Purposes of Export & Summary_PointsAnalysis_30March2021
    networks_list = Dict("4-Person Network" => DataFrames.DataFrame(), "3-Person Network" => DataFrames.DataFrame())
    net_keys = String.(keys(networks_list))
    network_counts = [3; 4]
    for i in eachindex(network_counts)
        networks_list[net_keys[i]] = exchange_counts[(exchange_counts.network_size .== network_counts[i]), :]
    end

#   Eliminating Position a_2 to get rid of missing values & Converting a_2 points to Integer
    networks_list[net_keys[1]] = select(networks_list[net_keys[1]], Not(:a2_points))
    networks_list[net_keys[2]][!,9] = parse.(Int64, networks_list[net_keys[2]][:,9])

#   Calculation Points to Trials relation
    for i in eachindex(net_keys)
        if(i == 1)
            networks_list[net_keys[i]] = @eachrow networks_list[net_keys[i]]  begin
                @newcol points_trials_ratio::Array{Float64}
                :points_trials_ratio = (:a1_points + :b1_points + :b2_points)/:ntrials
            end
        else
            networks_list[net_keys[i]] = @eachrow networks_list[net_keys[i]]  begin
                @newcol points_trials_ratio::Array{Float64}
                :points_trials_ratio = (:a1_points + :a2_points + :b1_points + :b2_points)/:ntrials
            end
        end
    end

#   Eliminating Practice Rows & Irrelevant Variables
    for i in eachindex(net_keys)
        networks_list[net_keys[i]] = select!(networks_list[net_keys[i]], Not([:semester, :condition]))
        networks_list[net_keys[i]] = networks_list[net_keys[i]][(networks_list[net_keys[i]].exchange_type .!= "PR"), :]
        networks_list[net_keys[i]] = networks_list[net_keys[i]][(networks_list[net_keys[i]].exchange_type .!= "PN"), :]
    end

#   Creatign Ratio Dataset for Purposes of Generating the Block Plots
    nets_list = deepcopy(networks_list)
    for i in eachindex(net_keys)
        nets_list[net_keys[i]] = nets_list[net_keys[i]][:,[2,4,size(nets_list[net_keys[i]])[2]]]
    end
    exchange_ratio = [nets_list[net_keys[1]]; nets_list[net_keys[2]]]

#   Calculating Mean Ratios
    group_means = combine(groupby(exchange_ratio, [:network_size, :exchange_type]), nrow, :points_trials_ratio => mean => :mean)

#   Exporting to Dataplot
    Julia_Utilities.dataplot_export(exchange_ratio, "cog_exchange_ratios")

#   Stacking Data
    for i in eachindex(net_keys)
        if(i == 1)
            networks_list[net_keys[i]] = stack(networks_list[net_keys[i]], [:a1_points, :b1_points, :b2_points])
            rename!(networks_list[net_keys[i]], Dict(:variable => string("position")))
        else
            networks_list[net_keys[i]] = stack(networks_list[net_keys[i]], [:a1_points, :a2_points,:b1_points, :b2_points])
            rename!(networks_list[net_keys[i]], Dict(:variable => string("position")))
        end
    end 
    exchange_positions = [networks_list[net_keys[1]]; networks_list[net_keys[2]]]

#   Calculating Mean Points & SD
    points_means = combine(groupby(exchange_positions, [:network_size, :exchange_type]), nrow, :value => mean => :mean)
    points_sd = combine(groupby(exchange_positions, [:network_size, :exchange_type]), nrow, :value => Statistics.std => :sd)

#   Exporting to Dataplot
    Julia_Utilities.dataplot_export(exchange_positions, "exchange_positions")

#################################
#   IMPORTING SIMULATION DATA   #
#################################

#   Setting Directory to the Simulation Directory
    cd("/Users/jonathan.h.morgan/Desktop/DNAC/Cog.Exchange/Data_Scripts/Simulations")

#   Getting a list of txt files in the Directory
    text_files = glob("*.txt")
    text_files = sort(text_files)

#   Creating Import Dictionary & Key
    file_names = split.(text_files, ".")
    list_names = vec(fill("a", 1, length(file_names)))
    for i in eachindex(file_names)
        file_name = vec(fill(file_names[i][1], 1 , 1))
        list_names[i] = string(collect(file_name)[1])
    end
    list_names = sort(list_names)

    simulation_conditions = Dict(list_names[i] => DataFrames.DataFrame() for i = 1:1:length(list_names))
    simulation_conditions = sort(simulation_conditions)
    sim_keys = String.(keys(simulation_conditions))

#   Importing Text Files
    for i in eachindex(sim_keys)
        simulation_conditions[sim_keys[i]] = DataFrame!(CSV.File(text_files[i]))
    end

#########################
#   A-POSITION RATIOS   #
#########################

#   Note: Assumes that simulation_conditions has been generated: Lines 520-543 

#   Aggregating: By Task & Position
    simulation_outputs = sort(Dict(sim_keys[i] => DataFrames.DataFrame() for i = 1:1:length(sim_keys)))
    for i in eachindex(sim_keys)
        agg_data = combine(groupby(simulation_conditions[sim_keys[i]], [:Group, :Partner]), nrow, :Payoff => sum => :sum)
        data_label = split(sim_keys[i], "-")
        var_label = string(data_label[1], " ", data_label[2])
        agg_data = [vec(fill(var_label, 1, nrow(agg_data))) agg_data]
        rename!(agg_data, Dict(:x1 => string("Condition")))
        
        group_id = sort(unique(agg_data.Group))
        group_list = vec(fill(DataFrames.DataFrame(), 1, length(group_id)))
        for j in eachindex(group_id)
            group_data = agg_data[(agg_data.Group .== group_id[j]), :]
            total = sum(group_data.sum)
            group_data.p_ratio = group_data.sum ./ total
            group_list[j] = group_data
        end

        condition_data = group_list[1]
        for j in 2:length(group_list)
                condition_data = [condition_data; group_list[j]]
        end
        
        simulation_outputs[sim_keys[i]] = condition_data
    end

#   Stacking Data
    sim_data = simulation_outputs[sim_keys[1]]
    for i in 2:length(simulation_outputs)
        iteration_data = simulation_outputs[sim_keys[i]]
        sim_data = [sim_data; iteration_data]
    end

#   Create Position IDs for Simulation Data
    partner_labels = sort(unique(sim_data.Partner))
    partner_ids = [1; 1; 2; 3; 4]
    partner_index = DataFrame([partner_labels, partner_ids])
    DataFrames.rename!(partner_index, Symbol.(["Partner", "p_id"]))
    sim_data = rightjoin(partner_index, sim_data, on = :Partner)

#   Calculating the Mean Ratios & SDs across Groups for the Simulated Data
    #sim_data = combine(groupby(sim_data, [:Condition, :Partner]),:p_ratio => mean => :mean_ratio, :p_ratio => Statistics.std  => :ratio_std)

#   Sorting Stacked Data & Assigning Numeric ID
    sim_data = sort(sim_data, [:Condition, :Partner])
    partner_id = sort(unique(sim_data.Partner))
    id = [1; 1; 2; 3; 4]
    partner_key = DataFrames.DataFrame([partner_id id])
    partner_key[!,1] = convert.(String, partner_key[:,1])
    partner_key[!,2] = convert.(Int64, partner_key[:,2])
    DataFrames.rename!(partner_key, Symbol.(["Partner", "p_id"]))

    sim_data = rightjoin(partner_key, sim_data, on = :Partner)

#   IMPORTANT: Calculating the Means across the 12 groups from exchange_positions
    #points_means = combine(groupby(exchange_positions, [:network_size, :exchange_type, :position]), nrow, :value => mean => :mean)
    #points_means = sort(points_means, [:network_size, :exchange_type, :position])

#   Importing Exchange Counts
    exchange_counts = DataFrame!(CSV.File("/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/DNAC/Cog.Exchange/Data_Scripts/Summary_PointsAnalysis_30March2021/ExchangeCounts_31March2021.csv"))
    exchange_counts = sort(exchange_counts, [:semester_id, :network_size, :exchange_type, :case])

#   Calculating Ratios for each Position
    network_sizes = sort(unique(exchange_counts.network_size))
    networks = vec(fill(DataFrames.DataFrame(),1,length(network_sizes)))
    for i in eachindex(network_sizes)
        network_data = exchange_counts[(exchange_counts.network_size .== network_sizes[i]), :]
        exchange_types = sort(unique(network_data.exchange_type))
        exchange_types = ["Negotiated"; "Reciprocated"]    #Filtering for BRiMS Here
        types_list = vec(fill(DataFrames.DataFrame(), 1, length(exchange_types)))
        for j in eachindex(exchange_types)
            exchange_data = network_data[(network_data.exchange_type .== exchange_types[j]), :]
            if (network_sizes[i] == 3)
                exchange_data.total = exchange_data.a1_points .+ exchange_data.b1_points .+ exchange_data.b2_points
                exchange_data.a1_ratio = exchange_data.a1_points ./  exchange_data.total
                exchange_data.b1_ratio = exchange_data.b1_points ./  exchange_data.total
                exchange_data.b2_ratio = exchange_data.b2_points ./  exchange_data.total
                exchange_data = select(exchange_data, [:network_size, :exchange_type, :a1_ratio, :b1_ratio, :b2_ratio])
                exchange_data = stack(exchange_data, [:a1_ratio, :b1_ratio, :b2_ratio])
            else
                exchange_data[!,9] = parse.(Int64, exchange_data[:,9])
                exchange_data.total = exchange_data.a1_points .+ exchange_data.a2_points .+ exchange_data.b1_points .+ exchange_data.b2_points
                exchange_data.a1_ratio = exchange_data.a1_points ./  exchange_data.total
                exchange_data.a2_ratio = exchange_data.a2_points ./  exchange_data.total
                exchange_data.b1_ratio = exchange_data.b1_points ./  exchange_data.total
                exchange_data.b2_ratio = exchange_data.b2_points ./  exchange_data.total
                exchange_data = select(exchange_data, [:network_size, :exchange_type, :a1_ratio, :a2_ratio, :b1_ratio, :b2_ratio])
                exchange_data = stack(exchange_data, [:a1_ratio, :a2_ratio, :b1_ratio, :b2_ratio])
            end
            types_list[j] = exchange_data
        end

        types = [types_list[1]; types_list[2]]
        DataFrames.rename!(types, Symbol.(["network_size", "exchange_type ", "position", "p_ratio"]))

        networks[i] = types
    end
    exchange_ratios = [networks[1]; networks[2]]

#   Creating Numeric Indicator Variables
    position_id =  sort(unique(exchange_ratios.position))
    id = [1; 2; 3; 4]
    position_key = DataFrames.DataFrame([position_id id])
    position_key[!,1] = convert.(String, position_key[:,1])
    position_key[!,2] = convert.(Int64, position_key[:,2])
    DataFrames.rename!(position_key, Symbol.(["position", "p_id"]))

    exchange_ratios = rightjoin(position_key, exchange_ratios, on = :position)
    DataFrames.rename!(exchange_ratios, Symbol.(["position", "p_id", "network_size", "exchange_type", "p_ratio"]))

#   Creating Condition ID for Experimental and Empirical Data
    condition_labels = sort(unique(string.(exchange_ratios.exchange_type, " ", exchange_ratios.network_size)))
    condition_labels = lowercasefirst.(condition_labels)
    c_labels = vec(fill("a", 1, nrow(exchange_ratios)))
    for i in eachindex(c_labels)
        if (exchange_ratios[i,3] == 3) & (exchange_ratios[i,4] == "Negotiated")
            c_labels[i] = condition_labels[1]
        elseif (exchange_ratios[i,3] == 4) & (exchange_ratios[i,4] == "Negotiated")
            c_labels[i] = condition_labels[2]
        elseif (exchange_ratios[i,3] == 3) & (exchange_ratios[i,4] == "Reciprocated")
            c_labels[i] = condition_labels[3]
        else
            c_labels[i] = condition_labels[4]
        end
    end

#   Creating Ratio Variable
    exchange_ratios = [c_labels exchange_ratios]
    rename!(exchange_ratios, Dict(:x1 => string("condition")))

#   Creating Condition IDs
    conditions_key = DataFrames.DataFrame([[1: 1: length(condition_labels);] condition_labels])
    conditions_key[!,1] = convert.(Int64, conditions_key[:,1])
    conditions_key[!,2] = convert.(String, conditions_key[:,2])
    DataFrames.rename!(conditions_key, Symbol.(["con_id", "condition"]))

    empirical_data = rightjoin(conditions_key, exchange_ratios, on = :condition)

    condition_labels = sort(unique(sim_data.Condition))
    conditions_key = DataFrames.DataFrame([[1: 1: length(condition_labels);] condition_labels])
    conditions_key[!,1] = convert.(Int64, conditions_key[:,1])
    conditions_key[!,2] = convert.(String, conditions_key[:,2])
    DataFrames.rename!(conditions_key, Symbol.(["con_id", "Condition"]))

    sim_data = rightjoin(conditions_key, sim_data, on = :Condition)

#   Moving into Final Stacked Dataset
    sim_ratio = sim_data[:,[3,2,8]]
    sim_ratio = [vec(fill("Simulation", 1, size(sim_ratio)[1])) sim_ratio]
    rename!(sim_ratio, Dict(:x1 => string("data")))

    emprical_ratio = empirical_data[:,[1,4,7]]
    emprical_ratio = [vec(fill("Empirical",1,size(emprical_ratio)[1]))  emprical_ratio]
    rename!(emprical_ratio, Dict(:x1 => string("data")))

    SimExp_Comparison = [emprical_ratio; sim_ratio]

#   Exporting to Dataplot for Analysis & Visualization
    cd("/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/DNAC/Cog.Exchange/Data_Scripts/Summary_PointsAnalysis_30March2021")
    Julia_Utilities.dataplot_export(SimExp_Comparison, "SimExp_Ratios")

#################################################################
#   POINTS per OPPORTUNITY RATIO: SIMULATION VS. EXPERIMENTAL   #
#################################################################

#   Note: Assumes that simulation_conditions has been generated: Lines 520-543 
#   Note: Assumes that exchange_positions has been generated: In Points & Conditions by Node, Lines 451-482 & 497-507

#   Aggregating into 10 groups for the purposes of comparing with the empirical data
    condition_ratios = vec(fill(DataFrames.DataFrame(), 1, length(sim_keys)))
    for i in eachindex(condition_ratios)
        #   Calculating Group Sums & Getting Number of Trials per Group
            condition_data = simulation_conditions[sim_keys[i]]
            group_sums = combine(groupby(condition_data, [:Group]), nrow, :Payoff => sum => :sum)
            group_id = sort(unique(condition_data.Group))
            trials = vec(fill(1, 1, length(group_id)))
            for j in eachindex(group_id)
                group_data = condition_data[(condition_data.Group .== group_id[j]), :]
                trials[j] = maximum(group_data.Trial)
            end
            group_sums.n_trials = trials

        #   Subsetting into 10 blocks of 10 groups to make the data more comparable for visualizaton purpose
            group_index = Julia_Utilities.partition_array_indices(group_id[length(group_id)], 10)
            block_list = vec(fill(DataFrames.DataFrame(), 1, length(group_index)))
            for j in eachindex(group_index)
                iteration_value = group_index[j]
                groups = group_id[iteration_value]
                block_data = group_sums[in(groups).(group_sums.Group),:]
                block_data.points_trials_ratio = block_data.sum ./ block_data.n_trials
                block_output = DataFrame([Int64,Float64],[:block,:points_trials_ratio], 1)
                block_mean = mean(block_data.points_trials_ratio)
                block_output[1,1] = j
                block_output[1,2] = block_mean
                block_list[j] = block_output
            end

        #   Stacking Blocks & Adding Group Index for Reference
            block_data = block_list[1]
            for j in 2:length(group_index)
                iteration_data = block_list[j]
                block_data  = [block_data; iteration_data]
            end
            block_data = [group_index block_data]
            rename!(block_data, Dict(:x1 => string("groups")))

        #   Assign Condition Label
            condition_label = sim_keys[i]
            c_label = split(condition_label, "-")
            condition_label = string(c_label[1], " ", c_label[2])
            condition_labels = vec(fill(condition_label, 1, size(block_data)[1]))
            block_data = [condition_labels block_data]
            rename!(block_data, Dict(:x1 => string("condition")))

        #   Populating condition_ratios
            condition_ratios[i] = block_data
    end

#   Stacking Conditions
    sim_ratio_data = condition_ratios[1]
    for i in 2:length(condition_ratios)
        iteration_data = condition_ratios[i]
        sim_ratio_data = [sim_ratio_data; iteration_data]
    end

#   Creatign Ratio Dataset for Purposes of Generating the Block Plots
    nets_list = deepcopy(networks_list)
    for i in eachindex(net_keys)
        nets_list[net_keys[i]] = nets_list[net_keys[i]][:,[2,3,4,6]]
    end
    exchange_ratio = [nets_list[net_keys[1]]; nets_list[net_keys[2]]]

#   Calculating Mean Ratios
    exchange_ratios = combine(groupby(exchange_ratio, [:network_size, :exchange_type, :case]), nrow, :points_trials_ratio => mean => :mean)
    exchange_ratios = sort(exchange_ratios, [:network_size, :exchange_type, :case])

#   Subsetting (Doing This for BRiMS, Will Remove for Next Paper) & Formatting Empirical Data in order to Stacked
    exchange_ratios = exchange_ratios[.!in(["REN", "NER"]).(exchange_ratios.exchange_type),:]

    condition_labels = string.(exchange_ratios.exchange_type, " ", exchange_ratios.network_size)
    condition_labels = lowercasefirst.(condition_labels)
    exchange_ratios = [condition_labels exchange_ratios]
    rename!(exchange_ratios, Dict(:x1 => string("condition")))
    rename!(exchange_ratios, Dict(:mean => string("points_trials_ratio")))
  
#   Stacking
    sim_ratio_data = sim_ratio_data[:,[1,3,4]]
    rename!(sim_ratio_data , Dict(:block => string("case")))
    sim_ratio_data = [vec(fill("Simulations", 1, size(sim_ratio_data)[1])) sim_ratio_data]
    rename!(sim_ratio_data , Dict(:x1 => string("data")))

    exchange_ratios = exchange_ratios[:,[1,4,6]]
    exchange_ratios = [vec(fill("Experimental Data", 1, size(exchange_ratios)[1])) exchange_ratios]
    rename!(exchange_ratios, Dict(:x1 => string("data")))

    ratios_stacked = [exchange_ratios; sim_ratio_data]

#   Creat Numeric ID for condition
    condition = sort(unique(ratios_stacked.condition))
    condition_id = [1; 2; 1; 2; 3; 4; 3; 4]
    condition_index = DataFrame([String, Int64],[:condition,:condition_id], length(condition))
    condition_index.condition = condition
    condition_index.condition_id = condition_id
    condition_index = sort(condition_index, [:condition_id])
    ratios_stacked = rightjoin(condition_index, ratios_stacked, on = :condition)
    ratios_stacked = ratios_stacked[:,[3,2,4,5]]

#   Exporting to Visualize in Dataplot
    cd("/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/DNAC/Cog.Exchange/Data_Scripts/Summary_PointsAnalysis_30March2021")
    Julia_Utilities.dataplot_export(ratios_stacked, "ratios_stacked")

#COME BACK HERE: CHECK MEANS BY DATA TYPE AND CONDITION

    Julia_Utilities.show_df(agg_data)

#########################
#   CREATING EDGELIST   #
#########################



   
#############
#   NOTES   #
#############

#   Looking at the Full List of Variables Names
    Julia_Utilities.myshowall(Base.stdout, names(condition_counts), false)

#   VARIABLE FAMILIES
#   COND
#   CASE
#   SEX
#   EXP
#   FORM
#   HIST
#   CENT
#   VAL (a1b1, a2b2, a1a2, b1b2, a1b2)
#   RDSTR
#   NTRIAL, NTRIAL1, NTRIAL2
#   NPOINTS (S1a1, S2a1, Sa1, S1a2, S2a2, Sa2, S1b1, S2b1, Sb1, S1b2, S2b2, Sb2)
#   EARN (1a1, 2a1, a1, 1a2, 2a2, a2, 1b1, 2b1, b1, 1b2, 2b2, b2)
#   P1 (a1, a2, b1, b2)
#   P2 (a1, a2, b1, b2)
#   P3 (a1, a2, b1, b2)
#   P4 (a1, a2, b1, b2)
#   A1 (a1b1, a1b2, b1b2, a1a2, a2b2)
#   A2 (a1b1, a1b2, b1b2, a1a2, a2b2)
#   A3 (a1b1, a1b2, b1b2, a1a2, a2b2)
#   P1 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   P2 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   P3 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   P4 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   R1 ("", a1b1, a1b2, b1b2, a2b2, a1a2)
#   R2 ("", a1b2, a1b2, b1b2, a2b2, a1a2)
#   R3 ("", a1b1, a1b2, b1b2, a2b2, a1a2)
#   R4 ("", a1b1, a1b2, b1b2, a2b2, a1a2)
#   G1 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   G2 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   G3 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   G4 (a1b1, b1a1, a1b2, b2a1, b1b2, b2b1, a2b2, b2a2, a1a2, a2a1)
#   Q1 (a1, a2, b1, b2)
#   Q2 (a1, a2, b1, b2)
#   Q3 (a1, a2, b1, b2)
#   Q4 (a1, a2, b1, b2)
#   Q5 (a1, a2, b1, b2)
#   Q6 (a1, a2, b1, b2)
#   Q7 (a1, a2, b1, b2)
#   Q8 (a1, a2, b1, b2)
#   Q9 (a1, a2, b1, b2)
#   Q10 (a1, a2, b1, b2)
#   Q11 (a1, a2, b1, b2)
#   Q12 (a1, a2, b1, b2)
#   Q13 (a1, a2, b1, b2)
#   Q14 (a1, a2, b1, b2)
#   Q15 (a1, a2, b1, b2)
#   Q16 (a1, a2, b1, b2)
#   Q17 (a1, a2, b1, b2)
#   Q18 (a1, a2, b1, b2)
#   Q19 (a1, a2, b1, b2)
#   Q20 (a1, a2, b1, b2)
#   Q21 (a1, a2, b1, b2)
#   Q22 (a1, a2, b1, b2)
#   Q23 (a1, a2, b1, b2)
#   Q24 (a1, a2, b1, b2)
#   Q25 (a1, a2, b1, b2)
#   Q26 (a1, a2, b1, b2)
#   Q27 (a1, a2, b1, b2)
#   Q28 (a1, a2, b1, b2)
#   Q29 (a1, a2, b1, b2)
#   Q30 (a1, a2, b1, b2)
#   Q31 (a1, a2, b1, b2)
#   Q32 (a1, a2, b1, b2)
#   Q33 (a1, a2, b1, b2)
#   Q34 (a1, a2, b1, b2)
#   Q35 (a1, a2, b1, b2)
#   Q36 (a1, a2, b1, b2)
#   Q37 (a1, a2, b1, b2)
#   Q38 (a1, a2, b1, b2)
#   Q39 (a1, a2, b1, b2)
#   Q40 (a1, a2, b1, b2)
#   Q41 (a1, a2, b1, b2)
#   Q42 (a1, a2, b1, b2)
#    V349

###################
#   JULIA NOTES   #
###################

#   Subsetting by Row based on Column Value
    #   Not In
        df[.!in(x).(df.X),:]

    #   In
        df[in(x).(df.X),:]

#   Subsetting Columns

    #   Subsetting a specifc column
        select(data, :NTRIAL0)

    #   Selecting every column except the designated column
        select(data, Not(:NTRIAL0))


    #   Any Column that has the substring NPOINTS
        select(data, occursin.("NPOINTS0", names(data)))
        
    #   Any Column that does not have the substring NPOINTS
        data[:, Not(occursin.("NPOINTS0", names(data)))] 

    #   Eliminating more than one column from a DataFrame using Not()
        select!(networks_list[net_keys[i]], Not([:semester, :case])

#   Easy Way to Eliminate Leading and Trailing White Space
    condition = filter.(x -> !isspace(x), condition) 

#   Stacking or Gather a Dataset
    stack(networks_list[net_keys[i]], [:a1_points, :b1_points, :b2_points])

#   Example Use of Julia_Utilities function: partition_array_indices
    condition_data = simulation_conditions[sim_keys[i]]
    group_id = unique(condition_data.Group)
    group_index = Julia_Utilities.partition_array_indices(group_id[length(group_id)], 10)

#   Preallocating a DataFrame
    DataFrame([Real,Real,Real],[:a,:b,:c], 10000)

#   Rowbinding to create a DataFrame: Avoids having to convet columns from type Any to restore the variables type information.
    partner_index = DataFrame([partner_labels, partner_ids])

#   Example of Getting Group Mean & SD
    agg_data = combine(groupby(simulation_conditions[sim_keys[i]], [:Partner]), nrow, :Payoff => mean => :mean, :Payoff => Statistics.std  => :std)
