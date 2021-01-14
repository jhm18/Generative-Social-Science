#PPCC Distribution Fit Parser
#Jonathan H. Morgan
#22 March 2020

#Setting Work Directory: Home
cd("/Users/jonathan.h.morgan/dataplot/Dataplot_Tests/Distribution_Tests")
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
using RCall             #Supports Calling R Functions

#################
#   FUNCTIONS   #
#################

function best_fit_parse(;dat_files::String=dat_files, results=results)
    #Reading-In File
    file = readlines(dat_files, keep=true)

    #Split File by White Space Delimiter
    file = split(file, " \n")

    #Getting Rid of 0 length file vector elements
    empty_vector = isempty.(file)

    #Identifying Index Number of Empty Elements
    empty_index = collect(1:1:length(empty_vector))
    for i in 1:length(empty_vector)
        if empty_vector[i] == 1
            empty_index[i] = i
        else
            empty_index[i] = 0
        end
    end

    #Isolating empty elements
    empty_index = filter!(x->x ≠ 0,empty_index)

    file = deleteat!(file, empty_index)

    #Getting Factor Variable ID and Response Variable
    lengths = length.(file)
    length_index = collect(1:1:length(lengths))
    for i  in 1:length(lengths)
        if lengths[i] != 2
            length_index[i] = i
        else
            length_index[i] = 0
        end
    end

    #Isolating elements of 2
    length_index = filter!(x->x ≠ 0,length_index)
    meta_data = deleteat!(file, length_index)

    meta_index = collect(1:1:length(meta_data))
    for i  in 1:length(meta_data)
        if meta_data[i][1][1:10] == "Fit Method"
            meta_index[i] = i
        else
            meta_index[i] = 0
        end
    end

    meta_index = filter!(x->x ≠ 0,meta_index)
    meta_data = deleteat!(meta_data, meta_index)

    #Isolating Results Tables
    file = readlines(dat_files, keep=true)
    file = split(file, " \n")

    table_index = collect(1:1:length(file))
    for i  in 1:length(table_index)
        if length(file[i]) == 55
            table_index[i] = 0
        else
            table_index[i] = i
        end
    end

    table_index = filter!(x->x ≠ 0,table_index)
    data_tables = deleteat!(file, table_index)

    for i  in 1:length(data_tables)
        table_values = fill(String[],(length(data_tables[i]), 1))
        table_distributions = Array{String}(undef, length(table_values), 1)
        var_names = ["Distribution", "Goodness of Fit Statistic", "Estimate of Location",
                 "Estimate of Scale", "Estimate of Shape Parameter 1", "Estimate of Shape Parameter 2"]

        data_tables[i] = data_tables[i][7:length(data_tables[i])]

        for j in 1:length(data_tables[i])
            dist_type = string((split(data_tables[i][j], "0"))[1])
            val_max = length(split.(data_tables[i][j]))
            val_min = (val_max - 5) + 1
            row_values = split.(data_tables[i][j])[val_min:val_max]
            table_values[j] = row_values
            table_distributions[j] = dist_type
        end

        table_values = table_values[1:length(data_tables[i])]
        table_distributions = table_distributions[1:length(data_tables[i])]

        v_names = Symbol.(var_names[2:length(var_names)])

        #Construct an empty DataFrame
        data_table  = DataFrames.DataFrame()

        for (j, name) in enumerate(v_names)
            data_table[:,name] = [table_values[l][j] for l in 1:length(table_values)]
        end

        data_table = insertcols!( data_table, 1, :Obs_ID => collect(1:1:nrow(data_table)))

        distributions = DataFrame(Distributions = table_distributions[:,1])
        distributions = insertcols!(distributions, 1, :Obs_ID => collect(1:1:nrow(distributions)))

        #Merging Distributions Column
        data_table = join(distributions, data_table, on =:Obs_ID, kind = :outer)

        #Inserting Meta-Data
        data_table = @byrow! data_table begin
            @newcol Response_Variable::Array{String}
            :Response_Variable = strip(replace(meta_data[i][1], "\n" => ""))
        end

        data_table = @byrow! data_table begin
            @newcol Factor_Variable::Array{String}
            :Factor_Variable = strip(replace(meta_data[i][2], "\n" => ""))
        end

        col_index = [8, 9, 2, 3, 4, 5, 6, 7]
        data_table = data_table[:, col_index]

        #Transforming String into Proper Variables by Exporting
        CSV.write("data_table.csv", data_table, header=names(data_table))
        data_table = CSV.read("data_table.csv")
        rm("data_table.csv", force=true)

        results = append!(results, data_table)
    end
    return results
end

######################
#   IMPORTING DATA   #
######################

#Get .DAT files from the Directory
path = "/Users/jonathan.h.morgan/dataplot/Dataplot_Tests/Distribution_Tests"

#Creating and Writing Out Files List for Iterative Processing in Dataplot
dat_files = glob("*.DAT",path)
dat_y1 = dat_files[5]
dat_y2 = dat_files[6]

y1_results = DataFrames.DataFrame()
y2_results = DataFrames.DataFrame()

best_fit_parse(dat_files=dat_y1, results=y1_results)
best_fit_parse(dat_files=dat_y2, results=y2_results)

######################
#   EXPORTING TO R   #
######################

@rput y1_results
@rput y2_results

R"""

#Home
setwd("/Users/jonathan.h.morgan/dataplot/Dataplot_Tests/Distribution_Tests")
getwd()

#Saving Example Best Fit Files
save(y1_results, file = "y1_bestfit_23March2020.Rda")
save(y2_results, file = "y2_bestfit_23March2020.Rda")

"""
