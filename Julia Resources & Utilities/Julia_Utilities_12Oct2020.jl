#Useful Data Management Functions
#Jonathan H. Morgan
#28 May 2020

#Checking for Dat File
path = pwd()

###############
#  PACKAGES   #
###############

using CSV               #Export Files as CSV
using DataFrames        #Generates Julia Style DataFrames and DataFrame Manipulation
using DataFramesMeta    #Facilitates DataFrame Manipulation
using Glob              #Useful Package for String Manipulation
using Distributions     #Julia Package for Estimating Univariate Statistics
using StatsBase         #Using Countmap Functionality

#################
#   FUNCTIONS   #
#################

#Creating and Writing Out Files List for Iterative Processing in Dataplot
dat_files = glob("*.DAT",path)

if length(dat_files) >= 1
    dataplot_files = fill(DataFrames.DataFrame(), length(dat_files), 1)
else
    dat_files = dat_files
end

#OLS Regression: https://juliaeconomics.com/2014/06/15/introductory-example-ordinary-least-squares/
function OLSestimator(y,x)
    estimate = inv(x'*x)*(x'*y)
    return estimate
end

#Displays all the elements of a data object
function myshowall(io, x, limit = false)
  println(io, summary(x), ":")
  Base.print_matrix(IOContext(io, :limit => limit), x)
end

# myshowall(Base.stdout, clu_files, false)

#Remove an element from a string
function remove!(a, item)
    deleteat!(a, findall(x->x==item, a))
end

#Nice Function that Divides a large dataset into chunks for processing
function partition_array_indices(nb_data::Int, nb_data_per_chunk::Int)
    nb_chunks = ceil(Int, nb_data / nb_data_per_chunk)
    ids = UnitRange{Int}[]
    for which_chunk = 1:nb_chunks
        id_start::Int = 1 + nb_data_per_chunk * (which_chunk - 1)
        id_end::Int = id_start - 1 + nb_data_per_chunk
        if id_end > nb_data
            id_end = nb_data
        end
        push!(ids, id_start:id_end)
    end
    return ids
end

#Parsing Dataplot Best Fit Output
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

#dat_y1 = dat_files[5]
#dat_y2 = dat_files[6]

#y1_results = DataFrames.DataFrame()
#y2_results = DataFrames.DataFrame()

#best_fit_parse(dat_files=dat_y1, results=y1_results)
#best_fit_parse(dat_files=dat_y2, results=y2_results)

#Data Plot Import Function
function dataplot_import(file, dataplot_files)
  #Getting Subset of dataplot files
  dat_files = file

  #Creating Empty DataFrame Array
  dataplot_files = dataplot_files


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
end

#Example
#file = dat_files[1:4]
#dataplot_files =  dataplot_files[1:4]
#dataplot_import(file, dataplot_files)

#Dataplot Export
function dataplot_export(data_frame, data_name)
    #Check for Obs_ID
    if sum(occursin.("Obs_ID", names(data_frame))) == true
        data_frame = data_frame[:,:]
    else
        #If not present, create an Obs_ID Variable to Ensure Row Order
        Obs_ID = [1: 1: nrow(data_frame);]
        data_frame = insertcols!(data_frame, 1, :Obs_ID => Obs_ID)
    end

    #Determine Variable Types
    types_list = eltype.(eachcol(data_frame))
    types_list = string.(types_list)
    col_id = [1: 1: length(types_list);]
    types_list = [col_id types_list]

    #Separate Out and Create a Key for String Variables
    types = types_list[types_list[:,2] .== "String", :]
    string_var = data_frame[:,types[:,1]]

    string_variables = vec(fill(DataFrames.DataFrame(), ncol(string_var), 1))
    for i in 1:ncol(string_var)
        labels = unique(string_var[:,i])
        id = [1: 1: length(labels);]
        id_set = DataFrames.DataFrame([id labels])
        id_names = [string(names(string_var)[i],"_","id"); names(string_var)[i]]
        id_names = Symbol.(id_names)
        rename!(id_set, id_names)
        id_set[!,1] = convert.(Int64, id_set[:,1])
        id_set[!,2] = convert.(String, id_set[:,2])
        string_variables[i] = id_set
    end

    #Merging numeric ids into string_var
    for i in 1:length(string_variables)
        string_var = leftjoin(string_var, string_variables[i], on = Symbol(names(string_var)[i]))
    end

    key = string_var[:,:]

    id_names = vec(fill("a", length(string_variables), 1))
    for i in 1:length(id_names)
        id_names[i] = names(string_variables[i])[1]
    end
    id_names = Symbol.(id_names)

    string_var = string_var[:,id_names]

    #Separate Integer and Float Variables
    types = types_list[types_list[:,2] .!= "String", :]
    numeric_var = data_frame[:,types[:,1]]

    #Checking for Duplicate Variables and Eliminating Them
    data_names = [names(string_var); names(numeric_var)]
    name_count = DataFrames.DataFrame(StatsBase.countmap(data_names))
    var_count = collect(name_count[1,:])
    data_names = names(name_count)
    data_names = [data_names var_count]
    data_names = data_names[data_names[:,2] .== 2, :]
    data_names = data_names[:,1]
    data_names = Symbol.(data_names)

    #Eliminating Duplicate Variables
    select!(string_var, Not(data_names))

    data = hcat(string_var, numeric_var)
    data_names = names(data)
    filter!(x->x≠"Obs_ID",data_names)
    data_names = ["Obs_ID"; data_names]
    data_names = Symbol.(data_names)
    select!(data, data_names)

    #CSV.write("data.csv", data, header=names(data))
    #data = DataFrame!(CSV.File("data.csv"))
    #rm("data.csv")

    #Creating Types & Names for the Output Processing
    data_names = names(data)
    col_dim = ncol(data)
    if col_dim <= 10
        col_dim = 10
    else
        col_dim = col_dim
    end

    row_length = length(string(collect(data[1,:])))
    data_segment = convert(Int64,round(row_length/100, digits=0))

    if data_segment >= 1
        export_list = fill(DataFrames.DataFrame(), data_segment, 1)

        col_segment = convert(Int64,round((ncol(data)/data_segment), digits=0))

        segment = vec([1, col_segment])
        for i in 1:(length(export_list)-1)
            seq_segments = [(segment[1]+(col_segment*i)) ,(segment[2]+(col_segment*i))]
            segment = hcat(segment, seq_segments)
        end

        segment[2,data_segment] = ncol(data)

        for i in 1:length(export_list)
            i_segment = segment[1,i]
            j_segment = segment[2,i]
            export_list[i] = data[:,i_segment:j_segment]
        end

        data_names = vec(fill("a", length(export_list), 1))
        for i in 1:length(export_list)
            data_names[i] = string(data_name, "_", i,".csv")
        end

        for i in 1:length(export_list)
            export_dataframe = export_list[i]
            CSV.write(data_names[i], export_dataframe, header=names(export_dataframe))
        end
    else
        data_names = vec(fill("a", 1, 1))
        data_names[1] = string(data_name,".csv")
        export_list = fill(DataFrames.DataFrame(), 1, 1)
        for i in 1:length(export_list)
            export_list[i] = data[:, :]
        end
        CSV.write(data_names[1], data, header=names(data))
    end

    if maximum(length.(String.(names(data_frame)))) > 8
        io_commands = vec(fill("a", 10, 1))
        io_commands[1] = string("CD", " ", pwd())
        io_commands[2] = string("DIMENSION", " ", col_dim, " ", "COLUMNS")
        io_commands[3] = "MAXIMUM RECORD LENGTH  9999"
        io_commands[4] = "SET READ DELIMITER ,"
        io_commands[5] = "SET DATE DELIMITER /"
        io_commands[6] = "SET TIME DELIMITER :"
        io_commands[7] = "SET TABLE HEADER ON"
        io_commands[8] = "SET DATA MISSING VALUE missing"
        io_commands[9] = "SET READ MISSING VALUE 999"
        io_commands[10] = "SKIP 1"

        read_commands = vec(fill("a", length(export_list), 1))
        for i in 1:length(read_commands)
            vars = [1: 1: length(names(export_list[i]));]
            var_names = vec(fill("a", length(vars), 1))
            for j in 1:length(var_names)
                var_names[j] = string("COL_", i, "_",vars[j])
            end

            read_commands[i] = string("READ", " ", data_names[i], " ", var_names)
            read_commands[i] = replace(read_commands[i], "[" => "")
            read_commands[i] = replace(read_commands[i], "]" => "")
            read_commands[i] = replace(read_commands[i], "," => " ")
            read_commands[i] = replace(read_commands[i], "\"" => "")
        end

        label_commands = string("VARIABLE LABEL", " ", "COL_1", " ", names(data_frame)[1])
        for i in 1:length(export_list)
            vars = [1: 1: length(names(export_list[i]));]
            var_names = vec(fill("a", length(vars), 1))
            for j in 1:length(var_names)
                var_names[j] = string("COL_", i, "_",vars[j])
            end

            for j in 1:length(names(export_list[i]))
                label_command = string("VARIABLE LABEL", " ", var_names[j], " ", names(export_list[i])[j])
                label_commands = vcat(label_commands, label_command)
            end
        end
        label_commands = label_commands[2:length(label_commands)]

        outfile = string("read_",data_name, ".DP")
        open(outfile, "w") do f
            for i in 1:length(io_commands)
                println(f, io_commands[i])
            end
            for i in 1:length(export_list)
                println(f, read_commands[i])
            end
            for i in 1:length(label_commands)
                println(f, label_commands[i])
            end
        end
    else
        io_commands = vec(fill("a", 10, 1))
        io_commands[1] = string("CD", " ", pwd())
        io_commands[2] = string("DIMENSION", " ", col_dim, " ", "COLUMNS")
        io_commands[3] = "MAXIMUM RECORD LENGTH  9999"
        io_commands[4] = "SET READ DELIMITER ,"
        io_commands[5] = "SET DATE DELIMITER /"
        io_commands[6] = "SET TIME DELIMITER :"
        io_commands[7] = "SET TABLE HEADER ON"
        io_commands[8] = "SET DATA MISSING VALUE missing"
        io_commands[9] = "SET READ MISSING VALUE 999"
        io_commands[10] = "SKIP 1"

        read_commands = vec(fill("a", length(export_list), 1))

        for i in 1:length(read_commands)
            var_names = String.(names(export_list[i]))
            read_commands[i] = string("READ", " ", data_names[i], " ", names(export_list[i]))
            read_commands[i] = replace(read_commands[i], "[" => "")
            read_commands[i] = replace(read_commands[i], "]" => "")
            read_commands[i] = replace(read_commands[i], "," => " ")
            read_commands[i] = replace(read_commands[i], "\"" => "")
        end

        label_commands = vec(fill("a", length(names(data)),1))
        for i in 1:length(label_commands)
            label_commands[i] = string("VARIABLE LABEL", " ", names(data)[i], " ", names(data)[i])
        end

        outfile = string("read_",data_name, ".DP")
        open(outfile, "w") do f
            for i in 1:length(io_commands)
                println(f, io_commands[i])
            end
            for i in 1:length(export_list)
                println(f, read_commands[i])
            end
            for i in 1:length(label_commands)
                println(f, label_commands[i])
            end
        end
    end
    return key
end

#dataplot_export(data, "test")
