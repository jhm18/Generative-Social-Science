#Useful Data Management Functions
#Jonathan H. Morgan
#Earliest Version: 28 May 2020
#Current Vesion: 28 February 2021

#   Create Local Environment
    using Pkg
    Pkg.activate("/Users/jonathan.h.morgan/Julia Resources")
    Pkg.status()

#   Checking for Dat File
    path = pwd()

#   Adding Packages to path
    using Pkg
    Pkg.add("CSV")
    Pkg.add("Chain")
    Pkg.add("DataFrames")
    Pkg.add("DataFramesMeta")
    Pkg.add("Distributions")
    Pkg.add("Formatting")
    Pkg.add("Glob")
    Pkg.add("RCall")
    Pkg.add("StatsBase")
    Pkg.add("TableView")
    Pkg.add("Blink")

#################
#   FUNCTIONS   #
#################

#   Creating and Writing Out Files List for Iterative Processing in Dataplot
    using Glob
    dat_files = glob("*.DAT",path)

    if length(dat_files) >= 1
        dataplot_files = fill(DataFrames.DataFrame(), length(dat_files), 1)
    else
        dat_files = dat_files
    end

module Julia_Utilities

    #   Loading Dependencies
        using CSV               #Export Files as CSV
        using Chain             #Provides Piping Functionality similar to %>% in R
        using DataFrames        #Generates Julia Style DataFrames and DataFrame Manipulation
        using DataFramesMeta    #Facilitates DataFrame Manipulation
        using Distributions     #Julia Package for Estimating Univariate Statistics
        using Formatting        #Convert exponential numbers to decimal format
        using Glob              #Useful Package for String Manipulation
        using RCall             #Used to work with R Objects
        using StatsBase         #Using Countmap Functionality
        using Statistics        #Julia's statistics standard library
        using TableView         #Used to Show Table Contents
        using Blink             #Creating a Julia Table View

    #   Exported Functions
        export get_pkg_status
        export OLSestimator
        export pci
        export myshowall
        export show_df
        export remove!
        export partition_array_indices
        export best_fit_parse
        export dataplot_import
        export dataplot_export
        export R_import
        export R_export

    #   Checking Package Dependencies
    #   Created by Bogumił Kamiński
    #   27 February 2021
        get_pkg_status(;direct::Bool=true) = @chain Pkg.dependencies() begin
            values
            DataFrame
            direct ? _[_.is_direct_dep, :] : _
            select(:name, :version,
            [:is_tracking_path, :is_tracking_repo, :is_tracking_registry] =>
            ByRow((a, b, c) -> ["path", "repo", "registry"][a+2b+3c]) =>
            :tracking)
        end

    #   Shows All Loaded Modules
    #   get_pkg_status()       

    #   Shows All Direct and Indirect Dependencies Associated with Loaded Modules
    #   get_pkg_status(direct=false)

    #   OLS Regression: https://juliaeconomics.com/2014/06/15/introductory-example-ordinary-least-squares/
        function OLSestimator(y,x)
            estimate = inv(x'*x)*(x'*y)
            return estimate
        end

    #   Percentile Credible Interval
        function pci(x, prob)
            #   Setting Interval Span
                a = (1-prob)/2
            
            #   Determining the Lower and Upper Confidence Intervals
                pci_interval = zeros(2);
                Statistics.quantile!(pci_interval, x, [a, (1-a)]) 

            #   Returning Percentile Credible Interval
                return pci_interval
        end

        # pci(samples, 0.89)

    #   Displays all the elements of a data object
        function myshowall(io, x, limit = false)
            println(io, summary(x), ":")
            Base.print_matrix(IOContext(io, :limit => limit), x)
        end

        # myshowall(Base.stdout, clu_files, false)

    #   Displays the full contents of a DataFrame
        function show_df(df)
            w =Blink.Window()
            body!(w, TableView.showtable(df))
        end

        # show_df(df)

    #   Remove an element from a string
        function remove!(a, item)
            deleteat!(a, findall(x->x==item, a))
        end

    #   Nice Function that Divides a large dataset into chunks for processing
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

    #   Parsing Dataplot Best Fit Output
        function best_fit_parse(;dat_files::String=dat_files, results=results)
            #   Reading-In File
                file = readlines(dat_files, keep=true)

            #   Split File by White Space Delimiter
                file = split(file, " \n")

            #   Getting Rid of 0 length file vector elements
                empty_vector = isempty.(file)

            #   Identifying Index Number of Empty Elements
                empty_index = collect(1:1:length(empty_vector))
                for i in 1:length(empty_vector)
                    if empty_vector[i] == 1
                        empty_index[i] = i
                    else
                        empty_index[i] = 0
                    end
                end

            #   Isolating empty elements
                empty_index = filter!(x->x ≠ 0,empty_index)

                file = deleteat!(file, empty_index)

            #   Getting Factor Variable ID and Response Variable
                lengths = length.(file)
                length_index = collect(1:1:length(lengths))
                for i  in 1:length(lengths)
                    if lengths[i] != 2
                        length_index[i] = i
                    else
                        length_index[i] = 0
                    end
                end

            #   Isolating elements of 2
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

            #   Isolating Results Tables
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
                    #   Getting Table Values
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

                    #   Construct an empty DataFrame
                        data_table  = DataFrames.DataFrame()

                        for (j, name) in enumerate(v_names)
                            data_table[:,name] = [table_values[l][j] for l in 1:length(table_values)]
                        end

                        data_table = insertcols!( data_table, 1, :Obs_ID => collect(1:1:nrow(data_table)))

                        distributions = DataFrame(Distributions = table_distributions[:,1])
                        distributions = insertcols!(distributions, 1, :Obs_ID => collect(1:1:nrow(distributions)))

                    #   Merging Distributions Column
                        data_table = join(distributions, data_table, on =:Obs_ID, kind = :outer)

                    #   Inserting Meta-Data
                        data_table = @eachrow data_table begin
                            @newcol Response_Variable::Array{String}
                            :Response_Variable = strip(replace(meta_data[i][1], "\n" => ""))
                        end

                        data_table = @eachrow data_table begin
                            @newcol Factor_Variable::Array{String}
                            :Factor_Variable = strip(replace(meta_data[i][2], "\n" => ""))
                        end

                        col_index = [8, 9, 2, 3, 4, 5, 6, 7]
                        data_table = data_table[:, col_index]

                    #   Transforming String into Proper Variables by Exporting
                        CSV.write("data_table.csv", data_table, header=names(data_table))
                        data_table = CSV.read("data_table.csv")
                        rm("data_table.csv", force=true)

                    results = append!(results, data_table)
                end
            return results
        end

    #   dat_y1 = dat_files[5]
    #   dat_y2 = dat_files[6]

    #   y1_results = DataFrames.DataFrame()
    #   y2_results = DataFrames.DataFrame()

    #   best_fit_parse(dat_files=dat_y1, results=y1_results)
    #   best_fit_parse(dat_files=dat_y2, results=y2_results)

    #   Data Plot Import Function
        function dataplot_import()
            #   Creating and Writing Out Files List for Iterative Processing in Dataplot
                dat_files = glob("*.DAT", pwd())

            #   Creating a List of Data Frames if there is more than one File
                if length(dat_files) >= 1
                    #   Isolating File Names
                        file_names = vec(fill("a", 1, length(dat_files)))
                        for i in 1:length(file_names)
                            name_components = split(dat_files[i], "/")
                            file_name = name_components[length(name_components)]
                            file_name = split(file_name, ".")[1]
                            file_names[i] = file_name
                        end

                    #   Creating List
                        global dataplot_files = Dict(file_names[i] => DataFrames.DataFrame() for i = 1:1:length(file_names))
                else
                    dat_files = dat_files
                end

            #   Checking if each .DAT file is a Data File as opposed to an Output File
                output_files = String[]
                for i in 1:length(dat_files)
                    #   Reading in File
                        file = readlines(dat_files[i], keep=true)

                    #   Checking if there is a header of the same length at lines 2 and 4
                        if (length(file[2]) == length(file[4]) && file[2] != "  \n")
                            dat_files = dat_files[:,:]
                            dataplot_files = dataplot_files
                        else
                            file_name = dat_files[i]
                            output_files = [output_files; file_name]
                        end
                end

            #   Removing Output Files from dat_files & dataplot_files
                dat_files = glob("*.DAT", pwd())
                for i in eachindex(output_files)
                    #   Removing Output Files from dat_files
                        file_name = output_files[i]
                        dat_files = filter!(x->x≠file_name, dat_files)

                    #   Removing Output Files from dataplot_files
                        dictionary_element = split(file_name, "/")[length(split(file_name, "/"))]
                        dictionary_element = split(dictionary_element, ".")[1]
                        dataplot_files = delete!(dataplot_files, dictionary_element)
                end

            #   Reading-In Files
                for i in 1:length(dat_files)
                    #   Reading in File
                        file = readlines(dat_files[i], keep=true)

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

                    #   Adding Obs_ID for Joining
                        file_df = insertcols!(file_df, 1, :Obs_ID => collect(1:1:nrow(file_df)))

                    #   Reading into Dictionary
                        dataplot_files[file_names[i]] = file_df

                    #   Creating New DataFrme as Final Output if there is only One file (No Need For List)
                        if length(dat_files) == 1
                            dataplot_file = file_df
                        else
                            file_df = file_df[:,:]
                        end      
                end

            #   Returning Dictionary
                if length(dat_files) > 1
                    return dataplot_files
                else
                    return dataplot_file
                end
        end

    #   dataplot_import()
    #   Looking at Tables
    #   file_keys = String.(keys(dataplot_files))
    #   dataplot_files[file_keys[1]]
    
    #   Dataplot Export
        function dataplot_export(data_frame, data_name)
            #   Check for Obs_ID
                if sum(occursin.("Obs_ID", names(data_frame))) == true
                    data_frame = data_frame[:,:]
                else
                    #If not present, create an Obs_ID Variable to Ensure Row Order
                    Obs_ID = [1: 1: nrow(data_frame);]
                    data_frame = insertcols!(data_frame, 1, :Obs_ID => Obs_ID)
                end

            #   Determine Variable Types
                types_list = eltype.(eachcol(data_frame))
                types_list = string.(types_list)
                col_id = [1: 1: length(types_list);]
                types_list = [col_id types_list]

            #   Separate Out and Create a Key for String & Boolean Variables
                s_types = types_list[types_list[:,2] .== "String", :]
                b_types = types_list[types_list[:,2] .== "Bool", :]
                types = [s_types; b_types]
                string_var = data_frame[:,types[:,1]]

            #   Transforming Boolean Variables into Strings for the Purposes of Processing
                string_types = eltype.(eachcol(string_var))
                string_types = string.(string_types)
                string_types = Any[[1: 1: length(string_types);] string_types]

                if length(b_types) > 0
                    string_index = string_types[(string_types[:,2] .== "Bool"), :][:,1]
                    for i in 1:length(string_index)
                        string_var[!, string_index[i]] = string.(convert.(Int64, string_var[!, string_index[i]]))
                    end
                else
                    b_types = b_types
                end

                string_variables = vec(fill(DataFrames.DataFrame(), ncol(string_var), 1))
                for i in 1:ncol(string_var)
                    labels = unique(string_var[:,i])
                    id = [1: 1: length(labels);]
                    id_set = DataFrames.DataFrame([id, labels], :auto)
                    id_names = [string(names(string_var)[i],"_","id"); names(string_var)[i]]
                    id_names = Symbol.(id_names)
                    rename!(id_set, id_names)
                    id_set[!,1] = convert.(Int64, id_set[:,1])
                    id_set[!,2] = convert.(String, id_set[:,2])
                    string_variables[i] = id_set
                end

            #   Merging numeric ids into string_var
                for i in 1:length(string_variables)
                    string_var = leftjoin(string_var, string_variables[i], on = Symbol(names(string_var)[i]))
                end

                key = string_var[:,:]
                if size(b_types)[1] > 0
                    boolean_variables = Symbol.(names(key)[string_index])
                    select!(key, DataFrames.Not(boolean_variables))
                    for i in 1:length(string_index)
                        DataFrames.insertcols!(key, string_index[i], boolean_variables[i] => data_frame[:,b_types[:,1]][:,i])
                    end
                else
                    key = key[:,:]
                end
    
                id_names = vec(fill("a", length(string_variables), 1))
                for i in 1:length(id_names)
                    id_names[i] = names(string_variables[i])[1]
                end
                id_names = Symbol.(id_names)
                string_var = string_var[:,id_names]

            #   Separate Integer and Float Variables
                types = types_list[types_list[:,2] .!= "String", :]
                numeric_var = data_frame[:,types[:,1]]

            #   Checking for Duplicate Variables and Eliminating Them
                data_names = [names(string_var); names(numeric_var)]
                name_count = DataFrames.DataFrame(StatsBase.countmap(data_names))
                var_count = collect(name_count[1,:])
                data_names = names(name_count)
                data_names = [data_names var_count]
                data_names = data_names[data_names[:,2] .== 2, :]
                data_names = data_names[:,1]
                data_names = Symbol.(data_names)

            #   Eliminating Duplicate Variables
                select!(string_var, DataFrames.Not(data_names))

                data = hcat(string_var, numeric_var)
                data_names = names(data)
                filter!(x->x≠"Obs_ID",data_names)
                data_names = ["Obs_ID"; data_names]
                data_names = Symbol.(data_names)
                select!(data, data_names)

            #   Separating by type to Read-Out
                types_list = eltype.(eachcol(data))
                types_list = string.(types_list)
                col_id = [1: 1: length(types_list);]
                types_list = [col_id types_list]

            #   Pulling-Out Integers & Floats
                data_types = Dict("Integers" => DataFrames.DataFrame(), "Floats" => DataFrames.DataFrame())
                data_keys = String.(keys(data_types))

                var_types = Dict("Integers" => vec(fill(0, 1, size(types_list)[1])), "Floats" => vec(fill(0, 1, size(types_list)[1])))
                for i in 1:length(var_types) 
                    if data_keys[i] ==  "Integers" 
                        data_type = vec(fill(0, 1, size(types_list)[1]))
                        for j in 1:length(data_type)
                            if types_list[j,2] == "Int64" || types_list[j,2] == "Union{Missing, Int64}"
                                data_type[j] = 1
                            else
                                data_type[j] = 0
                            end
                        end
                        var_types[data_keys[i]] = data_type
                    else
                        data_type = vec(fill(0, 1, size(types_list)[1]))
                        for j in 1:length(data_type)
                            if types_list[j,2] == "Float64" || types_list[j,2] == "Union{Missing, Float64}"
                                data_type[j] = 1
                            else
                                data_type[j] = 0
                            end
                        end
                        var_types[data_keys[i]] = data_type
                    end
                end

                for i in 1:length(var_types)
                    class_list = [var_types[data_keys[i]] types_list]
                    var_types[data_keys[i]] = class_list[(class_list[:,1] .== 1), :][:,2]
                end
        
                for i in 1:length(data_types)
                    data_types[data_keys[i]] = data[:, var_types[data_keys[i]]]
                end

            #   Add Set, Read, and End Dataplot Commands
                io_commands = vec(fill("a", 5, 1))
                io_commands[1] = string("CD", " ", pwd())
                io_commands[1] = replace(io_commands[1], "My Mac (Jonathan’s MacBook Pro)" => "")
                io_commands[1] = replace(io_commands[1], "Dropbox//" => "")

                if size(data)[2] > 10
                    if size(data)[1] > 1000
                        io_commands[2] = string("DIMENSION", " ", size(data)[1], " ", "ROWS")
                    else
                        io_commands[2] = string("DIMENSION", " ", 1000, " ", "ROWS")
                    end
                else
                    io_commands[2] = string("DIMENSION", " ", 1000, " ", "ROWS")
                end

                io_commands[3] = "MAXIMUM RECORD LENGTH  9999"
                io_commands[4] = "SET DATA MISSING VALUE missing"
                io_commands[5] = "SET READ MISSING VALUE 999"

            #   Writing-Out Files
                space_element = " "
                end_command = string("END OF DATA")

            #   Adding Label Dataplot Command  for Intergers
                name_lengths =  Dict("Integers" => vec(fill(0, 1, length(names(data_types[data_keys[2]])))), "Floats" => vec(fill(0, 1, length(names(data_types[data_keys[1]])))))
                for i in 1:length(name_lengths)
                    for j in 1:length(name_lengths[data_keys[i]])
                        if (length(names(data_types[data_keys[i]])[j]) > 8)
                            name_lengths[data_keys[i]][j] = 1
                        else
                            name_lengths[data_keys[i]][j] = 0
                        end
                    end
                end

                label_checks = Dict("Integers" => DataFrames.DataFrame(), "Floats" => DataFrames.DataFrame())
                for i in 1:length(data_types)
                    length_check = [collect(1:1:length(names(data_types[data_keys[i]]))) names(data_types[data_keys[i]])]
                    length_check = Any[name_lengths[data_keys[i]] length_check]
                    length_check = length_check[(length_check[:,1] .== 1), :]
                    label_checks[data_keys[i]] = DataFrames.DataFrame(length_check, :auto)
                end

                labels_list = Dict("Integers" =>  vec(fill("a", 1, nrow(label_checks[data_keys[2]]))), "Floats" =>  vec(fill("a", 1, nrow(label_checks[data_keys[1]]))))
                for i in 1:length(labels_list)
                    if data_keys[i] == "Integers"
                        for j in 1:length(labels_list[data_keys[i]])
                            label_command = string("VARIABLE LABEL", " ", "Int_", label_checks[data_keys[i]][j,2], " ", label_checks[data_keys[i]][j,3])
                            labels_list[data_keys[i]][j] = label_command       
                        end
                    else
                        for j in 1:length(labels_list[data_keys[i]])
                            label_command = string("VARIABLE LABEL", " ", "Flo_", label_checks[data_keys[i]][j,2], " ", label_checks[data_keys[i]][j,3])
                            labels_list[data_keys[i]][j] = label_command       
                        end
                    end
                end

            #   Writing-Out Files
                outfile = string("read_",data_name, ".DP")
                open(outfile, "w") do f
                    #   Reading in IO
                        for i in 1:length(io_commands)
                            println(f, io_commands[i])
                        end

                    #   Writing-Out Integer Variables
                        println(f, space_element)
                        for i in 1:length(names(data_types[data_keys[2]]))
                            width = maximum(length.(string.(data_types[data_keys[2]][:,i])))
                            i_set_command = string("SET READ FORMAT"," ",1,"F",width,".",0)
                            println(f, i_set_command)

                            if length(names(data_types[data_keys[2]])[i]) > 8
                                #   Creating Export Variable Names
                                    integer_name = string("Int_", i)

                                #   Generating Read Command
                                    i_read_command = string("READ", " ", integer_name)

                                #   Writing Out Variable Name Commands
                                    i_var_name_command = string("LET STRING", " ","iv_",i, " = ", "Int_", i)

                                #   Writing Out Variable
                                    col_string = string.(data_types[data_keys[2]][:,i])

                                #   Printing Commands
                                    println(f, i_read_command)
                                    for j in 1:length(col_string)
                                        println(f, col_string[j])
                                    end
                                    println(f, end_command)
                                    println(f, i_var_name_command)
                                    println(f, space_element)
                            else
                                #   Generating Read Command
                                    i_read_command = string("READ", " ", names(data_types[data_keys[2]])[i])

                                #   Writing Out Variable Name Commands
                                    i_var_name_command = string("LET STRING", " ","iv_",i, " = ", names(data_types[data_keys[2]])[i])
                   
                                #   Writing Out Variable
                                    col_string = string.(data_types[data_keys[2]][:,i])

                                #   Printing Commands
                                    println(f, i_read_command)
                                    for j in 1:length(col_string)
                                        println(f, col_string[j])
                                    end
                                    println(f, end_command)
                                    println(f, i_var_name_command)
                                    println(f, space_element)
                            end
                        end

                    #   Writing-Out Float Variables
                        println(f, space_element)
                        for i in 1:length(names(data_types[data_keys[1]]))
                            float_elements = split.(string.(data_types[data_keys[1]][:,i]), ".")
                            w_elements = vec(fill("a", 1, length(float_elements)))
                            dp_elements = vec(fill("a", 1, length(float_elements)))
                            for j in 1:length(float_elements)
                                float_element = collect(float_elements[j])
                                w_elements[j] = float_element[1]
                                dp_elements[j] = float_element[2]
                            end

                            width = maximum(length.(w_elements))
                            dp = maximum(length.(dp_elements))

                            if length(names(data_types[data_keys[1]])[i]) > 8
                                #   Creating Export Variable Names
                                    float_name = string("Flo_", i)

                                #   Generating Read Command
                                    f_read_command = string("READ", " ", float_name)

                                #   Writing Out Variable
                                    col_string = string.(data_types[data_keys[1]][:,i])
                                    if length(findall(x -> occursin("e", x), col_string)) > 0
                                        e_index = findall(x -> occursin("e", x), col_string)
                                        for j in 1:length(e_index)
                                            col_string[e_index[j]] = Formatting.format(data_types[data_keys[1]][:,i][e_index[j]])
                                        end

                                        float_elements = split.(col_string, ".")
                                        w_elements = vec(fill("a", 1, length(float_elements)))
                                        dp_elements = vec(fill("a", 1, length(float_elements)))

                                        for j in 1:length(float_elements)
                                            float_element = collect(float_elements[j])
                                            if length(float_element)   == 2
                                                w_elements[j] = float_element[1]
                                                dp_elements[j] = float_element[2]
                                            else
                                                w_elements[j] = float_element[1]
                                                dp_elements[j] = "0"
                                            end
                                        end

                                        width = maximum(length.(w_elements))
                                        dp = maximum(length.(dp_elements))
                                    else
                                        col_string = col_string
                                    end

                                #   Padding-Out Interger Values to Facilitate Reading-In into Dataplot
                                    string_diff = vec(fill(0, 1, length(col_string)))
                                    string_diff = width .- length.(w_elements) 
                                    for j in 1:length(string_diff)
                                        if string_diff[j] != 0
                                            zero_values = string(fill(0, string_diff[j]))
                                            w_element = string.(collect(w_elements[j]))
                                            if w_element[1] == "-"
                                                sign = string(w_element[1])
                                                core_values = w_element[2:length(w_element)]
                                                w_element = string(sign, zero_values, core_values)
                                                w_element = replace(w_element, "[" => " ")
                                                w_element = replace(w_element, "]" => " ")
                                                w_element = replace(w_element, "String" => "")
                                                w_element = replace(w_element, "," => "")
                                                w_element = replace(w_element, " " => "")
                                            else
                                                w_element = string(zero_values, w_element)
                                                w_element = replace(w_element, "[" => " ")
                                                w_element = replace(w_element, "]" => " ")
                                                w_element = replace(w_element, "String" => "")
                                                w_element = replace(w_element, "," => "")
                                                w_element = replace(w_element, "\"" => "")
                                                w_element = replace(w_element, " " => "")
                                            end
                                            float_elements = split(col_string[j], ".")
                                            float_elements[1] = w_element
                                            col_string[j] = string(float_elements[1], ".", float_elements[2])
                                        else
                                            col_string[j] = col_string[j]
                                        end
                                    end

                                #   Padding-Out Decimal Values to Facilitate Reading-In into Dataplot
                                    string_diff = vec(fill(0, 1, length(col_string)))
                                    string_diff = dp .- length.(dp_elements) 
                                    for j in 1:length(string_diff)
                                        if string_diff[j] != 0
                                            zero_values = string(fill(0, string_diff[j]))
                                            cs_element = string(col_string[j], zero_values)
                                            cs_element = replace(cs_element, "[" => "")
                                            cs_element = replace(cs_element, "]" => "")
                                            cs_element = replace(cs_element, "," => " ")
                                            cs_element = replace(cs_element, "\"" => "")
                                            cs_element = replace(cs_element, " " => "")
                                            col_string[j] = cs_element
                                        else
                                            col_string[j] = col_string[j]
                                        end
                                    end

                                #   Getting Final Widths & Dp Values
                                    float_elements = split.(col_string, ".")
                                    w_elements = vec(fill("a", 1, length(float_elements)))
                                    dp_elements = vec(fill("a", 1, length(float_elements)))
                                    for j in 1:length(float_elements)
                                        float_element = collect(float_elements[j])
                                        if length(float_element)   == 2
                                            w_elements[j] = float_element[1]
                                            dp_elements[j] = float_element[2]
                                        else
                                            w_elements[j] = float_element[1]
                                            dp_elements[j] = "0"
                                        end
                                    end
                                
                                    width = maximum(length.(col_string))
                                    dp = maximum(length.(dp_elements))

                                #   Writing-Out Set and Read Commands
                                    f_set_command = string("SET READ FORMAT"," ",1,"F",width,".",dp)

                                #   Writing Out Variable Name Commands
                                    f_var_name_command = string("LET STRING", " ","fv_",i, " = ", "Flo_", i)

                                #   Printing Commands
                                    println(f, f_set_command)
                                    println(f, f_read_command)
                                    for j in 1:length(col_string)
                                        println(f, col_string[j])
                                    end
                                    println(f, end_command)
                                    println(f, f_var_name_command)
                                    println(f, space_element)
                            else
                                #   Generating Read Command
                                    f_read_command = string("READ", " ", names(data_types[data_keys[1]])[i])
                   
                                #   Writing Out Variable
                                    col_string = string.(data_types[data_keys[1]][:,i])
                                    if length(findall(x -> occursin("e", x), col_string)) > 0
                                        e_index = findall(x -> occursin("e", x), col_string)
                                        for j in 1:length(e_index)
                                            col_string[e_index[j]] = Formatting.format(data_types[data_keys[1]][:,i][e_index[j]])
                                        end

                                        float_elements = split.(col_string, ".")
                                        w_elements = vec(fill("a", 1, length(float_elements)))
                                        dp_elements = vec(fill("a", 1, length(float_elements)))
                                        for j in 1:length(float_elements)
                                            float_element = collect(float_elements[j])
                                            if length(float_element)   == 2
                                                w_elements[j] = float_element[1]
                                                dp_elements[j] = float_element[2]
                                            else
                                                w_elements[j] = float_element[1]
                                                dp_elements[j] = "0"
                                            end
                                        end

                                        width = maximum(length.(w_elements))
                                        dp = maximum(length.(dp_elements))
                                    else
                                        col_string = col_string
                                    end

                                #   Padding-Out Interger Values to Facilitate Reading-In into Dataplot
                                    string_diff = vec(fill(0, 1, length(col_string)))
                                    string_diff = width .- length.(w_elements) 
                                    for j in 1:length(string_diff)
                                        if string_diff[j] != 0
                                            zero_values = string(fill(0, string_diff[j]))
                                            w_element = string.(collect(w_elements[j]))
                                            if w_element[1] == "-"
                                                sign = string(w_element[1])
                                                core_values = w_element[2:length(w_element)]
                                                w_element = string(sign, zero_values, core_values)
                                                w_element = replace(w_element, "[" => " ")
                                                w_element = replace(w_element, "]" => " ")
                                                w_element = replace(w_element, "String" => "")
                                                w_element = replace(w_element, "," => "")
                                                w_element = replace(w_element, " " => "")
                                            else
                                                w_element = string(zero_values, w_element)
                                                w_element = replace(w_element, "[" => " ")
                                                w_element = replace(w_element, "]" => " ")
                                                w_element = replace(w_element, "String" => "")
                                                w_element = replace(w_element, "," => "")
                                                w_element = replace(w_element, "\"" => "")
                                                w_element = replace(w_element, " " => "")
                                            end
                                            float_elements = split(col_string[j], ".")
                                            float_elements[1] = w_element
                                            col_string[j] = string(float_elements[1], ".", float_elements[2])
                                        else
                                            col_string[j] = col_string[j]
                                        end
                                    end

                                #   Padding-Out Decimal Values to Facilitate Reading-In into Dataplot
                                    string_diff = vec( fill(0, 1, length(col_string)))
                                    string_diff = dp .- length.(dp_elements) 
                                    for j in 1:length(string_diff)
                                        if string_diff[j] != 0
                                            zero_values = string(fill(0, string_diff[j]))
                                            cs_element = string(col_string[j], zero_values)
                                            cs_element = replace(cs_element, "[" => "")
                                            cs_element = replace(cs_element, "]" => "")
                                            cs_element = replace(cs_element, "," => " ")
                                            cs_element = replace(cs_element, "\"" => "")
                                            cs_element = replace(cs_element, " " => "")
                                            col_string[j] = cs_element
                                        else
                                            col_string[j] = col_string[j]
                                        end
                                    end

                                #   Getting Final Widths & Dp Values
                                    float_elements = split.(col_string, ".")
                                    w_elements = vec(fill("a", 1, length(float_elements)))
                                    dp_elements = vec(fill("a", 1, length(float_elements)))
                                    for j in 1:length(float_elements)
                                        float_element = collect(float_elements[j])
                                        if length(float_element) == 2
                                            w_elements[j] = float_element[1]
                                            dp_elements[j] = float_element[2]
                                        else
                                            w_elements[j] = float_element[1]
                                            dp_elements[j] = "0"
                                        end
                                    end
                                
                                    width = maximum(length.(col_string))
                                    dp = maximum(length.(dp_elements))

                                #   Writing-Out Set and Read Commands
                                    f_set_command = string("SET READ FORMAT"," ",1,"F",width,".",dp)

                                #   Writing Out Variable Name Commands
                                    f_var_name_command = string("LET STRING", " ","fv_",i, " = ", names(data_types[data_keys[1]])[i])

                                #   Printing Commands
                                    println(f, f_set_command)
                                    println(f, f_read_command)
                                    for j in 1:length(col_string)
                                        println(f, col_string[j])
                                    end
                                    println(f, end_command)
                                    println(f, f_var_name_command)
                                    println(f, space_element)
                            end
                        end

                    #   Writing-Out Integer Labels
                        println(f, space_element)
                        for i in 1:length(labels_list[data_keys[2]])
                            println(f, labels_list[data_keys[2]][i])
                        end

                    #   Writing-Out Float Labels
                        println(f, space_element)
                        for i in 1:length(labels_list[data_keys[1]])
                            println(f, labels_list[data_keys[1]][i])
                        end

                end
        
            #   Writing-Out Key & Returning Key
                if size(key)[1] > 0
                    key_name = string(data_name,"_key.csv")
                    CSV.write(key_name, key, header=names(key))
                    return key
                else
                    key = key
                end
        end

    #  dataplot_export(data_frame, "test")

    #   R Import
        function R_import(R_directory)
            #   Pulling R Data Types
                dir = R_directory
                rda_files = glob("*.rda", dir)
                Rda_files = glob("*.Rda", dir)
                rdata_files = glob("*.rdata", dir)
                Rdata_files = glob("*.Rdata", dir)
        
                R_files = [rda_files; Rda_files; rdata_files; Rdata_files]

            #   Pulling-Out File Names
                file_names = vec(fill("a", 1, length(R_files)))
                for i in 1:length(file_names)
                    file_elements = split(R_files[i], "/")
                    file_name = file_elements[length(file_elements)]
                    file_name =  replace(file_name, ".Rda" => "")
                    file_name =  replace(file_name, ".rda" => "")
                    file_name =  replace(file_name, ".rdata" => "")
                    file_name =  replace(file_name, ".Rdata" => "")
                    file_names[i] = file_name
                end

            #   Making Sure All Names Are with Underscores
                for i in 1:length(file_names)
                    file_names[i] = replace(file_names[i], "." => "_")
                end

                for i in 1:length(R_files)
                    iter = i
                    #   Loading Objects into R
                        R"""
                            setwd($dir)
                            getwd()

                            load($R_files[$iter])

                            myGlobals <- objects()
                            type <- vector('character', length(myGlobals))
                            for (i in seq_along(myGlobals)) {
                                obj_type <- class(get(myGlobals[[i]]))
                                if (length(obj_type) > 1 ) {
                                    obj_type <- obj_type[obj_type == "data.frame" | obj_type == "list"]
                                } else {
                                    obj_type <- obj_type
                                }
                                type[[i]] <- obj_type
                                rm(obj_type)
                            }

                            myGlobals <- as.data.frame(cbind(myGlobals, type))
                            myGlobals <- myGlobals[myGlobals$type == "data.frame" | myGlobals$type == "list", ]

                            if (myGlobals[1,2] == "data.frame"){
                                R_df <- get(myGlobals$myGlobals)
  
                                for (i in seq_along(colnames(R_df))) {
                                    if (class(R_df[[i]])[[1]] == 'numeric'){
                                        if(sum(as.integer(na.omit(R_df)[[i]])) == sum(na.omit(R_df)[[i]])){
                                            R_df[[i]] <- as.integer(R_df[[i]])
                                        }else{
                                            R_df[[i]] <- R_df[[i]]
                                        }
                                    }else{
                                        R_df[[i]] <- R_df[[i]]
                                    }
                                }
                            }else{
                                R_list <- get(myGlobals$myGlobals)
                            }

                            rm(myGlobals, type)
                            R_objects <- ls(all.names = TRUE)
                        """

                    #   Pulling Formatted Object into Julia
                        R_elements = reval("R_objects")
                        R_elements = rcopy(R_elements)
                        R_elements = R_elements[in(["R_df";"R_list"]).(R_elements)]
               
                        if R_elements[1] == "R_df"
                            global R_df = reval("R_df")
                            global R_dataframe = rcopy(R_df)
                            @eval $(Symbol(file_names[iter])) = R_dataframe[:,:]
                        else
                            global R_list = reval("R_list")
                            global R_dictionary = rcopy(R_list)
                            @eval $(Symbol(file_names[iter])) = R_dictionary
                        end

                    #   Clearing Global Environment
                        R"""
                            rm(list = ls(all = TRUE))
                        """
                end

            #   Loading R Objects
                R_objects = Dict(file_names[i] => @eval $(Symbol(file_names[i])) for i = 1:1:length(file_names))
                R_keys = String.(keys(R_objects))
                return R_objects
        end

    #   cd("/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/Themis.Cog/Themis.Cog_Mapping/Data_Scripts/")
    #   R_objects = R_import(pwd())
    #   R_keys = String.(keys(R_objects))
    #   R_objects[R_keys[2]]

    #   R Export
        function R_export(data_object, R_directory, file_name, save_name)
            #   Exporting Object to R_object
                @rput data_object
            
            #   Creating File Name
                save_name = string(save_name, ".Rda")

            #   Saving as R Object
                R"""
                    setwd($R_directory)
                    getwd()
            
                    assign(x = $file_name, value = data_object,.GlobalEnv) 
                    save(list=c($file_name), file = $save_name)
                """
        end

    #   R_directory = "/Users/jonathan.h.morgan/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/Themis.Cog/Themis.Cog_Mapping/Data_Scripts/"
    #   R_export(cities, R_directory, "cities_list", "cities_25Feb2021")

end