#Useful Data Management Functions
#Jonathan H. Morgan
#Earliest Version: 28 May 2020
#Current Vesion: 4 July 2022

#   Create Local Environment
    using Pkg
    Pkg.activate("/mnt/c/Users/metal/Julia_Resources")
    Pkg.status()
    Pkg.update() 

#   Checking for Dat File
    path = pwd()

#   Adding Packages to path
    Pkg.add("CSV")
    Pkg.add("Chain")
    Pkg.add("DataFrames")
    Pkg.add("DataFramesMeta")
    Pkg.add("Distributions")
    Pkg.add("Formatting")
    Pkg.add("Glob")
    Pkg.add("Missings")
    Pkg.add("RCall")
    Pkg.add("StatsBase")
    Pkg.add("TableView")
    Pkg.add("Blink")

#################
#   FUNCTIONS   #
#################

#   Creating and Writing Out Files List for Iterative Processing in Dataplot
    using Glob
    using DataFrames
    dat_files = glob("*.DAT",path)

    if length(dat_files) >= 1
        dataplot_files = fill(DataFrames.DataFrame(), length(dat_files), 1)
    else
        dat_files = dat_files
    end


 #  Clears objects from memmory by assigning them a value of nothing
    function clear(workspace_objects::Union{Vector{String}, String})
        #   Isolate Subset from names(Main)
            named_objects = string.(names(Main))
            if(length(workspace_objects) > 1)
                named_objects = named_objects[in(workspace_objects).(named_objects)]
            else
                named_objects = named_objects[(named_objects .== workspace_objects)]
            end
            named_objects = Symbol.(named_objects)

        #   Looping through and the named objects and making them equal to nothing.
            for name in named_objects
                eval(:(
                    $name isa Function ? Base.delete_method.(methods($name)) :
                        $name isa Module || ($name = nothing)))
            end

        #   Forcing Garbage Collection
            GC.gc()
    end

    #clear("x")
    #clear(["x", "y"])
    #clear(string.(names(Main)))

#   Dropping either a single element or multiple elements from a String, Integer, or Float Vector
    function drop(vector, element)
        #   Checking if element is a string for the purpose of determining length
            if(typeof(element) == String || typeof(element) == Missing)
                element = [element]
            else
                element = element
            end
        
        #   Filtering by either equality or using in.() functionality
            if (length(element) == 1)
                if(typeof(element[1])  == Missing)
                    vector = vector[(ismissing.(vector) .!= 1)]
                else
                    vector = vector[(vector .!= element)]
                end
            else
                check = in(element).(vector)
                index = [1:1:length(vector);]
                index = [index check]
                retained_elements = index[(index[:,2] .!= 1), 1]
                vector = vector[retained_elements]
            end

        #   Return Object
            return vector
    end

    #vector = drop(["1", "2", "3", "4", "5", "9", "10"], ["3", "3", "4"])
    #vector = drop([1, 2, 3, 4, 5, 9, 10], [2, 3])
    #vector = drop([1.0, 2.0, 3.0, 4.0, 5.0, 9.0, 10.0], [2.0, 3.0])
    #vector = drop([1.0, 2.0, 3.0, 4.0, 5.0, 9.0, 10.0], 1.0)
    #vector = drop([1.0, missing, 3.0, missing, 5.0, 9.0, 10.0], missing)

#   Keeping either a single element or multiple elements from a String, Integer, or Float Vector
    function keep(vector, element)
        #   Checking if element is a string for the purpose of determining length
            if(typeof(element) == String || typeof(element) == Missing)
                element = [element]
            else
                element = element
            end

        #   Filtering by either equality or using in.() functionality
            if (length(element) == 1)
                if(typeof(element[1])  == Missing)
                    vector = vector[(ismissing.(vector) .== 1)]
                else
                    vector = vector[(vector .== element)]
                end
            else
                vector = vector[in(element).(vector)]
            end

        #   Return Object
            return vector
    end

    #vector = keep(["1", "2", "3", "4", "5", "9", "10"], ["3", "3", "4"])
    #vector = keep([1, 2, 3, 4, 5, 9, 10], [2, 3])
    #vector = keep([1.0, 2.0, 3.0, 4.0, 5.0, 9.0, 10.0], [2.0, 3.0])
    #vector = keep([1.0, 2.0, 3.0, 4.0, 5.0, 9.0, 10.0], 3.0)
    #vector = keep([1.0, missing, 3.0, missing, 5.0, 9.0, 10.0], missing)

#   Creates an index of the a DataFrame's column IDs, names, and types.
    function column_index(data::DataFrames.DataFrame, data_name::String)
        #   Getting Column Names
            data_names = names(data)

        #   Creating index
            data_index = DataFrames.DataFrame(id=[1: 1: length(data_names);], label=data_names)

        #   Adding types
            data_index.type = eltype.(eachcol(data))

        #   Creating index name
            output_name = string(data_name,"_index")

        #   Assigning output name to data_index using @eval & Returning Object
            return @eval $(Symbol(output_name)) = $data_index
    end

    #column_index(countries, "countries")

module Julia_Utilities

    #   Loading Dependencies
        using CSV               #Export Files as CSV
        using Chain             #Provides Piping Functionality similar to %>% in R
        using DataFrames        #Generates Julia Style DataFrames and DataFrame Manipulation
        using DataFramesMeta    #Facilitates DataFrame Manipulation
        using Distributions     #Julia Package for Estimating Univariate Statistics
        using Formatting        #Convert exponential numbers to decimal format
        using Glob              #Useful Package for String Manipulation
        using Missings          #Useful Package for Handling Data Objects with Missing Values
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
        export replace_conversion
        export string_to_parse
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
    #   function remove!(a, item)
    #       deleteat!(a, findall(x->x==item, a))
    #   end

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
        
    #   Function for transforming vectors with missing vlaues to either a Float or Integer
        function replace_conversion(type::DataType, vector)
            #   In the event there are missing values
                z = missings(type, length(vector))
                for i in eachindex(z)
                    if(ismissing(vector[i]) == false)
                        if(typeof(vector) == Vector{Int64})
                            vector[i] = parse(type, string.(vector[i]))
                        else
                            vector[i] = convert(type, vector[i])
                        end
                
                        z[i] = vector[i]
                    else
                        z[i] = z[i]
                    end
                end

            #   Return vector
                vector = z
        end

        #x = replace_conversion(Float64, x)
        #y = replace_conversion(Int64, y)

    #   Converting vectors with missing values from Strings
        function string_to_parse(T, vector)
            #   Transforming values into a string
                str = string.(vector)

            #   Replace missing with "" for the purpose of parsing
                str =  replace(str, "missing" => "")

            #   Parsing & Adding Back Missing
                vector_type = string(typeof(vector))
                float_check = length(findall("Float", vector_type))
                if(string(T)[1:3] == "Int" && float_check == 1)
                    for j in eachindex(str)
                        if(str[j] != "")
                            str[j] = string(convert(T, vector[j]))
                        else
                            str[j] = str[j]
                        end
                    end

                    str = something(tryparse.(T, str), missing)

                    str =  replace(str, nothing => missing)
                else
                    str = something(tryparse.(T, str), missing)

                    str =  replace(str, nothing => missing)
                end

            #   Returning transformed vector
                return str
        end

        #x = string.([missing, 2, 3])
        #x = string_to_parse(Float64, x)
        #x = string_to_parse(Int64, x)
        #x = string_to_parse(Float64, x)

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
                        data_table = DataFrames.outerjoin(distributions, data_table, on =:Obs_ID)

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
            #   Identifying Variable Types
                types_list = string.(eltype.(eachcol(data_frame)))
                col_id = [1: 1: length(types_list);]
                types_list = DataFrames.DataFrame(col_id = col_id, types=types_list)
    
            #   Subsetting by Type
                s_variables = types_list[findall(x -> occursin("String", x), types_list[:,2]), :]
                n_variables = types_list[findall(x -> !occursin("String", x), types_list[:,2]), :]
                b_variables = types_list[findall(x -> occursin("Bool", x), types_list[:,2]), :]
    
            #   Creating a Dictionary to Store String Variables & Everything Else
                if (size(s_variables)[1] != 0)
                    #   Creating Key
                        key = data_frame[:, s_variables[:,1]]
                        for i in eachindex(s_variables[:,1])
                            #   Isolating Variable
                                id = s_variables[i,1]
                                s_var = data_frame[:,id]
    
                            #   Identifying Unique Values
                                values = sort(unique(s_var))
    
                            #   Creating Index
                                var_index = DataFrames.DataFrame(id= [1:1:length(values);], value=values)
                                DataFrames.rename!(var_index, :value => Symbol(names(key)[i]))
    
                            #   Assigning Values
                                key = DataFrames.leftjoin(key, var_index, on=Symbol(names(key)[i]))
    
                            #   Renaming ID variables
                                DataFrames.rename!(key, :id => string(names(key)[i],"_","id"))
                        
                            #   Preserving Type
                                key[!,string(names(key)[i],"_","id")] = convert.(Int64, key[:,string(names(key)[i],"_","id")])
                        end
    
                    #   Creating Dictionary
                        elements_list = Dict("Strings" => key, "Numeric" => data_frame[:, n_variables[:,1]])
                else
                    #   Creating Dictionary
                        elements_list = Dict( "Numeric" => deepcopy(data_frame))
                end
    
            #   Generating Dictionary Keys
                data_keys = String.(keys(elements_list))
    
            #   Converting Boolean Variabls to Integers Before Transforming into Strings
                if (size(b_variables)[1] != 0)
                    id = names(data_frame)[b_variables[:,1]]
                    elements_list[data_keys[1]][!,id] = convert.(Int64, elements_list[data_keys[1]][:,id])
                else
                    b_variables =  b_variables
                end
    
            #   Creating Integer & Float Dataset
                id_names = string.(names(data_frame[:, s_variables[:,1]]), "_", "id")
                if (length(elements_list) == 2)
                    base_data = [elements_list[data_keys[1]] elements_list[data_keys[2]][:, id_names]]
                else
                    base_data = elements_list[data_keys[1]]
                end

            #   Creating export name & true names
                true_names = names(base_data)
    
            #   Creating Row-ID Variable if not already present
                if (size(true_names[findall(x -> occursin("Obs_ID", x), names(base_data)), :])[1] == 0)
                    DataFrames.insertcols!(base_data, 1, :Obs_ID => [1:1:size(base_data)[1];])
                else
                    base_data = base_data
                end
    
            #   Renaming for Export
                true_names = names(base_data)
                export_names = vec(fill("a", 1, length(true_names)))
                for i in eachindex(export_names)
                    #   Checking character length
                        if (length(true_names[i]) <= 8)
                            export_names[i] = true_names[i]
                        else
                            export_names[i] = string("c_", i)
                        end
                end
                name_index = DataFrames.DataFrame(true_name = true_names, export_name=export_names)
                DataFrames.rename!(base_data, Symbol.(export_names))
    
            #   Isolating Floats
                types_list = string.(eltype.(eachcol(base_data)))
                col_id = [1: 1: length(types_list);]
                types_list = DataFrames.DataFrame(col_id = col_id, types=types_list)
                f_variables = types_list[findall(x -> occursin("Float", x), types_list[:,2]), :]
                if (size(f_variables)[1] != 0)
                    floats = base_data[:,f_variables[:,1]]
                else
                    f_variables = f_variables
                end
    
            #   Isolating Integers
                i_variables = types_list[findall(x -> occursin("Int", x), types_list[:,2]), :]
                if (size(i_variables) != 0)
                    integers = base_data[:,i_variables[:,1]]
                else
                    i_variables
                end
    
            #   Moving ID variables to the left
                cat_names = name_index[(in(id_names).(name_index.true_name) ), 2]
                integer_names = unique(["Obs_ID"; cat_names; names(integers)])
                integers = integers[:, integer_names]
    
            #   Specifying Dataplot IO Settings
                io_commands = vec(fill("a", 4, 1))
                if (size(base_data)[2] > 10)
                    if (size(data_frame)[1] < 1000 )
                        io_commands[1] = string("DIMENSION", " ","1000", " ", "ROWS")
                    else
                        io_commands[1] = string("DIMENSION", " ", size(base_data)[1], " ", "ROWS")
                    end
                    
                    io_commands[2] = "MAXIMUM RECORD LENGTH  9999"
                    io_commands[3] = "SET DATA MISSING VALUE missing"
                    io_commands[4] = "SET READ MISSING VALUE 999"
                else
                    io_commands[1] = "MAXIMUM RECORD LENGTH  9999"
                    io_commands[2] = "SET DATA MISSING VALUE missing"
                    io_commands[3] = "SET READ MISSING VALUE 999"
                    io_commands = io_commands[1:3]
                end
    
            #   Converting into Dataplot Format: Integers
            #   There is always at least one integer, the Obs_ID variable
                inter_strings = vec(fill(String[], 1, size(integers)[2], 1))
                for i in eachindex(inter_strings)
                    #   Isolating the variable
                        variable = integers[:, i]
    
                    #   Converting to a string
                        s_variable = string.(variable)
    
                    #   Checking that there is no scientific notation
                        e_index = findall(x -> occursin("e", x), s_variable)
                        if (size(e_index)[1] > 0)
                            for j in 1:length(e_index)
                                s_variable[e_index[j]] = Formatting.format(variable[e_index[j]])
                            end
                        else
                            s_variable = s_variable
                        end
    
                    #   Padding-Out the Variable
                        width = maximum(length.(s_variable))
                        string_diff = width .- length.(s_variable)
                        for j in eachindex(string_diff)
                            if (variable[j] < 0)
                                sign = string(s_variable[j][1])
                                digit = s_variable[j][2:length(s_variable[j])]
                                s_variable[j] = join([sign; fill("0",string_diff[j]); digit])
                            else
                                s_variable[j] = join([fill("0",string_diff[j]); s_variable[j]])
                            end 
                        end
    
                    #   Populating inter_strings
                        inter_strings[i] = s_variable
                end            
                
            #   Converting into Dataplot Format: Floats
            #   There is not necessarily always a float, thus the if statement
                if (size(f_variables)[1] != 0)
                    whole_values = vec(fill(String[], 1, size(floats)[2], 1))
                    decimal_values = vec(fill(String[], 1, size(floats)[2], 1))
                    for i in eachindex(f_variables[:,1])
                        #   Isolating variable
                            variable = floats[:, i]
    
                        #   Transforming into a string
                            s_variable = string.(variable)
    
                        #   Checking that not scientific notation is present
                            e_index = findall(x -> occursin("e", x), s_variable)
                            if (size(e_index)[1] > 0 )
                                for j in eachindex(e_index)
                                    s_variable[e_index[j]] = Formatting.format(variable[e_index[j]])
                                end
                            else 
                                e_index = e_index 
                            end
    
                        #   Splitting float
                            float_elements = split.(s_variable, ".")
    
                            w_elements = vec(fill("a", 1, length(float_elements)))
                            dp_elements = vec(fill("a", 1, length(float_elements)))
                            for j in 1:length(float_elements)
                                float_element = collect(float_elements[j])
                                if (length(float_element))   == 2
                                    w_elements[j] = float_element[1]
                                    dp_elements[j] = float_element[2]
                                else
                                    w_elements[j] = float_element[1]
                                    dp_elements[j] = "0"
                                end
                            end
    
                        #   Padding-Out the decimal points
                            dp = maximum(length.(dp_elements))
                            string_diff = dp .- length.(dp_elements)
                            for j in eachindex(string_diff)
                                dp_elements[j] = join([dp_elements[j]; fill("0",string_diff[j])])
                            end
                    
                        #   Padding-Out whole numbers
                            width = maximum(length.(w_elements))
                            string_diff = width .- length.(w_elements)
                            for j in eachindex(string_diff)
                                if (variable[j] < 0)
                                    sign = string(w_elements[j][1])
                                    digit = w_elements[j][2:length(w_elements[j])]
                                    w_elements[j] = join([sign; fill("0",string_diff[j]); digit])
                                else
                                    w_elements[j] = join([fill("0",string_diff[j]); w_elements[j]])
                                end 
                            end
    
                        #   Populating whole and decimal lists
                            whole_values[i] = w_elements
                            decimal_values[i] = dp_elements
                    end
                else 
                    f_variables = f_variables
                end
    
            #   Create label commands
                data_labels = vec(fill("a", length(true_names), 1))
                for i in eachindex(data_labels)
                    data_labels[i] = string("VARIABLE LABEL", " ", export_names[i], " ", true_names[i])
                end
    
            #   Create print elements
                end_command = string("END OF DATA")
                space_element = " "
    
            #   Writing-Out Data
                if (size(f_variables)[1] > 0)
                    #   Creating Read Commands
                        i_set = vec(fill("a", size(i_variables)[1], 1))
                        i_read = vec(fill("a", size(i_variables)[1], 1))
                        for i in eachindex(i_read)
                            #   Determining width
                                width = maximum(length.(inter_strings[i]))
    
                            #   Specifying set command
                                set_command = string("SET READ FORMAT"," ",1,"F",width,".",0)
    
                            #   Specifying run command
                                read_command = string("READ", " ", names(integers)[i])
    
                            #   Populating command lists
                                i_set[i] = set_command
                                i_read[i] = read_command
                        end
    
                        f_set = vec(fill("a", size(f_variables)[1], 1))
                        f_read = vec(fill("a", size(f_variables)[1], 1))
                        for i in eachindex(f_read)
                            #   Determining width
                                width = maximum(length.(whole_values[i]))
    
                            #   Determining dp (decimal point width)
                                dp = maximum(length.(decimal_values[i]))
    
                            #   Specifying set command
                                set_command = string("SET READ FORMAT"," ",1,"F",width+dp+1,".",dp)
    
                            #   Specifying run command
                                read_command = string("READ", " ", names(floats)[i])
    
                            #   Populating command lists
                                f_set[i] = set_command
                                f_read[i] = read_command
                        end
                    
                    #   Opening File
                        outfile = string("read_",data_name, ".DP")
                        open(outfile, "w") do f
                            #   Write IO Commands
                                for i in 1:length(io_commands)
                                    println(f, io_commands[i])
                                end
    
                            #   Adding spacer
                                println(f, space_element)
    
                            #   Writing-Out Integers
                                for i in 1:length(i_set)
                                    #   Writing-Out Set Command
                                        println(f, i_set[i])
    
                                    #   Writing-Out Run Command
                                        println(f, i_read[i])
    
                                    #   Writing-Out Values
                                        variable = inter_strings[i]
                                        for j in eachindex(variable)
                                            println(f, variable[j])
                                        end
    
                                    #   Writing-Out End Command & space_element
                                        println(f, end_command)
                                        println(f, space_element)     
                                end
    
                            #   Writing-Out Floats
                                for i in 1:length(f_set)
                                    #   Writing-Out Set Command
                                        println(f, f_set[i])
    
                                    #   Writing-Out Run Command
                                        println(f, f_read[i])
    
                                    #   Writing-Out Values
                                        whole_value = whole_values[i]
                                        decimal_value = decimal_values[i]
                                        for j in eachindex(whole_value)
                                            value = string(whole_value[j], ".", decimal_value[j])
                                            println(f, value)
                                        end
    
                                    #   Writing-Out End Command & space_element
                                        println(f, end_command)
                                        println(f, space_element)     
                                end
    
                            #   Writing-Out Label Commands
                                for i in eachindex(data_labels)
                                    println(f, data_labels[i])
                                end
                        end
                else
                    #   Creating Read Commands
                        i_set = vec(fill("a", size(i_variables)[1], 1))
                        i_read = vec(fill("a", size(i_variables)[1], 1))
                        for i in eachindex(i_read)
                            #   Determining width
                                width = maximum(length.(inter_strings[i]))
    
                            #   Specifying set command
                                set_command = string("SET READ FORMAT"," ",1,"F",width,".",0)
    
                            #   Specifying run command
                                read_command = string("READ", " ", names(integers)[i])
    
                            #   Populating command lists
                                i_set[i] = set_command
                                i_read[i] = read_command
                        end
    
                    #   Opening File
                        outfile = string("read_",data_name, ".DP")
                        open(outfile, "w") do f
                            #   Write IO Commands
                                for i in 1:length(io_commands)
                                    println(f, io_commands[i])
                                end
    
                            #   Adding spacer
                                println(f, space_element)
    
                            #   Writing-Out Integers
                                for i in 1:length(i_set)
                                    #   Writing-Out Set Command
                                        println(f, i_set[i])
    
                                    #   Writing-Out Run Command
                                        println(f, i_read[i])
    
                                    #   Writing-Out Values
                                        variable = inter_strings[i]
                                        for j in eachindex(variable)
                                            println(f, variable[j])
                                        end
    
                                    #   Writing-Out End Command & space_element
                                        println(f, end_command)
                                        println(f, space_element)     
                                end
    
                            #   Writing-Out Label Commands
                                for i in eachindex(data_labels)
                                    println(f, data_labels[i])
                                end
                        end
                end
    
            #   Returning the key data.frame if made
                if (length(s_variables[:,1]) > 0)
                    return(key)
                else
                    s_variables[:,1] = s_variables[:,1]
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
