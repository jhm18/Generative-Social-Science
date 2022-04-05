#Revised Dataplot Export Function
#Jonathan H. Morgan
#3 April 2022

#   Activating the Environment
    using Pkg
    Pkg.activate("C:\\Users\\metal\\Documents\\Performance_Tests\\Julia_Scripts")
    Pkg.status()

################
#   PACKAGES   #
################
    using CSV                   #Export Files as CSV
    using DataFrames            #Generates Julia Style DataFrames and DataFrame Manipulation
    using DataFramesMeta        #Facilitates DataFrame Manipulation

###########################
#   IMPORTING TEST DATA   #
###########################

#   Setting to Data Directory
    cd("C:\\Users\\metal\\Documents\\Performance_Tests\\Data")
    pwd()

#   Importing Data
    raw_data = CSV.read("socialization_and_networkspread_combined_raw_withIdentifiers.csv", DataFrame)
    data_frame = raw_data

###################################################
#   SPECIFYING REVISED DATAPLOT EXPORT FUNCTION   #
###################################################

#Dataplot Export
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
        base_data = [elements_list[data_keys[1]] elements_list[data_keys[2]][:, id_names]]

    #   Creating export name & true names
        true_names = names(base_data)

    #   Creating Row-ID Variable if not already present
        if (size(true_names[findall(x -> occursin("Obs_ID", x), names(base_data)), :])[1] == 0)
            DataFrames.insertcols!(base_data, 1, :Obs_ID => [1: 1: nrow(base_data); ])
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
            io_commands[1] = string("DIMENSION", " ", size(base_data)[1], " ", "ROWS")
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
                            s_variable[e_index[j]] = Formatting.format(s_variable[e_index[j]])
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
        if (size(f_variables) != 0)
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
                        for j in 1:length(e_index)
                            s_variable[e_index[j]] = Formatting.format(s_variable[e_index[j]])
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

dataplot_export(raw_data, "gpm_data")