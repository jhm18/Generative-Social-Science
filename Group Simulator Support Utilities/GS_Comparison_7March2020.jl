#Comparing Group Simulator Implementations
#Jonathan H. Morgan
#24 February 2019

#Setting Work Directory: Work
#cd("/Users/jhmorgan/Desktop/GroupSimulator Paper/GroupSimulator_Processing/Group Simulator Batch Scripts")
#pwd()

#Setting Work Directory: Home
cd("/Users/jonathan.h.morgan/Desktop/Group_Simulator")
pwd()

#Saving Data: https://github.com/JuliaIO/JLD.jl

################
#   PACKAGES   #
################

using JLD               #Saving Data in Julia Format
using PyCall            #Supports Calling Python Functions
using Pandas            #Supports Converting Python Objects
using DataFrames        #Generates Julia Style DataFrames and DataFrame Manipulation
using DataFramesMeta    #Facilitates DataFrame Manipulation
using RCall             #Supports Calling R Functions
using StatsPlots        #Generate Basic Stats Plots
using CSV               #Export Files as CSV

#################
#   FUNCTIONS   #
#################

function check_str(a)
    try
        parse(Float64,a)
        true
    catch
        false
    end
end

function remove!(a, item)
    deleteat!(a, findall(x->x==item, a))
end

#Chacking Python Library
#PyCall.libpython

#Specifying Path Objects for Simulations
    #Path for Netlogo on each machine
    #Path of the Model File on each machine

#work_netlogo = "/Applications/NetLogo_6.0.4/netlogo-headless.sh"
home_netlogo = "/Applications/NetLogo 6.1.0/netlogo-headless.sh"

#work_path = "/Users/jhmorgan/Desktop/GroupSimulator Paper/GroupSimulator_Processing/Group Simulator Batch Scripts/GroupSimulator_6March2020.nlogo"
home_path = "/Users/jonathan.h.morgan/Desktop/Group_Simulator/GroupSimulator_fixed.nlogo"

#work output_csv
#output_csv = "/Users/jhmorgan/Desktop/GroupSimulator Paper/GroupSimulator_Processing/Group Simulator Batch Scripts/output.csv"

#home output_csv
output_csv = "/Users/jonathan.h.morgan/Desktop/Group_Simulator/output.csv"

################################
#   GROUP SIMULATOR: CLASSIC   #
################################

#Running Subprocesses: https://stackoverflow.com/questions/89228/calling-an-external-command-from-python
#Advanced Netlogo Commands: https://ccl.northwestern.edu/netlogo/docs/behaviorspace.html

#Behavior Spaces Called from GS
    #GS_Egalitarian_AddressGroup_Comparisons
    #GS_Egalitarian_Reciprocity_Comparisons
    #GS_Hierarchical_AddressGroup_Comparisons
    #GS_Hierarchical_Reciprocity_Comparisons

const experiments_array = ["GS_Egalitarian_AddressGroup_Comparisons", "GS_Egalitarian_Reciprocity_Comparisons",
                           "GS_Hierarchical_AddressGroup_Comparisons", "GS_Hierarchical_Reciprocity_Comparisons"]

const output_array = fill(String[],length(experiments_array), 1)
const a_output_array = fill(String[],length(experiments_array), 1)

const run_array = fill(String[], 11 ,length(experiments_array))
const actions_array = fill(String[], 11 ,length(experiments_array))

const results = DataFrames.DataFrame()
const actions_data = DataFrames.DataFrame()
const e_stats = DataFrames.DataFrame()
const h_stats = DataFrames.DataFrame()

#NOTE: Before running the simulation loop, ensure the proper directories are set.

#Simulations Loop
for i in 1:length(experiments_array)
        experiment = experiments_array[i]
py"""

import subprocess
subprocess.run([$home_netlogo, "--model",
$home_path,
"--experiment", $experiment,
"--threads", "1",
"--table", $output_csv])

"""
    output_array[i,1] = readlines("data_GroupSimulator_runs.txt", keep=true)
    rm("data_GroupSimulator_runs.txt", force=true)

    a_output_array[i,1] = readlines("data_GroupSimulator_actions.txt", keep=true)
    rm("data_GroupSimulator_actions.txt", force=true)

    #CREATING PER SIMULATION ELEMENTS
    simulation = output_array[i,1]
    sim_actions = a_output_array[i,1]

    #Identifying the model and variable index rows with which to subset the data
    model_elements = Int64[]
    variable_elements = Int64[]
    a_model_elements = Int64[]
    a_variable_elements = Int64[]

    #Identifying the model rows: Runs Data
    for k in 1:length(simulation)
        #global model_elements = model_elements
        local element = [0]
        if simulation[k][2:6] == "MODEL"
            element = k
            append!(model_elements, element)
            else
            model_elements = model_elements
        end
        element = nothing
    end

    #Identifying the model rows: Actions Data
    for k in 1:length(sim_actions)
        #global a_model_elements = a_model_elements
        local element = [0]
        if sim_actions[k][2:6] == "MODEL"
            element = k
            append!(a_model_elements, element)
            else
            a_model_elements = a_model_elements
        end
        element = nothing
    end

    #Identifying the variable rows: Runs
    for k in 1:length(simulation)
        #global variable_elements = variable_elements
        local element = [0]
        if simulation[k][2:9] == "VARIABLE"
            element = k
            append!(variable_elements, element)
            else
            variable_elements = variable_elements
        end
        element = nothing
    end

    #Identifying the variable rows: Actions
    for k in 1:length(sim_actions)
        #global a_variable_elements = a_variable_elements
        local element = [0]
        if sim_actions[k][1:16] == "OUTPUT VARIABLES"
            element = k
            append!(a_variable_elements, element)
            else
            a_variable_elements = a_variable_elements
        end
        element = nothing
    end

    #Creating an index element to use for subsetting: Runs
    i_elements = variable_elements .+ 1
    j_elements = i_elements .+ (model_elements[2] - (variable_elements[1]+2))
    index_id = [i_elements, j_elements]

    #Creating an index element to use for subsetting: Actions
    a_i_elements = a_variable_elements .+ 1
    a_j_elements = a_i_elements .+ (a_model_elements[2] - (a_variable_elements[1]+2))
    a_index_id = [a_i_elements, a_j_elements]

    rate = collect(0:.1:1)
    results_array = fill(map(_ -> DataFrames.DataFrame(), 1:length(run_array[:,1])), 1)
    a_results_array = fill(map(_ -> DataFrames.DataFrame(), 1:length(actions_array[:,1])), 1)

    for j = 1:length(run_array[:,1])

        #CREATING PER RUN ELEMENTS

        #Subsetting and putting into a 11x4 array: Runs
        for j in 1:length(index_id[1,1])
            run_array[j,i] = simulation[index_id[1,1][j]:index_id[2,1][j]]
        end

        #Subsetting and putting into a 11x4 array: Actions
        for j in 1:length(a_index_id[1,1])
            actions_array[j,i] = sim_actions[a_index_id[1,1][j]:a_index_id[2,1][j]]
        end

        #Parsing the array elements
        run = run_array[j,i]
        action_set = actions_array[j,i]

        #Getting rid of leading an trailing brackets: Runs
        for k in 1:length(run)
            run[k] = run[k][2:(length(run[k])-2)]
        end

        #Getting rid of leading an trailing brackets: Actions
        for k in 1:length(action_set)
            action_set[k] = action_set[k][2:(length(action_set[k])-2)]
        end

        #Transforming Action Set into a DataFrame
        action_characteristics = fill(String[], length(action_set), 1)
        for k in 1:length(action_set)
            action_set[k] = replace(action_set[k], "male " => "male_")
            action_set[k] = replace(action_set[k], "(whole-group 0)" => "(whole-group)")
            action_set[k] = replace(action_set[k], "  " => " ")
            local action_line = split(action_set[k], " ")
            action_line = remove!(action_line, "")
            action_characteristics[k] = action_line
        end

        #Creating Variable Names for the Actions Set
        action_names = ["Run Number", "Event Number", "IPA Number", "Behavior Evaluation", "Behavior Potency", "Behavor Activity", "Deflection", "Actor Reciprocated", "Actor",
                        "Actor Fundamental Evaluation", "Actor Fundamental Potency", "Actor Fundamental Activity", "Actor Transient Evaluation", "Actor Transient Potency",
                        "Actor Transient Activity", "Actor Emotion Evaluation", "Actor Emotion Potency", "Actor Emotion Activity", "Actor Deflection",
                        "Acts Originated by the Actor", "Acts Received by the Actor", "Object", "Object Fundamental Evaluation", "Object Fundamental Potency",
                        "Object Fundamental Activity", "Object Transient Evaluation", "Object Transient Potency", "Object Transient Activity", "Object Emotion Evaluation",
                        "Object Emotion Potency", "Object Emotion Activity", "Object Deflection"]
        action_names = Symbol.(action_names)

        #Construct an empty DataFrame
        action_variables  = DataFrames.DataFrame()

        #Loop through the namelist array, create a column in the DataFrame entitled namelist[i]
        #Assign its values by using an array comprehension to build an array with the appropriate values,
        #Starting at the second array in array
        for (k, name) in enumerate(action_names)
            action_variables[:,name] =  [action_characteristics[l][k] for l in 1:length(action_characteristics)]
        end

        #Transforming String into Proper Variables by Exporting
        CSV.write("action_characteristics.csv", action_variables, header=names(action_variables))
        action_characteristics = CSV.read("action_characteristics.csv")
        rm("action_characteristics.csv", force=true)

        insertcols!(action_characteristics, 1, :experiment => experiments_array[i])

        #Appending action_characteristics to action_data
        append!(actions_data, action_characteristics)

        #Pulling Out Firt Major Run Element and Transforming into a DataFrame
        percents_ipa = fill(Float64[], length(run), 1)
        for k in 1:length(run)
            local ipa_line = strip(replace((split(run[k], "] "))[1], "[" => ""))
            percents_ipa[k] = [parse(Float64, ss) for ss in split(ipa_line)]
        end

        #Converting and Array of an Array of Intergers into a DataFrame
        percent_ipa = convert(DataFrames.DataFrame, percents_ipa)

        #Splitting Array Column into 12 Separate Columns
        percent_ipa = hcat(percent_ipa, DataFrames.DataFrame(reduce(vcat, permutedims.(collect.(percent_ipa[:,1]))),
                           [:ipa_1, :ipa_2, :ipa_3, :ipa_4, :ipa_5, :ipa_6,
                           :ipa_7, :ipa_8, :ipa_9, :ipa_10, :ipa_11, :ipa_12]))

        #Dropping Old String Array
        percent_ipa = percent_ipa[:,2:13]

        #Adding Obs_ID for Joining
        percent_ipa = insertcols!(percent_ipa, 1, :Obs_ID => collect(1:1:nrow(percent_ipa)))

        #Pulling Out Second Major Run Element and Transforming into a DataFrame
        agent_characteristics = fill(String[], length(run), 1)
        for k in 1:length(run)
            local agent_line = strip(replace((split(run[k], "] "))[2], "[" => ""))
            local rstring = "\\([^)]*\\)"
            local group_agent = match(Regex(rstring), agent_line)
            group_agent = group_agent.match
            group_agent = group_agent[2:12]
            agent_line = replace(agent_line, "(whole-group 0)" => group_agent)
            agent_line = replace(agent_line, "male " => "male_")
            agent_line = String.(split(agent_line))
            agent_characteristics[k] = agent_line
        end

        #Creating Namelist
        name_elements = ["E_", "P_", "A_", "acts-initiated_", "acts-received_"]
        name_elements = collect(name_elements)

        agents = fill(String[], convert(Int64,(length(agent_characteristics[1])/6)), 1 )
        for k in 1:length(agents)
            agents[k] = String.(split(string("Agent_")))
        end

        for k in 1:length(agents)
            agents[k] = append!(agents[k], name_elements)
            agents[k] = string.(agents[k], k)
        end

        namelist = String[]
        for k in 2:length(agents)
            #global namelist = namelist
            namelist = append!(agents[1], agents[k])
        end

        #Convert each item in array x[1] to a Symbol by broadcasting Symbol() across the array with dot syntax#
        namelist = Symbol.(namelist)

        #Construct an empty DataFrame
        agent_variables  = DataFrames.DataFrame()

        #Loop through the namelist array, create a column in the DataFrame entitled namelist[i]
        #Assign its values by using an array comprehension to build an array with the appropriate values,
        #Starting at the second array in array
        for (k, name) in enumerate(namelist)
            agent_variables[:,name] =  [agent_characteristics[l][k] for l in 1:length(agent_characteristics)]
        end

        #Transforming String into Proper Variables by Exporting
        CSV.write("agent_characteristics.csv", agent_variables, header=names(agent_variables))
        agent_characteristics = CSV.read("agent_characteristics.csv")
        rm("agent_characteristics.csv", force=true)

        #Adding Obs_ID for Joining
        agent_characteristics = insertcols!(agent_characteristics, 1, :Obs_ID => collect(1:1:nrow(agent_characteristics)))

        #Pulling Out Fourth Major Run Element and Transforming into a DataFrame
        #Network Tie Matrix

        tie_matrix = fill(Int64[], length(run), 1)
        for k in 1:length(run)
            local tie_line = strip(replace((split(run[k], " ["))[3], "]" => ""))
            tie_matrix[k] = [parse(Int, ss) for ss in split(tie_line)]
        end

        tie_matrix = convert(DataFrames.DataFrame, tie_matrix)
        rename!(tie_matrix, Dict(:x1 => string("Tie Matrix")))

        tie_matrix = insertcols!(tie_matrix, 1, :Obs_ID => collect(1:1:nrow(tie_matrix)))

        #Combining Run Elements
        result = join(percent_ipa, agent_characteristics, tie_matrix, on =:Obs_ID, kind = :outer)
        insertcols!(result, 2, :rate => rate[j])
        results_array[1][j] = result
    end

    result = DataFrames.DataFrame()
    for k = 1:length(results_array[1])
        append!(result, results_array[1][k])
    end

    insertcols!(result, 2, :experiment => experiments_array[i])
    append!(results, result)

    output = CSV.read("output.csv", header=7, skipto=8)
    rm("output.csv", force=true)

    #Formatting and Appending Output Files
    if i <= 2
        rename!(output, [:run_number, :save_IO, :grp_act_all, :IPA_CodingBasis, :fast,
                         :male_goodness, :male_dominance, :male_activation,
                         :female_goodness, :female_dominance, :female_activation, :percent_females,
                         :group_size, :individuality, :initial_tension, :actor_choice, :object_choice,
                         :address_group_Pr, :reciprocal_act_Pr, :equations, :run_size, :change_logic, :step,
                         :Mean_Personal_Deflection__Object_Persons,	:male_count, :Mean_Personal_Deflection_Males,
                         :Mean_Behavior_E, :Mean_Behavior_P, :Mean_Behavior_A, :Mean_Transients_E, :Mean_Transients_P,
                         :Mean_Transients_A, :Mean_Emotion_E, :Mean_Emotion_P, :Mean_Emotion_A])
        insertcols!(output, 1, :experiment => experiments_array[i])
        append!(e_stats, output)
        else
        rename!(output, [:run_number, :save_IO, :grp_act_all, :IPA_CodingBasis, :fast,
                         :male_goodness, :male_dominance, :male_activation,
                         :female_goodness, :female_dominance, :female_activation, :percent_females,
                         :group_size, :individuality, :initial_tension, :actor_choice, :object_choice,
                         :address_group_Pr, :reciprocal_act_Pr, :equations, :run_size, :change_logic, :step,
                         :Mean_Personal_Deflection_Object_Persons, :count_males, :Mean_Personal_Deflection_Males,
                         :Mean_Male_Behavior_E,	:Mean_Male_Behavior_P, :Mean_Male_Behavior_A,
                         :Mean_Male_Transient_E, :Mean_Male_Transient_P, :Mean_Male_Transient_A,
                         :Mean_Male_Emotion_E, :Mean_Male_Emotion_P, :Mean_Male_Emotion_A,
                         :count_females, :Mean_Personal_Deflection_Females,
                         :Mean_Female_Behavior_E, :Mean_Female_Behavior_P, :Mean_Female_Behavior_A,
                         :Mean_Female_Transient_E, :Mean_Female_Transient_P, :Mean_Female_Transient_A,
                         :Mean_Female_Emotion_E, :Mean_Female_Emotion_P, :Mean_Female_Emotion_A])
        insertcols!(output, 1, :experiment => experiments_array[i])
        append!(h_stats, output)
    end
end

##########################
#   MOVING DATA INTO R   #
##########################

@rput results
@rput actions_data
@rput e_stats
@rput h_stats

R"""

#Work
#setwd("/Users/jhmorgan/Desktop/GroupSimulator Paper/GroupSimulator_Processing/Group Simulator Batch Scripts")
#getwd()

#Home
setwd("~/Desktop/Group_Simulator/GroupSim_Comparisons")
getwd()

#Saving Simulation Outputs: GS Classic
#save(results, file = "GS_Results_9March2020.Rda")
#save(actions_data, file = "GS_Actions_9March2020.Rda")
#save(e_stats, file = "GS_EStats_9March2020.Rda")
#save(h_stats, file = "GS_HStats_9March2020.Rda")

#Saving Simulation Outputs: GS Fixed
save(results, file = "GSFixed_Results_10March2020.Rda")
save(actions_data, file = "GSFixed_Actions_10March2020.Rda")
save(e_stats, file = "GSFixed_EStats_10March2020.Rda")
save(h_stats, file = "GSFixed_HStats_10March2020.Rda")

"""

############
#  TESTS   #
############

#Looking at actions_data
#CSV.write("actions_data.csv", actions_data, header=names(actions_data))


#To See the first 6 rows of a DataFrame
#first(result, 6)

#To see all the columns
#show(agent_characteristics,allcols=true)


#Saving Variable String
#jldopen("gs_variables.jld", "w") do file
#    write(file, "variables", variables)  # alternatively, say "@write file A"
#end

#Loading Variable String
#variables = jldopen("gs_variables.jld", "r") do file
#    read(file, "variables")
#end

#Splitting a string into separate elements
#deleteat!(collect("Hello!"), 2)

#Subsetting based on one or more dimensions
#A = [1,2,3,4,5,6,7,8]
#k = 4

#A[1:end .!= k]
#B[  1:end .!= i,   1:end .!= j,   1:end .!= k  ]

#Converting strings of numeric characters into intergers
#test = [parse(Int, ss) for ss in split(percents_ipa[i])]

#Splitting One Column into Multiple Columns based on Delimiter
#df = DataFrames.DataFrame(X = ["A", "B", "C"], Y = ["a|b", "a|c", "b|b"])
#hcat(df, DataFrames.DataFrame(reduce(vcat, permutedims.(split.(df.Y, "|"))), [:Y1, :Y2]))

#Turning substrings into Integers
#percent_ipa = parse.(Int64,percent_ipa)
