#Group Simulator Sensitivity Analyses: Verification
#Jonathan H. Morgan
#17 February 2020

#Work Directory
cd("/Users/jhmorgan/Desktop/GroupSimulator Paper/GroupSimulator_Processing/ESSC  2019 Paper Files")
pwd()

#Home Directory
#cd("/Users/jonathan.h.morgan/Desktop/Themis.Cog/ESSC  2019 Paper Files")
#pwd()

################
#   PACKAGES   #
################
using PyCall            #Supports Calling Python Functions
using Pandas            #Supports Converting Python Objects
using DataFrames        #Generates Julia Style DataFrames and DataFrame Manipulation
using DataFramesMeta    #Facilitates DataFrame Manipulation
using RCall             #Supports Calling R Functions
using StatsPlots        #Generate Basic Stats Plots

#For more info on StatsPlots: https://github.com/JuliaPlots/StatsPlots.jl

#Setting Simulation Directory: Work
pushfirst!(PyVector(pyimport("sys")."path"), "/Users/jhmorgan/Desktop/GroupSimulator Paper/groupsimulator_workshop")

#Setting Simulation Directory: Home
#pushfirst!(PyVector(pyimport("sys")."path"), "/Users/jonathan.h.morgan/Desktop/Group_Simulator/groupsimulator_workshop")

#Creating an empty arrays to populate simulation results: ipa, agent medians, role medians
ag_simulations = [Int[] for i=1:2, j=1:3]
rec_simulations = [Int[] for i=1:2, j=1:3]

##########################################
#   Address the Group Rate Simulations   #
##########################################

#EGALITARIAN GROUPS
py"""
import sys
import os
sys.path.insert(0,'/Users/jhmorgan/Desktop/GroupSimulator Paper/groupsimulator_workshop')
os.environ['KMP_DUPLICATE_LIB_OK']='True'

from group_simulator_base import *
import itertools
import pickle
import networkx as nx
from nxpd import draw
from IPython.display import display
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from mesa.batchrunner import BatchRunner

#helper class, because variable parameters passed to Batchrunner have to be hashable
class Hashabledict(dict):
    def __hash__(self):
        return hash('N' + str(self['N']))

def agent_data_collector(model):
    return model.datacollector.get_agent_vars_dataframe()

def model_data_collector(model):
    return model.datacollector.get_model_vars_dataframe()

#Setting Parameters: Fixed and Variable
fixed_params = {
                "data_model": "us_unisex",
                "actor_choice" : "max self-tension",
                "action_on_group" : True,
                "reciprocity_rate": 0.8,
                "agents": [{'epa' : [1.0, 1.50, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.50, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.50, 1.0], 'initial_tension': 1.0, 'individuality': 1.0}
                          ]
               }

variable_params = {
                   "group_action_rate" : [x/10 for x in range(0,11)]
                  }

batch_run = BatchRunner(GroupModel,
                        fixed_parameters=fixed_params,
                        variable_parameters=variable_params,
                        iterations=200,
                        max_steps=500,
                        model_reporters={"model data": model_data_collector, "agent data": agent_data_collector}
                       )

batch_run.run_all()

run_data = batch_run.get_model_vars_dataframe()

#Getting Stacked Agent Data
agent_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,2]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    agent_data = pd.concat([agent_data,run],ignore_index=True)
    os.remove('result.csv')

print(agent_data.tail())
print(len(agent_data))

model_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,3]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    model_data = pd.concat([model_data,run],ignore_index=True, sort=True)
    os.remove('result.csv')

print(model_data.tail())
print(len(model_data))
"""

#HIERARCHICAL GROUPS

py"""
#Packages
import sys
import os
sys.path.insert(0,'/Users/jonathan.h.morgan/Desktop/Group_Simulator/groupsimulator_workshop')
os.environ['KMP_DUPLICATE_LIB_OK']='True'

from group_simulator_base import *
import itertools
import pickle
import networkx as nx
from nxpd import draw
from IPython.display import display
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from mesa.batchrunner import BatchRunner

epa_colors=["#cb9f00", "#058c7d", "#78325f"]

#helper class, because variable parameters passed to Batchrunner have to be hashable
class Hashabledict(dict):
    def __hash__(self):
        return hash('N' + str(self['N']))

def agent_data_collector(model):
    return model.datacollector.get_agent_vars_dataframe()

def model_data_collector(model):
    return model.datacollector.get_model_vars_dataframe()

#Setting Parameters: Fixed and Variable
fixed_params = {
                "data_model": "us_unisex",
                "actor_choice" : "max self-tension",
                "action_on_group" : True,
                "reciprocity_rate": 0.8,
                "agents": [{'epa' : [1.0, 1.25, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.05, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.05, 1.0], 'initial_tension': 1.0, 'individuality': 1.0}
                          ]
               }

variable_params = {
                   "group_action_rate" : [x/10 for x in range(0,11)]
                  }

batch_run = BatchRunner(GroupModel,
                        fixed_parameters=fixed_params,
                        variable_parameters=variable_params,
                        iterations=200,
                        max_steps=500,
                        model_reporters={"model data": model_data_collector, "agent data": agent_data_collector}
                       )

batch_run.run_all()

run_data = batch_run.get_model_vars_dataframe()

#Getting Stacked Agent Data
agent_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,2]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    agent_data = pd.concat([agent_data,run],ignore_index=True)
    os.remove('result.csv')

#Getting Stacked Model Data
model_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,3]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    model_data = pd.concat([model_data,run],ignore_index=True, sort=True)
    os.remove('result.csv')

"""

###############################
#   Reciprocity Simulations   #
###############################

#EGALITARIAN

py"""
#Packages
import sys
import os

#Home
#sys.path.insert(0,'/Users/jonathan.h.morgan/Desktop/Group_Simulator/groupsimulator_workshop')

#Work
sys.path.insert(0,'/Users/jhmorgan/Desktop/GroupSimulator Paper/groupsimulator_workshop')
os.environ['KMP_DUPLICATE_LIB_OK']='True'

from group_simulator_base import *
import itertools
import pickle
import networkx as nx
from nxpd import draw
from IPython.display import display
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from mesa.batchrunner import BatchRunner

epa_colors=["#cb9f00", "#058c7d", "#78325f"]

#helper class, because variable parameters passed to Batchrunner have to be hashable
class Hashabledict(dict):
    def __hash__(self):
        return hash('N' + str(self['N']))

def agent_data_collector(model):
    return model.datacollector.get_agent_vars_dataframe()

def model_data_collector(model):
    return model.datacollector.get_model_vars_dataframe()

#Setting Parameters: Fixed and Variable
fixed_params = {
                "data_model": "us_unisex",
                "actor_choice" : "max self-tension",
                "action_on_group" : True,
                "group_action_rate": 0.4,
                "agents": [{'epa' : [1.0, 1.50, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.50, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.50, 1.0], 'initial_tension': 1.0, 'individuality': 1.0}
                          ]
               }

variable_params = {
                   "reciprocity_rate" : [x/10 for x in range(0,11)]
                  }

batch_run = BatchRunner(GroupModel,
                        fixed_parameters=fixed_params,
                        variable_parameters=variable_params,
                        iterations=20,
                        max_steps=500,
                        model_reporters={"model data": model_data_collector, "agent data": agent_data_collector}
                       )

batch_run.run_all()

run_data = batch_run.get_model_vars_dataframe()

#Getting Stacked Agent Data
agent_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,2]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    agent_data = pd.concat([agent_data,run],ignore_index=True)
    os.remove('result.csv')

#Getting Stacked Model Data
model_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,3]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    model_data = pd.concat([model_data,run],ignore_index=True, sort=True)
    os.remove('result.csv')

"""

#HIERARCHICAL

py"""
#Packages
import sys
import os

#Home
#sys.path.insert(0,'/Users/jonathan.h.morgan/Desktop/Group_Simulator/groupsimulator_workshop')

#Work
sys.path.insert(0,'/Users/jhmorgan/Desktop/GroupSimulator Paper/groupsimulator_workshop')
os.environ['KMP_DUPLICATE_LIB_OK']='True'

from group_simulator_base import *
import itertools
import pickle
import networkx as nx
from nxpd import draw
from IPython.display import display
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from mesa.batchrunner import BatchRunner

epa_colors=["#cb9f00", "#058c7d", "#78325f"]

#helper class, because variable parameters passed to Batchrunner have to be hashable
class Hashabledict(dict):
    def __hash__(self):
        return hash('N' + str(self['N']))

def agent_data_collector(model):
    return model.datacollector.get_agent_vars_dataframe()

def model_data_collector(model):
    return model.datacollector.get_model_vars_dataframe()

#Setting Parameters: Fixed and Variable
fixed_params = {
                "data_model": "us_unisex",
                "actor_choice" : "max self-tension",
                "action_on_group" : True,
                "group_action_rate": 0.4,
                "agents": [{'epa' : [1.0, 1.25, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.05, 1.0], 'initial_tension': 1.0, 'individuality': 1.0},
                           {'epa' : [1.0, 1.05, 1.0], 'initial_tension': 1.0, 'individuality': 1.0}
                          ]
               }

variable_params = {
                   "reciprocity_rate" : [x/10 for x in range(0,11)]
                  }

batch_run = BatchRunner(GroupModel,
                        fixed_parameters=fixed_params,
                        variable_parameters=variable_params,
                        iterations=20,
                        max_steps=500,
                        model_reporters={"model data": model_data_collector, "agent data": agent_data_collector}
                       )

batch_run.run_all()

run_data = batch_run.get_model_vars_dataframe()

#Getting Stacked Agent Data
agent_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,2]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    agent_data = pd.concat([agent_data,run],ignore_index=True)
    os.remove('result.csv')

#Getting Stacked Model Data
model_data = pd.DataFrame()

for i in range(len(run_data)):
    run = run_data.iloc[i,3]
    run['rate'] = run_data.iloc[i,0]
    run['run'] = run_data.iloc[i,1]
    run.to_csv('result.csv')
    run = pd.read_csv('result.csv')
    model_data = pd.concat([model_data,run],ignore_index=True, sort=True)
    os.remove('result.csv')

"""

##################
#   FORMATTING   #
##################

#In later iterations, have the scripts simulation specific names.

#Importing Means Table and Results
agent_results = py"agent_data"
model_results = py"model_data"

#Converting to DataFrames
agent_results = Pandas.DataFrame(agent_results)
agent_results = DataFrames.DataFrame(agent_results)
model_results = Pandas.DataFrame(model_results)
model_results = DataFrames.DataFrame(model_results)

#Cleaning Up Results Tables: Agent and Model
agent_results = DataFrames.select(agent_results, ([:rate,:run, :Step,:AgentID, :Deflection]))

#Getting Row Numbers as an ID variable
model_results = hcat(model_results, collect(1:size(model_results,1)));
model_results = DataFrames.select(model_results, ([:x1, :rate,:run,:actor,:object]))
rename!(model_results, [:Obs_ID,:rate,:run,:actor,:object])

#Calculating Mean Deflection for each agent by address-the-group rate
agent_means = DataFrames.select(agent_results, ([:rate,:AgentID, :Deflection]))
agent_means = DataFrames.by(agent_results, [:rate, :AgentID], :Deflection => median)

#Getting Unique values per agent
agents = sort(unique(agent_results.AgentID))
agents_array = map(_ -> DataFrames.DataFrame(), 1:length(agents))
for i = 1:length(agents)
    local letters = string("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    local i_letter = letters[i]
    agents_array[i] = agent_results[agent_results.AgentID .== agents[i], :]
    agents_array[i] = sort!(agents_array[i],[:rate, :run, :Step, :AgentID])
    local id = string("agent_", i_letter)
    local def_id = string("Deflection_", i_letter)
    rename!(agents_array[i], Dict(:AgentID => id))
    rename!(agents_array[i], Dict(:Deflection => def_id))
end

#Merging AgentIDs and Deflection with model_resluts
actor_agents = join(agents_array[1], agents_array[2], agents_array[3], on = [:rate,:run,:Step], kind = :outer)
actor_agents = hcat(actor_agents, collect(1:size(actor_agents,1)), makeunique=true );
rename!(actor_agents, Dict(:x1 => string("Obs_ID")))

#Deleting Duplicate Columns
actor_agents = actor_agents[3:10]

actor_agents = join(model_results, actor_agents,on =:Obs_ID, kind = :outer)

#Getting Rid of Missing from Type Specification
actor_agents[:,1] =  convert(Array{Int64}, actor_agents[:,1])
actor_agents[:,2] =  convert(Array{Float64}, actor_agents[:,2])
actor_agents[:,3] =  convert(Array{Int64}, actor_agents[:,3])
actor_agents[:,4] =  convert(Array{Int64}, actor_agents[:,4])
actor_agents[:,5] =  convert(Array{Int64}, actor_agents[:,5])
actor_agents[:,6] =  convert(Array{Int64}, actor_agents[:,6])
actor_agents[:,7] =  convert(Array{Int64}, actor_agents[:,7])
actor_agents[:,8] =  convert(Array{Float64}, actor_agents[:,8])
actor_agents[:,9] =  convert(Array{Int64}, actor_agents[:,9])
actor_agents[:,10] =  convert(Array{Float64}, actor_agents[:,10])
actor_agents[:,11] =  convert(Array{Int64}, actor_agents[:,11])
actor_agents[:,12] =  convert(Array{Float64}, actor_agents[:,12])

#Creating Actor Deflection and Object Deflection
actor_agents = @byrow! actor_agents begin
    @newcol actor_deflection::Array{Float64}
    if :actor == :agent_A
        :actor_deflection = :Deflection_A
    elseif :actor == :agent_B
        :actor_deflection = :Deflection_B
        else
        :actor_deflection = :Deflection_C
    end
end

actor_agents = @byrow! actor_agents begin
    @newcol object_deflection::Array{Float64}
    if :object == :agent_A
        :object_deflection = :Deflection_A
    elseif :object == :agent_B
        :object_deflection = :Deflection_B
    elseif :object == :agent_C
        :object_deflection = :Deflection_C
        else
        :object_deflection = 999999
    end
end

#Creating Aggregate Medians for Actors and Objects
actor_medians = DataFrames.select(actor_agents, ([:rate,:actor_deflection]))
actor_medians = DataFrames.by(actor_medians, [:rate], :actor_deflection => median)
rename!(actor_medians, Dict(:actor_deflection_median => string("Median")))
actor_medians = @byrow! actor_medians begin
    @newcol Outcome_Variable::Array{String}
    :Outcome_Variable = "Actor Deflection"
end

object_medians = DataFrames.select(actor_agents, ([:rate,:object, :object_deflection]))
object_medians = object_medians[object_medians.object_deflection .!= 999999, :]
object_medians = DataFrames.by(object_medians, [:rate], :object_deflection => median)
rename!(object_medians, Dict(:object_deflection_median => string("Median")))
object_medians = @byrow! object_medians begin
    @newcol Outcome_Variable::Array{String}
    :Outcome_Variable = "Object Deflection"
end

egalitarian_deflection = [actor_medians; object_medians]
#hierarchical_deflection = [actor_medians; object_medians]
#rec_egalitarian_def = [actor_medians; object_medians]

#Importing IPA Category Counts
#ipa_counts = py"ipa_results"
#ipa_counts = Pandas.DataFrame(ipa_counts)
#ipa_counts = DataFrames.DataFrame(ipa_counts)

model_results = py"model_data"
model_results = Pandas.DataFrame(model_results)
model_results = DataFrames.DataFrame(model_results)

#Aggregating to get counts per category
rename!(model_results, Symbol("Unnamed: 0")=>Symbol("Step"))
ipa_counts = DataFrames.select(model_results, ([:rate,:run,:Step,:bales_category]))
ipa_counts = by(ipa_counts, [:rate, :bales_category], nrow)
#unique(ipa_counts.bales_category)
rename!(ipa_counts, [:rate, :bales_id, :bales_count])

StatsPlots.ea_histogram(ipa_counts.bales_id, bins = :scott, fillalpha = 0.4)

unique(model_results.bales_category)

#Creating Higher Level Bales Categories
  #Task Behaviors (4-9)
  #Active Task Behaviors: 4-6
  #Passive Task Behaviors: 7-9

ipa_counts = @byrow! ipa_counts begin
    @newcol bales_categories::Array{String}
    if :bales_id < 4
        :bales_categories = "Positive Socio-Emotive"
    elseif :bales_id >= 10
        :bales_categories = "Negative Socio-Emotive"
    elseif :bales_id >= 4 && :bales_id <= 6
        :bales_categories = "Active Task"
        else
        :bales_categories = "Passive Task"
    end
end

#Getting Group Level Counts by Higher-Level IPA Category
#ipa_counts = DataFrames.select(ipa_counts, ([:rate, :bales_categories, :bales_count]))
ipa_counts = DataFrames.by(ipa_counts, [:rate, :bales_categories], :bales_count => sum)

#Getting Totals
sums = DataFrames.by(ipa_counts, [:rate], :bales_count_sum => sum)
rename!(ipa_counts, [:rate, :bales_categories, :bales_count])
rename!(sums, [:rate, :bales_sum])

#Joining Sums to Counts to Get Proportions
ipa_counts = join(ipa_counts, sums, on = :rate, kind = :right)

#Calculating Proportions
ipa_counts = @linq ipa_counts |>
             transform(bales_proportion = :bales_count./:bales_sum)


#Rounding Proportions for Visualization Purposes
roundmult(val, prec) = (inv_prec = 10 / prec; round(val * inv_prec) / inv_prec);

ipa_counts = @byrow! ipa_counts begin
    @newcol Group::Array{String}
    @newcol Simulation::Array{String}
    :bales_proportion = roundmult(:bales_proportion, 0.01)
    :Group = "Egalitarian"
    :Simulation = "Address the Group"
end

#ipa_counts = @byrow! ipa_counts begin
#    @newcol Group::Array{String}
#    @newcol Simulation::Array{String}
#    :bales_proportion = roundmult(:bales_proportion, 0.01)
#    :Group = "Hierarchical"
#    :Simulation = "Address the Group"
#end

#ipa_counts = @byrow! ipa_counts begin
#    @newcol Group::Array{String}
#    @newcol Simulation::Array{String}
#    :bales_proportion = roundmult(:bales_proportion, 0.01)
#    :Group = "Egalitarian"
#    :Simulation = "Reciprocity"
#end

egalitarian_counts = ipa_counts
#hierarchical_counts = ipa_counts
#rec_egalitarian_counts = ipa_counts

#Adding Simulation and Group Identifier to the Deflection Figure
egalitarian_deflection = @byrow! egalitarian_deflection begin
    @newcol Simulation::Array{String}
    @newcol Group::Array{String}
    :Group = "Egalitarian"
    :Simulation = "Address the Group"
end

#hierarchical_deflection = @byrow! hierarchical_deflection begin
#    @newcol Simulation::Array{String}
#    @newcol Group::Array{String}
#    :Group = "Hierarchical"
#    :Simulation = "Address the Group"
#end

#rec_egalitarian_def = @byrow! rec_egalitarian_def begin
#    @newcol Simulation::Array{String}
#    @newcol Group::Array{String}
#    :Group = "Egalitarian"
#    :Simulation = "Reciprocity Rate"
#end

egalitarian_agent_medians = agent_means
#hierarchical_agent_medians = agent_means
#rec_egalitarian_medians = agent_means

rename!(egalitarian_agent_medians, Dict(:Deflection_median => string("Median")))
egalitarian_agent_medians = @byrow! egalitarian_agent_medians begin
    @newcol Outcome_Variable::Array{String}
    @newcol Simulation::Array{String}
    @newcol Group::Array{String}
    :Group = "Egalitarian"
    :Simulation = "Address the Group"
    :Outcome_Variable = "Deflection"
end

#rename!(hierarchical_agent_medians, Dict(:Deflection_median => string("Median")))
#hierarchical_agent_medians = @byrow! hierarchical_agent_medians begin
#    @newcol Outcome_Variable::Array{String}
#    @newcol Simulation::Array{String}
#    @newcol Group::Array{String}
#    :Group = "Hierarchical"
#    :Simulation = "Address the Group"
#    :Outcome_Variable = "Deflection"
#end

#rename!(rec_egalitarian_medians, Dict(:Deflection_median => string("Median")))
#rec_egalitarian_medians = @byrow! rec_egalitarian_medians begin
#    @newcol Outcome_Variable::Array{String}
#    @newcol Simulation::Array{String}
#    @newcol Group::Array{String}
#    :Group = "Egalitarian"
#    :Simulation = "Reciprocity Rate"
#    :Outcome_Variable = "Deflection"
#end

######################
#   VISUALIZATIONS   #
######################

#EGALITARIAN GROUPS: Address-the-Group
@rput egalitarian_deflection
@rput egalitarian_agent_medians
@rput egalitarian_counts

#HIERARCHICAL GROUPS: Address-the-Group
#@rput hierarchical_counts
#@rput hierarchical_agent_medians
#@rput hierarchical_deflection

#@rput rec_egalitarian_counts
#@rput rec_egalitarian_medians
#@rput rec_egalitarian_def

R"""

#Work
setwd("/Users/jhmorgan/Desktop/GroupSimulator Paper/GroupSimulator_Processing/ESSC  2019 Paper Files")
getwd()

#Home
#setwd("~/Desktop/Themis.Cog/ESSC  2019 Paper Files")
#getwd()

#Saving Simulation Outputs
save(egalitarian_deflection, file = "egalitarian_deflection_20Feb2020.Rda")
save(egalitarian_agent_medians, file = "egalitarian_agent_medians_20Feb2020.Rda")
save(egalitarian_counts, file = "egalitarian_counts_20Feb2020.Rda")

#save(hierarchical_deflection, file = "hierarchical_deflection_20Feb2020.Rda")
#save(hierarchical_agent_medians, file = "hierarchical_agent_medians_20Feb2020.Rda")
#save(hierarchical_counts, file = "hierarchical_counts_20Feb2020.Rda")

#save(rec_egalitarian_counts, file = "rec_egalitarian_counts_21Feb2020.Rda")
#save(rec_egalitarian_medians, file = "rec_egalitarian_medians_21Feb2020.Rda")
#save(rec_egalitarian_def, file = "rec_egalitarian_def_21Feb2020.Rda")

"""

#############
#   TESTS   #
#############

#Julia             dplyr            LINQ
#---------------------------------------------
#@where            filter           Where
#@transform        mutate           Select (?)
#@by                                GroupBy
#groupby           group_by
#@based_on         summarise/do
#@orderby          arrange          OrderBy
#@select           select           Select

#typeof(ipa_deflection[!, :3])

#Nice function for insterting a column at a particular place
#insertcols!(actors_array[i], 4, :AgentID => actors_array[i].actor, makeunique=false)

#x = 5; y = 10
#if x < y
#    relation = "less than"
#    elseif x == y
#    relation = "equal to"
#    else
#    relation = "greater than"
#end
#println("x is ", relation, " y.")


#Conditionally Make Variables in a DataFrame
#test_df = DataFrames.DataFrame(a = 1:10, b = rand([0, 1], 10),
#                   c = rand([0, 1], 10), d = rand([0, 1], 10))
#@linq test_df |>
#    transform(e = Int.((:b .== 1) .| (:c .== 1) .| (:d .== 1)))

#@linq test_df |>
#    transform(e = ifelse.((:b .== 1) .| (:c .== 1) .| (:d .== 1), "yes", "no"))

#@linq test_df |>
#    transform(e = ifelse.((:a .>= 3) .& (:a .<= 8), "yes", "no"))
