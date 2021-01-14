#Calling GS II in Julia
#Jonathan H. Morgan
#15 February 2019

#Setting Working Directory
cd("/Users/jonathan.h.morgan/Desktop/Group_Simulator/groupsimulator_workshop")
pwd()

#For more inforation: https://github.com/JuliaPy/PyCall.jl
#For information about building the file: http://www.math.brown.edu/~sswatson/classes/data1010/setup.html
#For more information about installing Mesa: https://stackoverflow.com/questions/46603380/how-to-install-mesa-python-package-for-use-in-python-3/46603478

################
#   PACAKGES   #
################

#using PyCall

########################################
#   Executing Python Script in Julia   #
########################################
using PyCall

#Example of Executing a Python Function
math = pyimport("math")
local x = math.sin(math.pi / 4)::Float64

#Displaying a Plot
plt = pyimport("matplotlib.pyplot")
x = range(0;stop=2*pi,length=1000); y = sin.(3*x + 4*cos.(2*x));
plt.plot(x, y, color="red", linewidth=2.0, linestyle="--")
plt.show()

###########################
#   SIMPLE MESA EXAMPLE   #
###########################

using PyCall
py"""
from mesa import Agent, Model
from mesa.time import RandomActivation

class MoneyAgent(Agent):
    def __init__(self, unique_id, model):
        super().__init__(unique_id, model)
        self.wealth = 1

    def step(self):
        if self.wealth == 0:
            return
        other_agent = self.random.choice(self.model.schedule.agents)
        other_agent.wealth += 1
        self.wealth -= 1

class MoneyModel(Model):
    def __init__(self, N):
        self.num_agents = N
        self.schedule = RandomActivation(self)
        # Create agents
        for i in range(self.num_agents):
            a = MoneyAgent(i, self)
            self.schedule.add(a)

    def step(self):
        '''Advance the model by one step.'''
        self.schedule.step()

model = MoneyModel(10)
for i in range(10):
    model.step()

import matplotlib.pyplot as plt
agent_wealth = [a.wealth for a in model.schedule.agents]
plt.hist(agent_wealth)
plt.savefig('example.png')
"""

####################
#   RUNNING GSII   #
####################
using PyCall, DataFrames
pushfirst!(PyVector(pyimport("sys")."path"), "/Users/jonathan.h.morgan/Desktop/Group_Simulator/groupsimulator_workshop")

py"""
from group_simulator_base import *
import itertools
import pickle
import networkx as nx
from nxpd import draw
from IPython.display import display
import numpy as np
import seaborn as sns
from mesa.batchrunner import BatchRunner
from mesa.space import MultiGrid

epa_colors=["#cb9f00", "#058c7d", "#78325f"]

N=3

model = GroupModel({'N': N, 'epa' : [1.0, 1.5, 1], 'initial_tension': 1.0, 'individuality': 1.0},
                   "us_unisex", reciprocity_rate = 0.8, actor_choice = "max self-tension",
                   action_on_group = True,
                   group_action_rate = 0.2)
for i in range(500):
    model.step()

agent_df = model.datacollector.get_agent_vars_dataframe()
model_df = model.datacollector.get_model_vars_dataframe()

agent_df.head()

frames=[]
for i in range(N):
    frame = agent_df.xs(i, level="AgentID")[["Deflection"]]
    frame.columns=["def. agent " + str(i)]
    frames.append(frame)
result=pd.concat(frames, axis=1, sort=False)

result[:100].plot()
plt.ylabel("deflection")
plt.savefig('deflection.png')

agent_array = agent_df.to_numpy()

"""
agent_df = py"agent_df"
agent_array = py"agent_array"

using Pandas
agent_df = Pandas.DataFrame(agent_df)
typeof(agent_df)

x = agent_df[:Deflection]
typeof(x)

using DelimitedFiles
writedlm( "Test.csv",  agent_array, ',')
