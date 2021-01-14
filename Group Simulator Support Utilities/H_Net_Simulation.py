import sys
import os
sys.path.insert(0,'/Users/jonathan.h.morgan/Desktop/Group_Simulator/groupsimulator_workshop')

from group_simulator_base_25Feb2020 import *
import itertools
import pickle
import networkx as nx
from nxpd import draw
from IPython.display import display
import numpy as np
import matplotlib.pyplot as plt
from mesa.batchrunner import BatchRunner

testnets = [((0,1,1),(1,0,1),(1,1,0)),
            ((0,1,1),(1,0,0),(1,0,0)),
            ((0,1,1), (0,0,1), (0,1,0)),
            ((0,1,0),(0,0,1),(1,0,0))]

model = GroupModel([{'epa' : random_epa([1.0, 2.5, 1.0], 1.0), 'initial_tension': 1.0},
                    {'epa' : random_epa([1.0, 0.5, 1.0], 1.0), 'initial_tension': 1.0},
                    {'epa' : random_epa([1.0, 0.5, 1.0], 1.0), 'initial_tension': 1.0}],
                    data_model = "us_unisex",
                    actor_choice = "max self-tension",
                    action_on_group = True,
                    group_action_rate = r.ag_rate[r.j_iter],
                    reciprocity_rate = r.rec_rate[1],
                    network_structure = testnets[r.i_iter])
for i in range(500):
    model.step()

#Collecting Data
agent_data = model.datacollector.get_agent_vars_dataframe()
model_data = model.datacollector.get_model_vars_dataframe()

#Converting to CSV to Preserve AgentID and Step
model_data.to_csv('result.csv')
model_data = pd.read_csv('result.csv')
os.remove('result.csv')

agent_data.to_csv('result.csv')
agent_data = pd.read_csv('result.csv')
os.remove('result.csv')
