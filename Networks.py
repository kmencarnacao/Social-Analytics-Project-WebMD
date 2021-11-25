#!/usr/bin/env python
# coding: utf-8

# In[169]:


import pandas as pd
import networkx as nx
from matplotlib import pyplot as plt


# In[2]:


webmd = pd.read_csv('webmd.csv')


# In[18]:


webmd.head()


# In[78]:


#Set of unique conditions that drugs are classified for in dataset    
conditions = set(webmd['Condition'].tolist())
drugs = set(webmd['Drug'].tolist())


# In[14]:


len(conditions)


# In[57]:


len(drugs)


# In[71]:


#Create Nodes list with each unique drug, conditions drug is used for, and side effects. All from webmd site information
nodeslist = []
for drug in drugs:
    node = webmd[webmd['Drug']==drug].drop(columns = ['Age','Date','EaseofUse','Effectiveness',
                                                 'Reviews','Satisfaction','Sex','UsefulCount',]).drop_duplicates()
    nodeslist+=[[drug,node['Condition'].tolist()[0],node['Sides'].tolist()[0]]]


# In[72]:


drugNodes = pd.DataFrame(data = nodeslist,columns = ['Drug','Condition','Sides'])


# In[73]:


drugNodes.head()


# In[156]:


#Create nodes list from every node in file
#Create edgelist for drugs based on condition, connect if they treat the same condition
G = nx.Graph()
edgeList=[]
nodeList=[]
for drugName in drugNodes['Drug'].tolist():
    #Create nodelist from dataframe with drug, condition, and side effects.
    row = drugNodes[drugNodes['Drug']==drugName]
    fromNode = (drugName,{'Condition':row['Condition'].tolist()[0],'Sides':row['Sides'].tolist()[0]})
    nodeList+=[fromNode]
    #print(fromNode[1]['Condition'])
    #For each node, get list of nodes with same condition to make edges
    toNodes = [(i[1]['Drug'],{'Condition':i[1]['Condition'],'Sides':i[1]['Sides']})
               for i in drugNodes[drugNodes['Condition']==(fromNode[1]['Condition'])].iterrows()
               if i[1]['Drug'] != drugName]
    nodeList+=[i for i in toNodes if i not in nodeList]
    for edgeNode in toNodes:
        edge = (fromNode[0], edgeNode[0])
        edgeList+=[edge]

G.add_nodes_from(nodeList)
G.add_edges_from(edgeList)


# In[172]:


plt.figure(figsize = (12,12))
nx.draw_networkx(G, font_size=12, with_labels=False)


# In[173]:


edgeList


# In[ ]:




