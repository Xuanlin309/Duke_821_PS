#!/usr/bin/env python
# coding: utf-8

# In[72]:


import os
os.chdir('C:\\821_ps')
from scipy.io import loadmat
annots = loadmat('WI_sort_100.mat')
import pandas as pd 
import numpy as np 
from sklearn import linear_model 
from sklearn import metrics 
import pylogit as pl 
from collections import OrderedDict
from itertools import chain 
from scipy.optimize import minimize


# In[87]:


#imports data 
person = pd.DataFrame(np.column_stack((annots['I_names'], annots['I'])))
person_char = person.set_index(0)
del person_char.index.name 
#add personal id 
person_char.loc['idcase'] = range(1, 2405,1)

index = range(1, 2405,1 )
price = pd.DataFrame(np.column_stack((index, annots['Z'])))
travel_cost = price.set_index(0)
del travel_cost.index.name 
travel_cost = travel_cost.T
#change row name by siteid 
travel_cost = travel_cost.rename(index = str)
travel_cost = travel_cost.rename(index = lambda x: "travelcostsite" + x )


site = pd.DataFrame(np.column_stack((annots['X_names'], annots['X'])))
site_char = site.set_index(0)
del site_char.index.name 

column= ['site_choice']
choice = pd.DataFrame(np.row_stack((column,annots['L'])))
choice.columns = choice.iloc[0]
choice = choice.iloc[1:]


# In[88]:


##data arrangement for multinominal logit estimation, wide framework 
frame = [person_char, travel_cost]
data = pd.concat(frame)

site_char = site_char.rename(columns = str)
site_char = site_char.rename(columns = lambda x: 'site'+x )
site_char_long = pd.DataFrame(site_char.unstack())
site_char_long = site_char_long.reset_index()
#site_char_long['new'] = site_char_long[['level_1','level_0']].apply(lambda x: ''.join(x),axis=1)
#site_char_long['new'] = site_char_long['level_1']+ "-" + site_char_long['level_0']
site_char_long['new'] = site_char_long['level_1'].str.cat(site_char_long['level_0'])
site_char_long = site_char_long.set_index('new')
del site_char_long.index.name 
site_char_long = pd.DataFrame(site_char_long[0])
site_char_long = site_char_long.rename(columns = str)
numcopies = 2403
for i in range(1,numcopies+1):
    site_char_long[chr(ord("0") + i)] = site_char_long["0"]
site_char_long.columns = np.arange(1,2405)

frame = [data, site_char_long ]
data = pd.concat(frame)

choice = choice.T
del choice.index.name 
frame = [data, choice ]
data = pd.concat(frame)
data = data.T
data.columns = [c.replace(" ", "") for c in data.columns]
data


# In[100]:


data["choice"] = data["site_choice"].astype(int)
long_fishing_df["ramp"] = pd.to_numeric(long_fishing_df["ramp"])
long_fishing_df["restroom"] = pd.to_numeric(long_fishing_df["restroom"])
long_fishing_df["walleye"] = pd.to_numeric(long_fishing_df["walleye"])
long_fishing_df["salmon"] = pd.to_numeric(long_fishing_df["salmon"])
long_fishing_df["panfish"] = pd.to_numeric(long_fishing_df["panfish"])
long_fishing_df["travelcost"] = pd.to_numeric(long_fishing_df["travelcost"])

#all site choices are available to individual, thus availability  columns are filled by 1 for every choice 
for i in range (1,101):
    data["availability_{}".format(i)] = 1
data
 


# In[101]:


#################need explanation 
fishing_alt_id = "alt_id"

travelcost= OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["travelcostsite{}".format(i) for i in range(1, 101)]):
    travelcost[alt_id] = var
    
ramp= OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["rampsite{}".format(i) for i in range(1, 101)]):
    ramp[alt_id] = var

restroom= OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["restroomsite{}".format(i) for i in range(1, 101)]):
    restroom[alt_id] = var

walleye= OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["walleyesite{}".format(i) for i in range(1, 101)]):
    walleye[alt_id] = var

salmon= OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["salmonsite{}".format(i) for i in range(1, 101)]):
    salmon[alt_id] = var

panfish= OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["panfishsite{}".format(i) for i in range(1, 101)]):
    panfish[alt_id] = var

shares= OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["sharessite{}".format(i) for i in range(1, 101)]):
    shares[alt_id] = var

fishing_alt_varying_variables = {"travelcost": travelcost, "ramp": ramp, "restroom": restroom, "walleye": walleye, "salmon":salmon, "panfish":panfish, "shares" :shares}

fishing_obs_id_col = "idcase"

fishing_choice_column = "choice"

fishing_ind_variables = ["site_choice","boat", "kids", "ones"]

fishing_availability_variables = OrderedDict()
for alt_id, var in zip(range(1, 101),
                       ["availability_{}".format(i) for i in range(1, 101)]):
    fishing_availability_variables[alt_id] = var


# In[105]:


##########
# Actually perform the conversion to long format
##########
long_fishing_df = pl.convert_wide_to_long(wide_data=data,
                                          ind_vars=fishing_ind_variables,
                                          alt_specific_vars=fishing_alt_varying_variables,
                                          availability_vars=fishing_availability_variables,
                                          obs_id_col=fishing_obs_id_col,
                                          choice_col=fishing_choice_column,
                                          new_alt_id_name=fishing_alt_id)
# 240400 rows × 14 columns
# Look at the long format Heating data
long_fishing_df.head()
long_fishing_df.to_csv("long_data.csv")


# In[84]:


long_fishing_df


# In[ ]:





# In[ ]:




