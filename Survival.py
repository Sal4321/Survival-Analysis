# -*- coding: utf-8 -*-
"""
Created on Sun Feb  9 13:37:13 2020

@author: Salehin
"""

import lifelines
from lifelines import KaplanMeierFitter
import pandas as pd
from lifelines.statistics import logrank_test

from lifelines.datasets import load_lung
lungs = load_lung()
lungs.head()
print(lungs.describe())
duration=lungs['time']
events=lungs['status']
events=events.replace(1,0)  
events=events.replace(2,1)     
print(events)
kmf=KaplanMeierFitter()
a=kmf.fit(duration,events)
kmf.plot()
#d=lungs["sex"]
#i1=(d==1)
#i2=(d==2)
#kmf1=KaplanMeierFitter()
#b=kmf1.fit(duration[i1],events[i1],label="Male")
#kmf1.plot()
#c=kmf1.fit(duration[i2],events[i2],label="Female")
#kmf1.plot()


