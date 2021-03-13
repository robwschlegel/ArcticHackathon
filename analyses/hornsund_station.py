# -*- coding: utf-8 -*-
"""

@author: Adrien Wehrlé, GEUS (Geological Survey of Denmark and Greenland)

"""


import pandas as pd

data_rh = pd.read_csv('H:/Hack_the_Arctic/Hornsund/datasets/Hornsund_daily_RH.tab',
                      sep='\t', skiprows=14)

data_rh.to_csv('C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/data_for_shiny/Hornsund_RH.csv')

data_precip = pd.read_csv('H:/Hack_the_Arctic/Hornsund/datasets/Hornsund_daily_Precip.tab',
                          sep='\t', skiprows=14)

data_precip.to_csv('C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/data_for_shiny/Hornsund_Precip.csv')

data_temp = pd.read_csv('H:/Hack_the_Arctic/Hornsund/datasets/Hornsund_daily_TAmean.tab',
                          sep='\t', skiprows=14)

data_temp.to_csv('C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/data_for_shiny/Hornsund_Tmean.csv')