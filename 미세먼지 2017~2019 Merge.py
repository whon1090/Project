# import module
import pandas as pd
import numpy as np
import os
from datetime import datetime
import datetime
from pandas import Series,DataFrame
import glob
import matplotlib.pyplot as plt
import matplotlib as mpl
import seaborn as sns
from sklearn import datasets, linear_model

os.chdir('D:/data')

# load pm 2019 01~07 data
pm_2019 = pd.DataFrame()
for file in glob.glob("*.txt"):
   data = pd.read_csv(file, encoding='CP949')
   pm_2019 = pd.concat([pm_2019, data])

# formatting 2017~2019

pm2017 = pd.read_csv('./pm2017/pm_2017.txt', encoding="CP949")
pm2018 = pd.read_csv('대기질측정정보(2018년).csv', encoding="CP949")

pm_2019= pm_2019.drop('시군코드',axis=1)
pm2018 = pm2018.drop('시군코드',axis=1)

columnk = pm2018.columns

pm_2019.columns = columnk
pm2017.columns = columnk

korea_pm = pd.concat([pm_2019,pm2017,pm2018])
korea_pm.to_csv('korea.csv',encoding='UTF-8')