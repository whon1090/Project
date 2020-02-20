# import module
import pandas as pd
import numpy as np
import os
from datetime import datetime
import datetime
from pandas import Series,DataFrame
from math import *
import re
import glob
import matplotlib.pyplot as plt
import matplotlib as mpl
import seaborn as sns

from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.regression.linear_model import OLS
import statsmodels.formula.api as sm
import time
import random
import warnings

warnings.filterwarnings(action='ignore')
start = time.time()


os.chdir('D:/data/Incheon/pm')
pm_result = pd.DataFrame()
for file in glob.glob('*.xlsx'):
    pm_data = pd.read_excel(file, encoding='CP949')
    pm_result = pd.concat([pm_result, pm_data])

pm_final = pm_result.copy()
pm_final = pm_final[pm_final['측정소명'] == '송산면']
pm_final = pm_final.iloc[:, [0, 1, 2, 3, 4, 5, 9, 11]]
pm_final.columns = ['co', 'no2', 'o3', 'pm10', 'pm25', 'so2', 'site', 'date']
pm_final['date'] = pm_final['date'].astype(str)
pm_final['year'] = pm_final['date'].str[:4]
pm_final['month'] = pm_final['date'].str[4:6]
pm_final['day'] = pm_final['date'].str[6:8]
pm_final['time'] = pm_final['date'].str[8:10]
pm_final['time'] = pm_final['time'] + ":00"
pm_final['date'] = pm_final['year'] + '-' + pm_final['month'] + '-' + pm_final['day'] + " " + pm_final['time']
pm_final['date'] = pm_final['date'].str.replace('24:00', '00:00')
pm_final['date'] = pd.to_datetime(pm_final['date'], format='%Y%m%d %H:%M')
pm_final.drop(['day', 'year', 'month', 'time'], axis=1, inplace=True)
pm_final = pm_final.sort_values('date').reset_index()
pm_final.drop(['index'], axis=1, inplace=True)
pm_final = pm_final[['date', 'site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']]

pm_final.isnull().sum()

pm_final['site'].unique().tolist()
pm_final.head()


# crawling data load
os.chdir('D:/data/airkorea/충남')
pm_result = pd.DataFrame()

for file in glob.glob('*.xls'):
    pm_data = pd.read_excel(file,encoding='CP949')
    pm_data['Unnamed: 0'].iloc[0]= re.split(r'\(',pm_data['Unnamed: 0'].iloc[0])[0].strip()
    pm_data['site'] = np.repeat(pm_data['Unnamed: 0'].iloc[0],pm_data.shape[0])
    for i,j  in zip(range(2,14,2),range(1,14,2)) :
        pm_data.loc[2,:][i] = pm_data.loc[2,:][j]
    pm_data.columns = pm_data.iloc[2,:].tolist()
    pm_data.drop([0,1,2,3,4],inplace=True)
    pm_data.rename(columns={pm_data.columns[13]:"site"}, inplace = True)
    pm_data = pm_data.iloc[:,[0,2,4,6,8,10,12,13]]
    pm_result = pd.concat([pm_result, pm_data])

pm_2019 = pm_result.copy()
pm_2019['date'] = pm_2019['날짜'].apply(lambda x : re.split(r'\:',x)[0])
pm_2019['time'] = pm_2019['날짜'].apply(lambda x : re.split(r'\:',x)[1])
pm_2019['time'] = pm_2019['time'] + ":00"
pm_2019['date'] = pm_2019['date'] + " " + pm_2019['time']
pm_2019['date'] = pm_2019['date'].str.replace('24:00', '00:00')
pm_2019.drop(['날짜','time'],axis=1,inplace=True)
pm_2019.columns = ['pm10','pm25','o3','no2','co','so2','site','date']
cols = ['date','site','so2','co','o3','no2','pm10','pm25']
pm_2019 =pm_2019[cols]
pm_2019.head()


chungnam_pm = pd.read_csv('D:/data/chungnam_3month.csv', encoding='CP949')
chungnam_pm = chungnam_pm.drop('Unnamed: 0', axis=1)
chungnam_pm.columns = ['date','site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
chungnam_pm['date'] = chungnam_pm['date'].str.replace('24:00', '00:00')
chungnam_pm['date'] = pd.to_datetime(chungnam_pm['date'], format='%Y-%m-%d %H:%M')

pm_final['date'] = pm_final['date'].astype(str)
chungnam_pm['date'] = chungnam_pm['date'].astype(str)

pm_chungnam = pd.concat([pm_final, chungnam_pm, pm_2019])

pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
for replace in pm_list:
    pm_chungnam[f'{replace}'] = pm_chungnam[f'{replace}'].apply(lambda x: np.float("nan") if x == '-' else x)

pm_chungnam.info()
pm_chungnam.isnull().sum()
pm_chungnam.to_csv('D:/data/chungnam_data.csv',encoding='CP949')

# weather 
weather_result =pd.DataFrame()
os.chdir('D:/data/충남기상')
for file in glob.glob('*.csv'):
    weather_data = pd.read_csv(file, encoding='CP949')
    weather_result = pd.concat([weather_result, weather_data])

weather_result.columns = ['site', 'date', 'weather','wind_direction','wind_speed','rain_yn','humid']

weather_result['site'] = weather_result['site'].apply(lambda x : '송산면' if x == 616 else x)
weather_result['rain_yn'] = weather_result['rain_yn'].apply(lambda x : 10 if x != 0 else x)
weather_result['date'] = weather_result['date'] +':00'

weather_result.to_csv('D:/data/chungnam_weather.csv',index =False)


# Data Merge

pm = pd.read_csv('D:/data/chungnam_data.csv',encoding='CP949')
weather = pd.read_csv('D:/data/chungnam_weather.csv')

pm.drop('Unnamed: 0',axis=1,inplace=True)
pm['site'] = pm['site'].str.replace("당진",'송산면')
# date type이 깨져있어서 format 맞추고 다시 진행
pm['date'] = pd.to_datetime(pm['date'], format='%Y%m%d %H:%M:%S')
weather['date'] = pd.to_datetime(weather['date'], format='%Y%m%d %H:%M:%S')
pm['date'] = pm['date'].astype(str)
weather['date'] =weather['date'].astype(str)
pm_weather = pd.merge(pm,weather,on=['date','site'],how='left')


pm_weather.to_csv('D:/data/chungnam_pm_weather.csv', index = False)

print("time :", (time.time() - start) / 60)  # 현재시각 - 시작시간