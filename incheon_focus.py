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
# 모델링 샘플 위한 인천시만 추출
start = time.time()


os.chdir('D:/data/Incheon/pm')
pm_result = pd.DataFrame()
for file in glob.glob('*.xlsx'):
    pm_data = pd.read_excel(file, encoding='CP949')
    pm_result = pd.concat([pm_result, pm_data])

pm_final = pm_result.copy()
pm_final['지역'] = pm_final['지역'].str[:2] 
pm_final = pm_final[pm_final['지역'] == '인천']
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
os.chdir('D:/data/pm1905_1908')
pm_result = pd.DataFrame()

for file, site in zip(glob.glob('*.xls'),incheon_list):
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


incheon_pm = pd.read_csv('D:/data/incheon_final.csv', encoding='CP949')
incheon_pm = incheon_pm.drop('Unnamed: 0', axis=1)
incheon_pm.columns = ['date','site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
incheon_pm['date'] = incheon_pm['date'].str.replace('24:00', '00:00')
incheon_pm['date'] = pd.to_datetime(incheon_pm['date'], format='%Y-%m-%d %H:%M')

pm_final['date'] = pm_final['date'].astype(str)
incheon_pm['date'] = incheon_pm['date'].astype(str)

pm_incheon = pd.concat([pm_final, incheon_pm, pm_2019])

pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
for replace in pm_list:
    pm_incheon[f'{replace}'] = pm_incheon[f'{replace}'].apply(lambda x: np.float("nan") if x == '-' else x)

pm_incheon.info()
pm_incheon.isnull().sum()
pm_incheon.to_csv('D:/data/incheon_data.csv',encoding='CP949')


print("time :", (time.time() - start) / 60)  # 현재시각 - 시작시간