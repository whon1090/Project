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


os.chdir('D:/data/car/고속도로명 주소')
car_result = pd.DataFrame()

for file in glob.glob('*.xls'):
    car_data = pd.read_html(file)
    car_data = pd.concat(car_data)
    car_data.columns = car_data.iloc[1,:].tolist()
    car_data.drop([0,1],inplace=True)
    car_data = car_data.dropna(subset=['지점번호']).reset_index(drop=True)
    car_result = pd.concat([car_result, car_data])

car_code = car_result.copy()
car_code['노선번호'] = car_code['노선번호'].apply(lambda x : x.split('선',1)[1])
car_code['노선번호'] = car_code['노선번호'].apply(lambda x : x.split('(',1)[0])
car_code['노선번호'] = car_code['노선번호'].str.replace('선','고속도로')

car_code['구간'] = car_code['구간'].str.replace(" ", "")
car_code = car_code[['노선번호','지점번호','도명','시/군','동/리','구간']]
cols = ['line_code','code','site','sigun','dong','line']
car_code.columns = cols
car_code.head()

#load traffic volumn data
os.chdir('D:/data/car/상시교통량')

car_result = pd.DataFrame()
for file in glob.glob('*.csv'):
    car_data = pd.read_csv(file,encoding='CP949')
    car_data.columns = car_data.iloc[0,:].tolist()
    car_data.drop([0],inplace=True)
    car_result = pd.concat([car_result, car_data])

car_traffic = car_result.copy()
car_traffic['노선 명'] = car_traffic['노선 명'].str.replace('선','고속도로')
car_traffic.head()

car_list = car_traffic.columns.tolist()
car_list[1]
for i in range(len(car_list)):
    car_list[i] = car_list[i].split('시',1)[0]

for i in range(5,29) :
    car_list[i] = car_list[i] +":00"
car_list
car_traffic.columns = car_list
car_traffic.drop(['차로','전일합계(대)'],axis=1,inplace=True)
car_traffic = pd.melt(car_traffic, id_vars=['노선 명','구간 명','날짜','방향'], var_name='time')
car_traffic['날짜'] =car_traffic['날짜']+ ' ' +car_traffic['time']
car_traffic.drop(['time','방향'],axis=1,inplace=True)
car_traffic['value'] = car_traffic['value'].apply(lambda x: np.float("nan") if x == '-' else x)
car_traffic['value'] = car_traffic['value'].str.replace(',','')
car_traffic['value'] = car_traffic['value'].str.replace(' ','')
car_traffic['value'] = car_traffic['value'].astype(float)
car_traffic = car_traffic.groupby(['노선 명','구간 명','날짜'])['value'].sum()
car_traffic = car_traffic.reset_index()
car_traffic.columns = ['line_code','line','date','value']
car_traffic['upline'] = car_traffic['line'].apply(lambda x : x.split('-',1)[1])
car_traffic['downline'] = car_traffic['line'].apply(lambda x : x.split('-',1)[0])
car_traffic.drop('line',axis=1,inplace=True)

# 시화산단 list

sihwa_list = ['월곶JC','서창JC','군자JC','장수IC','남안산IC']

car_sihwa = car_traffic[(car_traffic['upline'].isin(sihwa_list)) & (car_traffic['downline'].isin(sihwa_list))].reset_index(drop=True)
car_sihwa.drop(['upline','downline','line_code'], axis=1, inplace=True)
car_sihwa['site'] = np.repeat('시화산단',repeats=len(car_sihwa))
car_cols =['date','site','value']
car_sihwa.head()
car_sihwa =car_sihwa[car_cols]
car_sihwa.head()

car_sihwa.to_csv('D:/data/car_sihwa.csv',encoding='CP949',index=False)

# 대야동 list

daeya_list = ['시흥TG','안현JC','시흥IC','장수IC','신천IC']

car_daeya = car_traffic[(car_traffic['upline'].isin(daeya_list)) & (car_traffic['downline'].isin(daeya_list))].reset_index(drop=True)
car_daeya.drop(['upline','downline','line_code'], axis=1, inplace=True)
car_daeya['site'] = np.repeat('대야동',repeats=len(car_daeya))
car_daeya.head()

car_daeya.to_csv('D:/data/car_daeya.csv',encoding='CP949')

# 원시동 list

wonsi_list = ['서안산IC', '안산IC', '남안산IC', '마도IC']

car_wonsi = car_traffic[(car_traffic['upline'].isin(wonsi_list)) & (car_traffic['downline'].isin(wonsi_list))].reset_index(drop=True)
car_wonsi.drop(['upline','downline','line_code'], axis=1, inplace=True)
car_wonsi['site'] = np.repeat('원시동',repeats=len(car_wonsi))

car_wonsi.head()

car_wonsi.to_csv('D:/data/car_daeya.csv',encoding='CP949')

#### 일반 국도 ####

kokdo_2017 = pd.read_excel('D:/data/car/일반국도/2017.xlsx',encoding='CP949')
kokdo_2018 = pd.read_excel('D:/data/car/일반국도/2018.xlsx',encoding='CP949')

kokdo = pd.concat([kokdo_2017,kokdo_2018])
# 방향 0(합계), 1,2 (상,하행)이므로 합계만 사용
kokdo = kokdo[kokdo['방향'].isin([0,'양방향'])]
# 시흥 지점번호 4202-01만 사용하고 merge위해 시화산단으로 명칭 변경
kokdo['지점번호'].unique().tolist()
kokdo = kokdo[kokdo['지점번호'].isin(['4202-01','4202-001'])]
kokdo['지점번호'].replace('4202-01','시화산단',inplace=True)
kokdo['지점번호'].replace('4202-001','시화산단',inplace=True)
kokdo[['년도','월','일']] = kokdo[['년도','월','일']].astype(str)
kokdo['일'].unique().tolist()
# date_list = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
# for i in date_list:
#     kokdo['월'].replace(i,"0"+i,inplace=True)
#     kokdo['일'].replace(i,"0"+i,inplace=True)
kokdo['date'] = kokdo['년도']+ "-" + kokdo['월'] +'-' + kokdo['일']
kokdo['date'] = pd.to_datetime(kokdo['date'], format='%Y-%m-%d')
kokdo.drop(['년도','월','일','방향','합계'],axis=1,inplace=True)
kokdo =pd.melt(kokdo, id_vars=['date', '지점번호'], var_name='time', value_name= 'normal_road')
kokdo_list = ['1시', '2시', '3시', '4시', '5시', '6시', '7시', '8시', '9시']
for i in kokdo_list:
    kokdo['time'].replace(i,"0"+i,inplace=True)
kokdo['time'] = kokdo['time'].str.replace('시',':00')
kokdo['date'] = kokdo['date'].astype(str)
kokdo['date'] = kokdo['date'] + " " + kokdo['time']
kokdo['date'] = kokdo['date'].str.replace('24:00', '00:00')
kokdo['date'] = pd.to_datetime(kokdo['date'], format='%Y-%m-%d %H:%M')
kokdo.drop('time',axis=1,inplace=True)
kokdo.rename(columns={"지점번호": "site"},inplace=True)
kokdo.head()
kokdo.to_csv('D:/data/car/normal_road.csv',index=False)

car_sihwa = pd.read_csv('D:/data/car_sihwa.csv',encoding='CP949')
# car_sihwa.drop(['Unnamed: 0','line_code'],axis=1,inplace=True)
car_sihwa= car_sihwa.groupby(['date', 'site'])['value'].agg({'highway' : np.sum}).reset_index()
car_sihwa['date'] = pd.to_datetime(car_sihwa['date'], format='%Y-%m-%d %H:%M')

car_sihwa.head()

kokdo['date'] = kokdo['date'].astype(str)
car_sihwa['date'] = car_sihwa['date'].astype(str)

car_data = pd.merge(car_sihwa,kokdo,on=['date','site'])
car_data.head()
car_data.to_csv('D:/data/car/kokdo_highway.csv',encoding='CP949',index=False)

# code_list = car_traffic['line_code'].unique().tolist()
# code_list = pd.DataFrame(code_list)
# code_site_list = car_code['line_code'].unique().tolist()
# code_site_list = pd.DataFrame(code_site_list)

# line_list = car_traffic['line'].unique().tolist()
# line_list = pd.DataFrame(line_list)
# line_site_list = car_code['line'].unique().tolist()
# line_site_list = pd.DataFrame(line_site_list)
# df = pd.concat([code_list,code_site_list,line_list,line_site_list], axis=1)
# df.to_csv('D:/data/구간확인.csv',encoding='CP949')
# car_set = car_code[(car_traffic['line_code'].isin(code_list)) & (car_traffic['line'].isin(line_list))]
# car_set_code = car_set['line_code'].unique().tolist()
# car_set_line = car_set['line'].unique().tolist()

# for i in range(len(car_traffic)) :
#     if car_traffic[(car_traffic['line_code'].isin(car_set_code)) & (car_traffic['line'].isin(car_set_code))].iloc(i) :
#         car_traffic['site'] = car_code

# car_traffic[(car_traffic['line_code'].isin(car_set_code)) & (car_traffic['line'].isin(car_set_code))].iloc(i)

# car_traffic['site'] = car_traffic
