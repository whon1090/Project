# import module

import pandas as pd
import numpy as np
import os
from datetime import datetime
import datetime
import glob
import time
import warnings
import re

warnings.filterwarnings(action='ignore')

# 미세먼지 부천 중2동, 기상 부천, 광명시 철산동, 기상 광명
start = time.time()

os.chdir('D:/data/airkorea/전국')
pm_result = pd.DataFrame()
for file in glob.glob('*.xlsx'):
    pm_data = pd.read_excel(file, encoding='CP949')
    pm_result = pd.concat([pm_result, pm_data])
pm_final = pm_result.copy()
# pm_final['지역'] = pm_final['지역'].str.split(' ').str[0]
pm_final = pm_final[pm_final['측정소명'].isin(['광교동', '고색동', '원시동', '시화산단', '고잔동', '대야동', '운서',
                                           '산본동', '고천동', '중2동', '철산동'])]

# pm_2019.columns = ['date', 'site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
# pm_2019['site'] = pm_2019['site'].replace('도로변대기', '동구')
# pm_2019['date'] = pm_2019['date'].str.replace('24:00', '00:00')

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
# pm_final = pm_final[pm_final['date'] >= '2017-12-01']
pm_final.drop(['day', 'year', 'month', 'time'], axis=1, inplace=True)
pm_final = pm_final.sort_values('date').reset_index()
pm_final.drop(['index'], axis=1, inplace=True)
pm_final = pm_final[['date', 'site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']]

pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
for replace in pm_list:
    pm_final[f'{replace}'] = pm_final[f'{replace}'].apply(lambda x: np.float("nan") if x == '-' else x)

pm_final.head()

# 7~10
os.chdir('D:/data/airkorea/경기도최종확정')
site_name = ['광교동', '고색동', '고잔동', '원시동', '본오동', '고천동', '시화산단', '산본동', '중2동']
site_list = []
for i in site_name :
    site_list.extend([i,i,i])

pm_result = pd.DataFrame()
for file, site in zip(sorted(glob.glob('*.xls'),key=os.path.getctime), site_list):
    pm_data = pd.read_excel(file, encoding='CP949')
    pm_data.drop(0,inplace=True)
    pm_data['site'] = np.repeat(site,pm_data.shape[0])
    pm_result = pd.concat([pm_result, pm_data])

pm_result.columns = ['date','pm10','pm25','o3','no2','co','so2','site']

pm_result['ymd'] = "2019-" + pm_result['date'].str[0:5]
pm_result['time'] = pm_result['date'].str[-2:]
pm_result['date'] = pm_result['ymd'] + " " + pm_result['time'] +":00"
pm_result['date'] = pm_result['date'].str.replace('24:00', '00:00')
pm_result.drop(['ymd','time'],axis=1,inplace=True)
# equal 
# pm_result['md'] = "19-" + pm_result['date'].apply(lambda x : re.split(r'\-',x)[0]) +"-" + pm_result['date'].apply(lambda x : re.split(r'\-',x)[1]) 

# equal 2
# def date_re(x) :
#     m = re.search(r"(\d\d-\d\d)",x)
#     return(m.group(0))
# pm_result['md'] = "19-"+ pm_result['date'].apply(date_re)
air_finalcheck = pm_result.copy()

os.chdir('D:/data/airkorea/경기도측정자료')
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

pm_final = pd.concat([pm_final, air_finalcheck, pm_2019])

pm_final.to_csv('D:/data/add_final2.csv', encoding='CP949')


#인천
os.chdir('D:/data/Incheon/weather')
weather_incheon = pd.DataFrame()
for file in glob.glob('*.csv'):
    weather_data = pd.read_csv(file,encoding='CP949')
    weather_incheon = pd.concat([weather_incheon,weather_data])

weather_incheon.columns = ['site', 'date', 'weather', 'rain', 'wind_speed', 'wind_direction', 'humid']
weather_incheon['site'] = weather_incheon['site'].replace(112, "운서")
weather_incheon['rain_yn'] = weather_incheon['rain'].apply(lambda x: 10 if x > 0 else 0)
weather_incheon = weather_incheon.sort_values('date').reset_index()
weather_incheon.drop(['index', 'rain'], axis=1, inplace=True)
weather_incheon['date'] = pd.to_datetime(weather_incheon['date'], format='%Y%m%d %H:%M')
# 경기도 기상

os.chdir('D:/data/weather')
weather_result = pd.DataFrame()
for file in glob.glob("*.csv"):
    data = pd.read_csv(file, encoding='CP949')
    weather_result = pd.concat([weather_result, data])

# column select
weather_col = ['시군명', '지점명', '관측일자', '관측시간', '기온', '풍속', '풍향', '강수감지', '습도']
weather_result = weather_result[weather_col]
weather_result.columns = ['sigun', 'site', 'ymd', 'time', 'weather', 'wind_speed', 'wind_direction',
                          'rain_yn', 'humid']

# time formatting
weather_result['time'] = weather_result['time'].astype(str) + ":" + '00'
weather_result['time'].unique()
text = ['0:00', '1:00', '2:00', '3:00', '4:00', '5:00', '6:00', '7:00', '8:00', '9:00']
change_text = ['00:00', '01:00', '02:00', '03:00', '04:00', '05:00', '06:00', '07:00', '08:00', '09:00']

for i, j in zip(text, change_text):
    weather_result['time'] = weather_result['time'].replace(i, j)


# date formatting
weather_result['ymd'] = pd.to_datetime(weather_result['ymd'], format='%Y%m%d')
weather_result['date'] = weather_result['ymd'].astype(str) + " " + weather_result['time'].astype(str)
weather_result = weather_result.sort_values('date').reset_index(drop=True)
# weather_result = weather_result.drop('index', axis=1)
weather_result['date'] = pd.to_datetime(weather_result['date'], format='%Y-%m-%d %H:%M')

# 기상 활용지역 선정 오전동은 '오전동 '으로 되어있음
weather = weather_result[weather_result['site'].isin(['경기', '수원', '안산', '시흥', '고잔',
                                                      '신현동', '오전동 ', '군포', '광명', '부천'])]

# 미세먼지와 지역명 통일
weather['site'] = weather['site'].replace('고잔', '고잔동')
weather['site'] = weather['site'].replace('안산', '원시동')
weather['site'] = weather['site'].replace('경기', '광교동')
weather['site'] = weather['site'].replace('신현동', '대야동')
weather['site'] = weather['site'].replace('시흥', '시화산단')
weather['site'] = weather['site'].replace('수원', '고색동')
weather['site'] = weather['site'].replace('오전동 ', '고천동')
weather['site'] = weather['site'].replace('군포', '산본동')
weather['site'] = weather['site'].replace('광명', '철산동')
weather['site'] = weather['site'].replace('부천', '중2동')

# 미세먼지와 날짜 통일
# weather = weather[('2019-07-31' >= weather['ymd'])].reset_index()

weather.head()
weather.drop(['sigun', 'ymd', 'time'], axis=1, inplace=True)
weather['date'] = weather['date'].astype(str)
weather_incheon['date'] = weather_incheon['date'].astype(str)

weather = pd.concat([weather, weather_incheon])
# 기상 측정 오류 값 결측치 처리

columnw = ['weather', 'wind_speed', 'wind_direction', 'rain_yn', 'humid']
for i in columnw:
    weather.loc[weather[i] <= -990, f'{i}'] = np.float("nan")

# merge
pm_final['date'] = pd.to_datetime(pm_final['date'], format='%Y-%m-%d %H:%M')
pm_final['date'] = pm_final['date'].astype(str)


pm_final['site'].unique().tolist()
weather['site'].unique().tolist()

weather.tail(10)

pm_weather = pd.merge(pm_final, weather, on=['date', 'site'], how='left')
pm_weather.head()
pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']

for replace in pm_list:
    pm_weather[f'{replace}'] = pm_weather[f'{replace}'].apply(lambda x: np.float("nan") if x == '-' else x)

pm_weather.isnull().sum()
pm_weather = pm_weather[(pm_weather['site']!='당동') & (pm_weather['site']!='본오동')]
pm_weather.to_csv('D:/data/row_final.csv', encoding='UTF-8',index=False)

print("time :", (time.time() - start) / 60)


# %%
