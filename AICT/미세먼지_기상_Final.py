# import module

import pandas as pd
import numpy as np
import os
from datetime import datetime
import glob
import time
import warnings
import re

warnings.filterwarnings(action='ignore')

# 에어코리아 최종확정 측정자료 사용(전국)
# 미세먼지 부천 중2동, 기상 부천, 광명시 철산동, 기상 광명
start = time.time()

os.chdir('D:/data/airkorea/전국')
pm_result = pd.DataFrame()
for file in glob.glob('*.xlsx'):
    pm_data = pd.read_excel(file, encoding='CP949')
    pm_result = pd.concat([pm_result, pm_data])

pm_final = pm_result[pm_result['측정소명'].isin(['광교동', '고색동', '원시동', '시화산단', '고잔동', '대야동', '운서',
                                           '산본동', '고천동', '중2동', '철산동'])]

from dateutil.relativedelta import relativedelta

def date_format(ymd, time):
    if time=='24:00' :
        time = '00:00'
        ymd += relativedelta(days=1)
        ymd = ymd.strftime('%Y-%m-%d')
        date = ymd + ' '+  time
        return(date)
    else : 
        ymd = ymd.strftime('%Y-%m-%d')
        date = ymd + ' '+ time
        return(date)


def date_type(data) :
    data['date'] = data.apply(lambda x : date_format(x['ymd'],x['time']), axis=1)
    data['date'] = pd.to_datetime(data['date'], format='%Y%m%d %H:%M')
    data.drop(['ymd', 'time'], axis=1, inplace=True)

pm_final = pm_final.iloc[:, [0, 1, 2, 3, 4, 5, 9, 11]]
pm_final.columns = ['co', 'no2', 'o3', 'pm10', 'pm25', 'so2', 'site', 'date']
pm_final['date'] = pm_final['date'].astype(str)
pm_final['ymd'] = pm_final['date'].str[:8]
pm_final['ymd'] = pd.to_datetime(pm_final['ymd'], format='%Y%m%d')
pm_final['time'] = pm_final['date'].str[8:10] + ":00"
date_type(pm_final)
pm_final = pm_final[pm_final['date'] >= '2017-12-01']
pm_final = pm_final.sort_values('date').reset_index(drop=True)
pm_final = pm_final[['date', 'site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']]

# 미세먼지 7월~10월 데이터
os.chdir('D:/data/airkorea/경기도최종확정')

# 크롤링 다운로드 순서로 site 매칭
site_name = ['광교동', '고색동', '고잔동', '원시동', '본오동', '고천동', '시화산단', '산본동', '중2동']
site_list = []
for i in site_name :
    site_list.extend(np.repeat(i,3))

pm_result = pd.DataFrame()
for file, site in zip(sorted(glob.glob('*.xls'),key=os.path.getctime), site_list):
    pm_data = pd.read_excel(file, encoding='CP949')
    pm_data.drop(0,inplace=True)
    pm_data['site'] = np.repeat(site,pm_data.shape[0])
    pm_result = pd.concat([pm_result, pm_data])

pm_result.columns = ['date','pm10','pm25','o3','no2','co','so2','site']

pm_result['ymd'] = "2019-" + pm_result['date'].str[0:5]
pm_result['ymd'] = pd.to_datetime(pm_result['ymd'], format='%Y-%m-%d')
pm_result['time'] = pm_result['date'].str[-2:] + ":00"
date_type(pm_result)

# equal 
# pm_result['md'] = "2019-" + pm_result['date'].apply(lambda x : re.split(r'\-',x)[0]) +"-" + pm_result['date'].apply(lambda x : re.split(r'\-',x)[1]) 

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
pm_2019['ymd'] = pd.to_datetime(pm_2019['date'], format='%Y-%m-%d')
date_type(pm_2019)
pm_2019.drop(['날짜'],axis=1,inplace=True)
pm_2019.columns = ['pm10','pm25','o3','no2','co','so2','site','date']
cols = ['date','site','so2','co','o3','no2','pm10','pm25']
pm_2019 =pm_2019[cols]


#API 12
os.chdir('D:/data/airkorea/API')
pm_12 = pd.read_csv('pm_3month.csv',encoding='CP949')
pm_12.columns = ['date','site','so2','co','o3','no2','pm10','pm25']
pm_12['ymd'] = pm_12['date'].str[:10]
pm_12['ymd'] = pd.to_datetime(pm_12['ymd'], format='%Y-%m-%d')
pm_12['time'] = pm_12['date'].str[11:]
date_type(pm_12)
pm_12 = pm_12[(pm_12['date']>='2019-12-01') & (pm_12['date']<'2020-01-01')]

pm_final = pd.concat([pm_final, air_finalcheck, pm_2019, pm_12])
pm_final.to_csv('D:/data/add_final2.csv', encoding='CP949',index=False)

#인천 기상
os.chdir('D:/data/Incheon/weather')
weather_incheon = pd.DataFrame()
for file in glob.glob('*.csv'):
    weather_data = pd.read_csv(file,encoding='CP949')
    weather_incheon = pd.concat([weather_incheon,weather_data])

weather_incheon.columns = ['site', 'date', 'temp', 'rain', 'wind_speed', 'wind_direction', 'humid']
weather_incheon['site'] = weather_incheon['site'].replace(112, "운서")
weather_incheon['rain_yn'] = weather_incheon['rain'].apply(lambda x: 10 if x > 0 else 0)
weather_incheon = weather_incheon.sort_values('date').reset_index(drop=True)
weather_incheon['date'] = pd.to_datetime(weather_incheon['date'], format='%Y%m%d %H:%M')
weather_incheon.drop(['rain'], axis=1, inplace=True)

# 경기도 기상

os.chdir('D:/data/weather')
weather_result = pd.DataFrame()
for file in glob.glob("*.csv"):
    data = pd.read_csv(file, encoding='CP949')
    weather_result = pd.concat([weather_result, data])


# column select
weather_col = ['시군명', '지점명', '관측일자', '관측시간', '기온', '풍속', '풍향', '강수감지', '습도']
weather_result = weather_result[weather_col]
weather_result.columns = ['sigun', 'site', 'ymd', 'time', 'temp', 'wind_speed', 'wind_direction',
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
weather_result['date'] = pd.to_datetime(weather_result['date'], format='%Y-%m-%d %H:%M')

# 기상 활용지역 선정 오전동은 '오전동 '으로 되어있음
weather_list = ['고잔','안산','경기','신현동','시흥','수원','오전동','군포','광명','부천']
weather = weather_result[weather_result['site'].isin(weather_list)]

# 미세먼지와 지역명 통일
change_list = ['고잔동','원시동','광교동','대야동','시화산단','고색동','고천동','산본동','철산동','중2동']

for i,j in zip(weather_list,change_list):
    weather['site'].replace(i,j,inplace=True)

weather.drop(['sigun', 'ymd', 'time'], axis=1, inplace=True)
weather['date'] = weather['date'].astype(str)
weather_incheon['date'] = weather_incheon['date'].astype(str)

weather = pd.concat([weather, weather_incheon])

# 기상 측정 오류 값 결측치 처리

columnw = ['temp', 'wind_speed', 'wind_direction', 'rain_yn', 'humid']
for i in columnw:
    weather.loc[weather[i] <= -990, f'{i}'] = np.float("nan")

# merge
pm_final['date'] = pd.to_datetime(pm_final['date'], format='%Y-%m-%d %H:%M')
pm_final['date'] = pm_final['date'].astype(str)

pm_weather = pd.merge(pm_final, weather, on=['date', 'site'], how='left')

pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']

for replace in pm_list:
    pm_weather[replace] = pm_weather[replace].apply(lambda x: np.float("nan") if x == '-' else x)


pm_weather = pm_weather[(pm_weather['site']!='당동') & (pm_weather['site']!='본오동')]
pm_weather.to_csv('D:/data/row_final.csv', encoding='UTF-8',index=False)

print("time :", (time.time() - start) / 60) 