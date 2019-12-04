# import module
import pandas as pd
import numpy as np
import os
from datetime import datetime
import datetime
from pandas import Series, DataFrame
import glob
import matplotlib.pyplot as plt
import matplotlib as mpl
import seaborn as sns
from sklearn import datasets, linear_model
import time
import warnings

warnings.filterwarnings(action='ignore')

# 미세먼지
# 미세먼지 군포시 산본동, 의왕시 고천동, weather 군포시 군포, 의왕시 오전동 추가
start = time.time()

os.chdir('D:/data')
korea_pm = pd.read_csv('korea.csv', encoding='UTF-8')
korea_pm = korea_pm.drop('Unnamed: 0', axis=1)


# def
def pm_site(a):
    columnk = a['측정소명'].unique().tolist()
    for i, j in zip(columnk, range(len(columnk))):
        globals()['pm{}'.format(j)] = a[a['측정소명'] == i].iloc[:, [1, 4, 5, 6, 7, 8, 9, 10]]
        globals()['pm{}'.format(j)].columns = ['site', 'date', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
        globals()['pm{}'.format(j)]['date'] = globals()['pm{}'.format(j)]['date'].str.replace('24:00', '00:00')
        globals()['pm{}'.format(j)]['date'] = pd.to_datetime(globals()['pm{}'.format(j)]['date'],
                                                             format='%Y-%m-%d %H:%M')
        globals()['pm{}'.format(j)] = globals()['pm{}'.format(j)].sort_values('date').reset_index()
        globals()['pm{}'.format(j)] = globals()['pm{}'.format(j)].drop('index', axis=1)


def pm_na_count(a):
    site_list = a['측정소명'].unique().tolist()
    for i in range(len(site_list)):
        print(site_list[i], "pm%i" % i, 'Row 수', len(globals()['pm{}'.format(i)]), "", 'NA Count',
              globals()["pm%i" % i].isnull().sum(), "", "미세먼지 NA",
              globals()["pm%i" % i]['pm25'].isnull().sum(), "", sep='\n')


final = korea_pm[korea_pm['측정소명'].isin(['광교동', '고색동', '원시동', '시화산단', '고잔동', '대야동',
                                        '산본동', '고천동'])]

pm_site(final)

# 시간 duplicate 제거
columnk = final['측정소명'].unique().tolist()
for i in range(len(columnk)):
    globals()["pm{}".format(i)] = globals()["pm{}".format(i)].drop_duplicates().reset_index()
    globals()["pm{}".format(i)].drop('index', axis=1, inplace=True)

# na 값처리
columnk = final['측정소명'].unique().tolist()
for i in range(len(columnk)):
    globals()["pm{}".format(i)] = globals()["pm{}".format(i)][
        globals()["pm{}".format(i)]['date'] >= '2017-12-01'].reset_index()
    globals()["pm{}".format(i)]['date'] = pd.to_datetime(globals()["pm%i" % i]['date'])
    globals()["pm{}".format(i)].drop('index', axis=1, inplace=True)

columnk

# weather
# load pm 2019 01~07 data
os.chdir('D:/data/weather')
weather_result = pd.DataFrame()
for file in glob.glob("*.csv"):
    data = pd.read_csv(file, encoding='CP949')
    weather_result = pd.concat([weather_result, data])

# column select
weather_col = ['시군명', '지점명', '관측일자', '관측시간', '기온', '풍속', '풍향', '강수감지', '습도', '시간누적강우량']
weather_result = weather_result[weather_col]
weather_result.columns = ['sigun', 'site', 'ymd', 'time', 'weather', 'wind_speed', 'wind_direction',
                          'rain_yn', 'humid', 'rain_cumsum']

# time formatting
weather_result['time'] = weather_result['time'].astype(str) + ":" + '00'
weather_result['time'].unique()
text = ['0:00', '1:00', '2:00', '3:00', '4:00', '5:00', '6:00', '7:00', '8:00', '9:00']
change_text = ['00:00', '01:00', '02:00', '03:00', '04:00', '05:00', '06:00', '07:00', '08:00', '09:00']

for i, j in zip(text, change_text):
    weather_result['time'] = weather_result['time'].replace(i, j)

# date formatting
weather_result['ymd'] = pd.to_datetime(weather_result['ymd'], format='%Y%m%d')
weather_result = weather_result[weather_result['ymd'] >= '2017-11-01']  # 미세먼지 안산 시흥 모두 11월까지 측정 X
weather_result['date'] = weather_result['ymd'].astype(str) + " " + weather_result['time'].astype(str)
weather_result = weather_result.sort_values('date').reset_index()
weather_result = weather_result.drop('index', axis=1)
weather_result['date'] = pd.to_datetime(weather_result['date'], format='%Y-%m-%d %H:%M')

# 기상 활용지역 선정 오전동은 '오전동 '으로 되어있음
weather = weather_result[weather_result['site'].isin(['경기', '수원', '안산', '시흥', '고잔',
                                                      '신현동','오전동 ','군포'])]

# 미세먼지와 지역명 통일
weather['site'] = weather['site'].replace('고잔', '고잔동')
weather['site'] = weather['site'].replace('안산', '원시동')
weather['site'] = weather['site'].replace('경기', '광교동')
weather['site'] = weather['site'].replace('신현동', '대야동')
weather['site'] = weather['site'].replace('시흥', '시화산단')
weather['site'] = weather['site'].replace('수원', '고색동')
weather['site'] = weather['site'].replace('오전동 ', '고천동')
weather['site'] = weather['site'].replace('군포', '산본동')

# 미세먼지와 날짜 통일
weather = weather[('2019-07-31' >= weather['ymd']) & (weather['ymd'] >= '2017-12-01')].reset_index()
weather.drop(['sigun', 'index'], axis=1, inplace=True)
weather.head()

# 기상 측정 오류 값 결측치 처리

columnw = weather.columns.tolist()
columnw = ['weather', 'wind_speed', 'wind_direction', 'rain_yn', 'humid', 'rain_cumsum']
for i in columnw:
    weather.loc[weather[i] <= -990, f'{i}'] = np.float("nan")

weather['site'].unique().tolist()

# 미세먼지, 날씨 row data merge
pm = pd.concat([pm0, pm1, pm2, pm3, pm4, pm5, pm6, pm7], axis=0)
weather.drop(['ymd', 'time'], axis=1, inplace=True)
weather['date'] = weather['date'].astype(str)
pm['date'] = pm['date'].astype(str)

pm_weather = pd.merge(pm, weather, on=['date', 'site'], how='left')
pm_weather = pm_weather.drop_duplicates(subset=['date', 'site'], keep='last')
pm_weather['date'] = pd.to_datetime(pm_weather['date'], format='%Y-%m-%d %H:%M')
pm_weather.head()

pm_weather['pm10'] = round(pm_weather['pm10'])
pm_weather['pm25'] = round(pm_weather['pm25'])
# row data merge
pm_weather.to_csv('D:/data/row_pm_weather2.csv', encoding='UTF-8')

# 8개지역 커스터마이징
columnk = final['측정소명'].unique().tolist()
for i in range(len(columnk)):
    globals()["pm{}".format(i)]['date'] = globals()["pm%i" % i]['date'].astype(str)

pm_na_count(final)
gojan = pd.merge(pm5, weather, on=['date', 'site'], how='left')
wonsi = pd.merge(pm6, weather, on=['date', 'site'], how='left')
gwanggyo = pd.merge(pm2, weather, on=['date', 'site'], how='left')
daeya = pd.merge(pm3, weather, on=['date', 'site'], how='left')
sihwa = pd.merge(pm4, weather, on=['date', 'site'], how='left')
gosaek = pd.merge(pm1, weather, on=['date', 'site'], how='left')
gocheon = pd.merge(pm7, weather, on=['date', 'site'], how='left')
sanbon = pd.merge(pm0, weather, on=['date', 'site'], how='left')

column_site = ['gojan', 'wonsi', 'gwanggyo', 'daeya', 'sihwa', 'gosaek', 'gocheon', 'sanbon']
for site in column_site:
    globals()[f"{site}"] = globals()[f"{site}"].drop_duplicates(subset=['date', 'site'], keep='last')
    globals()[f"{site}"]['date'] = pd.to_datetime(globals()[f"{site}"]['date'].sort_values().reset_index(drop=True))


def na_preprocessing(site):
    column_variable = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25', 'weather', 'wind_speed',
                       'wind_direction', 'rain_yn']
    for k in column_variable:
        fill = []
        for j in range(len(site)):
            if site[k].iloc[j] != site[k].iloc[j]:
                c = site['date'].iloc[j]
                n, f = c + datetime.timedelta(hours=1), c - datetime.timedelta(hours=1)
                front_count = site[site['date'] == f].reset_index()
                next_count = site[site['date'] == n].reset_index()
                if j == 0:
                    fill.append(next_count[k][0])

                elif j + 1 > len(site):
                    fill.append(front_count[k][0])

                elif next_count.empty:
                    n += datetime.timedelta(hours=1)
                    next_count = site[site['date'] == n].reset_index()
                    if next_count.empty:
                        # continue
                        fill.append(np.nan)
                    elif next_count[k][0] != next_count[k][0]:
                        fill.append(np.nan)
                    else:
                        fill.append(sum(next_count[k] + front_count[k]) / 2)

                elif next_count[k][0] != next_count[k][0]:
                    n += datetime.timedelta(hours=1)
                    next_count = site[site['date'] == n].reset_index()
                    if next_count.empty:
                        n += datetime.timedelta(hours=1)
                        next_count = site[site['date'] == n].reset_index()
                        if next_count.empty:
                            fill.append(np.nan)
                        elif next_count[k][0] != next_count[k][0]:
                            fill.append(np.nan)
                        else:
                            fill.append(sum(next_count[k] + front_count[k]) / 2)
                    elif next_count[k][0] != next_count[k][0]:
                        n += datetime.timedelta(hours=1)
                        next_count = site[site['date'] == n].reset_index()
                        if next_count.empty:
                            n += datetime.timedelta(hours=1)
                            next_count = site[site['date'] == n].reset_index()
                            if next_count.empty:
                                fill.append(np.nan)
                            elif next_count[k][0] != next_count[k][0]:
                                fill.append(np.nan)
                            else:
                                fill.append(sum(next_count[k] + front_count[k]) / 2)
                            # else : pm0 = pm0.drop(pm0.index[j])
                        elif next_count[k][0] != next_count[k][0]:
                            n += datetime.timedelta(hours=1)
                            next_count = site[site['date'] == n].reset_index()
                            if next_count.empty:
                                fill.append(np.nan)
                            elif next_count[k][0] != next_count[k][0]:
                                fill.append(np.nan)
                            else:
                                fill.append(sum(next_count[k] + front_count[k]) / 2)
                        else:
                            fill.append(sum(next_count[k] + front_count[k]) / 2)
                    else:
                        fill.append(sum(next_count[k] + front_count[k]) / 2)
                else:
                    fill.append(sum(next_count[k] + front_count[k]) / 2)
            else:
                fill.append(site[k].iloc[j])
        df = pd.DataFrame(fill)
        site[k] = df


for site in column_site:
    na_preprocessing(globals()[f'{site}'])
    na_preprocessing(globals()[f'{site}'])
    na_preprocessing(globals()[f'{site}'])

pm_weather.isnull().sum()

for site in column_site:
    globals()[f"{site}"]['pm10'] = round(globals()[f"{site}"]['pm10'])
    globals()[f"{site}"]['pm25'] = round(globals()[f"{site}"]['pm25'])

final_merge = pd.concat([gojan, gosaek, wonsi, sihwa, daeya, gwanggyo, gocheon, sanbon], axis=0)

final_merge.to_csv('D:/data/pm_weather2.csv', encoding='CP949')
final_merge.isnull().sum()
print("time :", (time.time() - start) / 60)  # 현재시각 - 시작시간

# 전체코드 실행시간 약 35분
