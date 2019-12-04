# import module
import pandas as pd
import numpy as np
import os
from datetime import datetime
import datetime
from pandas import Series,DataFrame
from math import *
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
%matplotlib inline

font_name = mpl.font_manager.FontProperties(fname='C:/Windows/Fonts/malgunbd.ttf').get_name()
mpl.rc('font', family=font_name)
mpl.rcParams['axes.unicode_minus'] = False

## sklearn
from sklearn import datasets, linear_model
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import *
from sklearn.preprocessing import StandardScaler
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import *
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.feature_selection import SelectFromModel
from sklearn.ensemble import VotingClassifier

# xgboost
from xgboost import XGBClassifier
from xgboost import plot_importance
import xgboost as xgb

# randomforest
from sklearn.ensemble import RandomForestClassifier

# MLP
from sklearn.neural_network import MLPClassifier

# 모델링 샘플 위한 시화산단만 추출
start = time.time()


os.chdir('D:/data/Incheon/pm')
pm_result = pd.DataFrame()
for file in glob.glob('*.xlsx'):
    pm_data = pd.read_excel(file, encoding='CP949')
    pm_result = pd.concat([pm_result, pm_data])

pm_final = pm_result.copy()
pm_final = pm_final[pm_final['측정소명'] == '시화산단']
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

pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
for replace in pm_list:
    pm_final[f'{replace}'] = pm_final[f'{replace}'].apply(lambda x: np.float("nan") if x == '-' else x)
pm_final.isnull().sum()





korea_pm = pd.read_csv('D:/data/korea.csv', encoding='UTF-8')
korea_pm = korea_pm.drop('Unnamed: 0', axis=1)
korea_pm = korea_pm[korea_pm['측정소명'] == '시화산단'].iloc[:, [1, 4, 5, 6, 7, 8, 9, 10]]
korea_pm.columns = ['site', 'date', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
korea_pm['date'] = korea_pm['date'].str.replace('24:00', '00:00')
korea_pm['date'] = pd.to_datetime(korea_pm['date'], format='%Y-%m-%d %H:%M')
korea_pm = korea_pm[('2019-07-01'> korea_pm['date']) & (korea_pm['date'] >= '2019-05-01')]

pm_sihwa = pd.concat([pm_final, korea_pm])
sihwa_columns = ['site', 'date', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
pm_sihwa = pm_sihwa[sihwa_columns]







# 경기도 기상

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
weather_result['date'] = weather_result['ymd'].astype(str) + " " + weather_result['time'].astype(str)
weather_result = weather_result.sort_values('date').reset_index()
weather_result = weather_result.drop('index', axis=1)
weather_result['date'] = pd.to_datetime(weather_result['date'], format='%Y-%m-%d %H:%M')

weather = weather_result[weather_result['site'] == '시흥']

# 미세먼지와 지역명 통일
weather['site'] = weather['site'].replace('시흥', '시화산단')

# 미세먼지와 날짜 통일
weather = weather[('2019-07-31' >= weather['ymd'])].reset_index()
weather.drop(['sigun', 'index', 'rain_cumsum', 'ymd', 'time'], axis=1, inplace=True)

# 기상 측정 오류 값 결측치 처리
columnw = ['weather', 'wind_speed', 'wind_direction', 'rain_yn', 'humid']
for i in columnw:
    weather.loc[weather[i] <= -990, f'{i}'] = np.float("nan")

# merge
pm_sihwa['date'] = pm_sihwa['date'].astype(str)
weather['date'] = weather['date'].astype(str)

pm_weather = pd.merge(pm_sihwa, weather, on=['date', 'site'], how='left')
pm_weather.head()
pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']

for replace in pm_list:
    pm_weather[f'{replace}'] = pm_weather[f'{replace}'].apply(lambda x: np.float("nan") if x == '-' else x)

pm_weather.isnull().sum()
pm_weather.to_csv('D:/data/row_sihwa.csv', encoding='CP949')

# data preprocessing run time check
print("time :", (time.time() - start) / 60)


# 시화산단 코드 속도 12분

# modeling

start = time.time()

# train data road
df = pd.read_csv('D:/data/row_sihwa.csv',encoding='CP949')


# preprocessing
df.drop('Unnamed: 0',axis=1,inplace=True)
df = df[['site', 'date', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25', 'weather',
       'wind_speed', 'wind_direction', 'rain_yn', 'humid']]
df = df.iloc[:, [0,1,2,3,4,5,6,7,8,9,10,11,12]]
df.loc[df['weather'] <= -30, 'weather' ] = np.float("nan")
df.isnull().sum()

# test data
os.chdir('D:/data/test')
df1 = pd.read_csv('대기질측정정보(2019년 8월).txt', encoding='CP949')
df2 = pd.read_csv('sihwa_201908_10.csv',encoding='CP949')

df1.drop('시군코드', axis=1, inplace=True)
df2.drop('Unnamed: 0',axis=1, inplace=True)
df1= df1[df1['측정소명'] == '시화산단'].iloc[:, [1, 4, 5, 6, 7, 8, 9, 10]]
df1.columns = ['site', 'date', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
df2.columns = ['date', 'site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
df1['date'] = df1['date'].str.replace('24:00', '00:00')
df2['date'] = df2['date'].str.replace('24:00', '00:00')

pm = pd.concat([df1,df2])
pm = pm[['date', 'site', 'so2', 'co', 'o3', 'no2', 'pm10', 'pm25']]
pm = pm.drop_duplicates(subset=['date','site']).reset_index(drop=True)
pm.head()

pm_list = ['so2', 'co', 'o3', 'no2', 'pm10', 'pm25']
for replace in pm_list:
    pm[f'{replace}'] = pm[f'{replace}'].apply(lambda x: np.float("nan") if x == '-' else x)
pm.isnull().sum()


os.chdir('D:/data/test/weather')
weather_result = pd.DataFrame()
for file in glob.glob('*.csv'):
    weather_data = pd.read_csv(file, encoding='CP949')
    weather_result = pd.concat([weather_result, weather_data])

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
weather = weather_result[weather_result['site']=='시흥']
weather['site'] = weather['site'].replace('시흥', '시화산단')
weather.drop(['sigun', 'rain_cumsum'], axis=1, inplace=True)
columnw = weather.columns.tolist()
columnw = ['weather', 'wind_speed', 'wind_direction', 'rain_yn', 'humid']
for i in columnw:
    weather.loc[weather[i] <= -990, f'{i}'] = np.float("nan")

weather['site'].unique().tolist()
weather.drop(['ymd', 'time'], axis=1, inplace=True)
weather['date'] = weather['date'].astype(str)
weather['date'] = weather['date'].str[:16]
pm['date'] = pm['date'].astype(str)

# test data merge
test = pd.merge(pm, weather, on=['date', 'site'], how='left')
test = test.drop_duplicates(subset=['date', 'site'], keep='last')

test.to_csv('D:/data/test_dataset.csv', encoding='CP949')

test = pd.read_csv('D:/data/test_dataset.csv', encoding='CP949')
test.drop("Unnamed: 0",axis=1,inplace=True)
## modeling 파생변수 ymd, md, month, season ,weekday, pm categori 추가
def date_split(df) :
    df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%d %H:%M')
    df['ymd'] = [d.date() for d in df['date']]
    df['month'] = [d.month for d in df['date']]
    df['time'] = [d.time() for d in df['date']]
    df['time'] = df['time'].apply(lambda x: x.strftime('%H'))
    df['md'] = df['ymd'].apply(lambda x: x.strftime('%m-%d'))
    df['weekday'] =df['date'].apply(lambda x: x.strftime('%A'))
def season(x):
    if '05-31'>= x>='03-01':
        return('봄')
    elif '11-30' >= x >= '09-01':
        return('가을')
    elif '08-31' >=x >= '06-01':
        return('여름')
    else: return('겨울')

def categorical_PM(x):
    if 8>= x>=0:
        return('최고')
    elif 15 >= x >= 9 :
        return('좋음')
    elif 20 >=x >= 16:
        return('양호')
    elif 25 >=x >= 21:
        return('보통')
    elif 37 >=x >= 26:
        return('나쁨')
    elif 50 >=x >= 38:
        return('상당히 나쁨')
    elif 75 >=x >= 51:
        return('매우 나쁨')
    else: return('최악') 

date_split(df)
date_split(test)
df = df.dropna()
test = test.dropna()
df['season'] = df['md'].apply(season)
test['season'] = test['md'].apply(season)

df = df[df['site']=='시화산단']
df.drop(['date','site','md','time','ymd'],axis=1,inplace=True)
test.drop(['date','site','md','time','ymd'],axis=1,inplace=True)
df = df.reset_index(drop=True)
test = test.reset_index(drop=True)



random.seed(100)
## x, y setting
X = df.drop(["pm25"], axis=1)
Y = df["pm25"]

X2 = test.drop(["pm25"], axis=1)
Y2 = test["pm25"] 
# one-hot encoding
# object 변수만 해당
for i in ["season", "weekday"]:
    X[i] = pd.get_dummies(X[i])
    X2[i] = pd.get_dummies(X2[i])
# trian / test split 0.8/ 0.2
x_train, x_test, y_train, y_test = X, X2, Y, Y2

# Cross-Validation
# scores =cross_val_score(model, X, y, scoring=None, cv=None)
# predictions = cross_val_predict(model, df, y, cv=6)
# plt.scatter(y, predictions)

# Xgboost
model = XGBClassifier()

# model.fit(x_train, y_train, sample_weight = class_weights)
model.fit(x_train, y_train)


## Prediction
y_pred = model.predict(x_test)
y_pred_prob = model.predict_proba(x_test)
y_pred_prob_df = pd.DataFrame(y_pred_prob, columns=model.classes_)
# each data predict

xgb_predict = pd.DataFrame({'Actual': y_test, 'Predicted': y_pred})
## 확인
print("RMSE : ",(mean_squared_error(y_test, y_pred))**0.5)

#save file
xgb_predict.to_csv('D:/data/xgb_predict.csv',encoding='CP949')


# plotting
fit = np.polyfit(xgb_predict['Predicted'], xgb_predict['Actual'],1)
fit_fn = np.poly1d(fit)
plt.figure(figsize=(23,12))
plt.plot(xgb_predict['Predicted'], xgb_predict['Actual'],'o')
plt.plot(xgb_predict['Predicted'], fit_fn(xgb_predict['Predicted']), 'r')
plt.tick_params(labelsize=30)
plt.xlabel('예측값',size=30)
plt.ylabel('실제값',size=36)

#RF

model = RandomForestClassifier()
model.fit(x_train, y_train)
y_pred = model.predict(x_test)
y_pred_prob = model.predict_proba(x_test)
y_pred_prob_df = pd.DataFrame(y_pred_prob, columns=model.classes_)

# each data predict
RF_predict = pd.DataFrame({'Actual': y_test, 'Predicted': y_pred})

## 확인
print("RMSE : ", (mean_squared_error(y_test, y_pred))**0.5)

#save file
RF_predict.to_csv('D:/data/rf_predict.csv',encoding='CP949')

#plotting
fit = np.polyfit(RF_predict['Predicted'], RF_predict['Actual'],1)
fit_fn = np.poly1d(fit)
plt.figure(figsize=(23,12))
plt.plot(RF_predict['Predicted'], RF_predict['Actual'],'o')
plt.plot(RF_predict['Predicted'], fit_fn(RF_predict['Predicted']), 'r')
plt.tick_params(labelsize=30)
plt.xlabel('예측값',size=30)
plt.ylabel('실제값',size=36)

# Linear Regression

regressor = LinearRegression()  
regressor.fit(x_train, y_train)

print(regressor.intercept_)
print(regressor.coef_)
y_pred=regressor.predict(x_test)  
print("RMSE : ", (mean_squared_error(y_test, y_pred))**0.5)

LR_predict = pd.DataFrame({'Actual': y_test, 'Predicted': y_pred})

fit = np.polyfit(LR_predict['Predicted'], LR_predict['Actual'],1)
fit_fn = np.poly1d(fit)
plt.figure(figsize=(23,12))
plt.plot(LR_predict['Predicted'], LR_predict['Actual'],'o')
plt.plot(LR_predict['Predicted'], fit_fn(LR_predict['Predicted']), 'r')
plt.tick_params(labelsize=30)
plt.xlabel('예측값',size=30)
plt.ylabel('실제값',size=36)

#save file
LR_predict.to_csv('D:/data/LR.csv',encoding='CP949')



# modeling run time check
print("time :", (time.time() - start) / 60)


