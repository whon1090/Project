# import module
import pandas as pd
import numpy as np
import os
from datetime import datetime
import datetime
import glob
import time
import warnings
warnings.filterwarnings(action='ignore')
start = time.time()

df = pd.read_csv('D:/data/car/월별 차량 등록 데이터.csv', encoding='CP949')
df.info()
# 화물, 특수화물만 사용

car_incheon = df[df['시도'] == '인천']
car_incheon.drop('시군구', axis=1, inplace=True)
car_incheon = car_incheon[['연도', '시도', '화물 계', '특수 계']]
car_list = ['화물 계', '특수 계']
for i in car_list:
    car_incheon[f'{i}'] = car_incheon[f'{i}'].str.replace(',', "")
    car_incheon[f'{i}'] = car_incheon[f'{i}'].astype(float)

car_incheon = car_incheon.groupby(['시도', '연도'])[['화물 계', '특수 계']].sum().reset_index(drop=False)
car_incheon.info()
car_incheon.head()

df = df[['연도', '시군구', '화물 계', '특수 계']]
car = df[df['시군구'].isin(['시흥', '안산 단원', '안산 상록', '수원 권선', '수원 영통', '동', '군포', '의왕', '광명', '부천'])]
car.to_csv('D:/data/car/car_total.csv', encoding='CP949')

