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

# 미세먼지 부천 중2동, 기상 부천, 광명시 철산동, 기상 광명
start = time.time()

os.chdir('D:/data/car/상시교통량')
car_result = pd.DataFrame()
for file in glob.glob('*.csv'):
    car_data = pd.read_csv(file, encoding='CP949')
    car_result = pd.concat([car_result, car_data])

car_amount = car_result.copy()
car_columns = car_amount.iloc[0].tolist()
car_amount.columns = car_columns
car_amount.drop(0, inplace=True)
car_amount.drop(['방향', '차로', '전일합계(대)'], axis=1, inplace=True)
car_amount.columns.tolist()
car_list = ['00시~01시', '01시~02시', '02시~03시', '03시~04시',
            '04시~05시', '05시~06시', '06시~07시', '07시~08시', '08시~09시', '09시~10시',
            '10시~11시', '11시~12시', '12시~13시', '13시~14시', '14시~15시', '15시~16시',
            '16시~17시', '17시~18시', '18시~19시', '19시~20시', '20시~21시', '21시~22시',
            '22시~23시', '23시~24시']

for i in car_list:
    car_amount[f'{i}'] = car_amount[f'{i}'].replace('-', np.float("nan"))
    car_amount[f'{i}'] = car_amount[f'{i}'].str.replace(',', "")
    car_amount[f'{i}'] = car_amount[f'{i}'].astype(float)

car_group = car_amount.groupby(['노선 명', '구간 명', '날짜']).sum().reset_index(drop=False)
car_melt = pd.melt(car_group, id_vars=['노선 명', '구간 명', '날짜'], var_name='time')
car_melt.columns = ['road', 'section', 'ymd', 'time', 'car_amount']
car_melt['time_select'] = car_melt['time'].str[:2]
car_melt['time'] = car_melt['time_select'] + ":00"
car_melt['date'] = car_melt['ymd'] + " " + car_melt['time']
car_melt.drop(['time_select'], axis=1, inplace=True)
car_melt.info()

car_melt['section'].unique().tolist()

print("time :", (time.time() - start) / 60)
