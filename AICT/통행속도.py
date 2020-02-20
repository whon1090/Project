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

os.chdir('D:/data/car')
speed = pd.read_csv('시흥시 속도자료_제공.csv',encoding='CP949')
speed.columns = ['date','speed']
speed = speed.groupby(['date'])['speed'].min().reset_index(drop=False)
speed['date'] = pd.to_datetime(speed['date'], format='%Y-%m-%d %H:%M')
speed.head(30)
speed.to_csv('D:/data/speed.csv',index=False)
