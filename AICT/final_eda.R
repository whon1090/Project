# 고잔동 시화산단 비교 
setwd('D:/data')
library(data.table) ; library(car)
library(dplyr) ; library(ggplot2)
library(tidyverse); 
library(stringr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(ggpmisc)
#df 고잔동 df1 시화산단
df = fread('D:/data/row_pm_weather.csv',encoding='UTF-8')

df = df[,c(2,3,9)]
df = na.omit(df)
df$md<- substr(df$date,6,10)
df$ymd<- str_sub(df$date,1,10)
df$ymd<- ymd(df$ymd)
df$year<- substr(df$date,1,4)

df$season<- ifelse(df$md >='03-01' & df$md<= '05-31','봄',
                    ifelse(df$md >='06-01' & df$md<= '08-31','여름',
                           ifelse(df$md >='09-01' & df$md<= '11-30','가을','겨울')))

head(df)
str(df)


#지역별 계절별 그래프
a<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30')%>% group_by(ymd,site,season) %>% summarise(pm2_5=mean(pm25))
a$ymd<- ymd(a$ymd)

spring<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='봄' )%>% group_by(date,site) %>% summarise(pm2_5=pm25)


spring$ymd_hm<- ymd_hms(spring$date)

ggplot(data = spring, aes(x = ymd_hm, y = pm2_5)) +  geom_point(aes(color=site)) + 
  theme(text = element_text(size=45),legend.text = element_text( size = 35)) +guides(colour = guide_legend(override.aes = list(size=6, stroke=1.5)))+
  labs(x='봄', y='PM2.5') 

spring_day<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='봄' )%>% group_by(ymd,site) %>% summarise(pm2_5=mean(pm25))

spring_day$ymd<- ymd(spring_day$ymd)
head(spring_day)

ggplot(data = spring_day, aes(x = ymd, y = pm2_5)) +  geom_point(aes(color=site),size=1.5) + geom_line(aes(color=site),size=1.5)+
  theme(text = element_text(size=45),legend.text = element_text( size = 35)) +guides(colour = guide_legend(override.aes = list(size=4, stroke=2)))+
  labs(x='계절', y='PM2.5') 


# 여름
summer<-  df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='여름' )%>% group_by(date,site) %>% summarise(pm2_5=pm25)
summer$ymd_hm<- ymd_hms(summer$date)

ggplot(data = summer, aes(x = ymd_hm, y = pm2_5)) +  geom_point(aes(color=site)) + theme(text = element_text(size=15)) +labs(x='계절', y='PM2.5') 


summer_day<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='여름' )%>% group_by(ymd,site) %>% summarise(pm2_5=mean(pm25))
summer_day$ymd<- ymd(summer_day$ymd)
head(summer_day)
ggplot(data = summer_day, aes(x = ymd, y = pm2_5)) +  geom_point(aes(color=site)) + geom_line(aes(color=site))+
  theme(text = element_text(size=45),legend.text = element_text( size = 35)) +guides(colour = guide_legend(override.aes = list(size=2, stroke=4)))+
   labs(x='여름', y='PM2.5') 

#가을 

fall<-  df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='가을' )%>% group_by(date,site) %>% summarise(pm2_5=pm25)
fall$ymd_hm<- ymd_hms(fall$date)

ggplot(data = fall, aes(x = ymd_hm, y = pm2_5)) +  geom_point(aes(color=site)) + theme(text = element_text(size=15)) +labs(x='계절', y='PM2.5') 


fall_day<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='가을' )%>% group_by(ymd,site) %>% summarise(pm2_5=mean(pm25))
fall_day$ymd<- ymd(fall_day$ymd)
head(fall_day)
ggplot(data = fall_day, aes(x = ymd, y = pm2_5)) +  geom_point(aes(color=site)) + geom_line(aes(color=site))+
  theme(text = element_text(size=45),legend.text = element_text( size = 35)) +guides(colour = guide_legend(override.aes = list(size=2, stroke=4)))+
  labs(x='가을', y='PM2.5') 

#겨울
winter<-  df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='겨울' )%>% group_by(date,site) %>% summarise(pm2_5=pm25)
winter$ymd_hm<- ymd_hms(winter$date)

ggplot(data = winter, aes(x = ymd_hm, y = pm2_5)) +  geom_point(aes(color=site)) + theme(text = element_text(size=15)) +labs(x='겨울', y='PM2.5') 


winter_day<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='겨울' )%>% group_by(ymd,site) %>% summarise(pm2_5=mean(pm25))
winter_day$ymd<- ymd(winter_day$ymd)
head(winter_day)
ggplot(data = winter_day, aes(x = ymd, y = pm2_5)) +  geom_point(aes(color=site)) + geom_line(aes(color=site))+
  theme(text = element_text(size=45),legend.text = element_text( size = 35)) +guides(colour = guide_legend(override.aes = list(size=2, stroke=4)))+
  labs(x='겨울', y='PM2.5') 


#광교동 그래프

gwanggyo<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='봄',site=='광교동' )%>% group_by(date) %>% summarise(pm2_5=pm25)
gwanggyo$ymd_hm<- ymd_hms(gwanggyo$date)

ggplot(data = gwanggyo, aes(x = ymd_hm, y = pm2_5)) +  geom_point(aes(color='huls')) +labs(x='광교', y='PM2.5') +theme_dark()+theme(text = element_text(size=45),legend.position = "none") 

# 광교동 7월
str(df)
df$month<- substr(df$md,1,2)
gwanggyo_summer<- df %>% filter(month =='07' , year=='2018' ,site=='광교동' )%>% group_by(date) %>% summarise(pm2_5=pm25)
gwanggyo_summer$ymd_hm<- ymd_hms(gwanggyo_summer$date)
head(gwanggyo_summer)
ggplot(data = gwanggyo_summer, aes(x = ymd_hm, y = pm2_5)) +  geom_point(aes(color='huls')) +labs(x='광교', y='PM2.5') +theme_dark()+theme(text = element_text(size=15),legend.position = "none") 

#광교동 가을 11월

gwanggyo_11<- df %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30',season=='가을',site=='광교동' )%>% group_by(date) %>% summarise(pm2_5=pm25)
gwanggyo_11$ymd_hm<- ymd_hms(gwanggyo_11$date)

ggplot(data = gwanggyo_11, aes(x = ymd_hm, y = pm2_5)) +  geom_point(aes(color='huls')) +labs(x='광교', y='PM2.5') +theme_dark()+theme(text = element_text(size=45),legend.position = "none") 

xlim(as.Date("2018-03-01"),as.Date("2018-05-01"))








