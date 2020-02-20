# 고잔동 시화산단 비교 
setwd('D:/data')
library(data.table) ; library(car)
library(dplyr) ; library(ggplot2)
library(tidyverse); 
library(stringr)
library(lubridate)
library(plotly)
library(RColorBrewer)

#df 고잔동 df1 시화산단
df = fread('as_merge_weather.csv')
df1 = fread('sh_merge_weather.csv')

df = df[,-c(1)]
df1 = df1[,-c(1,2)]

df =df[which(df$site=='고잔동'),]
df1 =df1[which(df1$site=='시화산단'),]

# df$md<- substr(df$date,6,10)
# df$ymd<- str_sub(df$date,1,10)
# 
# df$season<- ifelse(df$md >='03-01' & df$md<= '05-31','봄',
#                     ifelse(df$md >='06-01' & df$md<= '08-31','여름',
#                            ifelse(df$md >='09-01' & df$md<= '11-30','가을','겨울')))
# 



head(df)
head(df1)

final<-  rbind(df,df1)
str(final)

final$md<- substr(final$date,6,10)
final$ymd<- str_sub(final$date,1,10)
final$ymd<- ymd(final$ymd)


final$season<- ifelse(final$md >='03-01' & final$md<= '05-31','봄',
                    ifelse(final$md >='06-01' & final$md<= '08-31','여름',
                           ifelse(final$md >='09-01' & final$md<= '11-30','가을','겨울')))

head(final)
str(final)

a<- final %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30')%>% group_by(ymd,site,season) %>% summarise(pm2_5=mean(pm25))
colSums(is.na(a))
str(a)
a$ymd<- ymd(a$date)

ggplot(data = a, aes(x = ymd, y = pm2_5)) + geom_point(aes(color=site),size=2) + facet_wrap(~ season, scales = "free_x", switch = 'x') + theme(text = element_text(size=15)) +
  ggtitle('고잔동 시화산업단지 미세먼지 계절별 비교')+ labs(x='계절', y='PM2.5') + geom_line(aes(color=site))

# a$ymd<- ymd(a$ymd)
# a$md<- substr(a$ymd,6,10)
# a$m<- month(a$ymd)
str(a)



a<- final %>% filter(ymd >='2017-12-01' & ymd<= '2018-11-30')
head(a)
str(a)




ggplot(data = a, aes(x = ymd, y = pm25)) + geom_point(aes(color=site)) + facet_wrap(~ season, scales = "free_x", switch = 'x') + labs(x='연도', y='PM2.5') + theme(text = element_text(size=15)) +
  ggtitle('고잔동 시화산업단지 미세먼지 계절별 비교')+ labs(x='계절', y='PM2.5') 



final$hm<- str_sub(final$date,-8,-4)
head(final)
str(final)
write.csv(final,'test_c.csv')
final2 = final[,c(1,8,18)]
head(final2)
str(final2)
unique(final2$site)
colSums(is.na(final2))
final2= na.omit(final2)
b <- final2 %>%  group_by(site,hm) %>% summarise(PM25= mean(pm25))
head(b)
tail(b)
ggplot(data = b,aes(x= hm, y= PM25,fill=PM25)) + geom_bar(stat='identity') +labs(title = "고잔동 시화산업단지 시간별 미세먼지 농도 ",x='time',y='PM2.5') + 
  theme(axis.text.x = element_text(angle = 90))+ facet_wrap(~site)+theme(text = element_text(size=10))


