##weather set 2019
setwd("C:\\rproject\\data")
set<-fread("전처리 정리.csv")
head(set)
colSums(is.na(set))

set2<-set[-which(set$지점명==""),]
head(set2)

#경남
setwd("C:\\rproject\\최종프로젝트\\2019")
a<- fread("경상남도.csv")

head(a)
##경남

set2<- set2[,c(1,2)]
set_1<- set2$지점

a[which(a$지점 %in% set_1),]

b<-  inner_join(a,set2, by='지점')
head(b)


colSums(is.na(b))
b<-na.omit(b)

b1<- b[!duplicated(b[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)

b1<-b1[which(b1$지점명 !="부산"),]
unique(b1$지점명)

write.csv(b1,file="경남_set.csv")

##전남
a1<- fread("전라남도.csv")

set2<- set2[,c(1,2)]
# set_1<- set2$지점
# 
# a1[which(a1$지점 %in% set_1),]

b1<-  inner_join(a1,set2, by='지점')
head(b1)

##na값 24, 18 제거
colSums(is.na(b1))
b1<-na.omit(b1)

b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
write.csv(b1,file="전남_set.csv")


##강원도
a2<- fread("강원도.csv")

set2<- set2[,c(1,2)]

set_1<- set2$지점

b1<-  inner_join(a2,set2, by='지점')

head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1$지점명<-ifelse(b1$지점명=='고성군','강원도고성군',b1$지점명)
head(b1)
write.csv(b1,file="강원도_set.csv")

##경기도
a2<- fread("경기도.csv")

set2<- set2[,c(1,2)]
set_1<- set2$지점

head(a2)
head(set2)
b1<-  inner_join(a2,set2, by='지점')

head(b1)
unique(b1$지점명)
##na값 28,21 제거
colSums(is.na(b1))
b1<-na.omit(b1)

b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
write.csv(b1,file="경기도_set.csv")

##경북
a2<- fread("경상북도.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 25,21 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
write.csv(b1,file="경북_set.csv")

##광주
a2<- fread("광주.csv")
b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 4 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
write.csv(b1,file="광주_set.csv")

##대구
a2<- fread("대구.csv")
wea_rep<-rep("대구",dim(a2)[1])
a2$도<- wea_rep
set2<- set2[,c(1,2)]
set_1<- set2$지점

a2[which(a2$지점 %in% set_1),]
head(a1)
b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 4 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)

write.csv(b1,file="대구_set.csv")

##대전
a2<- fread("대전.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)

write.csv(b1,file="대전_set.csv")

##부산
a2<- fread("부산.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1<-b1[which(b1$지점명!="의창구"),]
write.csv(b1,file="부산_set.csv")

##서울
a2<- fread("서울.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 4 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]

unique(b1$지점명)

write.csv(b1,file="서울_set.csv")

##세종
a2<- fread("세종.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1$지점명<- ifelse(b1$지점명=="세종시","세종",b1$지점명)

write.csv(b1,file="세종_set.csv")

##울산
a2<- fread("울산.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1<-b1[which(b1$지점명!="울주군"),]
write.csv(b1,file="울산_set.csv")

##인천
a2<- fread("인천.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1<-b1[which(b1$지점명!="서울"),]
head(b1)
b1$지점명<-ifelse(b1$지점명=="연수구","인천",b1$지점명)
unique(b1$지점명)
write.csv(b1,file="인천_set.csv")

##전남
a2<- fread("전라남도.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1<-b1[which(b1$지점명!="광주"& b1$지점명!="신안읍"& b1$지점명!="지도군"),]
unique(b1$지점명)
write.csv(b1,file="전남_set.csv")

##전북
a2<- fread("전라북도.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1<-b1[which(b1$지점명!="광주"& b1$지점명!="신안읍"),]
b1$지점명<- ifelse(b1$지점명=="접읍시","정읍시",b1$지점명)
unique(b1$지점명)
write.csv(b1,file="전북_set.csv")

##충남
a2<- fread("충청남도.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)
b1<-b1[which(b1$지점명!='대전'),]
head(b1)
write.csv(b1,file="충남_set.csv")

##충북
a2<- fread("충청북도.csv")

b1<-  inner_join(a2,set2, by='지점')
head(b1)

##na값 제거
colSums(is.na(b1))
b1<-na.omit(b1)
b1<- b1[!duplicated(b1[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)


write.csv(b1,file="충북_set.csv")

##rbind
weather<-"C:\\rproject\\최종프로젝트\\tree"
weather_csv<-dir(weather)
filepath_csv<-paste0(weather,"/",weather_csv)
filepath_csv
cr<-data.frame()

for(file in filepath_csv){
  
  data1<-read.csv(file)
  cr<-rbind(cr,data1)
}

str(cr)

write.csv(cr,"tree.csv")
#### 나무 전처리 
#폴더에 있는 파일 합치기
file_path<-paste0(set,"/",weather)
library(data.table)
data_result<-data.frame()
for(set in file_path){
  data<-fread(set)
  data_result<-rbind(data_result,data)
}
write.csv(data_result,file="기온.csv")
unique(data_result$도)
#colSums(is.na(data_result))
head(data_result)
weather_result<-data_result
head(data_result)
str(data_result)

#문자형태로 변환
weather_result$일시<-as.character(weather_result$일시)
date<-strsplit(weather_result$일시,"-")
head(date)

#년도 선택
year<-unlist(lapply(date,function(t){
  return(t[1])}))

#월 선택
month<-unlist(lapply(date,function(t){
  return(t[2])}))

#데이터 합치기
weather_result<-data.frame(weather_result,year,month)
head(weather_result2)
weather_result2<-weather_result[,c(6,7,8,9,3,4,5)]
## 알을 낳는 시기 선택  ##

weather_egg<- weather_result2[weather_result2$month=='01'|weather_result2$month=='02',]
head(weather_egg)
## 유충이 되는 시기 선택 ##
weather_bug<- weather_result2[weather_result2$month =='04'|weather_result2$month=='05'|weather_result2$month=='06',]
head(weather_bug)
head(weather_egg)

#컬럼명 변경
colnames(weather_egg)<-c("도","지점명","year","month","일시","평균기온","최저기온")
colnames(weather_bug)<-c("도","지점명","year","month","일시","평균기온","최저기온")


#최저기온이 10도 이하면 0, 아니면 1로 sum
min_egg<- weather_egg %>% 
  mutate( count = ifelse(최저기온>-10,1,0)) %>%
  group_by(지점명,year) %>% 
  summarise(min_egg=sum(count))

min_egg<-as.data.frame(min_egg)
head(mean_egg)


#평균기온이 10도 이하면 0, 아니면 1로 sum
mean_egg<- weather_egg %>% 
  mutate( count = ifelse(평균기온>-10,1,0)) %>%
  group_by(지점명,year) %>% 
  summarise(mean_egg=sum(count))

mean_egg<-as.data.frame(mean_egg)
head(mean_egg)


###########################################3

#평균기온이 15초과 34이하 1 , 아니면 0
mean_bug<- weather_bug %>% 
  mutate( count = ifelse(평균기온>15 & 평균기온<=34,1,0)) %>% 
  group_by(지점명,year)  %>% 
  summarise(mean_bug=sum(count))

mean_bug<-as.data.frame(mean_bug)
head(mean_bug)

weather_result<-cbind(mean_egg,min_egg,mean_bug)
weather<-weather_result[,c(1,2,3,6,9)]
head(weather)

setwd("C:\\Users\\Admin\\Desktop\\날씨_정제중")
write.csv(weather,file="weather.csv")

##bd

a<-read.csv("C:/Users/Admin/Desktop/EDA/final_2.csv")

library(dplyr)
str(a1)
a<-select(a,"sigun","s_lmp","lmp_cumsum","hw_cnt","ch_cnt","YEAR")
aa<-filter(a,YEAR=="2018")
str(aa)

da<-read.csv("E:/Rproject/kim/건물/새 폴더/bd_19.csv")
str(da)
colnames(da)<-c("X","V1","sigun","YEAR","BD_CNT")



str(da4_merge)
da4_merge<-merge(da,aa,by="sigun")

we<-read.csv("C:/Users/Admin/Downloads/weather_2019_3.csv")
str(we)
colnames(we)<-c("x","sigun","year","mean_egg","min_egg","mean_bug")
fi<-merge(da4_merge,we,by="sigun")
write.csv(fi,"C:/Users/Admin/Downloads/fin_2019.csv")

colSums(is.na(fi))



##
setwd("C:\\rproject\\최종프로젝트\\2019")
we<-fread("weather_2019.csv")
head(we)
we<-we[,-1]

###
setwd("C:\\rproject\\최종프로젝트\\data\\count")

news<-fread("news_2019.csv")
head(news)
colnames(news)<- c('bug','YEAR','sigun')
colnames(we)<- c('sigun','YEAR','mean_egg','min_egg','mean_bug')

nw<- full_join(we,news,by=c('sigun','YEAR'))
colSums(is.na(nw))

S## NA값은 피해가 발생하지 않은 지역이므로 NA 처리
nw$bug<- ifelse(is.na(nw$bug)==TRUE,0,1)

str(nw)  

write.csv(nw,file="nw.csv")


##여기다 봉섭이 merge한 코드 넣으면 됨
setwd("C:\\rproject\\최종프로젝트\\final")
fin<-fread("fin_2019.csv")
head(fin)
dim(fin)[1]
year<- rep(2019, dim(fin)[1])

fin$YEAR<- year
head(fin)
colSums(is.na(fin))
b1<-  left_join(nw,fin, by=c('sigun','YEAR'))
head(b1)

write.csv(b1,file="final_2019.csv")

colnames(b1)<-c('sigun','YEAR','mean_egg','min_egg','mean_bug','y_factor','bd_cnt','s_lmp',
                  'lmp_cumsum','hw_cnt','ch_cnt')

