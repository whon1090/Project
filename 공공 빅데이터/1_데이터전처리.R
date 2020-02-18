library(data.table);
library(dplyr) ; library(ggplot2)
library(readxl) ; library(stringr)
library(mice)
library(zoo)

##weather set 2015~2018
setwd("C:\\project\\참고자료")
##데이터 코드 자료 (해당 자료에 있는 코드와 지점명을 토대로 날씨 지점명 생성)
set<-fread("전처리 정리.csv")
##사용안하는 데이터 삭제
colSums(is.na(set))


##지점명 중 빈칸은 유효하지 않은 지역이므로 삭제하고 병합 
set2<-set[-which(set$지점명==""),]


##시단위 군단위 사용 -> 전처리 정리 
#경남
setwd("C:\\rproject\\최종프로젝트\\2019")
a<- fread("경상남도.csv")

head(a)
##경남

##변수 선택
set2<- set2[,c(1,2)]

## 지점의 vector화
set_1<- set2$지점

##지점을 기준으로 데이터 병합
b<-  inner_join(a,set2, by='지점')
head(b)


colSums(is.na(b))
b<-na.omit(b)

##지점명, 일시가 동일한 데이터 set이 존재하므로 삭제

b1<- b[!duplicated(b[,c('지점명','일시')],fromLast=TRUE),]
unique(b1$지점명)

##지점명 중 일치하지 않는 지역 제거

b1<-b1[which(b1$지점명 !="부산"),]
unique(b1$지점명)

write.csv(b1,file="경남_set.csv")

##전남
a1<- fread("전라남도.csv")

set2<- set2[,c(1,2)]

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

##지역명 통일
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


## 활엽수 

## 경기
setwd("C:\\project\\test_data\\산림")
d<- fread("경기_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)

#지역명 일치시키기

d1<-d
d1$SIG_KOR_NM<-ifelse(d1$SIG_KOR_NM=='남양주','남양주시',substr(d1$SIG_KOR_NM,1,3))  


#지역명 통일하기 위한 전처리
d_string<- c('수원시','성남시','용인시','고양시','안양시','안산시')
d1$SIG_KOR_NM<-ifelse(str_detect(d1$SIG_KOR_NM, d_string),substr(d1$SIG_KOR_NM,1,3),d1$SIG_KOR_NM)

## 지역명 select위한 데이터셋 생성
unique(d1$SIG_KOR_NM)
d_set<- c('수원시', '성남시', '의정부시', '안양시', '부천시', '광명시', '평택시', '동두천시', '안산시', '고양시', '과천시', 
             '구리시', '남양주시', '오산시', '시흥시', '군포시', '의왕시', '하남시', '용인시', 
             '파주시', '이천시', '안성시', '김포시', '화성시', '광주시', '양주시', 
             '포천시', '여주시', '연천군', '가평군', '양평군')

d1<- d1[which(d1$SIG_KOR_NM %in% d_set),]


unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
d1<- d1 %>%  group_by(SIG_KOR_NM) %>% summarise(hwal_count=sum(whal_count),chim_count=sum(chim_count))
d1
write.csv(d1,file="경기도_활엽수_set.csv")

##강원도 
setwd("C:\\project\\test_data\\산림")
d<- fread("강원도_침활엽수.csv")

head(d)
colSums(is.na(d))

d1<-d
unique(d1$SIG_KOR_NM)
d1<-d1[,-c(1,2)]
unique(d1$SIG_KOR_NM)
write.csv(d1,file="강원도_활엽수_set.csv")

## 경남
setwd("C:\\project\\test_data\\산림")
d<- fread("경상남도_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)

d1<-d
d1$SIG_KOR_NM<-d1$SIG_KOR_NM<- ifelse(str_detect(d1$SIG_KOR_NM, "창원시"),substr(d1$SIG_KOR_NM,5,7),d1$SIG_KOR_NM)  
unique(d1$SIG_KOR_NM)
d1<-d1[,-c(1,2)]
head(d1)
write.csv(d1,file="경남_활엽수_set.csv")

##경북
setwd("C:\\project\\test_data\\산림")
d<- fread("경상북도_침활엽수.csv")

head(d)
d$hwal_count<- d$hwal1_count+d$hwal2_count
d$chim_count<- d$chim1_count+d$chim2_count
head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)

d1<-d
d1$SIG_KOR_NM<-d1$SIG_KOR_NM<- ifelse(str_detect(d1$SIG_KOR_NM, "포항시"),paste(substr(d1$SIG_KOR_NM,1,2),str_sub(d1$SIG_KOR_NM,-2,-1),sep=""),
                                      ifelse(d1$SIG_KOR_NM=="북구"|d1$SIG_KOR_NM=="달성군"|d1$SIG_KOR_NM=="수성구"|d1$SIG_KOR_NM=="동구","대구",d1$SIG_KOR_NM))  
unique(d1$SIG_KOR_NM)
d1<-d1[,-c(1,2,4,5,6,7)]
write.csv(d1,file="경북_활엽수_set.csv")

## 광주
setwd("C:\\project\\test_data\\산림")
d<- fread("광주_침활엽수.csv")

str(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
tree_rep<-rep("광주",dim(d)[1])


d$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
d1<-d
d1<-d1[,-c(1,2)]
write.csv(d1,file="광주_활엽수_set.csv")

## 대구
setwd("C:\\project\\test_data\\산림")
d<- fread("대구_침활엽수.csv")

str(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
##대구광역시에 포함되지 않는 군 제거

d1<-d
d1<- d1[which(d1$SIG_KOR_NM=="중구"|d1$SIG_KOR_NM=="서구"|d1$SIG_KOR_NM=="동구"|
                d1$SIG_KOR_NM=="남구"|d1$SIG_KOR_NM=="달서구"|d1$SIG_KOR_NM=="수성구"|d1$SIG_KOR_NM=="달성군"),]

head(d1)
##이후 대구광역시로 통일 
tree_rep<-rep("대구",dim(d1)[1])

d1$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="대구_활엽수_set.csv")

## 대전

setwd("C:\\project\\test_data\\산림")
d<- fread("대전_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
##대전광역시에 포함되지 않는 군 제거

d1<-d
d1<- d1[which(d1$SIG_KOR_NM=="중구"|d1$SIG_KOR_NM=="서구"|d1$SIG_KOR_NM=="동구"|
                d1$SIG_KOR_NM=="유성구"|d1$SIG_KOR_NM=="대덕구"),]

head(d1)
##이후 대전광역시로 통일 
tree_rep<-rep("대전",dim(d1)[1])

d1$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="대전_활엽수_set.csv")

## 부산

setwd("C:\\project\\test_data\\산림")
d<- fread("부산_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
##부산광역시에 포함되지 않는 군 제거

d1<-d
d1<- d1[which(d1$SIG_KOR_NM!="김해시"& d1$SIG_KOR_NM!="울주군"& d1$SIG_KOR_NM!="양산시"),]

head(d1)

tree_rep<-rep("부산",dim(d1)[1])


d1$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="부산_활엽수_set.csv")


## 서울

setwd("C:\\project\\test_data\\산림")
d<- fread("서울_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d
d1<- d1[which(d1$SIG_KOR_NM!="과천시"& d1$SIG_KOR_NM!="고양시덕양구"&d1$SIG_KOR_NM!="구리시"
              &d1$SIG_KOR_NM!="안양시동안구"&d1$SIG_KOR_NM!="안양시만안구"&d1$SIG_KOR_NM!="하남시"&
                d1$SIG_KOR_NM!="부천시"&d1$SIG_KOR_NM!="남양주시"),]

head(d1)
tree_rep<-rep("서울",dim(d1)[1])

d1$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="서울_활엽수_set.csv")

## 세종

setwd("C:\\project\\test_data\\산림")
d<- fread("세종_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d
d1<- d1[which(d1$SIG_KOR_NM=="세종특별자치시"),]

head(d1)
tree_rep<-rep("세종",dim(d1)[1])

d1$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="세종_활엽수_set.csv")

## 울산

setwd("C:\\project\\test_data\\산림")
d<- fread("울산_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d
d1<- d1[which(d1$SIG_KOR_NM=="중구"|d1$SIG_KOR_NM=="남구"|d1$SIG_KOR_NM=="동구"|
                d1$SIG_KOR_NM=="북구"|d1$SIG_KOR_NM=="울주군"),]

head(d1)
tree_rep<-rep("울산",dim(d1)[1])


d1$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="울산_활엽수_set.csv")

## 인천

setwd("C:\\project\\test_data\\산림")
d<- fread("인천광역시_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d
d1<- d1[which(d1$SIG_KOR_NM!="시흥시"&d1$SIG_KOR_NM!="김포시"& d1$SIG_KOR_NM!="부천시"),]

head(d1)
tree_rep<-rep("인천",dim(d1)[1])

d1$SIG_KOR_NM<-tree_rep
unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="인천_활엽수_set.csv")

## 전남

setwd("C:\\project\\test_data\\산림")
d<- fread("전라남도_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d
d1<- d1[which(d1$SIG_KOR_NM!="동구" & d1$SIG_KOR_NM!="남구"&
                d1$SIG_KOR_NM!="북구"& d1$SIG_KOR_NM!="광산구"),]

head(d1)

unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="전남_활엽수_set.csv")

## 전북

setwd("C:\\project\\test_data\\산림")
d<- fread("전라북도_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d
d_string<-c("완산구","덕진구")
d1$SIG_KOR_NM<-ifelse(str_detect(d1$SIG_KOR_NM, d_string),str_sub(d1$SIG_KOR_NM,-3,-1),d1$SIG_KOR_NM)
                                       
unique(d1$SIG_KOR_NM)

d_set<- c("완산구", "덕진구", "군산시", 
             "익산시", "정읍시", "남원시", '김제시', "완주군", "진안군", 
             "무주군", "장수군", "임실군", "순창군", "고창군", "부안군")

d1<- d1[which(d1$SIG_KOR_NM %in% d_set),]



head(d1)

unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="전북_활엽수_set.csv")

## 충남

setwd("C:\\project\\test_data\\산림")
d<- fread("충청남도_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d

d1$SIG_KOR_NM<-ifelse(str_detect(d1$SIG_KOR_NM, "천안시"),str_sub(d1$SIG_KOR_NM,-3,-1),d1$SIG_KOR_NM)

unique(d1$SIG_KOR_NM)

d_set<- c("동남구", "서북구", "공주시", "보령시", "아산시", 
             "서산시", "논산시", "계룡시", "당진시", "금산군", 
             '부여군', "서천군", "청양군", '홍성군', "예산군", "태안군")

d1<- d1[which(d1$SIG_KOR_NM %in% d_set),]



head(d1)

unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="충남_활엽수_set.csv")

## 충북

setwd("C:\\project\\test_data\\산림")
d<- fread("충청북도_침활엽수.csv")

head(d)
colSums(is.na(d))

unique(d$SIG_KOR_NM)
d1<-d

d1$SIG_KOR_NM<-ifelse(str_detect(d1$SIG_KOR_NM, "천안시"),str_sub(d1$SIG_KOR_NM,-3,-1),d1$SIG_KOR_NM)

unique(d1$SIG_KOR_NM)

d_set<- c('상당구', '서원구', '흥덕구', '청원구', '충주시', 
             '제천시', '보은군', '옥천군', '영동군', '증평군', 
             '진천군', '괴산군', '음성군', '단양군')

d1<- d1[which(d1$SIG_KOR_NM %in% d_set),]



head(d1)

unique(d1$SIG_KOR_NM)
head(d1)
d1<-d1[,-c(1,2)]
write.csv(d1,file="충북_활엽수_set.csv")

#### 이름 잘못된거 전처리 
library(dplyr)
getwd()
setwd("C:/Users/Admin/Downloads/산림_최종")

tree1<-read.csv("서울_활엽수_set.csv")
tree2<-read.csv("울산_활엽수_set.csv")
tree3<-read.csv("세종_활엽수_set.csv")
tree4<-read.csv("광주_활엽수_set.csv")
tree5<-read.csv("대구_활엽수_set.csv")
tree5<-read.csv("대전_활엽수_set.csv")
tree6<-read.csv("충남_활엽수_set.csv")
tree7<-read.csv("충북_활엽수_set.csv")
tree8<-read.csv("전북_활엽수_set.csv")
tree9<-read.csv("전남_활엽수_set.csv")
tree10<-read.csv("인천_활엽수_set.csv")
tree11<-read.csv("경북_활엽수_set.csv")
tree12<-read.csv("경남_활엽수_set.csv")
tree13<-read.csv("경기도_활엽수_set.csv")
tree14<-read.csv("강원도_활엽수_set.csv")

##충남 이름 변경#### 

tree6$SIG_KOR_NM<- gsub('동남구','천안시', tree6$SIG_KOR_NM)
tree6$SIG_KOR_NM<- gsub('서북구','천안시', tree6$SIG_KOR_NM)


##전북 이름 변경##### 
tree8$SIG_KOR_NM<- gsub('덕진구','전주시', tree8$SIG_KOR_NM)
tree8$SIG_KOR_NM<- gsub('완산구','전주시', tree8$SIG_KOR_NM)

##경북 이름 변경####
tree11$SIG_KOR_NM<- gsub('울주군','울산', tree11$SIG_KOR_NM)
tree11$SIG_KOR_NM<- gsub('포항남구','포항', tree11$SIG_KOR_NM)
tree11$SIG_KOR_NM<- gsub('포항북구','포항', tree11$SIG_KOR_NM)

### 경남 이름 변경#####
tree12$SIG_KOR_NM<- gsub('북구','부산', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('금정구','부산', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('강서구','부산', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('기장군','부산', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('달성군','대구', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('울주군','울산', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('의창구','창원시', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('성산구','창원시', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('마산합','창원시', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('마산회','창원시', tree12$SIG_KOR_NM)
tree12$SIG_KOR_NM<- gsub('진해구','창원시', tree12$SIG_KOR_NM)

#광주변수명 변경####
tree4<-tree4 %>% mutate(chim_count=(chim1_count+chim2_count))
tree4<-tree4 %>% mutate(SIG_KOR_NM="광주")
tree4<-tree4 %>% select("X","SIG_KOR_NM","chim_count","hwal2_count")
colnames(tree4)<-c("X","SIG_KOR_NM","chim_count","hwal_count")

##경북 ####
tree11<-tree11 %>% mutate(chim_count=(chim1_count+chim2_count),
                          hwal_count=(hwal1_count+hwal2_count))
tree11<-tree11 %>% select("X","SIG_KOR_NM","chim_count","hwal_count")


##경기도 이름변경####
str(tree13)
colnames(tree13)<-c("X","SIG_KOR_NM","hwal_count","chim_count")

##데이터 합치기##### 
tree_최<-rbind(tree1,tree2)
tree_최<-rbind(tree_최,tree3)
tree_최<-rbind(tree_최,tree4)
tree_최<-rbind(tree_최,tree5)
tree_최<-rbind(tree_최,tree6)
tree_최<-rbind(tree_최,tree7)
tree_최<-rbind(tree_최,tree8)
tree_최<-rbind(tree_최,tree9)
tree_최<-rbind(tree_최,tree10)
tree_최<-rbind(tree_최,tree11)
tree_최<-rbind(tree_최,tree12)
tree_최<-rbind(tree_최,tree13)
tree_최<-rbind(tree_최,tree14)
str(tree_최)
##이름 변경### 
colnames(tree_최)<-c("X","SIGUN","hwal_count","chim_count")



###합산 구하기####
tree_최종<-tree_최 %>% group_by(SIGUN) %>% 
  summarise(hwal_cnt=sum(hwal_count),
            chim_cnt=sum(chim_count))

tree_최종<-tree_최종 %>% mutate(YEAR="2019")



write.csv(tree_최종,"tree_final.csv")





##보안등
setwd("C:\\project\\test_data\\보안등")
e<- fread("light_result.csv")
head(e)
str(e)
## NA, 이상치 제거
unique(sort(e$설치년도))
colSums(is.na(e))
## 설치년도를 개수 기준으로 1980~2019년만 사용한다
## 2071, 1899, 1970, 1975 제거 
e_string<- c(1899,1970,1975,2071)
e2<- e %>% filter(!(설치년도 %in% e_string)) 

str(e2)
unique(sort(e2$설치년도))

## 설치년도 기준으로 개수 누적합-> 2015~2018만 사용 

### 사용할 설치년도, 제공기관명, 설치개수만 남김
e2<-e2[,c(3,8,14)]
unique(e2$제공기관명)
colSums(is.na(e2))
head(e2)

str(e3)



e3<- e2 %>% group_by(제공기관명,설치년도) %>% summarise(sum=sum(설치개수),count=n()) %>%mutate(cumsum=cumsum(sum))
# %>%mutate(cumsum=cumsum(sum))
head(e3)
str(e3)

e3<-as.data.frame(e3)
str(e3)
colSums(is.na(e3))

##2015~2018년 데이터 filter 
e_string<- c(2015,2016,2017,2018)
e4<- e3 %>% filter(설치년도 %in% e_string) 

# 
unique(e4$제공기관명)
colnames(e4)<- c('sigun','year','sum','count','cumsum')
e4<-e4[,-3]
## 이름 통일 
e_string<- c('전라','경상','충청')
e2_string<- c('부산','서울','광주','울산','인천')
e3_string<- c('경기도','강원도')

e5<-e4
e5$sigun<- ifelse(str_detect(e5$sigun, "경기도"), str_sub(e5$sigun,-3,-1) , e5$sigun)
e5$sigun<- ifelse(str_detect(e5$sigun, "강원도"), str_sub(e5$sigun,-3,-1) , e5$sigun)
e5$sigun<- ifelse(str_detect(e5$sigun,"전라남도"), substr(e5$sigun,6,8),e5$sigun)
e5$sigun<- ifelse(str_detect(e5$sigun,"전라북도"), substr(e5$sigun,6,8),e5$sigun)
e5$sigun<- ifelse(str_detect(e5$sigun,"경상남도"), substr(e5$sigun,6,8),e5$sigun)
e5$sigun<- ifelse(str_detect(e5$sigun,"경상북도"), substr(e5$sigun,6,8),e5$sigun)
e5$sigun<- ifelse(str_detect(e5$sigun,"충청남도"), substr(e5$sigun,6,8),e5$sigun)
e5$sigun<- ifelse(str_detect(e5$sigun,"충청북도"), substr(e5$sigun,6,8),e5$sigun)
e5$sigun<-ifelse(str_detect(e5$sigun, "서울"), substr(e5$sigun,1,2),e5$sigun)
e5$sigun<-ifelse(str_detect(e5$sigun, "울산"), str_sub(e5$sigun,1,2),e5$sigun)
e5$sigun<-ifelse(str_detect(e5$sigun, "대구"), str_sub(e5$sigun,1,2),e5$sigun)
e5$sigun<-ifelse(str_detect(e5$sigun, "인천"), str_sub(e5$sigun,1,2),e5$sigun)
e5$sigun<-ifelse(str_detect(e5$sigun, "부산"), str_sub(e5$sigun,1,2),e5$sigun)
e5$sigun<-ifelse(str_detect(e5$sigun, "광주"), str_sub(e5$sigun,1,2),e5$sigun)


unique(e5$sigun)


e6<- e5 %>%  filter(!(sigun %in% c('제주특별자치도 서귀포시','제주특별자치도 제주시')))

unique(e6$sigun)


write.csv(e6, file="보안등.csv")

####가로등 

setwd("C:\\project\\test_data\\가로등")
c<- fread("전국_가로등_전처리(전).csv",header=T)
head(c)
c1<-c
c1$시도군<- ifelse(c$시도군=='세종',c$시도군,ifelse(c$시도군=='인천'|c$시도군=='부산'| c$시도군=='광주'|
                                           c$시도군=='대전'|c$시도군=='울산'|c$시도군=='대구',
                                         substr(c$시도군,1,2),str_sub(c$시도군,-3,-1)))  
str(c1)

c1$시도군<- gsub(" ","",c1$시도군)
colSums(is.na(c1))


colnames(c1)<- c('sigun','2015','2016','2017','2018')

write.csv(c1, file="가로등.csv")








