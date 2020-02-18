setwd('C:\\rproject\\data')
final<- fread("na_final.csv")

cn_set<- c('천안시', '공주시', '보령시', '아산시', 
           '서산시', '논산시', '계룡시', '당진시', '금산군', 
           '부여군', '서천군', '청양군', '홍성군', '예산군', '태안군')

cb_set<- c('청주시', '충주시', '제천시', '보은군', '옥천군', '영동군', 
           '증평군', '진천군', '괴산군', '음성군', '단양군')

gn_set<- c('창원시','진주시', '통영시', '사천시', '김해시', '밀양시', 
           '거제시', '양산시', '의령군', '함안군', '창녕군', '고성군', '남해군', 
           '하동군', '산청군', '함양군', '거창군', '합천군')

gb_set<- c('포항시', '경주시', '김천시', '안동시', '구미시', '영주시', '영천시', '상주시', '문경시',
           '경산시', '군위군', '의성군', '청송군', 
           '영양군', '영덕군', '청도군', '고령군', '성주군', '칠곡군', 
           '예천군', '봉화군', '울진군', '울릉군')
gw_set<- c('춘천시', '원주시', '강릉시', '동해시', '태백시', '속초시', '삼척시', '홍천군', '횡성군', 
           '영월군', '평창군', '정선군', '철원군', '화천군', '양구군', '인제군', '강원도고성군', '양양군')


jb_set<- c('전주시', '완산구', '덕진구', '군산시', '익산시', '정읍시', 
           '남원시', '김제시', '완주군', '진안군', '무주군', '장수군', '임실군', 
           '순창군', '고창군', '부안군')

jn_set<- c('목포시', '여수시', '순천시', '나주시', '광양시', '담양군', '곡성군', '구례군', 
           '고흥군', '보성군', '화순군', '장흥군', '강진군', '해남군', 
           '영암군', '무안군', '함평군', '영광군', '장성군', '완도군', '진도군', '신안군')

gg_set<- c('수원시', '성남시', '의정부시', '안양시', '부천시', '광명시', '평택시', '동두천시', '안산시', '고양시', '과천시', 
           '구리시', '남양주시', '오산시', '시흥시', '군포시', '의왕시', '하남시', '용인시', 
           '파주시', '이천시', '안성시', '김포시', '화성시', '광주시', '양주시', 
           '포천시', '여주시', '연천군', '가평군', '양평군')


    
  
cn1<- final[which(final$sigun %in% cn_set & final$y_factor==1),]
cn0<- final[which(final$sigun %in% cn_set& final$y_factor==0),]

head(cn0)
cn2<- cn0
cn3<- cn1

cn2$s_lmp<-  ifelse(cn2$YEAR==2015 & is.na(cn2$s_lmp)==FALSE, cn2$s_lmp,
                ifelse(cn2$YEAR==2016 & is.na(cn2$s_lmp)==FALSE, cn2$s_lmp,
                   ifelse(cn2$YEAR==2017 & is.na(cn2$s_lmp)==FALSE, cn2$s_lmp, 
                       ifelse(cn2$YEAR==2018 & is.na(cn2$s_lmp)==FALSE, cn2$s_lmp,mean(cn2$s_lmp)))))


cn2$lmp_cumsum<-  ifelse(cn2$YEAR==2015 & is.na(cn2$lmp_cumsum)==FALSE, cn2$lmp_cumsum,
                    ifelse(cn2$YEAR==2016 & is.na(cn2$lmp_cumsum)==FALSE, cn2$lmp_cumsum,
                           ifelse(cn2$YEAR==2017 & is.na(cn2$lmp_cumsum)==FALSE, cn2$lmp_cumsum, 
                                  ifelse(cn2$YEAR==2018 & is.na(cn2$lmp_cumsum)==FALSE, cn2$lmp_cumsum,mean(cn2$lmp_cumsum,na.rm=TRUE)))))

cn2$hw_cnt<-  ifelse(cn2$YEAR==2015 & is.na(cn2$hw_cnt)==FALSE, cn2$hw_cnt,
                     ifelse(cn2$YEAR==2016 & is.na(cn2$hw_cnt)==FALSE, cn2$hw_cnt,
                            ifelse(cn2$YEAR==2017 & is.na(cn2$hw_cnt)==FALSE, cn2$hw_cnt, 
                                   ifelse(cn2$YEAR==2018 & is.na(cn2$hw_cnt)==FALSE, cn2$hw_cnt,mean(cn2$hw_cnt,na.rm=TRUE)))))

cn2$ch_cnt<-  ifelse(cn2$YEAR==2015 & is.na(cn2$ch_cnt)==FALSE, cn2$ch_cnt,
                     ifelse(cn2$YEAR==2016 & is.na(cn2$ch_cnt)==FALSE, cn2$ch_cnt,
                            ifelse(cn2$YEAR==2017 & is.na(cn2$ch_cnt)==FALSE, cn2$ch_cnt, 
                                   ifelse(cn2$YEAR==2018 & is.na(cn2$ch_cnt)==FALSE, cn2$ch_cnt,mean(cn2$ch_cnt,na.rm=TRUE)))))

cn3$s_lmp<-  ifelse(cn3$YEAR==2015 & is.na(cn3$s_lmp)==FALSE, cn3$s_lmp,
                    ifelse(cn3$YEAR==2016 & is.na(cn3$s_lmp)==FALSE, cn3$s_lmp,
                           ifelse(cn3$YEAR==2017 & is.na(cn3$s_lmp)==FALSE, cn3$s_lmp, 
                                  ifelse(cn3$YEAR==2018 & is.na(cn3$s_lmp)==FALSE, cn3$s_lmp,mean(cn3$s_lmp)))))


cn3$lmp_cumsum<-  ifelse(cn3$YEAR==2015 & is.na(cn3$lmp_cumsum)==FALSE, cn3$lmp_cumsum,
                         ifelse(cn3$YEAR==2016 & is.na(cn3$lmp_cumsum)==FALSE, cn3$lmp_cumsum,
                                ifelse(cn3$YEAR==2017 & is.na(cn3$lmp_cumsum)==FALSE, cn3$lmp_cumsum, 
                                       ifelse(cn3$YEAR==2018 & is.na(cn3$lmp_cumsum)==FALSE, cn3$lmp_cumsum,mean(cn3$lmp_cumsum,na.rm=TRUE)))))

cn3$hw_cnt<-  ifelse(cn3$YEAR==2015 & is.na(cn3$hw_cnt)==FALSE, cn3$hw_cnt,
                     ifelse(cn3$YEAR==2016 & is.na(cn3$hw_cnt)==FALSE, cn3$hw_cnt,
                            ifelse(cn3$YEAR==2017 & is.na(cn3$hw_cnt)==FALSE, cn3$hw_cnt, 
                                   ifelse(cn3$YEAR==2018 & is.na(cn3$hw_cnt)==FALSE, cn3$hw_cnt,mean(cn3$hw_cnt,na.rm=TRUE)))))

cn3$ch_cnt<-  ifelse(cn3$YEAR==2015 & is.na(cn3$ch_cnt)==FALSE, cn3$ch_cnt,
                     ifelse(cn3$YEAR==2016 & is.na(cn3$ch_cnt)==FALSE, cn3$ch_cnt,
                            ifelse(cn3$YEAR==2017 & is.na(cn3$ch_cnt)==FALSE, cn3$ch_cnt, 
                                   ifelse(cn3$YEAR==2018 & is.na(cn3$ch_cnt)==FALSE, cn3$ch_cnt,mean(cn3$ch_cnt,na.rm=TRUE)))))

cn_f<- rbind(cn2,cn3)


###cb

cb1<- final[which(final$sigun %in% cb_set & final$y_factor==1),]
cb0<- final[which(final$sigun %in% cb_set& final$y_factor==0),]

head(cb0)
cb2<- cb0
cb3<- cb1

cb2$s_lmp<-  ifelse(cb2$YEAR==2015 & is.na(cb2$s_lmp)==FALSE, cb2$s_lmp,
                    ifelse(cb2$YEAR==2016 & is.na(cb2$s_lmp)==FALSE, cb2$s_lmp,
                           ifelse(cb2$YEAR==2017 & is.na(cb2$s_lmp)==FALSE, cb2$s_lmp, 
                                  ifelse(cb2$YEAR==2018 & is.na(cb2$s_lmp)==FALSE, cb2$s_lmp,mean(cb2$s_lmp)))))


cb2$lmp_cumsum<-  ifelse(cb2$YEAR==2015 & is.na(cb2$lmp_cumsum)==FALSE, cb2$lmp_cumsum,
                         ifelse(cb2$YEAR==2016 & is.na(cb2$lmp_cumsum)==FALSE, cb2$lmp_cumsum,
                                ifelse(cb2$YEAR==2017 & is.na(cb2$lmp_cumsum)==FALSE, cb2$lmp_cumsum, 
                                       ifelse(cb2$YEAR==2018 & is.na(cb2$lmp_cumsum)==FALSE, cb2$lmp_cumsum,mean(cb2$lmp_cumsum,na.rm=TRUE)))))

cb2$hw_cnt<-  ifelse(cb2$YEAR==2015 & is.na(cb2$hw_cnt)==FALSE, cb2$hw_cnt,
                     ifelse(cb2$YEAR==2016 & is.na(cb2$hw_cnt)==FALSE, cb2$hw_cnt,
                            ifelse(cb2$YEAR==2017 & is.na(cb2$hw_cnt)==FALSE, cb2$hw_cnt, 
                                   ifelse(cb2$YEAR==2018 & is.na(cb2$hw_cnt)==FALSE, cb2$hw_cnt,mean(cb2$hw_cnt,na.rm=TRUE)))))

cb2$ch_cnt<-  ifelse(cb2$YEAR==2015 & is.na(cb2$ch_cnt)==FALSE, cb2$ch_cnt,
                     ifelse(cb2$YEAR==2016 & is.na(cb2$ch_cnt)==FALSE, cb2$ch_cnt,
                            ifelse(cb2$YEAR==2017 & is.na(cb2$ch_cnt)==FALSE, cb2$ch_cnt, 
                                   ifelse(cb2$YEAR==2018 & is.na(cb2$ch_cnt)==FALSE, cb2$ch_cnt,mean(cb2$ch_cnt,na.rm=TRUE)))))

cb3$s_lmp<-  ifelse(cb3$YEAR==2015 & is.na(cb3$s_lmp)==FALSE, cb3$s_lmp,
                    ifelse(cb3$YEAR==2016 & is.na(cb3$s_lmp)==FALSE, cb3$s_lmp,
                           ifelse(cb3$YEAR==2017 & is.na(cb3$s_lmp)==FALSE, cb3$s_lmp, 
                                  ifelse(cb3$YEAR==2018 & is.na(cb3$s_lmp)==FALSE, cb3$s_lmp,mean(cb3$s_lmp)))))


cb3$lmp_cumsum<-  ifelse(cb3$YEAR==2015 & is.na(cb3$lmp_cumsum)==FALSE, cb3$lmp_cumsum,
                         ifelse(cb3$YEAR==2016 & is.na(cb3$lmp_cumsum)==FALSE, cb3$lmp_cumsum,
                                ifelse(cb3$YEAR==2017 & is.na(cb3$lmp_cumsum)==FALSE, cb3$lmp_cumsum, 
                                       ifelse(cb3$YEAR==2018 & is.na(cb3$lmp_cumsum)==FALSE, cb3$lmp_cumsum,mean(cb3$lmp_cumsum,na.rm=TRUE)))))

cb3$hw_cnt<-  ifelse(cb3$YEAR==2015 & is.na(cb3$hw_cnt)==FALSE, cb3$hw_cnt,
                     ifelse(cb3$YEAR==2016 & is.na(cb3$hw_cnt)==FALSE, cb3$hw_cnt,
                            ifelse(cb3$YEAR==2017 & is.na(cb3$hw_cnt)==FALSE, cb3$hw_cnt, 
                                   ifelse(cb3$YEAR==2018 & is.na(cb3$hw_cnt)==FALSE, cb3$hw_cnt,mean(cb3$hw_cnt,na.rm=TRUE)))))

cb3$ch_cnt<-  ifelse(cb3$YEAR==2015 & is.na(cb3$ch_cnt)==FALSE, cb3$ch_cnt,
                     ifelse(cb3$YEAR==2016 & is.na(cb3$ch_cnt)==FALSE, cb3$ch_cnt,
                            ifelse(cb3$YEAR==2017 & is.na(cb3$ch_cnt)==FALSE, cb3$ch_cnt, 
                                   ifelse(cb3$YEAR==2018 & is.na(cb3$ch_cnt)==FALSE, cb3$ch_cnt,mean(cb3$ch_cnt,na.rm=TRUE)))))

cb_f<- rbind(cb2,cb3)

## gg

gg1<- final[which(final$sigun %in% gg_set & final$y_factor==1),]
gg0<- final[which(final$sigun %in% gg_set& final$y_factor==0),]

head(gg0)
gg2<- gg0
gg3<- gg1

gg2$s_lmp<-  ifelse(gg2$YEAR==2015 & is.na(gg2$s_lmp)==FALSE, gg2$s_lmp,
                    ifelse(gg2$YEAR==2016 & is.na(gg2$s_lmp)==FALSE, gg2$s_lmp,
                           ifelse(gg2$YEAR==2017 & is.na(gg2$s_lmp)==FALSE, gg2$s_lmp, 
                                  ifelse(gg2$YEAR==2018 & is.na(gg2$s_lmp)==FALSE, gg2$s_lmp,mean(gg2$s_lmp)))))


gg2$lmp_cumsum<-  ifelse(gg2$YEAR==2015 & is.na(gg2$lmp_cumsum)==FALSE, gg2$lmp_cumsum,
                         ifelse(gg2$YEAR==2016 & is.na(gg2$lmp_cumsum)==FALSE, gg2$lmp_cumsum,
                                ifelse(gg2$YEAR==2017 & is.na(gg2$lmp_cumsum)==FALSE, gg2$lmp_cumsum, 
                                       ifelse(gg2$YEAR==2018 & is.na(gg2$lmp_cumsum)==FALSE, gg2$lmp_cumsum,mean(gg2$lmp_cumsum,na.rm=TRUE)))))

gg2$hw_cnt<-  ifelse(gg2$YEAR==2015 & is.na(gg2$hw_cnt)==FALSE, gg2$hw_cnt,
                     ifelse(gg2$YEAR==2016 & is.na(gg2$hw_cnt)==FALSE, gg2$hw_cnt,
                            ifelse(gg2$YEAR==2017 & is.na(gg2$hw_cnt)==FALSE, gg2$hw_cnt, 
                                   ifelse(gg2$YEAR==2018 & is.na(gg2$hw_cnt)==FALSE, gg2$hw_cnt,mean(gg2$hw_cnt,na.rm=TRUE)))))

gg2$ch_cnt<-  ifelse(gg2$YEAR==2015 & is.na(gg2$ch_cnt)==FALSE, gg2$ch_cnt,
                     ifelse(gg2$YEAR==2016 & is.na(gg2$ch_cnt)==FALSE, gg2$ch_cnt,
                            ifelse(gg2$YEAR==2017 & is.na(gg2$ch_cnt)==FALSE, gg2$ch_cnt, 
                                   ifelse(gg2$YEAR==2018 & is.na(gg2$ch_cnt)==FALSE, gg2$ch_cnt,mean(gg2$ch_cnt,na.rm=TRUE)))))

gg3$s_lmp<-  ifelse(gg3$YEAR==2015 & is.na(gg3$s_lmp)==FALSE, gg3$s_lmp,
                    ifelse(gg3$YEAR==2016 & is.na(gg3$s_lmp)==FALSE, gg3$s_lmp,
                           ifelse(gg3$YEAR==2017 & is.na(gg3$s_lmp)==FALSE, gg3$s_lmp, 
                                  ifelse(gg3$YEAR==2018 & is.na(gg3$s_lmp)==FALSE, gg3$s_lmp,mean(gg3$s_lmp)))))


gg3$lmp_cumsum<-  ifelse(gg3$YEAR==2015 & is.na(gg3$lmp_cumsum)==FALSE, gg3$lmp_cumsum,
                         ifelse(gg3$YEAR==2016 & is.na(gg3$lmp_cumsum)==FALSE, gg3$lmp_cumsum,
                                ifelse(gg3$YEAR==2017 & is.na(gg3$lmp_cumsum)==FALSE, gg3$lmp_cumsum, 
                                       ifelse(gg3$YEAR==2018 & is.na(gg3$lmp_cumsum)==FALSE, gg3$lmp_cumsum,mean(gg3$lmp_cumsum,na.rm=TRUE)))))

gg3$hw_cnt<-  ifelse(gg3$YEAR==2015 & is.na(gg3$hw_cnt)==FALSE, gg3$hw_cnt,
                     ifelse(gg3$YEAR==2016 & is.na(gg3$hw_cnt)==FALSE, gg3$hw_cnt,
                            ifelse(gg3$YEAR==2017 & is.na(gg3$hw_cnt)==FALSE, gg3$hw_cnt, 
                                   ifelse(gg3$YEAR==2018 & is.na(gg3$hw_cnt)==FALSE, gg3$hw_cnt,mean(gg3$hw_cnt,na.rm=TRUE)))))

gg3$ch_cnt<-  ifelse(gg3$YEAR==2015 & is.na(gg3$ch_cnt)==FALSE, gg3$ch_cnt,
                     ifelse(gg3$YEAR==2016 & is.na(gg3$ch_cnt)==FALSE, gg3$ch_cnt,
                            ifelse(gg3$YEAR==2017 & is.na(gg3$ch_cnt)==FALSE, gg3$ch_cnt, 
                                   ifelse(gg3$YEAR==2018 & is.na(gg3$ch_cnt)==FALSE, gg3$ch_cnt,mean(gg3$ch_cnt,na.rm=TRUE)))))

gg_f<- rbind(gg2,gg3)

## gw
gw1<- final[which(final$sigun %in% gw_set & final$y_factor==1),]
gw0<- final[which(final$sigun %in% gw_set& final$y_factor==0),]

head(gw0)
gw2<- gw0
gw3<- gw1

gw2$s_lmp<-  ifelse(gw2$YEAR==2015 & is.na(gw2$s_lmp)==FALSE, gw2$s_lmp,
                    ifelse(gw2$YEAR==2016 & is.na(gw2$s_lmp)==FALSE, gw2$s_lmp,
                           ifelse(gw2$YEAR==2017 & is.na(gw2$s_lmp)==FALSE, gw2$s_lmp, 
                                  ifelse(gw2$YEAR==2018 & is.na(gw2$s_lmp)==FALSE, gw2$s_lmp,mean(gw2$s_lmp)))))


gw2$lmp_cumsum<-  ifelse(gw2$YEAR==2015 & is.na(gw2$lmp_cumsum)==FALSE, gw2$lmp_cumsum,
                         ifelse(gw2$YEAR==2016 & is.na(gw2$lmp_cumsum)==FALSE, gw2$lmp_cumsum,
                                ifelse(gw2$YEAR==2017 & is.na(gw2$lmp_cumsum)==FALSE, gw2$lmp_cumsum, 
                                       ifelse(gw2$YEAR==2018 & is.na(gw2$lmp_cumsum)==FALSE, gw2$lmp_cumsum,mean(gw2$lmp_cumsum,na.rm=TRUE)))))

gw2$hw_cnt<-  ifelse(gw2$YEAR==2015 & is.na(gw2$hw_cnt)==FALSE, gw2$hw_cnt,
                     ifelse(gw2$YEAR==2016 & is.na(gw2$hw_cnt)==FALSE, gw2$hw_cnt,
                            ifelse(gw2$YEAR==2017 & is.na(gw2$hw_cnt)==FALSE, gw2$hw_cnt, 
                                   ifelse(gw2$YEAR==2018 & is.na(gw2$hw_cnt)==FALSE, gw2$hw_cnt,mean(gw2$hw_cnt,na.rm=TRUE)))))

gw2$ch_cnt<-  ifelse(gw2$YEAR==2015 & is.na(gw2$ch_cnt)==FALSE, gw2$ch_cnt,
                     ifelse(gw2$YEAR==2016 & is.na(gw2$ch_cnt)==FALSE, gw2$ch_cnt,
                            ifelse(gw2$YEAR==2017 & is.na(gw2$ch_cnt)==FALSE, gw2$ch_cnt, 
                                   ifelse(gw2$YEAR==2018 & is.na(gw2$ch_cnt)==FALSE, gw2$ch_cnt,mean(gw2$ch_cnt,na.rm=TRUE)))))

gw3$s_lmp<-  ifelse(gw3$YEAR==2015 & is.na(gw3$s_lmp)==FALSE, gw3$s_lmp,
                    ifelse(gw3$YEAR==2016 & is.na(gw3$s_lmp)==FALSE, gw3$s_lmp,
                           ifelse(gw3$YEAR==2017 & is.na(gw3$s_lmp)==FALSE, gw3$s_lmp, 
                                  ifelse(gw3$YEAR==2018 & is.na(gw3$s_lmp)==FALSE, gw3$s_lmp,mean(gw3$s_lmp)))))


gw3$lmp_cumsum<-  ifelse(gw3$YEAR==2015 & is.na(gw3$lmp_cumsum)==FALSE, gw3$lmp_cumsum,
                         ifelse(gw3$YEAR==2016 & is.na(gw3$lmp_cumsum)==FALSE, gw3$lmp_cumsum,
                                ifelse(gw3$YEAR==2017 & is.na(gw3$lmp_cumsum)==FALSE, gw3$lmp_cumsum, 
                                       ifelse(gw3$YEAR==2018 & is.na(gw3$lmp_cumsum)==FALSE, gw3$lmp_cumsum,mean(gw3$lmp_cumsum,na.rm=TRUE)))))

gw3$hw_cnt<-  ifelse(gw3$YEAR==2015 & is.na(gw3$hw_cnt)==FALSE, gw3$hw_cnt,
                     ifelse(gw3$YEAR==2016 & is.na(gw3$hw_cnt)==FALSE, gw3$hw_cnt,
                            ifelse(gw3$YEAR==2017 & is.na(gw3$hw_cnt)==FALSE, gw3$hw_cnt, 
                                   ifelse(gw3$YEAR==2018 & is.na(gw3$hw_cnt)==FALSE, gw3$hw_cnt,mean(gw3$hw_cnt,na.rm=TRUE)))))

gw3$ch_cnt<-  ifelse(gw3$YEAR==2015 & is.na(gw3$ch_cnt)==FALSE, gw3$ch_cnt,
                     ifelse(gw3$YEAR==2016 & is.na(gw3$ch_cnt)==FALSE, gw3$ch_cnt,
                            ifelse(gw3$YEAR==2017 & is.na(gw3$ch_cnt)==FALSE, gw3$ch_cnt, 
                                   ifelse(gw3$YEAR==2018 & is.na(gw3$ch_cnt)==FALSE, gw3$ch_cnt,mean(gw3$ch_cnt,na.rm=TRUE)))))

gw_f<- rbind(gw2,gw3)


## jb
jb1<- final[which(final$sigun %in% jb_set & final$y_factor==1),]
jb0<- final[which(final$sigun %in% jb_set& final$y_factor==0),]

head(jb0)
jb2<- jb0
jb3<- jb1

jb2$s_lmp<-  ifelse(jb2$YEAR==2015 & is.na(jb2$s_lmp)==FALSE, jb2$s_lmp,
                    ifelse(jb2$YEAR==2016 & is.na(jb2$s_lmp)==FALSE, jb2$s_lmp,
                           ifelse(jb2$YEAR==2017 & is.na(jb2$s_lmp)==FALSE, jb2$s_lmp, 
                                  ifelse(jb2$YEAR==2018 & is.na(jb2$s_lmp)==FALSE, jb2$s_lmp,mean(jb2$s_lmp)))))


jb2$lmp_cumsum<-  ifelse(jb2$YEAR==2015 & is.na(jb2$lmp_cumsum)==FALSE, jb2$lmp_cumsum,
                         ifelse(jb2$YEAR==2016 & is.na(jb2$lmp_cumsum)==FALSE, jb2$lmp_cumsum,
                                ifelse(jb2$YEAR==2017 & is.na(jb2$lmp_cumsum)==FALSE, jb2$lmp_cumsum, 
                                       ifelse(jb2$YEAR==2018 & is.na(jb2$lmp_cumsum)==FALSE, jb2$lmp_cumsum,mean(jb2$lmp_cumsum,na.rm=TRUE)))))

jb2$hw_cnt<-  ifelse(jb2$YEAR==2015 & is.na(jb2$hw_cnt)==FALSE, jb2$hw_cnt,
                     ifelse(jb2$YEAR==2016 & is.na(jb2$hw_cnt)==FALSE, jb2$hw_cnt,
                            ifelse(jb2$YEAR==2017 & is.na(jb2$hw_cnt)==FALSE, jb2$hw_cnt, 
                                   ifelse(jb2$YEAR==2018 & is.na(jb2$hw_cnt)==FALSE, jb2$hw_cnt,mean(jb2$hw_cnt,na.rm=TRUE)))))

jb2$ch_cnt<-  ifelse(jb2$YEAR==2015 & is.na(jb2$ch_cnt)==FALSE, jb2$ch_cnt,
                     ifelse(jb2$YEAR==2016 & is.na(jb2$ch_cnt)==FALSE, jb2$ch_cnt,
                            ifelse(jb2$YEAR==2017 & is.na(jb2$ch_cnt)==FALSE, jb2$ch_cnt, 
                                   ifelse(jb2$YEAR==2018 & is.na(jb2$ch_cnt)==FALSE, jb2$ch_cnt,mean(jb2$ch_cnt,na.rm=TRUE)))))

jb3$s_lmp<-  ifelse(jb3$YEAR==2015 & is.na(jb3$s_lmp)==FALSE, jb3$s_lmp,
                    ifelse(jb3$YEAR==2016 & is.na(jb3$s_lmp)==FALSE, jb3$s_lmp,
                           ifelse(jb3$YEAR==2017 & is.na(jb3$s_lmp)==FALSE, jb3$s_lmp, 
                                  ifelse(jb3$YEAR==2018 & is.na(jb3$s_lmp)==FALSE, jb3$s_lmp,mean(jb3$s_lmp)))))


jb3$lmp_cumsum<-  ifelse(jb3$YEAR==2015 & is.na(jb3$lmp_cumsum)==FALSE, jb3$lmp_cumsum,
                         ifelse(jb3$YEAR==2016 & is.na(jb3$lmp_cumsum)==FALSE, jb3$lmp_cumsum,
                                ifelse(jb3$YEAR==2017 & is.na(jb3$lmp_cumsum)==FALSE, jb3$lmp_cumsum, 
                                       ifelse(jb3$YEAR==2018 & is.na(jb3$lmp_cumsum)==FALSE, jb3$lmp_cumsum,mean(jb3$lmp_cumsum,na.rm=TRUE)))))

jb3$hw_cnt<-  ifelse(jb3$YEAR==2015 & is.na(jb3$hw_cnt)==FALSE, jb3$hw_cnt,
                     ifelse(jb3$YEAR==2016 & is.na(jb3$hw_cnt)==FALSE, jb3$hw_cnt,
                            ifelse(jb3$YEAR==2017 & is.na(jb3$hw_cnt)==FALSE, jb3$hw_cnt, 
                                   ifelse(jb3$YEAR==2018 & is.na(jb3$hw_cnt)==FALSE, jb3$hw_cnt,mean(jb3$hw_cnt,na.rm=TRUE)))))

jb3$ch_cnt<-  ifelse(jb3$YEAR==2015 & is.na(jb3$ch_cnt)==FALSE, jb3$ch_cnt,
                     ifelse(jb3$YEAR==2016 & is.na(jb3$ch_cnt)==FALSE, jb3$ch_cnt,
                            ifelse(jb3$YEAR==2017 & is.na(jb3$ch_cnt)==FALSE, jb3$ch_cnt, 
                                   ifelse(jb3$YEAR==2018 & is.na(jb3$ch_cnt)==FALSE, jb3$ch_cnt,mean(jb3$ch_cnt,na.rm=TRUE)))))

jb_f<- rbind(jb2,jb3)


##jn

jn1<- final[which(final$sigun %in% jn_set & final$y_factor==1),]
jn0<- final[which(final$sigun %in% jn_set& final$y_factor==0),]

head(jn0)
jn2<- jn0
jn3<- jn1

jn2$s_lmp<-  ifelse(jn2$YEAR==2015 & is.na(jn2$s_lmp)==FALSE, jn2$s_lmp,
                    ifelse(jn2$YEAR==2016 & is.na(jn2$s_lmp)==FALSE, jn2$s_lmp,
                           ifelse(jn2$YEAR==2017 & is.na(jn2$s_lmp)==FALSE, jn2$s_lmp, 
                                  ifelse(jn2$YEAR==2018 & is.na(jn2$s_lmp)==FALSE, jn2$s_lmp,mean(jn2$s_lmp)))))


jn2$lmp_cumsum<-  ifelse(jn2$YEAR==2015 & is.na(jn2$lmp_cumsum)==FALSE, jn2$lmp_cumsum,
                         ifelse(jn2$YEAR==2016 & is.na(jn2$lmp_cumsum)==FALSE, jn2$lmp_cumsum,
                                ifelse(jn2$YEAR==2017 & is.na(jn2$lmp_cumsum)==FALSE, jn2$lmp_cumsum, 
                                       ifelse(jn2$YEAR==2018 & is.na(jn2$lmp_cumsum)==FALSE, jn2$lmp_cumsum,mean(jn2$lmp_cumsum,na.rm=TRUE)))))

jn2$hw_cnt<-  ifelse(jn2$YEAR==2015 & is.na(jn2$hw_cnt)==FALSE, jn2$hw_cnt,
                     ifelse(jn2$YEAR==2016 & is.na(jn2$hw_cnt)==FALSE, jn2$hw_cnt,
                            ifelse(jn2$YEAR==2017 & is.na(jn2$hw_cnt)==FALSE, jn2$hw_cnt, 
                                   ifelse(jn2$YEAR==2018 & is.na(jn2$hw_cnt)==FALSE, jn2$hw_cnt,mean(jn2$hw_cnt,na.rm=TRUE)))))

jn2$ch_cnt<-  ifelse(jn2$YEAR==2015 & is.na(jn2$ch_cnt)==FALSE, jn2$ch_cnt,
                     ifelse(jn2$YEAR==2016 & is.na(jn2$ch_cnt)==FALSE, jn2$ch_cnt,
                            ifelse(jn2$YEAR==2017 & is.na(jn2$ch_cnt)==FALSE, jn2$ch_cnt, 
                                   ifelse(jn2$YEAR==2018 & is.na(jn2$ch_cnt)==FALSE, jn2$ch_cnt,mean(jn2$ch_cnt,na.rm=TRUE)))))

jn3$s_lmp<-  ifelse(jn3$YEAR==2015 & is.na(jn3$s_lmp)==FALSE, jn3$s_lmp,
                    ifelse(jn3$YEAR==2016 & is.na(jn3$s_lmp)==FALSE, jn3$s_lmp,
                           ifelse(jn3$YEAR==2017 & is.na(jn3$s_lmp)==FALSE, jn3$s_lmp, 
                                  ifelse(jn3$YEAR==2018 & is.na(jn3$s_lmp)==FALSE, jn3$s_lmp,mean(jn3$s_lmp)))))


jn3$lmp_cumsum<-  ifelse(jn3$YEAR==2015 & is.na(jn3$lmp_cumsum)==FALSE, jn3$lmp_cumsum,
                         ifelse(jn3$YEAR==2016 & is.na(jn3$lmp_cumsum)==FALSE, jn3$lmp_cumsum,
                                ifelse(jn3$YEAR==2017 & is.na(jn3$lmp_cumsum)==FALSE, jn3$lmp_cumsum, 
                                       ifelse(jn3$YEAR==2018 & is.na(jn3$lmp_cumsum)==FALSE, jn3$lmp_cumsum,mean(jn3$lmp_cumsum,na.rm=TRUE)))))

jn3$hw_cnt<-  ifelse(jn3$YEAR==2015 & is.na(jn3$hw_cnt)==FALSE, jn3$hw_cnt,
                     ifelse(jn3$YEAR==2016 & is.na(jn3$hw_cnt)==FALSE, jn3$hw_cnt,
                            ifelse(jn3$YEAR==2017 & is.na(jn3$hw_cnt)==FALSE, jn3$hw_cnt, 
                                   ifelse(jn3$YEAR==2018 & is.na(jn3$hw_cnt)==FALSE, jn3$hw_cnt,mean(jn3$hw_cnt,na.rm=TRUE)))))

jn3$ch_cnt<-  ifelse(jn3$YEAR==2015 & is.na(jn3$ch_cnt)==FALSE, jn3$ch_cnt,
                     ifelse(jn3$YEAR==2016 & is.na(jn3$ch_cnt)==FALSE, jn3$ch_cnt,
                            ifelse(jn3$YEAR==2017 & is.na(jn3$ch_cnt)==FALSE, jn3$ch_cnt, 
                                   ifelse(jn3$YEAR==2018 & is.na(jn3$ch_cnt)==FALSE, jn3$ch_cnt,mean(jn3$ch_cnt,na.rm=TRUE)))))

jn_f<- rbind(jn2,jn3)

##gn

gn1<- final[which(final$sigun %in% gn_set & final$y_factor==1),]
gn0<- final[which(final$sigun %in% gn_set& final$y_factor==0),]

head(gn0)
gn2<- gn0
gn3<- gn1

gn2$s_lmp<-  ifelse(gn2$YEAR==2015 & is.na(gn2$s_lmp)==FALSE, gn2$s_lmp,
                    ifelse(gn2$YEAR==2016 & is.na(gn2$s_lmp)==FALSE, gn2$s_lmp,
                           ifelse(gn2$YEAR==2017 & is.na(gn2$s_lmp)==FALSE, gn2$s_lmp, 
                                  ifelse(gn2$YEAR==2018 & is.na(gn2$s_lmp)==FALSE, gn2$s_lmp,mean(gn2$s_lmp)))))


gn2$lmp_cumsum<-  ifelse(gn2$YEAR==2015 & is.na(gn2$lmp_cumsum)==FALSE, gn2$lmp_cumsum,
                         ifelse(gn2$YEAR==2016 & is.na(gn2$lmp_cumsum)==FALSE, gn2$lmp_cumsum,
                                ifelse(gn2$YEAR==2017 & is.na(gn2$lmp_cumsum)==FALSE, gn2$lmp_cumsum, 
                                       ifelse(gn2$YEAR==2018 & is.na(gn2$lmp_cumsum)==FALSE, gn2$lmp_cumsum,mean(gn2$lmp_cumsum,na.rm=TRUE)))))

gn2$hw_cnt<-  ifelse(gn2$YEAR==2015 & is.na(gn2$hw_cnt)==FALSE, gn2$hw_cnt,
                     ifelse(gn2$YEAR==2016 & is.na(gn2$hw_cnt)==FALSE, gn2$hw_cnt,
                            ifelse(gn2$YEAR==2017 & is.na(gn2$hw_cnt)==FALSE, gn2$hw_cnt, 
                                   ifelse(gn2$YEAR==2018 & is.na(gn2$hw_cnt)==FALSE, gn2$hw_cnt,mean(gn2$hw_cnt,na.rm=TRUE)))))

gn2$ch_cnt<-  ifelse(gn2$YEAR==2015 & is.na(gn2$ch_cnt)==FALSE, gn2$ch_cnt,
                     ifelse(gn2$YEAR==2016 & is.na(gn2$ch_cnt)==FALSE, gn2$ch_cnt,
                            ifelse(gn2$YEAR==2017 & is.na(gn2$ch_cnt)==FALSE, gn2$ch_cnt, 
                                   ifelse(gn2$YEAR==2018 & is.na(gn2$ch_cnt)==FALSE, gn2$ch_cnt,mean(gn2$ch_cnt,na.rm=TRUE)))))

gn3$s_lmp<-  ifelse(gn3$YEAR==2015 & is.na(gn3$s_lmp)==FALSE, gn3$s_lmp,
                    ifelse(gn3$YEAR==2016 & is.na(gn3$s_lmp)==FALSE, gn3$s_lmp,
                           ifelse(gn3$YEAR==2017 & is.na(gn3$s_lmp)==FALSE, gn3$s_lmp, 
                                  ifelse(gn3$YEAR==2018 & is.na(gn3$s_lmp)==FALSE, gn3$s_lmp,mean(gn3$s_lmp)))))


gn3$lmp_cumsum<-  ifelse(gn3$YEAR==2015 & is.na(gn3$lmp_cumsum)==FALSE, gn3$lmp_cumsum,
                         ifelse(gn3$YEAR==2016 & is.na(gn3$lmp_cumsum)==FALSE, gn3$lmp_cumsum,
                                ifelse(gn3$YEAR==2017 & is.na(gn3$lmp_cumsum)==FALSE, gn3$lmp_cumsum, 
                                       ifelse(gn3$YEAR==2018 & is.na(gn3$lmp_cumsum)==FALSE, gn3$lmp_cumsum,mean(gn3$lmp_cumsum,na.rm=TRUE)))))

gn3$hw_cnt<-  ifelse(gn3$YEAR==2015 & is.na(gn3$hw_cnt)==FALSE, gn3$hw_cnt,
                     ifelse(gn3$YEAR==2016 & is.na(gn3$hw_cnt)==FALSE, gn3$hw_cnt,
                            ifelse(gn3$YEAR==2017 & is.na(gn3$hw_cnt)==FALSE, gn3$hw_cnt, 
                                   ifelse(gn3$YEAR==2018 & is.na(gn3$hw_cnt)==FALSE, gn3$hw_cnt,mean(gn3$hw_cnt,na.rm=TRUE)))))

gn3$ch_cnt<-  ifelse(gn3$YEAR==2015 & is.na(gn3$ch_cnt)==FALSE, gn3$ch_cnt,
                     ifelse(gn3$YEAR==2016 & is.na(gn3$ch_cnt)==FALSE, gn3$ch_cnt,
                            ifelse(gn3$YEAR==2017 & is.na(gn3$ch_cnt)==FALSE, gn3$ch_cnt, 
                                   ifelse(gn3$YEAR==2018 & is.na(gn3$ch_cnt)==FALSE, gn3$ch_cnt,mean(gn3$ch_cnt,na.rm=TRUE)))))

gn_f<- rbind(gn2,gn3)


##gb

gb1<- final[which(final$sigun %in% gb_set & final$y_factor==1),]
gb0<- final[which(final$sigun %in% gb_set& final$y_factor==0),]

head(gb0)
gb2<- gb0
gb3<- gb1

gb2$s_lmp<-  ifelse(gb2$YEAR==2015 & is.na(gb2$s_lmp)==FALSE, gb2$s_lmp,
                    ifelse(gb2$YEAR==2016 & is.na(gb2$s_lmp)==FALSE, gb2$s_lmp,
                           ifelse(gb2$YEAR==2017 & is.na(gb2$s_lmp)==FALSE, gb2$s_lmp, 
                                  ifelse(gb2$YEAR==2018 & is.na(gb2$s_lmp)==FALSE, gb2$s_lmp,mean(gb2$s_lmp)))))


gb2$lmp_cumsum<-  ifelse(gb2$YEAR==2015 & is.na(gb2$lmp_cumsum)==FALSE, gb2$lmp_cumsum,
                         ifelse(gb2$YEAR==2016 & is.na(gb2$lmp_cumsum)==FALSE, gb2$lmp_cumsum,
                                ifelse(gb2$YEAR==2017 & is.na(gb2$lmp_cumsum)==FALSE, gb2$lmp_cumsum, 
                                       ifelse(gb2$YEAR==2018 & is.na(gb2$lmp_cumsum)==FALSE, gb2$lmp_cumsum,mean(gb2$lmp_cumsum,na.rm=TRUE)))))

gb2$hw_cnt<-  ifelse(gb2$YEAR==2015 & is.na(gb2$hw_cnt)==FALSE, gb2$hw_cnt,
                     ifelse(gb2$YEAR==2016 & is.na(gb2$hw_cnt)==FALSE, gb2$hw_cnt,
                            ifelse(gb2$YEAR==2017 & is.na(gb2$hw_cnt)==FALSE, gb2$hw_cnt, 
                                   ifelse(gb2$YEAR==2018 & is.na(gb2$hw_cnt)==FALSE, gb2$hw_cnt,mean(gb2$hw_cnt,na.rm=TRUE)))))

gb2$ch_cnt<-  ifelse(gb2$YEAR==2015 & is.na(gb2$ch_cnt)==FALSE, gb2$ch_cnt,
                     ifelse(gb2$YEAR==2016 & is.na(gb2$ch_cnt)==FALSE, gb2$ch_cnt,
                            ifelse(gb2$YEAR==2017 & is.na(gb2$ch_cnt)==FALSE, gb2$ch_cnt, 
                                   ifelse(gb2$YEAR==2018 & is.na(gb2$ch_cnt)==FALSE, gb2$ch_cnt,mean(gb2$ch_cnt,na.rm=TRUE)))))

gb3$s_lmp<-  ifelse(gb3$YEAR==2015 & is.na(gb3$s_lmp)==FALSE, gb3$s_lmp,
                    ifelse(gb3$YEAR==2016 & is.na(gb3$s_lmp)==FALSE, gb3$s_lmp,
                           ifelse(gb3$YEAR==2017 & is.na(gb3$s_lmp)==FALSE, gb3$s_lmp, 
                                  ifelse(gb3$YEAR==2018 & is.na(gb3$s_lmp)==FALSE, gb3$s_lmp,mean(gb3$s_lmp)))))


gb3$lmp_cumsum<-  ifelse(gb3$YEAR==2015 & is.na(gb3$lmp_cumsum)==FALSE, gb3$lmp_cumsum,
                         ifelse(gb3$YEAR==2016 & is.na(gb3$lmp_cumsum)==FALSE, gb3$lmp_cumsum,
                                ifelse(gb3$YEAR==2017 & is.na(gb3$lmp_cumsum)==FALSE, gb3$lmp_cumsum, 
                                       ifelse(gb3$YEAR==2018 & is.na(gb3$lmp_cumsum)==FALSE, gb3$lmp_cumsum,mean(gb3$lmp_cumsum,na.rm=TRUE)))))

gb3$hw_cnt<-  ifelse(gb3$YEAR==2015 & is.na(gb3$hw_cnt)==FALSE, gb3$hw_cnt,
                     ifelse(gb3$YEAR==2016 & is.na(gb3$hw_cnt)==FALSE, gb3$hw_cnt,
                            ifelse(gb3$YEAR==2017 & is.na(gb3$hw_cnt)==FALSE, gb3$hw_cnt, 
                                   ifelse(gb3$YEAR==2018 & is.na(gb3$hw_cnt)==FALSE, gb3$hw_cnt,mean(gb3$hw_cnt,na.rm=TRUE)))))

gb3$ch_cnt<-  ifelse(gb3$YEAR==2015 & is.na(gb3$ch_cnt)==FALSE, gb3$ch_cnt,
                     ifelse(gb3$YEAR==2016 & is.na(gb3$ch_cnt)==FALSE, gb3$ch_cnt,
                            ifelse(gb3$YEAR==2017 & is.na(gb3$ch_cnt)==FALSE, gb3$ch_cnt, 
                                   ifelse(gb3$YEAR==2018 & is.na(gb3$ch_cnt)==FALSE, gb3$ch_cnt,mean(gb3$ch_cnt,na.rm=TRUE)))))

gb_f<- rbind(gb2,gb3)

final<- rbind(cn_f,cb_f,gb_f,gg_f,gn_f,gw_f,jb_f,jn_f)

write.csv(final,file='final.csv')
