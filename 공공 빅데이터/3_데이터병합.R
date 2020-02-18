##merge
setwd("C:\\rproject\\data")
news<-fread("news.csv")
head(news)

we<-fread("weather_final.csv")
str(we)
head(we)
we<-we[,-1]
colSums(is.na(we))

colnames(news)<- c('bug','sigun','YEAR')
colnames(we)<- c('sigun','YEAR','mean_egg','min_egg','mean_bug')

nw<- full_join(we,news,by=c('sigun','YEAR'))
colSums(is.na(nw))

S## NA값은 피해가 발생하지 않은 지역이므로 NA 처리
nw$bug<- ifelse(is.na(nw$bug)==TRUE,0,1)

nw$YEAR<-as.factor(nw$YEAR)
str(nw)  

write.csv(nw,file="nw.csv")

##가로등 merge
setwd("C:\\rproject\\data")
f<- fread("가로등.csv",header=T)
str(f)
f<-f[,-1]
unique(f$`2015`)
unique(f$`2016`)
##2015년에 - 2016년에 -, ""있어서 chr형태로 존재NA 처리후 merge
f$`2015`<- gsub("-",NA,f$`2015`)
f$`2016`<- gsub("",NA,f$`2016`)
f$`2016`<- gsub("-",NA,f$`2016`)

unique(f$`2015`)
unique(f$`2016`)


f$`2015`<- as.numeric(f$`2015`)
f$`2016`<- as.numeric(f$`2016`)
colSums(is.na(f))
## na값 각각 2015= 2,2016= 220, 2017= 41, 2018= 180 
f_melt <- melt(data = f,
               id.vars=c("sigun"),
               measure.vars = c("2015","2016","2017","2018"))
head(f_melt)
colnames(f_melt)<-c("sigun","YEAR",'s_lmp')
colSums(is.na(f_melt))

f_melt$YEAR<-as.factor(f_melt$YEAR)
b1<-  left_join(nw,f_melt, by=c('sigun','YEAR'))
head(b1)
colSums(is.na(b1))
write.csv(b1,file="nwl.csv")
nwl<-b1
unique(nwl$sigun)

##보안등 merge
setwd("C:\\rproject\\data")
g<- fread("보안등.csv")
str(g)
head(g)
g<-g[,-1]
colSums(is.na(g))


colnames(g)<-c('sigun','YEAR','lmp_count','lmp_cumsum')
g$YEAR<-as.factor(g$YEAR)


b1<-  left_join(nwl,g, by=c('sigun','YEAR'))
nwlg<-b1
colSums(is.na(nwlg))
unique(nwlg$sigun)
write.csv(b1,"nwlg.csv")

##merge
tree<-"C:\\project\\final\\산림"
tree_csv<-dir(tree)
filepath_csv<-paste0(tree,"/",tree_csv)
filepath_csv
cr<-data.frame()

for(file in filepath_csv){
  
  data1<-fread(file)
  cr<-rbind(cr,data1)
}

str(cr)
head(cr)

tree1<-cr; tree2<- cr ; tree3<- cr; tree4<-cr
tree1_rep<-rep(2015,dim(tree1)[1]); tree2_rep<-rep(2018,dim(tree1)[1]); tree3_rep<-rep(2017,dim(tree1)[1])
tree4_rep<-rep(2016,dim(tree1)[1])
tree1$YEAR<- tree1_rep ;tree2$YEAR<- tree2_rep; tree3$YEAR<- tree3_rep; tree4$YEAR<- tree4_rep

tree<-rbind(tree1,tree2,tree3,tree4)

tree<-tree[,-1]

str(tree)

colnames(tree)<- c('sigun','hw_cnt','ch_cnt','YEAR')
tree$YEAR<-as.factor(tree$YEAR)
write.csv(tree,file="tree.csv")



## 산림 2015~2018 같다고 가정 
setwd("C:\\rproject\\data")
tree<- fread("tree_final.csv")
head(tree)
tree<-tree[,-1]
colnames(tree)<- c('sigun','hw_cnt','ch_cnt','YEAR')
tree$YEAR<-as.factor(tree$YEAR)

unique(tree$YEAR)

tree1<-tree; tree2<- tree ; tree3<- tree; tree4<-tree
tree1_rep<-rep(2015,dim(tree1)[1]); tree2_rep<-rep(2018,dim(tree2)[1]); tree3_rep<-rep(2017,dim(tree3)[1])
tree4_rep<-rep(2016,dim(tree4)[1])
tree1$YEAR<- tree1_rep ;tree2$YEAR<- tree2_rep; tree3$YEAR<- tree3_rep; tree4$YEAR<- tree4_rep

str(tree1)

tree<-rbind(tree1,tree2,tree3,tree4)
head(tree)
unique(tree$YEAR)
str(tree)

tree$YEAR<-as.factor(tree$YEAR)


b1<-  left_join(nwlg,tree, by=c('sigun','YEAR'))
write.csv(b1,"full.csv")
join<-b1

colSums(is.na(join))
head(join)

##BD
setwd("C:\\project")
bd<-fread("building_result.csv")
head(bd)
bd$YEAR<-substr(bd$YEAR,3,6)
unique(bd$YEAR)
bd<- bd %>%  filter(YEAR !=2014 & YEAR != 2019)
bd2<- bd
unique(bd2$SIGUN)

bd2$SIGUN<- ifelse(str_detect(bd2$SIGUN,"서울_"),substr(bd2$SIGUN,1,3),
                   ifelse(str_detect(bd2$SIGUN,"부산_"),substr(bd2$SIGUN,1,3),
                          ifelse(str_detect(bd2$SIGUN,"대구_"),substr(bd2$SIGUN,1,3),
                                 ifelse(str_detect(bd2$SIGUN,"인천_"),substr(bd2$SIGUN,1,3),
                                        ifelse(str_detect(bd2$SIGUN,"광주_"),substr(bd2$SIGUN,1,3),
                                               ifelse(str_detect(bd2$SIGUN,"대전_"),substr(bd2$SIGUN,1,3),
                                                      ifelse(str_detect(bd2$SIGUN,"울산_"),substr(bd2$SIGUN,1,3),
                                                             ifelse(str_detect(bd2$SIGUN,"제주_"),substr(bd2$SIGUN,1,3),
                                                            bd2$SIGUN))))))))

##중복되는 구는 정제 진행하면서 먼저 진행되는 값으로 함


## 서울 중구는 서울로 가정 
d_set<- c("종로구", '중구', '용산구', '성동구', '광진구', '동대문구', '중랑구', 
          '성북구', '강북구', '도봉구', '노원구', '은평구', '서대문구', '마포구', 
          '양천구', '강서구', '구로구', '금천구', '영등포구', '동작구', '관악구', 
          '서초구', '강남구', '송파구', '강동구')

bd2$SIGUN<- ifelse(bd2$SIGUN %in% d_set, '서울', bd2$SIGUN )
bd2$SIGUN<- ifelse(bd2$SIGUN =='세종시ㅣ', '세종', bd2$SIGUN )

## 부산
d_set<- c('중구', '서구', '동구', '영도구', '부산진구', '동래구', 
            '남구', '북구', '해운대구', '사하구', '금정구', 
            '강서구', '연제구', '수영구', '사상구', '기장군')
bd2$SIGUN<- ifelse(bd2$SIGUN %in% d_set, '부산', bd2$SIGUN )        
                   
##대구
d_set<- c('중구', '동구', '서구', '남구', '북구', '수성구', '달서구', '달성군')
bd2$SIGUN<- ifelse(bd2$SIGUN %in% d_set, '대구', bd2$SIGUN)  



unique(bd2$SIGUN)
head(bd2)
write.csv(bd2,file='bd.csv')

#### 중복 값 처리. summarise -> sum

setwd("C:\\rproject\\data")

bd<- fread("bd.csv")
colnames(bd)<- c('sigun','YEAR','bd_cnt')
head(bd)

bd<-bd[,-c(1,2)]
str(bd)


bd2<- bd %>% group_by(sigun,YEAR) %>% summarise(bd_cnt=sum(bd_cnt))

bd2<- as.data.frame(bd2)

head(bd2)

write.csv(bd2, file="bd_final.csv")



##merge tree, bd


bd_final<- fread("bd_final.csv")
head(bd_final)

bd_final<-bd_final[,-1]

bd_final$YEAR<-as.factor(bd_final$YEAR)

b1<-  left_join(join,bd_final, by=c('sigun','YEAR'))
nwlb<-b1
colSums(is.na(nwlb))

write.csv(nwlb, file="nwlb.csv")

######
setwd("C:\\rproject\\data")
full<- fread("nwlb.csv")
ha<- fread("2018피해면적(ha).csv")

head(full)
head(ha)

full<- full[,-1]

colnames(ha)<- c('sigun','ha','YEAR')

str(ha)
str(full)
full_ha<- left_join(full,ha, by=c('sigun','YEAR'))

write.csv(full_ha,"final.csv")

