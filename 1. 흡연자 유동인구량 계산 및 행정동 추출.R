setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/성균관대학교/2022-1/데이터사이언스와 R/project_R/dataset_R")
people_moving<-read.csv(file="LOCAL_PEOPLE_202204.csv")
View(people_moving)

library(dplyr)

#영등포구 데이터만 추출
ydp<-people_moving %>% filter(행정동코드 %in% c(11560540,11560550,11560560,11560610,11560620,11560630,11560650,11560660,11560670,11560680,11560690,11560700,11560710,11560720,11560515,11560535,11560585,11560605))
View(ydp)

#열 변수 영어로 변환
names(ydp)<-c("id","time","region_code","total","man0_10","man10_15","man15_20","man20_25","man25_30","man30_35","man35_40","man40_45","man45_50","man50_55","man55_60","man60_65","man65_70","man70_","woman0_10","woman10_15","woman15_20","woman20_25","woman25_30","woman30_35","woman35_40","woman40_45","woman45_50","woman50_55","woman55_60","woman60_65","woman65_70","woman70_")


#10대, 20대, ~ 70대 이상으로 묶어 계산
#남성
ydp %>% select("man10_15","man15_20") %>% rowSums()->ydp$man_10age
ydp %>% select("man20_25","man25_30") %>% rowSums()->ydp$man_20age
ydp %>% select("man30_35","man35_40") %>% rowSums()->ydp$man_30age
ydp %>% select("man40_45","man45_50") %>% rowSums()->ydp$man_40age
ydp %>% select("man50_55","man55_60") %>% rowSums()->ydp$man_50age
ydp %>% select("man60_65","man65_70") %>% rowSums()->ydp$man_60age

#여성
ydp %>% select("woman10_15","woman15_20") %>% rowSums()->ydp$woman_10age
ydp %>% select("woman20_25","woman25_30") %>% rowSums()->ydp$woman_20age
ydp %>% select("woman30_35","woman35_40") %>% rowSums()->ydp$woman_30age
ydp %>% select("woman40_45","woman45_50") %>% rowSums()->ydp$woman_40age
ydp %>% select("woman50_55","woman55_60") %>% rowSums()->ydp$woman_50age
ydp %>% select("woman60_65","woman65_70") %>% rowSums()->ydp$woman_60age

#앞의 모든 열 삭제 
ydp_second<-subset(ydp,select=-c(man0_10,man10_15,man15_20,man20_25,man25_30,man30_35,man35_40,man40_45,man45_50,man50_55,man55_60,man60_65,man65_70,woman0_10,woman10_15,woman15_20,woman20_25,woman25_30,woman30_35,woman35_40,woman40_45,woman45_50,woman50_55,woman55_60,woman60_65,woman65_70))

View(ydp_second)

#행정동 코드, 시간별로 그룹화하여 평균 유동인구량 구함 
dong <- ydp_second %>%
  group_by(region_code,time) %>%
  summarise(mean_total_people=mean(total),
            mean_man_10age=mean(man_10age),
            mean_man_20age=mean(man_20age),
            mean_man_30age=mean(man_30age),
            mean_man_40age=mean(man_40age),
            mean_man_50age=mean(man_50age),
            mean_man_60age=mean(man_60age),
            mean_man_70age=mean(man70_),
            mean_woman_10age=mean(woman_10age)
            ,mean_woman_20age=mean(woman_20age),
            mean_woman_30age=mean(woman_30age),
            mean_woman_40age=mean(woman_40age),
            mean_woman_50age=mean(woman_50age),
            mean_woman_60age=mean(woman_60age),
            mean_woman_70age=mean(woman70_)) 
View(dong)
write.csv(dong,file="ydp_people_moving_202204.csv")



View(smoke_df)
write.csv(smoke_df,file="age_sex_smoke_df.csv")
library(dplyr)

#흡연율과 유동인구량 계산 
smoke_ratio<-read.csv(file="age_smoking_ratio.csv")
View(smoke_ratio)

#error delete
smoke_ratio<-smoke_ratio[-c(1:6),]

#column rename
names(smoke_ratio)<-c("age","sex","total","smoking","ratio")

#계산
smoke_df<- smoke_ratio %>%
  group_by(age,sex) %>%
  summarise(total_ex=sum(total),
            total_smoking=sum(smoking),
            percent=round((total_smoking/total_ex)*100,2))

setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/성균관대학교/2022-1/데이터사이언스와 R/project_R/dataset_R")
smoking_ratio_age<-read.csv(file="age_sex_smoke_df.csv")
View(smoking_ratio_age)
ydp_people_moving<-read.csv(file="ydp_people_moving_202204.csv")
View(ydp_people_moving)

#행정동 코드만으로 그룹화하여 이동량 합하기
ydp_final<-ydp_people_moving %>%
  group_by(region_code) %>%
  summarise(total_people=sum(mean_total_people),
            man_20age=sum(mean_man_20age),
            man_30age=sum(mean_man_30age),
            man_40age=sum(mean_man_40age),
            man_50age=sum(mean_man_50age),
            man_60age=sum(mean_man_60age),
            man_70age=sum(mean_man_70age),
            woman_20age=sum(mean_woman_20age),
            woman_30age=sum(mean_woman_30age),
            woman_40age=sum(mean_woman_40age),
            woman_50age=sum(mean_woman_50age),
            woman_60age=sum(mean_woman_60age),
            woman_70age=sum(mean_woman_70age))

View(ydp_final)   

#흡연율*인구량 데이터프레임 만들기
man_20_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='20age' & smoking_ratio_age$sex=='남자',]$percent)/100
man_30_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='30age' & smoking_ratio_age$sex=='남자',]$percent)/100
man_40_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='40age' & smoking_ratio_age$sex=='남자',]$percent)/100
man_50_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='50age' & smoking_ratio_age$sex=='남자',]$percent)/100
man_60_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='60age' & smoking_ratio_age$sex=='남자',]$percent)/100
man_70_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='70age' & smoking_ratio_age$sex=='남자',]$percent)/100
woman_20_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='20age' & smoking_ratio_age$sex=='여자',]$percent)/100
woman_30_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='30age' & smoking_ratio_age$sex=='여자',]$percent)/100
woman_40_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='40age' & smoking_ratio_age$sex=='여자',]$percent)/100
woman_50_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='50age' & smoking_ratio_age$sex=='여자',]$percent)/100
woman_60_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='60age' & smoking_ratio_age$sex=='여자',]$percent)/100
woman_70_ratio<-(smoking_ratio_age[smoking_ratio_age$age=='70age' & smoking_ratio_age$sex=='여자',]$percent)/100

ydp_smoke_final<-ydp_final %>%
  group_by(region_code) %>%
  summarise(smoke_man_20age=(man_20age*man_20_ratio),
            smoke_man_30age=(man_30age*man_30_ratio),
            smoke_man_40age=(man_40age*man_40_ratio),
            smoke_man_50age=(man_50age*man_50_ratio),
            smoke_man_60age=(man_60age*man_60_ratio),
            smoke_man_70age=(man_70age*man_70_ratio),
            smoke_woman_20age=(woman_20age*woman_20_ratio),
            smoke_woman_30age=(woman_30age*woman_30_ratio),
            smoke_woman_40age=(woman_40age*woman_40_ratio),
            smoke_woman_50age=(woman_50age*woman_50_ratio),
            smoke_woman_60age=(woman_60age*woman_60_ratio),
            smoke_woman_70age=(woman_70age*woman_70_ratio))

View(ydp_smoke_final)   

#총 이동량 합하기
ydp_smoke_final$total_smoke<-ydp_smoke_final %>%
  select(smoke_man_20age:smoke_woman_70age) %>%
  rowSums()


#행정동 이름 추가
region_name<-c('영등포본동','영등포동','여의동','당산1동','당산2동','도림동','문래동','양평1동','양평2동','신길1동','신길3동','신길4동','신길5동','신길6동','신길7동','대림1동','대림2동','대림3동')

ydp_smoke_final<-cbind(ydp_smoke_final,region)

View(ydp_smoke_final)
write.csv(ydp_smoke_final,file="흡연자의 유동인구량.csv")

library(dplyr)
#이동량 순위 나타내기
setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/성균관대학교/2022-1/데이터사이언스와 R/project_R/dataset_R")
ydp_smoke_final<-read.csv("최종동별흡연자유동량.csv")
View(ydp_smoke_final)
library(ggplot2)

#하나의 동으로 합치기 (신길동, 대림동)
replace(ydp_smoke_final$region,ydp_smoke_final$region=="신길7동","신길동")->ydp_smoke_final$region

summarise_ydp_smoke_final<-ydp_smoke_final %>%
  group_by(region) %>%
  summarise(total_smoke=sum(total_smoke))

#시각화
p<-ggplot(ydp_smoke_final,aes(region,total_smoke))+
  geom_bar(stat='identity')+
  geom_hline(yintercept = mean(ydp_smoke_final$total_smoke),color="red")
p

mean(ydp_smoke_final$total_smoke)
View(summarise_ydp_smoke_final)
write.csv(summarise_ydp_smoke_final,file="최종동별흡연자유동량.csv")
