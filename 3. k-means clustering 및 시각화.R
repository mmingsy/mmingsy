##3. k-means clustering 
setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/성균관대학교/2022-1/데이터사이언스와 R/project_R/동별 nonsmoke, smoke")
non<-read.csv(file="non_yeong.csv",fileEncoding = 'euc-kr')
smoke<-read.csv(file="smoke_yeong.csv")
View(non)
View(smoke)

#가장 적절한 k 값 찾기 (팔꿈치 방법)
#k 별로 전체 분산 중에서 그룹 간 분산의 비율로 계산하는 "설명된 분산의 비율"을 계산하여 시각화 한후, 팔꿈치 모양으로 꺾이는 지점의 k를 선정
tot_withinss<-c()

for (i in 1:20){
  set.seed(1004)
  kmeans_cluster<-kmeans(smoke,center=i,iter.max=1000)
  tot_withinss[i]<-kmeans_cluster$tot.withinss
}

plot(c(1:20), tot_withinss, type = "b",
     main="The Elbow Method",
     xlab="Number of clusters",
     ylab="Total within-cluster sum of squares")

#k=(대림:4, 신길:5, 여의도:3, 영등포:4)로 하는 게 좋을 듯
#선정한 k개 초과일때부터는 tot.withniss의 변화가 매우 작아 보임
kmeans.result<-kmeans(smoke,4)
center<-kmeans.result$centers
center<-data.frame(center)
center
cn<-c(mean(center$lon),mean(center$lat))
seoul<-get_map(center=cn,zoom=15, maptype='roadmap')
ggmap(seoul)+
  geom_point(data=as.data.frame(kmeans.result$centers),
             aes(x=lon,y=lat),
             color="red", size=10,
             alpha=0.5)

#k-means 구현 
kmeans.result<-kmeans(smoke,4)
plot(smoke[c("lon","lat")],col=kmeans.result$cluster)

a<-ggmap(seoul)+
  geom_point(data=smoke,aes(x=lon,y=lat),
             color=kmeans.result$cluster)

a

#여의도, 영등포동는 결과값이 좋지 않아 평균으로 계산
cn<-c(mean(non$lon),mean(non$lat))


#동별 중심점 구하기
cn<-geocode("대림동")
cn<-as.numeric(cn)
cn


#지도 시각화 (ggmap 활용)
#일단 구현 실현됨

library(ggmap)

register_google(key="AIzaSyCMdK6l6U-f0Eo_3K1mWx-SYU6llT_2TTo")

seoul<-get_map(center=cn,zoom=15, maptype='roadmap')

seoulplot<-ggmap(seoul)+
  geom_point(data=non,aes(x=lon,y=lat),color="red")

seoulplot

map.eq.cluser<-seoulplot+
  geom_point(data=as.data.frame(kmeans.result$centers),
             aes(x=lon,y=lat),
             color="black", size=5)


map.eq.cluser

#위에 추가로 금연구역을 표시하여 겹치면 흡연구역 선정불가
#중심점과 금연구역간의 거리가 10m보다 작으면 선정불가

#거리계산
install.packages("geosphere")
library(geosphere)

#군집의 위도,경도 데이터프레임
kmeans.result$centers

#kmeans 군집의 위도경도 반환
pos<-kmeans.result$centers[4,]
a<-data.frame(pos)
a<-c(a$pos)
a

dist<-distGeo(a,non)
dist[dist<10]

#추가) 지도위에 시각화 (leaflet활용)
install.packages("leaflet")
library(leaflet)
library(dplyr)
library(ggplot2)

food1$clst<-as.factor(kmeans.result$cluster)

pal<-colorFactor('Dark2',food1$clst) #색을 입힐 factor 변수 지정. 첫번째 인자는 Color palette

seoul_leaf<-leaflet(food1) %>%
  addTiles() %>%
  setView(lng=126.9,
          lat=37.5,
          zoom=14)  %>%
  addProviderTiles('CartoDB.Positron')  %>%  #지도 타입 설정
  addCircleMarkers(data=food1 %>%
                     mutate(pop=paste0('<br> n번째 군집:',clst)),
                   popup=~pop,
                   lng=~lon, lat=~lat, color=~pal(clst),
                   radius=3) %>%
  addLegend('bottomright',pal=pal, values=~clst,
            title="군집번호",opacity=1)

seoul_leaf