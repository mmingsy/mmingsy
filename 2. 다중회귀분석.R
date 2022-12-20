#종속변수: 흡연자 생활인구량
#독립변수: 음식점 수, 체력단련장의 수
smoke<-read.csv('최종동별흡연자유동량.csv')
food<-read.csv('동별 음식점 수.csv')
gym<-read.csv('동별 체력단련장의 수.csv')
df<-merge(food, gym, by='행정동')
model<-lm(smoke~food+gym, data=df)
summary(model)