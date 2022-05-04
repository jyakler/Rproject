library(htmlwidgets)
library(htmltools)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)

#2014~2019 교통사고 데이터 읽어와서 원인간 연관성 찾기
#서울 전체 데이터 -seoul_data
#년도별 구별 사고 갯수 - year_acc2
#기상일 구분( 총 2191일)-weather
#밤사고 night 낮사고 day

raw_data<-read.table("2014_19acc.csv",sep=",",header=T)
View(raw_data)


#전처리 작업

raw_data %>% filter(발생지_시도=="서울")->seoul_data

seoul_data %>% mutate(년도=substr(seoul_data$발생일,1,4),발생월=substr(seoul_data$발생일,5,6))->seoul_data
seoul_data %>% group_by(발생지_시군구) ->data2;View(data2)

seoul_data %>% select(발생일,발생시간,발생지_시군구,사고유형_대분류,도로형태_대분류,
                         노면상태,기상상태)->acc1

seoul_data %>% select(발생일,발생시간,노면상태,기상상태,가해당사자종별,가해자성별,가해자연령,
                         가해자신체상해정도,피해당사자종별,피해자성별,피해자연령,피해자신체상해정도)->acc2


seoul_data %>%group_by(년도) %>% count(발생지_시군구)->year_acc;View(year_acc)
spread(year_acc,key=발생지_시군구,value=n)->year_acc2
year_acc2<-t(year_acc2)
colnames(year_acc2)<-c("2014","2015","2016","2017","2018","2019")
year_acc2[-1,]->year_acc2
Sys.sleep(2)
for(i in 1:6){
  as.numeric(year_acc2[,i])->year_acc2[,i]
}
data.frame(year_acc2)->year_acc2

#---------------------------------------------------------------------------------------------------------------------
#밤낮 사고율 분석
acc2$발생시간<-as.numeric(substr(acc2$발생시간,1,2))
acc2 %>% filter(발생시간>=20 | 발생시간<4) ->night
acc2 %>% filter(4<=발생시간 & 발생시간<20) ->day
day %>% count(기상상태)->dayw
night %>% count(기상상태)->nightw
time_weather<-inner_join(nightw,dayw,by='기상상태')
colnames(time_weather)<-c("기상상태","밤","낮")
gather(time_weather,key=day_night,value=value_t,-기상상태)->time_weather2
#바그래프 만들기
ggplot(time_weather2,aes(x=기상상태,y=value_t,fill=day_night))+ylab("사고수")+
  geom_bar(stat="identity",position=position_dodge())+
  scale_y_continuous(breaks = c(0,100,1000,10000,50000,150000),trans='log10')+
  geom_text(aes(label=value_t),vjust=1.5,colour="white",position=position_dodge(.9),size=3)+
  scale_fill_manual(values=c("#ee765d","#363a7c"))->g
ggsave("daynightacc.png")
#낮-164912 사건(총16시간) 밤-74265사건(8시간)
#파이그래프
plot_ly(dayw,labels = ~기상상태, values = ~n, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(title = '낮',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin=10) ->pieday

plot_ly(nightw,labels = ~기상상태, values = ~n, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(title = '밤',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=10
         ) ->pienight
saveWidget(as.widget(pieday),"pieday.html")
saveWidget(as.widget(pienight),"pienight.html")

#여기부터 화요일작업업
#-------------------------------------------------------------------------------------------------------------------------------
#지도 그리고 구별로 사고개수 보기쉽게 (수평 바그래프로 순서 정렬해서 볼 수 있게)

manual_color=c("강남구"=rainbow(25)[1],"강동구"=rainbow(25)[2],"강북구"=rainbow(25)[3],"강서구"=rainbow(25)[4],"관악구"=rainbow(25)[5],
               "광진구"=rainbow(25)[6],"구로구"=rainbow(25)[7],"금천구"=rainbow(25)[8], "노원구"="#003300",
               "도봉구"=rainbow(25)[10],"동대문구"=rainbow(25)[11],"동작구"=rainbow(25)[12],"마포구"=rainbow(25)[13],
               "서대문구"="#996600", "서초구"=rainbow(25)[15],"성동구"=rainbow(25)[16],"성북구"=rainbow(25)[17],
               "송파구"=rainbow(25)[18],"양천구"=rainbow(25)[19],"영등포구"=rainbow(25)[20],
               "용산구"=rainbow(25)[21],"은평구"=rainbow(25)[22],"종로구"=rainbow(25)[23],"중구"="brown","중랑구"="#6666cc")


temp<-year_acc2
temp %>% mutate(name=row.names(temp))->temp
for(i in 1:6){
  t<-paste(2013+i,"년 구별 사고",sep="")
  temp$name<-factor(temp$name,levels=temp$name[order(temp[,i])])
  temp %>% 
    ggplot(aes(x=name,y=temp[,i],fill=name))+
    geom_bar(stat="identity") +
    scale_fill_manual('구별',values = manual_color)+
    ylim(0,5000)+
    coord_flip() +
    labs(title=t,y="사고수")+
    theme_bw() +
    xlab("")+
    geom_hline(yintercept = c(1000,2000,3000,4000),linetype="dashed",color="black")
  
  filename=paste("region_acc",i+2013,".png",sep="")
  ggsave(filename,dpi=200)
}

#모든 년도 통합 - 아쉬운점 : 코로나로 인한 사고관계 파악하고싶었지만 2020이후자료가 없음
year_acc %>% group_by(발생지_시군구) %>% ggplot(aes(x=발생지_시군구,y=n,fill=년도))+
  geom_bar(stat="identity",position=position_dodge(),width=0.7)+
  labs(y="사고수")+
  theme(axis.text.x=element_text(size=20,face='bold'),axis.title=element_text(size=40))+
  geom_hline(yintercept = c(1000,2000,3000),linetype="dashed",color="black")
ggsave("total_region.png",width=30)

#-------------------------------------------------------------------------------------------------------------------------
#시간별 사고그래프 -유동인구 많을 시간에 사고가 높음- 새벽시간에 차량단독사고율이 높음-> 졸음운전 의심가능
acc1$발생시간<-as.numeric(substr(acc1$발생시간,1,2))
acc1 %>% group_by(발생시간) %>% count(사고유형_대분류)->time_acc


time_acc %>% mutate(pct = paste(as.numeric(sprintf("%0.2f",prop.table(n)))*100,"%",sep="")) %>% ggplot(aes(x=발생시간,y=n,fill=사고유형_대분류))+
  geom_bar(stat="identity",width=0.9)+geom_text(aes(label=pct),size=3,position=position_stack(vjust=.5))+
  labs(title="시간별 사고건수",subtitle="사고유형별로 분류")+theme(plot.title=element_text(size=30))
  
ggsave("timeacc.png",dpi=300,width=12,height=8)  





#수요일


#차대차, 자전거 사고
acc2 %>% filter(가해당사자종별!='자전거' & 피해당사자종별!='자전거')->temp
age<- substr(temp$가해자연령,1,2)
age <-ifelse(age=="불명",NA,age)
age<-as.numeric(age)

ggplot(data=data.frame(age),aes(x=age))+geom_histogram(bins=100,binwidth = 1,na.rm=T,fill=rainbow(90),
                                                       color="black")+
  labs(title="차대차사고 가해자 연령")->g
ggplotly(g)->g
save_html(g,"age_acc_car.html")
acc2 %>% filter(가해당사자종별=='자전거')->temp2
age2<-as.numeric(gsub("[세]","",temp2$가해자연령))

acc2 %>% filter(피해당사자종별=='자전거')->temp2
age2_1<-as.numeric(gsub("[세]","",temp2$피해자연령))
data.frame(factor(age2))->a
a %>% count(factor.age2.)->a
data.frame(factor(age2_1))->b
b %>% count(factor.age2_1.)->b
colnames(b)<-c("age","num")
colnames(a)<-c("age","num")

full_join(a,b,by="age")->c
c$num.x<- ifelse(is.na(c$num.x),0,c$num.x)
c$num.y<- ifelse(is.na(c$num.y),0,c$num.y)
c$age<- ifelse(is.na(c$age),0,c$age)
colnames(c)<-c("age","가해자","피해자")
as.numeric(c$age)->c$age
c %>% group_by(age) %>% gather(key="가해피해",value=num,-age)->c


ggplot(data=c,aes(x=age,y=num,fill=가해피해,label=num))+geom_vline(xintercept=c(10,20,30,40,50,60,70,80,90,100),linetype="dashed")+
  geom_bar(stat="identity")+scale_x_continuous(breaks=c(0:110))+
  labs(title="자전거 사고연령별")+geom_text(aes(label=num),position=position_stack(vjust=.5),size=3,check_overlap = T)+
  theme(axis.text.x=element_text(size=10),axis.title=element_text(size=30))->g2

ggsave("age_acc_bike.png",dpi=300,width=40,height=10)

#-------------------------------------------------------------------------------------------
#차종별 비교
acc2 %>% select(가해당사자종별,피해당사자종별)->car
car$가해당사자종별<-factor(car$가해당사자종별)
car$피해당사자종별<-factor(car$피해당사자종별)
data.frame(table(car))->tb
spread(tb,key=피해당사자종별,value=Freq)->aa
aa<-aa[c(1:4,6:13,5),]
aa<-aa[,c(1:5,8:10,12:16,7,6,11)]
rownames(aa)<-aa[,1]
spare<-aa[,1]
aa<-aa[,-1]

aa2<-aa
for(i in 1:13){
  total<-aa[i,] %>% sum()
  aa2[i,]<-as.numeric(sprintf("%0.4f",aa[i,]/total))*100
}
cbind(aa2,가해=spare)->aa2
aa2 %>% gather(key=피해당사자종별,value=Freq,-가해)->aa3
aa3 %>% filter(가해!="불명" & 피해당사자종별!="불명")->aa3

#색확장
mycolor=colorRampPalette(brewer.pal(9,"Set1"))(16)
ggplot(aa3,aes(x=가해,y=Freq,fill=피해당사자종별))+geom_bar(position="fill",stat="identity",color="black")+
  scale_fill_manual(values=mycolor)
  #geom_text(aes(label=Freq),vjust=0.5,check_overlap = T)
ggsave("typeofcar.png",dpi=200)

#---------------------------------------------------------------------------
#요일별
as.POSIXlt(as.character(acc1$발생일),format="%Y%m%d")->acc1$발생일
acc1 %>% mutate(요일=weekdays(acc1$발생일))->acc1
acc1 %>% count(요일)->wdays

ggplot(wdays,aes(x=factor(요일,level=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일")),y=n,fill=요일))+
  geom_bar(stat="identity")+labs(title="요일별 사고",x="요일",y="")
ggsave("weekday_acc.png")


# register_google(key='AIzaSyCt0Oc-F_-g2KAhVvm7oEheSKzAhQoUMd0')
# #사이트 https://www.its.go.kr/opendata/opendataList?service=event
# api_key="a3d252ca4a3b4693889d4eb9bdf8e648"

