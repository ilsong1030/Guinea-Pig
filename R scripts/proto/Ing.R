##체온, 코티솔 ~ 나이
Data_total <- read.csv("total.csv")
names(Data_total)
attach(Data_total)
detach(Data_0.5)
mean(Temperatur_durchschnitt)
sd(Temperatur_durchschnitt)
mean(Cortisol_1)
sd(Cortisol_1)
summary(Data_0.5)
summary(Data_2.5)
summary(Data_total)
library(ggplot2)
library(lattice)

shapiro.test(Temperatur_durchschnitt[Alter=='2.5 Jahre'])
##나이별 체온은 정규분포
t.test(Temperatur_durchschnitt~Alter)

##나이별 체온 연관딱히 없음

mean(Temperatur_durchschnitt)

Cortisol_1_by_Alter<- ggplot(Data_total,aes(Alter, Cortisol_1))+geom_boxplot()+labs(y='Cortisol(ng/ml)')+stat_compare_means(method = 't.test')+
  lims(y=c(5,35))+geom_point()
Cortisol_2_by_Alter<- ggplot(Data_total,aes(Alter, Cortisol_2))+geom_boxplot()+labs(y='Cortisol(ng/ml)')+stat_compare_means(method = 't.test')+
  lims(y=c(5,35))+geom_point()
grid.arrange(Cortisol_1_by_Alter,Cortisol_2_by_Alter,ncol=2)
t.test(Cortisol_1~Alter)
## 코티솔 1주차의 변화 의미없음
## 코티솔 2주차의 변화는 의미있음

cor.test(Temperatur_durchschnitt,Cortisol_1)
## 코티솔1,2와 체온 상관관계 없음

lm(Temperatur_durchschnitt~Cortisol_2,data = Data_total)

Cortisol_1_by_Temper <- ggplot(Data_total, aes(Temperatur_durchschnitt
                                 ,Cortisol_1))+
  geom_point()+
  geom_abline(slope = 0.01995, intercept = 37.72373)+
  labs(x='Temperatur(°C)', y='Cortisol(ng/ml)')+geom_smooth()

Cortisol_2_by_Temper <- ggplot(Data_total, aes(Temperatur_durchschnitt
                                               ,Cortisol_2))+
  geom_point()+
  geom_abline(slope = 0.00899, intercept = 37.92337)+
  labs(x='Temperatur(°C)', y='Cortisol(ng/ml)')+geom_smooth()
grid.arrange(Cortisol_1_by_Temper,Cortisol_2_by_Temper, ncol=2)
## 체온과 코티솔~나이 관계없음



## 체온과 코티솔~성향
Data_Dominanz <- read.csv("Dominanz.csv")
detach(Data_Dominanz)
attach(Data_Dominanz)
names(Data_Dominanz)
head(Data_Dominanz)
summary(Data_Dominanz)
shapiro.test(Temperatur_durchschnitt[Dominanz=='Sub'])
## 성향별 체온은 정규분포
shapiro.test(Cortisol_1[Dominanz=='Sub'])
## 성향별 코티솔은 정규분포


t.test(Temperatur_durchschnitt~Dominanz)
## 성향별 체온 수치 유의미한 차이
t.test(Cortisol_2~Dominanz)
## 성향별 코티솔 수치 유의미
lm(Cortisol_2~Dominanz, data = Data_Dominanz)
ggplot(Data_Dominanz, aes(Dominanz,Temperatur_durchschnitt))+
  geom_boxplot()+labs(y='Temperatur(°C)')+
  stat_compare_means(method = 't.test')+geom_point()

Cort_1_by_Domi <- ggplot(Data_Dominanz, aes(Dominanz,Cortisol_1))+
  geom_boxplot()+labs(y="Cortisol(ng/ml)")+
  stat_compare_means(method = 't.test')

Cort_2_by_Domi <-ggplot(Data_Dominanz, aes(Dominanz,Cortisol_2))+
  geom_boxplot()+labs(y="Cortisol(ng/ml)")+
  stat_compare_means(method = 't.test')

grid.arrange(Cort_1_by_Domi,Cort_2_by_Domi, ncol=2)

## 지배층 체온이 더 높다.
### 성향에 따라 체온과 코티솔은 차이가 있다.
### 코티솔 1,2는 왜 차이가 나는가 생각해보자
### 코티솔은 kotprobe로 측정(혈당검사하고 아레나에서 채취.)
### 

##1주차,2주차 index
data_index_corti <- read.csv('index_cortisol.csv')
attach(data_index_corti)
names(data_index_corti)
plot_index_1st <- ggplot(data_index_corti, aes(x=Code, y=Index_1))+
  geom_bar(stat="identity",fill='red',alpha=0.1)+labs(y='Hierachie Index Erster Woche',x='Individuen')

plot_index_2nd <- ggplot(data_index_corti, aes(x=Code, y=Index_2))+
  geom_bar(stat="identity",fill='blue',alpha=0.1)+labs(y='Hierachie Index Zweiter Woche',x='Individuen')
plot_index_1st
plot_index_2nd
library(reshape)
grid.arrange(plot_index_1st,plot_index_2nd,ncol=2)

## cortisol 1주차, 2주차
plot_cortisol_1st <- ggplot(data_index_corti, aes(x=Code, y=Cortisol_1))+
  geom_bar(stat='identity',fill='red',alpha=0.1)+labs(y='Cortisol Erster Woche', x='Individuen')

plot_cortisol_2nd <- ggplot(data_index_corti, aes(x=Code, y=Cortisol_2))+
  geom_bar(stat='identity',fill='blue',alpha=0.1)+labs(y='Cortisol Zweiter Woche', x='Individuen')
grid.arrange(plot_cortisol_1st,plot_cortisol_2nd, ncol=2)


##1주차 인덱스 별 코티솔 수치
ggplot(data_index_corti, aes(x=Index_1, y=Cortisol_1))+
  geom_point()
##2주차 인덱스 별 코티솔 수치
ggplot(data_index_corti, aes(x=Index_2, y=Cortisol_2))+
  geom_point()


##다시!!!

