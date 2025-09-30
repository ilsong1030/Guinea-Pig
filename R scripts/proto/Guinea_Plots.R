setwd('/Users/ilsongjeon/Desktop/For_Job_Interviews/Guinea_pig/data')
data_1<-read.csv('index_cortisol_1st.csv')
data_2<-read.csv('index_cortisol_2nd.csv')
data_3<-read.csv('index_cortisol.csv')
data_Rang_1st <-read.csv('1st_Rang.csv')
data_Rang_2nd <-read.csv('2nd_Rang.csv')
names(data_3)
attach(data_Rang_1st)
mean(Index_1)
sd()
summary(data_1)
detach(data_Rang_1st)

library(ggplot2)
library(ggpubr)
library(lattice)
library(dplyr)
library(carData)
library(gridExtra)
## 1주차 코티솔과 체온 상관관계 상관없음
attach(data_1)
shapiro.test(Cortisol)
shapiro.test(Temperatur)
t.test(Temperatur~Alter)
cor.test(Temperatur,Cortisol)
detach(data_1)
lm(Temperatur~Cortisol, data= data_1)
ggplot(data_1, aes(x=Temperatur, y=Cortisol))+
  geom_point()+geom_abline(slope = 0.01995, intercept = 37.72373)
## 2주차 코티솔과 체온 상관관계딱히 상관없음
attach(data_2)
shapiro.test(Cortisol)##체온, 코티솔 정규분포.
t.test(Temperatur,Cortisol)
cor.test(Temperatur, Cortisol)
lm(Temperatur~Cortisol, data= data_2)
ggplot(data_2, aes(x=Temperatur, y=Cortisol))+
  geom_point()+geom_abline(slope = 0.00899, intercept = 37.92337)
detach(data_2)

##1주차 2주차 index 비교
plot_index_1st <- ggplot(data_1, aes(x=Code, y=Index))+
  geom_bar(stat="identity",fill='red',alpha=0.1)+labs(y='Hierachie Index',x='Individuen')

plot_index_2nd <- ggplot(data_2, aes(x=Code, y=Index))+
  geom_bar(stat="identity",fill='blue',alpha=0.1)+labs(y='Hierachie Index',x='Individuen')

########################## hierachy 일치도 확인하기 ##################################
# 공통 개체만 사용
df <- subset(data_3, is.finite(Index_1) & is.finite(Index_2))
# 1) 랭크 상관
spearman <- cor.test(df$Index_1, df$Index_2, method = "spearman", exact = FALSE)
print(spearman)

# Kendall tau-b (동점 처리에 유리)
kendall <- cor.test(df$Index_1, df$Index_2, method = "kendall", exact = FALSE)
print(kendall)

# 2) 페어 전복율
n <- nrow(df)
inv <- 0; tot <- 0
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    d1 <- df$Index_1[i] - df$Index_1[j]
    d2 <- df$Index_2[i] - df$Index_2[j]
    if (d1 != 0 && d2 != 0) {       # 완전 동률쌍은 제외
      tot <- tot + 1
      if (d1 * d2 < 0) inv <- inv + 1
    }
  }
}
inv_rate <- inv / tot
cat(sprintf("Pairwise inversions: %d/%d (%.1f%%)\n", inv, tot, 100*inv_rate))

# 3) 수준 변화(평균 차이)
if (shapiro.test(df$Index_1 - df$Index_2)$p.value > 0.05) {
  lvl <- t.test(df$Index_1, df$Index_2, paired = TRUE)
} else {
  lvl <- wilcox.test(df$Index_1, df$Index_2, paired = TRUE, exact = FALSE)
}
########## 공통 개체 수 n = 17, 쌍 비교 총 수 tot = 134, 전복 쌍 inv = 16, 전복률 inv_rate = 11.94% ##########
########## 전복이 없진 않으나 대다수 유지된다 ############################
###### visualization ###########
# 주차별 라인/포인트용 long 테이블 (data_3의 Index_1/Index_2 사용)
long <- rbind(
  transform(data_3[, c("Code","Index_1")], Week = "Week1", Index = Index_1)[, c("Code","Week","Index")],
  transform(data_3[, c("Code","Index_2")], Week = "Week2", Index = Index_2)[, c("Code","Week","Index")]
)

# x축 코드 순서를 data_1/2 등장 순서로 고정(있으면 가독성↑)
lvl <- unique(c(data_1$Code, data_2$Code, data_3$Code))
long$Code <- factor(long$Code, levels = lvl)

ggplot() +
  # overlay 막대
  geom_col(data = data_1, aes(x = Code, y = Index), fill = "red",  alpha = 0.20, width = 0.6) +
  geom_col(data = data_2, aes(x = Code, y = Index), fill = "blue", alpha = 0.20, width = 0.6) +
  # 주차별 가로 라인(Week1/Week2를 Code 축 따라 이어줌)
  geom_line(data = long, aes(x = Code, y = Index, color = Week, group = Week),
            linewidth = 0.9, na.rm = TRUE) +
  # 끝점(Week1/Week2) 점 표시
  geom_point(data = long, aes(x = Code, y = Index, color = Week),
             size = 2.2, na.rm = TRUE) +
  scale_color_manual(values = c(Week1 = "red", Week2 = "blue"), guide = "none") +
  labs(y = "Hierachie Index", x = "Individuen",
       title = "Hierachie Index : Week1(Red) vs Week2(Blue)") +
  theme_minimal()

#################################### Index 변화 비교 완 ####################

##### cortisol 1주차, 2주차 #################################
plot_cortisol_1st <- ggplot(data_1, aes(x=Code, y=Cortisol))+
  geom_bar(stat='identity',fill='red',alpha=0.1)+labs(y='Cortisol(ng/ml)', x='Individuen')

plot_cortisol_2nd <- ggplot(data_2, aes(x=Code, y=Cortisol))+
  geom_bar(stat='identity',fill='blue',alpha=0.1)+labs(y='Cortisol(ng/ml)', x='Individuen')
grid.arrange(plot_cortisol_1st,plot_cortisol_2nd, ncol=2)


# 주차별 라인/포인트용 long 테이블 (data_3의 Cortisol_1/_2 사용)
long <- rbind(
  transform(data_3[, c("Code","Cortisol_1")], Week = "Week1", Cortisol = Cortisol_1)[, c("Code","Week","Cortisol")],
  transform(data_3[, c("Code","Cortisol_2")], Week = "Week2", Cortisol = Cortisol_2)[, c("Code","Week","Cortisol")]
)

# x축 코드 순서를 data_1/2 등장 순서로 고정(있으면 가독성↑)
lvl <- unique(c(data_1$Code, data_2$Code, data_3$Code))
long$Code <- factor(long$Code, levels = lvl)

ggplot() +
  # overlay 막대
  geom_col(data = data_1, aes(x = Code, y = Cortisol), fill = "red",  alpha = 0.20, width = 0.6) +
  geom_col(data = data_2, aes(x = Code, y = Cortisol), fill = "blue", alpha = 0.20, width = 0.6) +
  # 주차별 가로 라인(Week1/Week2를 Code 축 따라 이어줌)
  geom_line(data = long, aes(x = Code, y = Cortisol, color = Week, group = Week),
            linewidth = 0.9, na.rm = TRUE) +
  # 끝점(Week1/Week2) 점 표시
  geom_point(data = long, aes(x = Code, y = Cortisol, color = Week),
             size = 2.2, na.rm = TRUE) +
  scale_color_manual(values = c(Week1 = "red", Week2 = "blue"), guide = "none") +
  labs(y = "Cortisol level", x = "Individuen",
       title = "Cortisol level : Week1(Red) vs Week2(Blue)") +
  theme_minimal()
########################## Cortisol level 일치도 확인하기 ##################################
# 공통 개체만 사용
df <- subset(data_3, is.finite(Cortisol_1) & is.finite(Cortisol_2))
# 1) 랭크 상관
spearman <- cor.test(df$Cortisol_1, df$Cortisol_2, method = "spearman", exact = FALSE)
print(spearman)

# Kendall tau-b (동점 처리에 유리)
kendall <- cor.test(df$Cortisol_1, df$Cortisol_2, method = "kendall", exact = FALSE)
print(kendall)

# 2) 페어 전복율
n <- nrow(df)
inv <- 0; tot <- 0
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    d1 <- df$Cortisol_1[i] - df$Cortisol_1[j]
    d2 <- df$Cortisol_2[i] - df$Cortisol_2[j]
    if (d1 != 0 && d2 != 0) {       # 완전 동률쌍은 제외
      tot <- tot + 1
      if (d1 * d2 < 0) inv <- inv + 1
    }
  }
}
inv_rate <- inv / tot
cat(sprintf("Pairwise inversions: %d/%d (%.1f%%)\n", inv, tot, 100*inv_rate))

# 3) 수준 변화(평균 차이)
if (shapiro.test(df$Cortisol_1 - df$Cortisol_2)$p.value > 0.05) {
  lvl <- t.test(df$Cortisol_1, df$Cortisol_2, paired = TRUE)
} else {
  lvl <- wilcox.test(df$Cortisol_1, df$Cortisol_2, paired = TRUE, exact = FALSE)
}
######### 개체 n = 14, inv = 38, inv_rate = 41.76% 코티솔은 많이 변화한다 ##########
##### 서열 변동률 보다 코티솔 변동률이 많은 이유는? ##########
############### Cortisol level 비교 완료################

## 1주차 index 별 체온
attach(data_1)
shapiro.test(Temperatur)
##분포도
t.test(Temperatur~Dominanz)
cor.test(Temperatur, Index_1)
##### cor = 0.6025778
##1주차 index별 체온 - 유의미
##그래프
lm(Temperatur~Index,data = data_1)
plot_index_tem_1st<-ggplot(data_1, aes(x=Index, y=Temperatur))+geom_point()+labs(y='Temperatur(°C)',x='Hierachie Index', title = 'Temperatur by Hierachie Index : Week 1 (t = 2.89, p = 0.012)')+
  geom_abline(slope = 0.6709, intercept = 37.7617)+stat_compare_means(method = 't.test')+geom_smooth()
plot_index_tem_1st

detach(data_1)

##2주차 index 별 체온
attach(data_2)
shapiro.test(Temperatur)
##정규분포
t.test(Temperatur~Dominanz)
##2주차 index별 체온 - 의미없음
##그래프
lm(Temperatur~Index,data = data_2)
plot_index_tem_2nd<-ggplot(data_2, aes(x=Index, y=Temperatur))+geom_point()+labs(y='Temperatur(°C)',x='Hierachie Index', title='Temperatur by Hierachie Index : Week 2 (t = 0.98, p = 0.342)')+
  geom_abline(slope = 0.4596, intercept = 37.8697)+geom_smooth()
plot_index_tem_2nd
detach(data_2)
grid.arrange(plot_index_tem_1st,plot_index_tem_2nd,ncol=2)

##1주차 index 별 코티솔 수치
attach(data_1)
shapiro.test(Cortisol)
##정규분포 아님
wilcox.test(Cortisol~Dominanz)
cor.test(Cortisol,Index, method = 'spearman')
##1주차 index별 코티솔 - 유의미
##그래프
lm(Cortisol~Index,data = data_1)
plot_index_Cor_1st<-ggplot(data_1, aes(x=Index, y=Cortisol))+geom_point()+labs(y='Cortisol(ng/ml)',x='Hierachie Index', title='Cortisol level by Hierachie Index : Week 1 (p = 0.009)')+
  geom_abline(slope = 13.91, intercept = 10.19)+geom_smooth()
plot_index_Cor_1st
detach(data_1)

##2주차 index 별 코티솔 수치
attach(data_2)
shapiro.test(Cortisol)
##정규분포
t.test(Cortisol~Dominanz)
cor.test(Cortisol_2,Index_2)
## 2주차 index 별 코티솔 수치 - 의미없음
lm(Cortisol~Index,data = data_2)
plot_index_Cor_2nd<-ggplot(data_2, aes(x=Index, y=Cortisol))+geom_point()+labs(y='Cortisol(ng/ml)',x='Hierachie Index', title='Cortisol level by Hierachie Index : Week 2 (p = 0.833)')+
  geom_abline(slope = 4.603, intercept = 14.682)+geom_smooth()
plot_index_Cor_2nd
detach(data_2)
grid.arrange(plot_index_Cor_1st,plot_index_Cor_2nd,ncol=2)

##1주차 서열이랑 코티솔 상관
attach(data_Rang_1st)
shapiro.test(Cortisol_1)##정규분포 노노
wilcox.test(Cortisol_1~Rang)##상관관계가 있다
cor.test(Cortisol_1,Index_1)## 상관관계 있고 cor값은 0.606
ggplot(data_Rang_1st, aes(x=Rang, y=Cortisol_1))+geom_boxplot()+stat_compare_means()+geom_point()+labs(y='Cortisol(ng/ml)',x='Rank',title = 'Correlation between Rank and Cortisol : Week 1')
detach(data_Rang_1st)
##2주차 서열 코티솔 상관
attach(data_Rang_2nd)
shapiro.test(Cortisol_2)##정규분포
t.test(Cortisol_2~Rang)
ggplot(data_Rang_2nd, aes(x=Rang, y=Cortisol_2))+geom_boxplot()+stat_compare_means()+geom_point()+labs(y='Cortisol(ng/ml)',x='Rank',title = 'Correlation between Rank and Cortisol : Week 2')
detach(data_Rang_2nd)
##서열과 체온 1주
attach(data_Rang_1st)
shapiro.test(Temperatur)##정규분포
t.test(Temperatur~Rang)##유의미한 편차
cor.test(Temperatur,Index_1)## 1주차 인덱스에 따라 차이 있다
Rang_1<-ggplot(data_Rang_1st, aes(x=Rang, y=Temperatur))+geom_boxplot()+
  stat_compare_means(method = 't.test')+geom_point()+labs(y='Temperatur(°C)', title='Temperatur by Rank : Week 1')
Rang_1_smooth<-ggplot(data_Rang_1st, aes(x=Index_1, y=Temperatur))+geom_smooth()+
  stat_compare_means(method = 't.test')+geom_point()+labs(y='Temperatur(°C)',x='Hierachie Index',title='Temperatur by Rank : Week 1')
detach(data_Rang_1st)
## 서열 체온 2주
attach(data_Rang_2nd)
shapiro.test(Temperatur)##정규분포
t.test(Temperatur~Rang)## 의미없음
cor.test(Temperatur,Index_2)
Rang_2<-ggplot(data_Rang_2nd, aes(x=Rang, y=Temperatur))+geom_boxplot()+
  stat_compare_means(method = 't.test')+geom_point()+labs(y='Temperatur(°C)',title='Temperatur by Rank : Week 2')
Rang_2_smooth<-ggplot(data_Rang_2nd, aes(x=Index_2, y=Temperatur))+geom_smooth()+
  stat_compare_means(method = 't.test')+geom_point()+labs(y='Temperatur(°C)',x='Hierachie Index',title='Temperatur by Rank : Week 2')

grid.arrange(Rang_1,Rang_2,ncol=2)
grid.arrange(Rang_1_smooth,Rang_2_smooth,ncol=2)
detach(data_Rang_2nd)
##1주차 2주차 서열의 상관관계
attach(data_3)
ggplot(data_3, aes(x=Index_1, y=Index_2))+
  geom_smooth()+geom_point()+geom_abline(slope = 1.073184, intercept = 0.008567)+
  labs(x= 'Hierachie Index 1st Woche', y= 'Hierachie Index 2nd Woche', title = 'Correlation between 1st and 2nd Hiearchie Index (p = 3.565e-06)')

lm(Index_1~Index_2, data = data_3)
wilcox.test(Index_1, Index_2,paired = T)
cor(Index_1,Index_2)
cor.test(Index_1,Index_2,paired=T)
