getwd()

#----다변량 시각화 연습----
# longley 데이터 불러오기
data("longley")
head(longley)

# 산점도 행렬 그리기
pairs(longley)

# 피어슨 상관계수 행렬 확인
round(cor(longley, use = "complete.obs"), 3)

# 별그림 그리기
longley_m <- longley[,-6] # year 변수 제거
head(longley_m)
stars(longley_m)

# 얼굴그림 그리기
library(aplpack)
faces(longley_m, face.type = 1, na.rm = T)

# csv 파일로 저장
write.csv(longley, file = "ex_longley.csv")

#---- 주성분분석 연습 ----
# 데이터 불러오기
crime <- read.csv("ex2-4.csv")
head(crime)

# 주성분분석
crime_pca <- princomp(crime[2:5], cor=T, scores = T)
summary(crime_pca)
names(crime_pca)

# 스크리 그림, 누적분산 그림 확인
screeplot(crime_pca, type = "lines", pch=17, main = "Scree Plot : crime")
crime_var = crime_pca$sdev^2
crime_var
crime_var_ratio = crime_var/sum(crime_var)
round(crime_var_ratio, 3)
plot(cumsum(crime_var_ratio), type='b', pch=17, 
     xlab='Component', ylab='Cumulative Proportion')
title('누적분산 그림')

# 주성분계수 확인
crime_pca$loadings[,c(1:2)]

# 주성분점수와 행렬도
data.frame(crime$state, crime_pca$scores[,c(1:2)])
biplot(crime_pca, cex=0.7, col=c("Red", "Blue"))

# ---- 군집분석 연습 ----

# 데이터 불러오기
customer <- read.csv("mall_customer.csv")
head(customer)
md_customer = transform(customer, Gender_male = ifelse(Gender=="Male", 1, 0))
head(md_customer)
md_customer1 = md_customer[,-2]
head(md_customer1)

m_customer = subset(md_customer1, Gender_male==1)
rownames(m_customer) = m_customer$CustomerID
head(m_customer)
f_customer = subset(md_customer1, Gender_male==0)
rownames(f_customer) = f_customer$CustomerID
head(f_customer)

summary(m_customer[,-1])
summary(f_customer[,-1])

# 자료 표준화
zm_customer = scale(m_customer[,-c(1, 5)])
zf_customer = scale(f_customer[,-c(1, 5)])
round(apply(zm_customer, 2, mean), 3)
round(apply(zm_customer, 2, sd), 3)
round(apply(zf_customer, 2, mean), 3)
round(apply(zf_customer, 2, sd), 3)

# 거리행렬 계산
zm_euc = dist(zm_customer)
zf_euc = dist(zf_customer)

# 계층적 군집분석 : 와드연결법
m_cl_w = hclust(zm_euc, method = "ward.D")
m_cl_w

f_cl_w = hclust(zf_euc, method = "ward.D")
f_cl_w

# 덴드로그램
plot(m_cl_w, hang = -1)
plot(f_cl_w, hang = -1)

# 계층적 군집분석 : 최장연결법
m_cl_c = hclust(zm_euc, method = "complete")
m_cl_c

f_cl_c = hclust(zf_euc, method = "complete")
f_cl_c

# 덴드로그램
plot(m_cl_c, hang = -1)
plot(f_cl_c, hang = -1)

# k-평균 군집분석 : 군집 6개
m_kmc = kmeans(zm_customer, centers=6)
m_kmc
f_kmc = kmeans(zf_customer, centers=6)
f_kmc


help("write.csv")
help("faces")
rm(list = ls())