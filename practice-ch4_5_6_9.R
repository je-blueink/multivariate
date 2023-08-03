#---- 4장 군집분석----
getwd()
cor_data <- read.csv("C:/Users/bluei/Rstudio_data/ex4-1.csv", header = T, row.names = 1)
head(cor_data, 3) # 데이터 입력 및 확인
summary(cor_data)

# 변수별 표준화
zdata <- scale(cor_data)
zdata

# 거리행렬 계산
zdata_euc <- dist(zdata) # 유클리디안
zdata_man <- dist(zdata, "manhattan") # 맨하탄
as.matrix(zdata_euc)[c(1:5),c(1:5)]
as.matrix(zdata_man)[c(1:5),c(1:5)]

# 군집분석 - 최단연결법
hc_sin1 <- hclust(zdata_euc, method = "single") # 유클리디안 거리행렬 이용
hc_sin1
plot(hc_sin1, hang = -1) # 덴드로그램 그리기
hc_sin1_25 <- cutree(hc_sin1, 2:5) # 소속 군집 알기
hc_sin1_25

hc_sin2 <- hclust(zdata_man, method = "single") # 맨하탄 거리행렬 이용
hc_sin2
plot(hc_sin2, hang = -1) # 덴드로그램 그리기
hc_sin2_25 <- cutree(hc_sin2, 2:5) # 소속 군집 알기
hc_sin2_25

# 군집분석 - 최장연결법
hc_com1 <- hclust(zdata_euc, method = "complete") # 유클리디안 거리행렬 이용
hc_com1
plot(hc_com1, hang = -1) # 덴드로그램 그리기
hc_com1_25 <- cutree(hc_com1, 2:5) # 소속 군집 알기
hc_com1_25

hc_com2 <- hclust(zdata_man, method = "complete") # 맨하탄 거리행렬 이용
hc_com2
plot(hc_com2, hang = -1) # 덴드로그램 그리기
hc_com2_25 <- cutree(hc_com2, 2:5) # 소속 군집 알기
hc_com2_25

# 군집별 중심점 찾기

hcmem1 <- cutree(hc_com1, k=4) # 최장_유클리디안_k4
data_euc <- cbind(cor_data, hcmem1)
aggregate(.~hcmem1, data_euc, mean)

hcmem2 <- cutree(hc_com2, k=4) # 최장_맨하탄_k4
data_man <- cbind(cor_data, hcmem2)
aggregate(.~hcmem2, data_man, mean) 

# 군집분석 - k평균 군집분석, k=4
kmc <- kmeans(zdata, centers = 4)
kmc
pairs(zdata, col=kmc$cluster, pch=10) # 군집 산점도 그리기

# ----5장 다차원척도법----

# 패키지 설치
install.packages("MASS")
library(MASS)

# 데이터 준비
data <- read.table("snackdata.txt", sep = ",")
snackname <- c("A","B","C","D","E","F")
data.snack <- as.matrix(data)
colnames(data.snack) <- snackname
rownames(data.snack) <- snackname
data.snack

# 비메트릭 MDS 분석
nmds <- isoMDS(data.snack, k=2)
nmds

# 그림 그리기
x <- nmds$points[,1]
y <- nmds$points[,2]
plot(x, y, xlab="", ylab="", main="과자 유사성 지도", type="n")
text(x,y, labels=row.names(data.snack), cex=0.9)
abline(h=0, v=0, lty=2)

# ----6장 정준상관분석----

# 패키지 설치
install.packages("ggplot2")
install.packages("GGally")
install.packages("CCA")

library(ggplot2)
library(GGally)
library(CCA)

# 데이터 준비
data("nutrimouse")
help("nutrimouse")

X <- nutrimouse$lipid
Y <- nutrimouse$gene
head(X,3)
head(Y,3)

# 상관계수 행렬 확인
matcor(X, Y)

# 정준상관분석 실행
cano_result <- cc(X, Y) 
z_x <- scale(X) # z-표준화
z_y <- scale(Y)
cano_result <- cc(z_x, z_y)
cano_result <- cancor(X,Y)

# 결과 확인
names(cano_result)
help(cancor)
cano_result
cano_result$cor
round(cano_result$xcoef, 3) 
round(cano_result$ycoef, 3)

# 산점도 그리기
V1 <- cano_result$ycoef[,1]
W1 <- cano_result$xcoef[,1]
cor(W1, V1)
plot(W1, V1)

#---- 9장 나무모형 ----

# 데이터 읽기
h.data <- read.csv("heart.csv", encoding = "UTF-8", header = T)
head(h.data)

# 분류나무 생성
library(rpart)
library(rpart.plot)

my.control <- rpart.control(maxdepth = 2)
h.tree <- rpart(target~., data = h.data, control = my.control)
print(h.tree)
summary(h.tree)

# 분류나무 시각화
prp(h.tree, type = 4, extra = 1, digits = 4)

rm(list = ls())