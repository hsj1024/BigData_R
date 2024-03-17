# 1.데이터 준비하기

# Load
# 원본 데이터 불러오기
ods <- read.csv("US Superstore data.csv" , stringsAsFactors = F , header = T)

# Column Subset
# 필요한 컬럼만 선택하고, 컬럼이름 부여하기
# 내가 선택한 컬럼 : 구매 년월일(ymd), State, Category

mycols <- c(3,11,15)
dataset <- ods[ , mycols ]            
colnames(dataset) <- c("ymd","State","Category")

# Missing Value 및 R을 위한 컬럼 Format 설정
summary( dataset )
dataset$ymd <- as.Date( dataset$ymd ); summary( dataset )
dataset$State <- as.character(dataset$State); summary( dataset )
dataset$Category <- as.character(dataset$Category); summary( dataset )


# 2. 데이터 탐색

# 기본 집계표 생성
install.packages("sqldf")

options( digits=3 )
Totals <- nrow( dataset )
TotalYmd <- as.data.frame(table( dataset$ymd ))
colnames(TotalYmd) <- c("ymd","x")
colnames(TotalState)<-c("State","y")
CountState <- table( dataset$State)
TotalState <- as.data.frame(CountState)
CountCategory <-  table( dataset$Category ) 
TotalCategory <- as.data.frame(CountCategory)
TotalState$per <- 100*TotalState$Freq/Totals
TotalCategory$per <- 100*TotalCategory$Freq/Totals

# 분석 대상 선정 -> 지역별 자주 구매하는 카테고리 수 
TopCategory <- TotalCategory[ order(-TotalCategory$Freq),c("Var1","Freq") ]
TopCategory <- TopCategory[1:10,] 
colnames( TopCategory ) <- c("Category","많이 사는 카테고리 수")
data <- merge(x=dataset,y=TopCategory,by='Category')
data <- data[,c("ymd","Category","State")]
summary(data); str(data)

==================================================================
#  및 시각화
#rownames(ReasonAge2) <- ReasonAge2[,1]
#ReasonAge2 <- ReasonAge2[-1]
ReasonDist <- dist(ReasonAge2, method="euclidean")
two_coord <- cmdscale(ReasonDist)
plot(two_coord, type="n", xlab="x", ylab="y")
text(two_coord, rownames(ReasonAge2) )

# 계층적 군집
library( cluster )
hcl <- hclust( dist(ReasonAge2), method="single")
plot(hcl, hang=-1, xlab="사망원인", ylab="거리")

# 분할적 군집


