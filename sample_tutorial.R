
###original
mtcars[1:2,] ## 1행부터 2행까지 출력, (열은 전부)
mtcars[1:2,c(3,5)] ##1행부터 2행까지 열은 3열과 5열 출력
mtcars[mtcars$mpg >30,] ## mpg가 30초과인 행, 열은 전부
mtcars[mtcars$mpg > 30 & mtcars$cyl == 4, 10:11] ##mpg가 30초과이면서 cyl이 4인 행 그리고 열은 10부터 11열까지


tapply(mtcars$mpg, mtcars$am, mean) ## am 그룹 별 평균
aggregate(mpg~am+cyl,data=mtcars,mean) ## am, cyl 그룹별 평균
table(transmission=mtcars$am, cylinder=mtcars$cyl)  ## am/cyl 빈도수


library(data.table)
dt <- data.table(mtcars)
class(dt)

dt[,mean(mpg)]   # 칼럼이름으로 바로 함수 적용 가능 / mpg평균
mtcars[,mean(mpg)]  # 일반적인 dataframe에는 적용 불가능 / 에러 출력


#######################
## Data[i,j,by]   Data[subset Row, subset Column, Within Group]
dt[,mean(mpg),by=am] ## am별 mpg 평균
## tapply(mtcars$mpg, mtcars$am, mean) 와 같은 결과 
dt[,mean(mpg),by=.(am,cyl)] ## am별 cyl 별 mpg 평균
## aggregate(mpg~am+cyl,data=mtcars,mean) 와 같은 결과
dt[,.(avg=mean(mpg)),by=.(am,cyl)] ## mpg평균의 label을 avg로 붙임 

dt[mpg > 20,.(avg=mean(mpg)),by=.(am,cyl)] ## mpg20 초과 인 데이터 중 am별 cyl별 mpg 평균 


dt[,.N] ## nrow(dt)
dt[,.N,by=cyl] ## cyl별 빈도수 table(dt$cyl)
dt[wt > 1.5, .(count=.N), by=am] ## wt 가 1.5 이상인것 중 am별 count
dt[wt > 1.5, .N, by=am] ## 같은결과 label만 없음 

dt[order(-mpg)][1:5] ## mpg순으로 정렬(내림차)
dt[order(-mpg,-wt)][1:5] ## mpg, wt순으로 정렬(내림차순)

dt[,.(mpg,wt,vs,am)] ## 네개의 칼럼
dt[,!("mpg")] ## mpg제외
dt[,!c("mpg","wt","vs")] ## mpg, wt, vs제외 

setnames(dt,c("wt"),c("wt2")) ## column이름 재 설정
dt



  
mydata = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")
setkey(mydata, origin) ## origin is key
system.time(mydata[origin %in% c("JFK", "LGA")])
system.time(mydata[c("JFK", "LGA")]) # more fast

setkey(mydata, origin, dest)
mydata[.("JFK", "MIA")] ## origin = JFK, dest = MIA
mydata[origin == "JFK" & dest == "MIA"] ## same code
key(mydata)


mydata01 = setorder(mydata, origin) ## sorting(오름차순)
mydata02 = setorder(mydata, -origin)  ## 내림차순

mydata[, dep_sch:=dep_time - dep_delay] ## dep_sch 칼럼추가 


mydata[, c("dep_sch","arr_sch"):=list(dep_time - dep_delay, arr_time - arr_delay)] ## multi column 추가 


mydata[, dep_sch:=dep_time - dep_delay][,.(dep_time,dep_delay,dep_sch)] 
mydata[, .(mean = mean(arr_delay, na.rm = TRUE), ## arr_delay에 대한 통계 뽑기 
           median = median(arr_delay, na.rm = TRUE),
           min = min(arr_delay, na.rm = TRUE),
           max = max(arr_delay, na.rm = TRUE))]

mydata[, lapply(.SD, mean), .SDcols = c("arr_delay", "dep_delay")] ## SD = Subset of Data
## arr_dely와 dep_deplay의 평균 
mydata[, lapply(.SD, mean)]
mydata[, sapply(.SD, function(x) c(mean=mean(x), median=median(x)))]
mydata[, .(mean_arr_delay = mean(arr_delay, na.rm = TRUE)), by = origin] ## origin으로 묶으서 mean_arr_delay 평균
# origin mean_arr_delay
# 1:    LGA       6.601968
# 2:    JFK       7.731465
# 3:    EWR      10.026121

mydata[, .(mean(arr_delay, na.rm = TRUE), mean(dep_delay, na.rm = TRUE)), by = origin]
# origin        V1       V2
# 1:    LGA  6.601968 10.60500
# 2:    JFK  7.731465 11.44617
# 3:    EWR 10.026121 15.21248



setkey(mydata, "carrier") ## carrier 변수에 대해 중복제거
unique(mydata)

setkey(mydata, NULL) ## 모든 변수에 대해 중복제거 
unique(mydata)

mydata[, .SD[1:2], by=carrier] ## carrier 변수 별 1,2번째 데이터 보이기
mydata[, .SD[.N], by=carrier] ## carrier 변수별 마지막 데이터 보이기 
dat = mydata[, cum:=cumsum(distance), by=carrier]  ## carrier별 distance 누적합계
data.table(dat)


DT <- data.table(A=1:5)
DT[ , X := shift(A, 1, type="lag")]
DT[ , Y := shift(A, 1, type="lead")]
DT
#    A  X  Y
# 1: 1 NA  2
# 2: 2  1  3
# 3: 3  2  4
# 4: 4  3  5
# 5: 5  4 NA


DT = data.table(x=6:10)
DT[x %between% c(7,9)] 

DT = data.table(Name=c("dep_time","dep_delay","arrival"), ID=c(2,3,4))
DT[Name %like% "dep"] ## dep이 들어간 글자만 찾기 

(dt1 <- data.table(A = letters[rep(1:3, 2)], X = 1:6, key = "A"))
(dt2 <- data.table(A = letters[rep(2:4, 2)], Y = 6:1, key = "A"))
merge(dt1, dt2, by="A") # merge
merge(dt1, dt2, by="A", all.x = TRUE) # left join
merge(dt1, dt2, by="A", all.y = TRUE) # right join
merge(dt1, dt2, all=TRUE) # full join

