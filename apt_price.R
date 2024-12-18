library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(XML)

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
getwd()   # 확인

loc <- read.csv("./01_code/sigun_code/sigun_code.csv", fileEncoding="UTF-8")  #  지역코드
loc$code <- as.character(loc$code) # 행정구역명 문자 변환     
head(loc, 2) # 확인

datelist <- seq(from = as.Date('2022-01-01'), # 시작
                to   = as.Date('2023-12-31'), # 종료
                by    = '1 month')            # 단위
datelist <- format(datelist, format = '%Y%m') # 형식변환(YYYY-MM-DD => YYYYMM) 
datelist[1:3]          # 확인        # 확인

service_key <- "CqPD97BFcqG5g1vWQlwtd4rtunAcqUMxOIuBz5PyF4CDd2o1Oqa%2Fw95dbMW7L6QqDGF942SkRQPAROU2oeHTvA%3D%3D"  # 인증키 입력

url_list <- list() # 빈 리스트 만들기
cnt <-0	           # 반복문의 제어 변수 초깃값 설정

for(i in 1:nrow(loc)){           # 반복: 25개 자치구
  for(j in 1:length(datelist)){  # 반복: 12개월
    cnt <- cnt + 1               # 누적
    url_list[cnt] <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?",
                            "LAWD_CD=", loc[i,1],         # 지역코드
                            "&DEAL_YMD=", datelist[j],    # 수집월
                            "&numOfRows=", 100,           # 한번에 가져올 최대 자료 수
                            "&serviceKey=", service_key)  # 인증키
  } 
}

length(url_list)                # 요청목록 갯수 확인

library(XML)        # install.packages("XML")      
library(data.table) # install.packages("data.table")
library(stringr)    # install.packages("stringr")

raw_data <- list()        # xml 임시 저장소
root_Node <- list()       # 거래내역 추출 임시 저장소
total <- list()           # 거래내역 정리 임시 저장소
dir.create("02_raw_data") # 새로운 폴더 만들기


for(i in 1:length(url_list)){   # 요청목록(url_list) 반복
  raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE,encoding = "utf-8") # 결과 저장
  root_Node[[i]] <- xmlRoot(raw_data[[i]])	# xmlRoot로 추출

  items <- root_Node[[i]][[2]][['items']]  # 전체 거래내역(items) 추출
  size <- xmlSize(items)                   # 전체 거래 건수 확인    
  
  item <- list()  # 전체 거래내역(items) 저장 임시 리스트 생성
  item_temp_dt <- data.table()  # 세부 거래내역(item) 저장 임시 테이블 생성
  Sys.sleep(.1)  # 0.1초 멈춤
  for(m in 1:size){  # 전체 거래건수(size)만큼 반복
    #---# 세부 거래내역 분리   
    item_temp <- xmlSApply(items[[m]],xmlValue)
    item_temp_dt <- data.table(year = item_temp[4],     # 거래 년 
                               month = item_temp[7],    # 거래 월
                               day = item_temp[8],      # 거래 일
                               price = item_temp[1],    # 거래금액
                               code = item_temp[12],    # 지역코드
                               dong_nm = item_temp[5],  # 법정동
                               jibun = item_temp[11],   # 지번
                               con_year = item_temp[3], # 건축연도 
                               apt_nm = item_temp[6],   # 아파트 이름   
                               area = item_temp[9],     # 전용면적
                               floor = item_temp[13])   # 층수 
    item[[m]] <- item_temp_dt}    # 분리된 거래내역 순서대로 저장
  apt_bind <- rbindlist(item)     # 통합 저장
  
  region_nm <- subset(loc, code== str_sub(url_list[i],115, 119))$addr_1 # 지역명 추출
  month <- str_sub(url_list[i],130, 135)   # 연월(YYYYMM) 추출
  path <- as.character(paste0("./02_raw_data/", region_nm, "_", month,".csv")) # 저장위치 설정
  write.csv(apt_bind, path)     # csv 저장
  msg <- paste0("[", i,"/",length(url_list), "] 수집한 데이터를 [", path,"]에 저장 합니다.") # 알림 메시지
  cat(msg, "\n\n")
}   # 바깥쪽 반복문 종료

load("./home3/03_apt_price.rdata")
df_code <- read.csv("./home3/code.csv", fileEncoding = "UTF-8")
df_code
apt_price

apt_pricet <- apt_price

apt_pricet$data <- paste0(apt_price$year,"",apt_price$month,"",apt_price$day)

apt_pricet
apt_pricet <- apt_pricet %>%
  mutate(data = sprintf("%04d%02d%02d", year, month, day))

apt_pricet

apt_pricet$data <- as.Date(apt_pricet$data, format = "%Y%m%d")
apt_pricet <- apt_pricet %>%
  mutate(price = as.numeric(gsub(",", "", price)))

apt_pricet2 <- apt_pricet %>% group_by(data) %>% summarise(mean = mean(price))
apt_pricet2 
apt_pricet3 <-as.data.frame(apt_pricet2)
apt_pricet3

apt_pricet3$data <- as.Date(apt_pricet3$data)

# 평균 가격 변동 그래프
ggplot(apt_pricet3, aes(x = data, y = mean)) +
  geom_point() +
  labs(title = "기간에 따른 평균 가격 변동 그래프",
       x = "날짜",
       y = "평균 가격") +
  stat_smooth(method = 'lm', colour = 'red', linetype = "dashed")
  
 # theme_minimal()





apt_price2 <- merge(apt_price,df_code, by = "code")
apt_price2
apt_price3 <- apt_price2 %>% select(year,month,price,dong_nm,jibun,apt_nm,addr_2,area)
head(apt_price3,4)
apt_price3$juso_jibun <-paste0(apt_price3$addr_2,"",apt_price3$dong_nm,"",apt_price3$jibun,"",apt_price3$apt_nm)
head(apt_price3,3)
apt_price4 <- apt_price3 %>% select(-dong_nm,-jibun,-addr_2,-apt_nm)
head(apt_price4,5)
apt_price4 <- apt_price4 %>%
  mutate(price = as.numeric(gsub(",", "", price)))
apt_price4
class(apt_price4$area)
apt_price4 <- apt_price4 %>% mutate(pricea = round(price/(area/3.3)))
head(apt_price4,10)
apt_price4 <- apt_price4 %>% select(-price,-area)
apt_juso<- data.frame(apt_price4$juso_jibun)
#apt_juso<- data.frame(apt_juso[!duplicated(apt_juso),])
apt_juso
add_list <-list()
nrow(apt_juso)
cnt <- 0
kakao_key = "04e275e96171c710f07dbccebb6188a4"
for(i in 1:nrow(apt_juso)){ 
  #---# 에러 예외처리 구문 시작
  tryCatch(
    {
      #---# 주소로 좌표값 요청
      lon_lat <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
                     query = list(query = apt_juso[i,]),
                     add_headers(Authorization = paste0("KakaoAK ", kakao_key)))
      #---# 위경도만 추출하여 저장
      coordxy <- lon_lat %>% content(as = 'text') %>% RJSONIO::fromJSON()
      #---# 반복횟수 카운팅
      cnt = cnt + 1
      #---# 주소, 경도, 위도 정보를 리스트로 저장
      add_list[[cnt]] <- data.table(apt_juso = apt_juso[i,], 
                                    coord_x = coordxy$documents[[1]]$x, 
                                    coord_y = coordxy$documents[[1]]$y)
      #---# 진행상황 알림 메시지
      message <- paste0("[", i,"/",nrow(apt_juso),"] 번째 (", 
                        round(i/nrow(apt_juso)*100,2)," %) [", apt_juso[i,] ,"] 지오코딩 중입니다: 
       X= ", add_list[[cnt]]$coord_x, " / Y= ", add_list[[cnt]]$coord_y)
      cat(message, "\n\n")
      #---# 예외처리 구문 종료
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}

dir.create("coordxy")
save(add_list, file="./coordxy/coo rdxy.rdata")



aad_list <- list()
for(i in 1:nrow(apt_juso)){ 
  aad_list[[i]] <- data.table(apt_juso = apt_juso[i,])
}

dir.create("sum08")
save(total2, file="./sum08/sum08.rdata")
write.csv(total2,"./sum08/sum08.csv")

asd_list <- add_list
juso <- rbindlist(add_list)
nrow(juso)
add_list[[988]][[3]]
for(i in 1:nrow(juso)){
  url <- paste0("https://dapi.kakao.com/v2/local/geo/coord2regioncode.json?x=", asd_list[[i]][[2]], "&y=", asd_list[[i]][[3]])
  response <- GET(url, add_headers("Authorization" = paste("KakaoAK", kakao_key)))
  result <- content(response, "parsed")
  result
  coordxy <- response %>%  content( as = "text") %>% fromJSON()
  coordxy
  aad_list[[i]] <- data.table(apt_juso = apt_juso[i,], 
                              hang = result$documents[[2]]$region_3depth_name)
}
aad_list
hang <- rbindlist(aad_list,fill = TRUE)
hang
hang <- na.omit(hang)
hang
head(apt_price4,3)
apt_price4

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # 작업폴더 설정
files <- dir("./02_raw_data")    # 폴더 내 모든 파일 이름 읽기
library(plyr)               # install.packages("plyr")
apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv) # 모든 파일 하나로 결합
tail(apt_price, 2)  # 확인

load("./hang/hang.rdata")
load("./home3/03_apt_price.rdata")
df_code <- read.csv("./home3/code.csv", fileEncoding = "UTF-8")
df_code
apt_price
hang <-as.data.frame(hang)
apt_price2 <- merge(apt_price,df_code, by = "code")
apt_price2
apt_price3 <- apt_price2 %>% select(year,month,price,dong_nm,jibun,apt_nm,addr_2,area)
head(apt_price3,4)
apt_price3$juso_jibun <-paste0(apt_price3$addr_2,"",apt_price3$dong_nm,"",apt_price3$jibun,"",apt_price3$apt_nm)
head(apt_price3,3)
apt_price4 <- apt_price3 %>% select(-dong_nm,-jibun,-addr_2,-apt_nm)
head(apt_price4,5)
apt_price4 <- apt_price4 %>%
  mutate(price = as.numeric(gsub(",", "", price)))
apt_price4
class(apt_price4$area)
apt_price4 <- apt_price4 %>% mutate(pricea = round(price/(area/3.3)))
head(apt_price4,10)
apt_price4 <- apt_price4 %>% select(-price,-area)
apt_price4
colnames(hang) <- c("juso_jibun","heng")
head(apt_price4,4)
head(hang,4)
apt_price5 <- merge(apt_price4,hang, by = "juso_jibun", all.x = TRUE)
apt_price5 <- unique(apt_price5)
head(apt_price6,4)
apt_price6 <- apt_price5 %>% select(pricea,heng)
apt_price6 <- na.omit(apt_price6)
apt_price7 <- apt_price6 %>%group_by(heng) %>%mutate(price_avg = round(mean(pricea))) %>% select(-pricea)
apt_price7 <- unique(apt_price7)
apt_price7 <- as.data.frame(apt_price7)
apt_price8 <- apt_price7[order(-apt_price7$price_avg), ]
apt_price8
apt_price9 <- head(apt_price8,5)
apt_price9
apt_price7
apt_price_top5 <-apt_price9
dir.create("apt_price")
save(apt_price7, file="./apt_price/apt_price.rdata")
write.csv(apt_price7,"./apt_price/apt_price.csv")

save(apt_price_top5, file="./apt_price/apt_price_top5.rdata")
write.csv(apt_price_top5,"./apt_price/apt_price_top5.csv")
