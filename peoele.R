library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(XML)
# 2022년 11월 부터 2023년 10월 까지 1년의 9시까지 도착기록 데이터가 들어있는 파일을 모아놓은 폴더
files <-dir("./home")
# home에 들어있는 9시까지 도착하는 파일을 전부 불러온다
df1 <- ldply(as.list(paste0("./home/",files)),read.csv)
df1
# 직장인 출근 시간이기 때문에 휴일인 토요일과 월요일 자료를 배제한다
df2 <- df1 %>% filter(요일 %in% c("월","화","수","목","금"))
# 일을 하는 직장인을 기준으로 잡았으니 미성년자와 노년의 정보를 배제하여준다
df3 <- df2 %>% filter(20<=나이 & 나이<=60)
# 이동인구수의 합의 *로 되어있는 이상치를 제거한다
df4 <- df3 %>% filter(이동인구.합.!="*")
# 도착 지역이 TOP5인것만 골라준다
df5 <- df4 %>% filter(도착.행정동.코드 %in% c("1122056","1122057","1122058","1123077","1123080"))
# 출발지역이 서울인것만 골라준다
df6 <- df5 %>% filter(substr(출발.행정동.코드,1,2)=="11")
df7 <- df6 %>% select(-대상연월,-요일,-도착시간,-성별,-나이,-이동유형)
df7$평균.이동.시간.분. <- as.numeric(df7$평균.이동.시간.분.)
df7$이동인구.합. <- as.numeric(df7$이동인구.합.)
str(df7)
colnames(df7) <- c("start", "end", "move_m", "pep")
df8 <- df7 %>% group_by(start,end) %>% summarise(avgw = weighted.mean(move_m, pep), .groups = "drop")
df9 <-as.data.frame(df7)

df_1 <- read_excel("./home8/20210907.xlsx")
df_1 %>%filter(name %in% c("반포본동","반포2동","반포1동","압구정동","개포2동"))
df_2 <- as.data.frame(df_1)
head(df9,4)
head(df_2,4)
total1 <- merge(df8, df_2, by.x = "start", by.y = "읍면동", all.x = TRUE)
total1
total2 <- total1 %>% select(-시도,-시군구,-full_name,-start)
total2
colnames(total2) <- c("end","avgw","start")
total3 <- merge(total2, df_2, by.x = "end", by.y = "읍면동", all.x = TRUE)
total3
total4 <- total3 %>% select(-시도,-시군구,-full_name,-end)
colnames(total4) <- c("avgw","start","end")
total5 <- total4 %>% group_by(start,end) %>% summarise(avg = mean(avgw))
total6 <- as.data.frame(total5)
start_end <- total6


dir.create("start_end")
save(start_end, file="./start_end/start_end.rdata")
write.csv(start_end,"./start_end/start_end.csv")
