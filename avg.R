library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(XML)

load("./apt_price/apt_price.rdata")
load("./apt_price/apt_price_top5.rdata")
load("./start_end/start_end.rdata")
apt_price_top5
apt_price7
start_end

head(apt_price7,4)
head(start_end,4)

end <- merge(start_end, apt_price7, by.x = "start", by.y = "heng", all.x = TRUE)
end

dir.create("avg_end")
save(end, file="./avg_end/avg_end.rdata")
write.csv(end,"./avg_end/avg_end.csv")

load("./avg_end/avg_end.rdata")
end
end2 <- end

sample <- end
sample <- sample %>% select(-end)
sample2 <- na.omit(sample)

sample2
summary(end)

ggplot(sample2, aes(x=avg,y=price_avg)) +
  geom_point() +
  stat_smooth(method="lm",colour="red",linetype = "dashed")

top1 <- end %>% filter(end == "반포본동")
top2 <- end %>% filter(end == "반포2동")
top3 <- end %>% filter(end == "반포1동")
top4 <- end %>% filter(end == "압구정동")
top5 <- end %>% filter(end == "개포2동")


##### 회귀분석
ggplot(top1, aes(x=price_avg,y=avg)) +
  geom_point() +
  stat_smooth(method="lm",colour="red",linetype = "dashed")         #반포본동
cor.test(top1$avg,top1$price_avg)                                   # p = 9.536e-05  cor = -0.285942

ggplot(top2, aes(x=price_avg,y=avg)) +
  geom_point() +
  stat_smooth(method="lm",colour="red",linetype = "dashed")         #반포2동
cor.test(top2$avg,top2$price_avg)                                   # p = 8.782e-09  cor = -0.2843548
 
ggplot(top3, aes(x=price_avg,y=avg)) +
  geom_point() +
  stat_smooth(method="lm",colour="red",linetype = "dashed")         #반포1동
cor.test(top3$avg,top3$price_avg)                                   # p = <2.2e-16  cor = -0.5627044

ggplot(top4, aes(x=price_avg,y=avg)) +
  geom_point() +
  stat_smooth(method="lm",colour="red",linetype = "dashed")         #압구정동
cor.test(top4$avg,top4$price_avg)                                   # p = <2.2e-16  cor = -0.5469636

ggplot(top5, aes(x=price_avg,y=avg)) +
  geom_point() +
  stat_smooth(method="lm",colour="red",linetype = "dashed")         #개포2동
cor.test(top5$avg,top5$price_avg)                                   # p = 1.376e-14 cor = -0.4007664
