library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
options(scipen=10000)

##(1)
##此為台灣歷年旅韓圖 : 
##參考圖: Korea_General_Plot.png
##csv 存於 github 上，下 code 可見。

my.tour <- read_csv(url("https://raw.githubusercontent.com/cplalexandtang/R-Data_Science/master/tour.csv"), col_names = FALSE)
Korea_data <- my.tour %>%
  filter(X2 == "韓國")

#Plot of Taiwanese tourist numbers in Korea
Korea_General_Plot <- ggplot() + 
  geom_point(mapping = aes(x = Korea_data$X1, y = Korea_data$X3)) +
  geom_smooth(mapping = aes(x = Korea_data$X1, y = Korea_data$X3)) +
  labs(x = "台灣歷年旅韓人數", y = "")

Korea_General_Plot

kor_tour <- cbind(Korea_data$X1, Korea_data$X3)
kor_tour <- as.data.frame(kor_tour)
colnames(kor_tour) <- c("year","count")
kor_tour <- kor_tour %>% mutate(year=year+1911)
# write_csv(kor_tour,"C:/Users/user/rlads_communicate/communicate.web.source/korea/kor_tour.csv")


##(2)
##此為台灣旅韓性別圖:
##參考圖: Plot_by_Gender.png
##資料: by_gender.csv

by_gender <- read_csv("by_gender.csv", 
                      col_names = FALSE)
by_gender_two <- dplyr::select(by_gender, X1, X2, X3)
by_gender_two <- slice(by_gender_two, 17:39)
my.X1 <- gsub("[(][0-9]+[)]", "", by_gender_two$X1)
factored_my.X1 <- factor(my.X1, levels = my.X1)
my.X2 <- gsub(",", "", by_gender_two$X2, fixed = TRUE)
my.X2 <- as.numeric(my.X2)
my.X3 <- gsub(",", "", by_gender_two$X3, fixed = TRUE)
my.X3 <- as.numeric(my.X3)

ggplot() +
  geom_point(data = by_gender_two, mapping = aes(x = factored_my.X1, y = my.X3, color = "Female")) +
  geom_point(data = by_gender_two, mapping = aes(x = factored_my.X1, y = my.X2, color = "Male")) +
  labs(x = "台灣旅韓性別圖", y = "")

year <- 83:105+1911
kor_gender <- cbind(year, my.X2, my.X3)
kor_gender <- as.data.frame(kor_gender)
colnames(kor_gender) <- c("year","male","female")
# write_csv(kor_gender,"C:/Users/user/rlads_communicate/communicate.web.source/korea/kor_gender_tour.csv")



##(3)
##台灣旅韓年齡圖:
##參考圖: 按年齡.png
##資料: by_age.xls.csv

by_age <- read_csv("by_age.xls.csv")
by_age <- slice(by_age, 18:40)
by_age <- by_age %>%
  rename(X2 = "韓國")
by_age_X2 <- gsub(",", "", by_age$X2, fixed = TRUE)
by_age_X2 <- as.numeric(by_age_X2)
by_age_X3 <- gsub(",", "", by_age$X3, fixed = TRUE)
by_age_X3 <- as.numeric(by_age_X3)
by_age_X4 <- gsub(",", "", by_age$X4, fixed = TRUE)
by_age_X4 <- as.numeric(by_age_X4)
by_age_X5 <- gsub(",", "", by_age$X5, fixed = TRUE)
by_age_X5 <- as.numeric(by_age_X5)
by_age_X6 <- gsub(",", "", by_age$X6, fixed = TRUE)
by_age_X6 <- as.numeric(by_age_X6)
by_age_X7 <- gsub(",", "", by_age$X7, fixed = TRUE)
by_age_X7 <- as.numeric(by_age_X7)
by_age_X8 <- gsub(",", "", by_age$X8, fixed = TRUE)
by_age_X8 <- as.numeric(by_age_X8)
by_age_x <- c(83:105)

ggplot(data = by_age) +
  geom_path(mapping = aes(x = by_age_x, y = by_age_X2, color = "12 and under")) +
  geom_path(mapping = aes(x = by_age_x, y = by_age_X3, color = "13-19")) +
  geom_path(mapping = aes(x = by_age_x, y = by_age_X4, color = "20-29")) +
  geom_path(mapping = aes(x = by_age_x, y = by_age_X5, color = "30-39")) +
  geom_path(mapping = aes(x = by_age_x, y = by_age_X6, color = "40-49")) +
  geom_path(mapping = aes(x = by_age_x, y = by_age_X7, color = "50-59")) +
  geom_path(mapping = aes(x = by_age_x, y = by_age_X8, color = "60 and above")) +
  labs(x = "按年齡區分", y = "") +
  geom_point(mapping = aes(x = by_age_x, y = by_age_X2, color = "12 and under")) +
  geom_point(mapping = aes(x = by_age_x, y = by_age_X3, color = "13-19")) +
  geom_point(mapping = aes(x = by_age_x, y = by_age_X4, color = "20-29")) +
  geom_point(mapping = aes(x = by_age_x, y = by_age_X5, color = "30-39")) +
  geom_point(mapping = aes(x = by_age_x, y = by_age_X6, color = "40-49")) +
  geom_point(mapping = aes(x = by_age_x, y = by_age_X7, color = "50-59")) +
  geom_point(mapping = aes(x = by_age_x, y = by_age_X8, color = "60 and above"))

year <- 83:105+1911
kor_age_tour <- cbind(year,by_age_X2,by_age_X3,by_age_X4,by_age_X5,by_age_X6,by_age_X7,by_age_X8)
kor_age_tour <- as.data.frame(kor_age_tour)
colnames(kor_age_tour) <- c("year","-12","13-19","20-29","30-39","40-49","50-59","60-")
# write_csv(kor_age_tour,"C:/Users/user/rlads_communicate/communicate.web.source/korea/kor_age_tour.csv")




##(4)
##台灣旅日人數圖
##參考圖: To_Japan.png
##資料: T_to_J.csv

T.to.J <- read_csv("T_to_J.csv")
T.to.J <- slice(T.to.J, 15:37)
T.to.J <- rename(T.to.J, X1 = "年份(Years)", X2 = "日本(Japan)", X3 = "小計(Subtotals)")
J.X1 <- gsub("[(][0-9]+[)]", "", T.to.J$X1)
J.X1 <- factor(J.X1, levels = J.X1)



To_Japan <- ggplot(data = T.to.J) +
  geom_point(mapping = aes(x = J.X1, y = X2)) +
  labs(x = "歷年旅日人數", y = "") +
  geom_smooth(mapping = aes(x = J.X1, y = X2, group = 1))

year <- 83:105+1911
jp_tour <- cbind(year, T.to.J)
jp_tour <- as.data.frame(jp_tour)[,c(-2,-4)]
colnames(jp_tour) <- c("year","count")
# write_csv(jp_tour,"C:/Users/user/rlads_communicate/communicate.web.source/korea/jp_tour.csv")



##(4)
##台灣旅日按性別
##參考圖: Japan.by.gen.png
##資料: by.gen.J.csv

by.gen.J <- read_csv("by.gen.J.csv")
by.gen.J <- slice(by.gen.J, 16:38)
by.gen.J <- rename(by.gen.J, M = "日本(Japan)")
by.gen.J <- rename(by.gen.J, F = "小計(Subtotals)")
by.gen.J.M <- gsub(",", "", by.gen.J$M, fixed = T)
by.gen.J.M <- as.numeric(by.gen.J.M)
by.gen.J.F <- gsub(",", "", by.gen.J$F, fixed = T)
by.gen.J.F <- as.numeric(by.gen.J.F)

ggplot() +
  geom_point(mapping = aes(x = 83:105, y = by.gen.J.F, color = "Female")) +
  geom_point(mapping = aes(x = 83:105, y = by.gen.J.M, color = "Male")) +
  geom_path(mapping = aes(x = 83:105, y = by.gen.J.M, color = "Male")) +
  geom_path(mapping = aes(x = 83:105, y = by.gen.J.F, color = "Female")) +
  labs(x = "台旅日按性別區分", y = "")

year <- 83:105+1911
jp_gender <- cbind(year, by.gen.J.M, by.gen.J.F)
jp_gender <- as.data.frame(jp_gender)
colnames(jp_gender) <- c("year","male","female")
# write_csv(jp_gender,"C:/Users/user/rlads_communicate/communicate.web.source/korea/jp_gender_tour.csv")


##(5)
##台灣旅日按年齡
##參考圖: J.by.age.png
##資料: age.J.csv

age.J <- read_csv("age.J.csv")
n.age.J <- slice(age.J, 18:40)
n.age.J <- rename(n.age.J, X2 = "日本")

n.age.X2 <- gsub(",", "", n.age.J$X2, fixed = TRUE)
n.age.X2 <- as.numeric(n.age.X2)
n.age.X3 <- gsub(",", "", n.age.J$X3, fixed = TRUE)
n.age.X3 <- as.numeric(n.age.X3)
n.age.X4 <- gsub(",", "", n.age.J$X4, fixed = TRUE)
n.age.X4 <- as.numeric(n.age.X4)
n.age.X5 <- gsub(",", "", n.age.J$X5, fixed = TRUE)
n.age.X5 <- as.numeric(n.age.X5)
n.age.X6 <- gsub(",", "", n.age.J$X6, fixed = TRUE)
n.age.X6 <- as.numeric(n.age.X6)
n.age.X7 <- gsub(",", "", n.age.J$X7, fixed = TRUE)
n.age.X7 <- as.numeric(n.age.X7)
n.age.X8 <- gsub(",", "", n.age.J$X8, fixed = TRUE)
n.age.X8 <- as.numeric(n.age.X8)

year <- 83:105+1911
n.age <- cbind(year,n.age.X2,n.age.X3,n.age.X4,n.age.X5,n.age.X6,n.age.X7,n.age.X8)
n.age <- as.data.frame(n.age)
colnames(n.age) <- c("year","-12","13-19","20-29","30-39","40-49","50-59","60-")
# write_csv(n.age,"C:/Users/user/rlads_communicate/communicate.web.source/korea/jp_age_tour.csv")

J.by.age <- ggplot() +
  geom_path(mapping = aes(x = 83:105, y = n.age.X2, color = "-12")) +
  geom_path(mapping = aes(x = 83:105, y = n.age.X3, color = "13-19")) +
  geom_path(mapping = aes(x = 83:105, y = n.age.X4, color = "20-29")) +
  geom_path(mapping = aes(x = 83:105, y = n.age.X5, color = "30-39")) +
  geom_path(mapping = aes(x = 83:105, y = n.age.X6, color = "40-49")) +
  geom_path(mapping = aes(x = 83:105, y = n.age.X7, color = "50-59")) +
  geom_path(mapping = aes(x = 83:105, y = n.age.X8, color = "60-")) +
  labs(x = "按年齡區分", y = "") +
  geom_point(mapping = aes(x = 83:105, y = n.age.X2, color = "12 and under")) +
  geom_point(mapping = aes(x = 83:105, y = n.age.X3, color = "13-19")) +
  geom_point(mapping = aes(x = 83:105, y = n.age.X4, color = "20-29")) +
  geom_point(mapping = aes(x = 83:105, y = n.age.X5, color = "30-39")) +
  geom_point(mapping = aes(x = 83:105, y = n.age.X6, color = "40-49")) +
  geom_point(mapping = aes(x = 83:105, y = n.age.X7, color = "50-59")) +
  geom_point(mapping = aes(x = 83:105, y = n.age.X8, color = "60 and above"))





