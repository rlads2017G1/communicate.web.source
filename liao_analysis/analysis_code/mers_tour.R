library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(grid)

options(scipen=999)

#----2015 MERS in Korea, count by month-----------
mers_Korea <- read_csv("./data/mers_Korea_cleaned.csv")

mers_month <- mers_Korea %>%
    mutate(month_date=as.Date(paste0("2015-", mers_Korea$month,"-01"),"%Y-%m-%d")) %>%
    group_by(month_date) %>%
    summarise(month_case=sum(case), month_death=sum(death))

m <- c(1:4, 8,9,12)
zero <- rep(0, times = length(m))
df <- as.data.frame(cbind(m,zero,zero))
colnames(df) <- colnames(mers_month)
df <- df %>%
    mutate(month_date=as.Date(paste0("2015-", df$month_date,"-01"),"%Y-%m-%d"))

mers_month <- full_join(df, mers_month) %>%
    arrange(month_date)
#--------------------------------

#------data: Taiwan tour to foreign, count by month--------------
YMD <- function(x){paste(x,'-','01',sep="")}
rp <- function(x) {
    library(stringr)
    str_replace_all(x,"Mainland","China")}

taiwan_tour <- read_csv("./data/taiwan_tour_to_foreign_cleaned.csv") %>%
    mutate(YMD=as.Date(YMD(Y_M)))
taiwan_tour <- taiwan_tour %>%
    separate(Country,c("Ch_Country","Country")) %>%
    mutate(Country=rp(Country))
#-------------------------------------------

tour_korea_2015 <- taiwan_tour %>%
    filter(T_Y==104) %>%
    filter(grepl("Japan|Korea",Country)) %>%
    select(YMD,Cases,Country)

Sys.setlocale("LC_TIME", "C")  # set locale to display date in en

plot_tour <- ggplot(tour_korea_2015,mapping = aes(x=YMD, y=Cases, color=Country))+
    geom_line() +
    geom_point() +
    scale_color_manual(values=c("#4621FF", "#FF0000")) +  
    scale_x_date(labels = function(x) format(x, "%h"), date_breaks="1 month") +
    labs(x = "", y="", colour = "Number of travelers")

plot_mers <- ggplot(data=mers_month)+
    geom_line(mapping = aes(x=month_date,y=month_case, color="Cases")) +
    geom_line(mapping = aes(x=month_date,y=month_death, color="Deaths")) +
    geom_point(mapping = aes(x=month_date,y=month_case, color="Cases")) +
    geom_point(mapping = aes(x=month_date,y=month_death, color="Deaths")) +
    scale_color_manual(values=c("#EE0000", "#FF9122")) +
    scale_x_date(labels = function(x) format(x, "%h"), date_breaks="1 month") +
    labs(x = "", y="", colour = "MERS in Korea")

write_csv(tour_korea_2015,path = "C:/Users/user/rlads_communicate/communicate.web.source/unemployment.disease/data/tour_korea_2015_month.csv")
write_csv(mers_month,path = "C:/Users/user/rlads_communicate/communicate.web.source/unemployment.disease/data/korea_mers_month.csv")


# ggplotly(plot_mers)
# 
# subplot(ggplotly(plot_tour), ggplotly(plot_mers), nrows = 2, shareX = TRUE)
grid.newpage()
grid.draw(rbind(ggplotGrob(plot_tour), ggplotGrob(plot_mers), size = "last"))
