library(readr)
library(ggplot2)
library(plotly)
library(timevis)

visa_country_tour <- read_csv("./currency/visa_country_tour.csv")
visa <- read_csv("./currency/visa.csv")

pl_visa_country <- ggplot(visa_country_tour)+
    geom_line(mapping=aes(x=Year,y=Count/10000,color=Location))+
    scale_x_continuous(breaks = seq(2000,2015, by=2))+
    scale_y_continuous(breaks=seq(0, 1200, by=50))+
    labs(x="",y="萬人",color="",title="免簽國家旅遊人數")

# ggplotly(pl_visa_country)

colnames(visa) <- c("start","content")
# timevis(visa)
