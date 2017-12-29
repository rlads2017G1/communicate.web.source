# setwd("C:/Users/user/rlads_communicate/communicate.web.source")
library(readr)
library(dplyr)
library(lubridate)
library(dygraphs)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(xts)
library(scales)
options(scipen=999)

tour_year <- read_csv("./unemployment.disease/data/SARS_tour_data.csv") %>% filter(Location!="中國大陸")
depart_2003 <- read_csv("./unemployment.disease/data/depart.cleaned.csv")
depart_2003 <- depart_2003[,-ncol(depart_2003)]


pl_tour_year=ggplot(tour_year,mapping = aes(x=Year, y=Count/10000))+
    geom_line(mapping = aes(color=Location))+
    scale_x_continuous(limits=c(1995,2015) ,breaks=seq(1995,2015,by=2))+
    scale_y_continuous(breaks=seq(0, 1000, by=100))+
    labs(Color="",y="萬人")
# ggplotly(pl_tour_year)


pl_depart_2003 <- dygraph(depart_2003, main = "桃機出境人數",xlab="月份") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("2003.d.count", axis = 'y', label = "2003", color = hue_pal()(2)[1]) %>%
    dySeries("2002.d.count", axis = 'y', label = "2002", color=hue_pal()(2)[2]) %>%
    dyAxis("y", label = "人數") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha=1) %>%
    dyLegend(labelsSeparateLines=T)
# pl_depart_2003

