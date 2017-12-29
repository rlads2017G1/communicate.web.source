# setwd("C:/Users/user/rlads_communicate/communicate.web.source")
library(readr)
library(dplyr)
library(lubridate)
library(dygraphs)
library(RColorBrewer)
library(xts)
library(scales)


tour_korea_2015 <- read_csv("./unemployment.disease/data/tour_korea_2015_month.csv")
mers_month <- read_csv("./unemployment.disease/data/korea_mers_month.csv")

tour_korea <- tour_korea_2015 %>%
    filter(Country=="Korea") %>%
    rename("Korea"=Cases) %>%
    select(YMD, Korea)
tour_japan <- tour_korea_2015 %>%
    filter(Country=="Japan") %>%
    rename("Japan"=Cases) %>%
    select(YMD, Japan)

tour_korea_2015 <- left_join(tour_japan,tour_korea,by="YMD")
tour_korea_2015 <- xts(tour_korea_2015,order.by = tour_korea_2015$YMD)[,-1]

mers_month <- xts(mers_month,order.by = mers_month$month_date)[,-1]

pl_MERS_tour <- dygraph(tour_korea_2015, main = "Travelers", group = "MERS") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("Korea", axis = 'y', label = "韓國", color = hue_pal()(2)[2]) %>%
    dySeries("Japan", axis = 'y2', label = "日本", color=hue_pal()(2)[1]) %>%
    dyAxis("y", label = "旅韓人數",axisLabelColor = hue_pal()(2)[2]) %>%
    dyAxis("y2", label = "旅日人數",axisLabelColor = hue_pal()(2)[1],independentTicks = TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts=list(strokeWidth = 3),highlightSeriesBackgroundAlpha=1) %>%
    dyLegend(labelsSeparateLines=T)

pl_MERS_case <- dygraph(mers_month, main = "MERS in Korea", group = "MERS") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("month_death", axis = 'y', label = "死亡人數", color=hue_pal()(4)[4]) %>%
    dySeries("month_case", axis = 'y', label = "病例數", color = hue_pal()(4)[3]) %>%
    dyAxis("y", label = "人數") %>%
    # dyAxis("y", label = "病例數",axisLabelColor = hue_pal()(4)[3],independentTicks = TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha=1) %>%
    dyLegend(labelsSeparateLines=T)
