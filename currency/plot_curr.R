# setwd("C:/Users/user/rlads_communicate/communicate.web.source")
library(readr)
library(ggplot2)
library(plotly)
library(tidyr)
library(dygraphs)
library(xts)
library(scales)

curr <- read_csv("./currency/NTD_against_JP_KR.csv") %>%
    separate(MONTH, into = c('year', 'month'), sep =-2) %>%
    mutate(year=1911+as.numeric(year))
curr$date <- paste(curr$year,"-",curr$month,"-01",sep = "")
curr <- curr %>%
    mutate(date=as.Date(date)) %>%
    select(date,NTD_JPY,NTD_KRW)
curr <- xts(curr, order.by = curr$date)[,-1]


pl_curr <- dygraph(curr, main = "2002-2017(月) 台幣兌日/韓匯率") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("NTD_JPY", axis = 'y', label = "日圓",strokeWidth =1.5,color=hue_pal()(2)[1]) %>%
    dySeries("NTD_KRW", axis = 'y2', label = "韓元",strokeWidth =1.5,color = hue_pal()(2)[2]) %>%
    dyAxis("y", label = "日圓/新台幣",axisLabelColor = hue_pal()(2)[1]) %>%
    dyAxis("y2", label = "韓元/新台幣",axisLabelColor = hue_pal()(2)[2],independentTicks = TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)
# pl_curr