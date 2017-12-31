# setwd("C:/Users/user/rlads_communicate/communicate.web.source")
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(dygraphs)
library(flexdashboard)
library(RColorBrewer)
library(scales)
library(xts)

# hue_pal()(4) # function to generate ggplot palette, 4 indicates 4 colors

h1n1_global <- read_csv("./unemployment.disease/data/h1n1_global_08_11.csv")

taiwan_tour_month <- read_csv("./unemployment.disease/data/taiwan_tour_byMonth.csv")

## function: tour to Asisan countries by month
tour_year_country <- function(date=c(98,97),country="Korea") {
    tour_year_country <- taiwan_tour_month %>%
        filter(T_Y==date[1]|T_Y==date[2]) %>%
        filter(grepl(country,Country)) %>% # "中國|日本|韓國|香港|澳門"
        # filter(grepl("China|Japan|Korea",Country))
        rename("Count"=Cases)
    }

h1n1_country <- function(country="Republic of Korea") {
    h1n1_country <- h1n1_global %>%
        filter(grepl(country,Country)) #Republic of Korea|China|Japan|Thailand|Indonesia|Singapore|Malaysia
        # filter(grepl("Republic of Korea|China|Japan",Country))
        # China|Japan|Republic of Korea|Hong Kong|Thailand|Indonesia|Singapore|Malaysia
}

#---Plotting data---------
tour_2009_10_Korea <- tour_year_country(c(98,99),"Korea") %>% select(YMD, Count)
h1n1_Korea <- h1n1_country("Republic of Korea") %>% select(YMD, Cases)
Korea_09_10 <- left_join(tour_2009_10_Korea,h1n1_Korea,by="YMD") %>% mutate(Count=Count/10000)
Korea_09_10 <- xts(Korea_09_10, order.by=(Korea_09_10$YMD))[,-1]

tour_2007_08_Korea <- tour_year_country(c(96,97),"Korea") %>% select(YMD, Count)
h1n1_Korea <- h1n1_country("Republic of Korea") %>% select(YMD, Cases)
Korea_07_08 <- left_join(tour_2007_08_Korea,h1n1_Korea,by="YMD") %>% mutate(Count=Count/10000)
Korea_07_08 <- xts(Korea_07_08, order.by=(Korea_07_08$YMD))[,-1]

tour_2011_12_Korea <- tour_year_country(c(100,101),"Korea") %>% select(YMD, Count)
h1n1_Korea <- h1n1_country("Republic of Korea") %>% select(YMD, Cases)
Korea_11_12 <- left_join(tour_2011_12_Korea,h1n1_Korea,by="YMD") %>% mutate(Count=Count/10000)
Korea_11_12 <- xts(Korea_11_12, order.by=(Korea_11_12$YMD))[,-1]


tour_2007_08_Japan <- tour_year_country(c(96,97),"Japan") %>% select(YMD, Count)
h1n1_Japan <- h1n1_country("Japan") %>% select(YMD, Cases)
Japan_07_08 <- left_join(tour_2007_08_Japan,h1n1_Japan,by="YMD") %>% mutate(Count=Count/10000)
Japan_07_08 <- xts(Japan_07_08, order.by=(Japan_07_08$YMD))[,-1]

tour_2009_10_Japan <- tour_year_country(c(98,99),"Japan") %>% select(YMD, Count)
h1n1_Japan <- h1n1_country("Japan") %>% select(YMD, Cases)
Japan_09_10 <- left_join(tour_2009_10_Japan,h1n1_Japan,by="YMD") %>% mutate(Count=Count/10000)
Japan_09_10 <- xts(Japan_09_10, order.by=(Japan_09_10$YMD))[,-1]

tour_2011_12_Japan <- tour_year_country(c(100,101),"Japan") %>% select(YMD, Count)
h1n1_Japan <- h1n1_country("Japan") %>% select(YMD, Cases)
Japan_11_12 <- left_join(tour_2011_12_Japan,h1n1_Japan,by="YMD") %>% mutate(Count=Count/10000)
Japan_11_12 <- xts(Japan_11_12, order.by=(Japan_11_12$YMD))[,-1]

#---------------

pl_h1n1_kr_07_08 <- 
    dygraph(Korea_07_08, main = "Korea", group = "7") %>%
        dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
        dySeries("Cases", axis = 'y2', label = "H1N1病例", color = hue_pal()(2)[1]) %>%
        dySeries("Count", axis = 'y', label = "旅遊人數(萬人)", color=hue_pal()(2)[2]) %>%
        dyAxis("y", label = "旅遊人數(萬)",axisLabelColor = hue_pal()(2)[2]) %>%
        dyAxis("y2", label = "H1N1病例",axisLabelColor = hue_pal()(2)[1],independentTicks = TRUE) %>%
        dyRangeSelector(height = 20, strokeColor = "") %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
        dyLegend(labelsSeparateLines=T)

pl_h1n1_kr_09_10 <- 
    dygraph(Korea_09_10, main = "Korea", group = "9") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("Cases", axis = 'y2', label = "H1N1病例", color = hue_pal()(2)[1]) %>%
    dySeries("Count", axis = 'y', label = "旅遊人數(萬人)", color=hue_pal()(2)[2]) %>%
    dyAxis("y", label = "旅遊人數(萬)",axisLabelColor = hue_pal()(2)[2]) %>%
    dyAxis("y2", label = "H1N1病例",axisLabelColor = hue_pal()(2)[1],independentTicks = TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)

pl_h1n1_kr_11_12 <- 
    dygraph(Korea_11_12, main = "Korea", group = "11") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("Cases", axis = 'y2', label = "H1N1病例", color = hue_pal()(2)[1]) %>%
    dySeries("Count", axis = 'y', label = "旅遊人數(萬人)", color=hue_pal()(2)[2]) %>%
    dyAxis("y", label = "旅遊人數(萬)",axisLabelColor = hue_pal()(2)[2]) %>%
    dyAxis("y2", label = "H1N1病例",axisLabelColor = hue_pal()(2)[1],independentTicks = TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)




pl_h1n1_jp_07_08 <-
    dygraph(Japan_07_08, main = "Japan", group = "7") %>%
        dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
        dySeries("Cases", axis = 'y2', label = "H1N1病例", color = hue_pal()(2)[1]) %>%
        dySeries("Count", axis = 'y', label = "旅遊人數(萬人)", color=hue_pal()(2)[2]) %>%
        dyAxis("y", label = "旅遊人數(萬)",axisLabelColor = hue_pal()(2)[2]) %>%
        dyAxis("y2", label = "H1N1病例",axisLabelColor = hue_pal()(2)[1],independentTicks = TRUE) %>%
        dyRangeSelector(height = 20, strokeColor = "") %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
        dyLegend(labelsSeparateLines=T)

pl_h1n1_jp_09_10 <-
    dygraph(Japan_09_10, main = "Japan", group = "9") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("Cases", axis = 'y2', label = "H1N1病例", color = hue_pal()(2)[1]) %>%
    dySeries("Count", axis = 'y', label = "旅遊人數(萬人)", color=hue_pal()(2)[2]) %>%
    dyAxis("y", label = "旅遊人數(萬)",axisLabelColor = hue_pal()(2)[2]) %>%
    dyAxis("y2", label = "H1N1病例",axisLabelColor = hue_pal()(2)[1],independentTicks = TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)

pl_h1n1_jp_11_12 <-
    dygraph(Japan_11_12, main = "Japan", group = "11") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("Cases", axis = 'y2', label = "H1N1病例", color = hue_pal()(2)[1]) %>%
    dySeries("Count", axis = 'y', label = "旅遊人數(萬人)", color=hue_pal()(2)[2]) %>%
    dyAxis("y", label = "旅遊人數(萬)",axisLabelColor = hue_pal()(2)[2]) %>%
    dyAxis("y2", label = "H1N1病例",axisLabelColor = hue_pal()(2)[1],independentTicks = TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)

# dygraph(Japan, main = "Japan", group = "H1N1")
# dygraph(Korea, main = "Korea", group = "H1N1")
