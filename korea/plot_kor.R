library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(dygraphs)

kor_age_tour <- read_csv("./korea/kor_age_tour.csv")
kor_gender_tour <- read_csv("./korea/kor_gender_tour.csv")
kor_tour <- read_csv("./korea/kor_tour.csv")

jp_age_tour <- read_csv("./korea/jp_age_tour.csv")
jp_gender_tour <- read_csv("./korea/jp_gender_tour.csv")
jp_tour <- read_csv("./korea/jp_tour.csv")

# pl_kor_age_tour
# pl_kor_gender_tour
# pl_jp_age_tour
# pl_jp_gender_tour
# pl_total

## plot kor age group-------------
for (i in 2:ncol(kor_age_tour)) {kor_age_tour[,i] <- kor_age_tour[,i]/10000}
pl_kor_age_tour <- dygraph(kor_age_tour, main = "旅韓人數：按年齡",group="age") %>%
    dyCSS("dygraph.css") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("-12", axis = 'y', label = "  -12", color = hue_pal()(7)[1]) %>%
    dySeries("13-19", axis = 'y', label = "13-19", color = hue_pal()(7)[2]) %>%
    dySeries("20-29", axis = 'y', label = "20-29", color = hue_pal()(7)[3]) %>%
    dySeries("30-39", axis = 'y', label = "30-39", color = hue_pal()(7)[4]) %>%
    dySeries("40-49", axis = 'y', label = "40-49", color = hue_pal()(7)[5]) %>%
    dySeries("50-59", axis = 'y', label = "50-59", color = hue_pal()(7)[6]) %>%
    dySeries("60-", axis = 'y', label = "60-  ", color = hue_pal()(7)[7]) %>%
    dyAxis("y", label = "人數(萬)") %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)

## plot jp age group-------------
for (i in 2:ncol(jp_age_tour)) {jp_age_tour[,i] <- jp_age_tour[,i]/10000}
pl_jp_age_tour <- dygraph(jp_age_tour, main = "旅日人數：按年齡",group="age") %>%
    dyCSS("dygraph.css") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("-12", axis = 'y', label = "  -12", color = hue_pal()(7)[1]) %>%
    dySeries("13-19", axis = 'y', label = "13-19", color = hue_pal()(7)[2]) %>%
    dySeries("20-29", axis = 'y', label = "20-29", color = hue_pal()(7)[3]) %>%
    dySeries("30-39", axis = 'y', label = "30-39", color = hue_pal()(7)[4]) %>%
    dySeries("40-49", axis = 'y', label = "40-49", color = hue_pal()(7)[5]) %>%
    dySeries("50-59", axis = 'y', label = "50-59", color = hue_pal()(7)[6]) %>%
    dySeries("60-", axis = 'y', label = "60-  ", color = hue_pal()(7)[7]) %>%
    dyAxis("y", label = "人數(萬)") %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)

## plot kor gender -------------------
for (i in 2:ncol(kor_gender_tour)) {kor_gender_tour[,i] <- kor_gender_tour[,i]/10000}
pl_kor_gender_tour <- dygraph(kor_gender_tour, main = "旅韓人數：按性別",group="gender") %>%
    dyCSS("dygraph.css") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("female", axis = 'y', label = "女性",strokeWidth=2,color = hue_pal()(2)[1]) %>%
    dySeries("male", axis = 'y', label = "男性",strokeWidth=2,color = hue_pal()(2)[2]) %>%
    dyAxis("y", label = "人數(萬)") %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)

## plot jp gender------------
for (i in 2:ncol(jp_gender_tour)) {jp_gender_tour[,i] <- jp_gender_tour[,i]/10000}
pl_jp_gender_tour <- dygraph(jp_gender_tour, main = "旅日人數：按性別",group="gender") %>%
    dyCSS("dygraph.css") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("female", axis = 'y', label = "女性",strokeWidth=2,color = hue_pal()(2)[1]) %>%
    dySeries("male", axis = 'y', label = "男性",strokeWidth=2,color = hue_pal()(2)[2]) %>%
    dyAxis("y", label = "人數(萬)") %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)



## plot jp kor total----------
kor_tour <- kor_tour %>% filter(year>=1994)
total <- left_join(kor_tour,jp_tour,by="year")
colnames(total)[2:3] <- c("korea","japan")
for (i in 2:ncol(total)) {total[,i] <- total[,i]/10000}

pl_total <- dygraph(total, main = "韓/日 旅遊人數") %>%
    dyCSS("dygraph.css") %>%
    dyOptions(axisLabelFontSize = 12, axisLineWidth = 0.8, drawGrid=F) %>%
    dySeries("korea", axis = 'y', label = "韓國",strokeWidth=1.5,color = hue_pal()(2)[2]) %>%
    dySeries("japan", axis = 'y2', label = "日本",strokeWidth=1.5,color = hue_pal()(2)[1]) %>%
    dyAxis("y", label = "旅韓人數(萬)",axisLabelColor=hue_pal()(2)[2]) %>%
    dyAxis("y2", label = "旅日人數(萬)",axisLabelColor=hue_pal()(2)[1],independentTicks =TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightSeriesBackgroundAlpha = 1) %>%
    dyLegend(labelsSeparateLines=T)

pl_total_2 <- ggplot(data=total)+
    geom_line(mapping = aes(x=year,y=korea,color ="韓國"))+
    geom_line(mapping = aes(x=year,y=japan,color="日本"))+
    scale_x_continuous(breaks=seq(1994,2015, by=2))+
    scale_y_continuous(breaks=seq(0, 400, by=50))+
    labs(title = "韓/日 旅遊人數",x="",y="人數(萬) \n ",color="")

# ggplotly(pl_total_2)


