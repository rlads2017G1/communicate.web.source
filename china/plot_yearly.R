Sys.setlocale(category = "LC_ALL", locale = "cht")
library(readr)
library(ggplot2)
library(plotly)
rankingline <- read_csv("./china/rankingline.csv") %>%
    rename("Travelers"=`number of visitors`)

## 1997-2016 Tour destination
pl_year_country <- ggplot(rankingline, mapping = aes(x=Year, y=Travelers/10000))+
    geom_line(mapping = aes(color=Location))+
    scale_x_continuous(limits=c(1997,2016) ,breaks=seq(1997,2016,by=2))+
    scale_y_continuous(breaks=seq(0, 500, by=50))+
    labs(title = "1997-2016 台灣人最常去的地方",
         x=" ",y="萬人\n\n",color="") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplotly(pl_year_country)