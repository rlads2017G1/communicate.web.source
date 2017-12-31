# setwd("C:/Users/user/rlads_communicate/communicate.web.source")
library(dplyr)
library(readr)
library(stringr)
library(stringi)
library(ggplot2)
library(plotly)
library(lubridate)
options(scipen=999)

#Transform to SARS_tour_data.csv
#---------------------------------------------------------
lo<- function(x) {str_replace_all(x,"合計","地區")}

## tour Country
tour_country <- function(country) {
    tour_country <- tour %>%
        filter(grepl(country, Location))
}

tour <- read_csv("C:/Users/user/DS_project/liao/tour.csv", col_names = FALSE) %>%
    rename("Year"=X1,"Location"=X2,"Count"=X3) %>%
    mutate(year=as.Date(paste(Year+1911,"-01-01", sep=""))) %>%
    mutate(Location=lo(Location)) #將"合計"統一為"地區"


non_region <- tour %>%
    filter(!grepl("地區",Location)) 

other_region <- tour %>%
    filter(Location=="其他地區")

tour_total <- bind_rows(non_region, other_region) %>%  #旅遊總人數
    group_by(Year) %>%
    summarise(Count=sum(Count,na.rm = TRUE)) %>%
    mutate(Year=Year+1911)%>%
    mutate(Location="總人數") %>%
    select(Year,Location,Count)

tour_continent <- tour %>%
    filter(grepl("地區",Location)) %>%
    filter(!grepl("其他地區",Location))

tour_asia <- tour_continent %>%
    group_by(Location, Year) %>%
    summarise(Count=sum(Count,na.rm = TRUE)) %>%
    mutate(Year=Year+1911)%>%
    filter(Location=="亞洲地區")%>%
    select(Year,Location,Count)


tour_asia_country <- tour_country("中國|香港|澳門") %>%
    mutate(Year=Year+1911)%>%
    select(Year,Location,Count)


x <- full_join(tour_asia_country,tour_asia)
asia_asia.country_total <- full_join(x,tour_total)

# write_csv(asia_asia.country_total,"./unemployment.disease/data/SARS_tour_data.csv")
#---------------------------------------------

#Transform depatarture from taiwan airport 2002, 2003
#-----------------------------------------------
rp <- function(x) {stri_replace_all_fixed(x,"%","")}



depart <- read_csv("./unemployment.disease/data/tai_depart_2003.csv")[,1:4]
colnames(depart) <- c("month","2003.d.count","2002.d.count","growth")
depart <- depart %>%
    filter(month != "小計") %>%
    mutate(growth=as.numeric(rp(growth))) %>%
    mutate(month=1:12)

# write_csv(depart,"./unemployment.disease/data/depart.cleaned.csv")
    

