library(DT)
library(timevis)
library(readr)
library(dplyr)
library(knitr)

rankingkable <- read_csv("./china/rankingkable.csv") %>%
    arrange(desc(Year))

# a custom table with both header and footer
sketch = htmltools::withTags(table(
    tableHeader(rankingkable),
    tableFooter(rankingkable)
))

## Highlight row in Table
table <- function() {
    datatable(rankingkable, rownames = FALSE, container = sketch,
          options = list(
              pageLength = 20,
              dom = 'tip',
              columnDefs = list(list(className = 'dt-center',targets="_all"))
          )) %>% 
    formatStyle('Year', ## select target column to compare     
                target = 'row',  ## highlight row
                backgroundColor = styleEqual(c("2003","2008","2009"),c('#ffb030','#ffcc00','#ffcc00')))
                    ## gray: for rows with 1.4 at Petal.Length
                    ## yellow: for rows with 1.7 at Petal.Length
}

## Flight between strait

flight_ch <- read_csv("./china/flight_ch.csv")
# timevis(flight_ch)

flight_tb <- flight_ch[,c(3,2)]
colnames(flight_tb) <- c("日期","事件")

table2 <- function() {
    datatable(flight_tb, rownames = FALSE,
              options = list(
                  pageLength = 6,
                  dom = 'tip',
                  columnDefs = list(list(className = 'dt-center',targets="_all"))
              ))
}