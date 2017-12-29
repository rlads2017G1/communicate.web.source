library(DT)
library(readr)
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
              pageLength = 14,
              dom = 'tip',
              columnDefs = list(list(className = 'dt-center',targets="_all"))
          )) %>% 
    formatStyle('Year', ## select target column to compare     
                target = 'row',  ## highlight row
                backgroundColor = styleEqual(c("2003","2008","2009"),c('#ffb030','#ffcc00','#ffcc00')))
                    ## gray: for rows with 1.4 at Petal.Length
                    ## yellow: for rows with 1.7 at Petal.Length
}