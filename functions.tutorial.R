library(DT)

# a custom table with both header and footer
sketch = htmltools::withTags(table(
    tableHeader(iris),
    tableFooter(iris)
))


## Highlight row in Table
datatable(iris, rownames = FALSE, container = sketch,
          options = list(pageLength = 5, dom = 'tip')) %>% 
    formatStyle('Petal.Length', ## select target column to compare     
        target = 'row',  ## highlight row
        backgroundColor = styleEqual(c(1.4, 1.7), c('gray', 'yellow')) 
                        ## gray: for rows with 1.4 at Petal.Length
                        ## yellow: for rows with 1.7 at Petal.Length
    )

