library(readr)
tw_SARS_case <- read_csv("./unemployment.disease/data/Age_County_Gender_SARS.csv")
tw_SARS_case <- tw_SARS_case[,c(2,3,4,5,9)]
colnames(tw_SARS_case) <- c("year","month","city","district","cases")

tw_SARS_summary <- tw_SARS_case %>%
    group_by(month) %>%
    summarize(cases = sum(cases))

month <- c(1,7:11)
cases <- rep(0, length(month))
df <- as.data.frame(cbind(month, cases))

tw_SARS_summary <- rbind(tw_SARS_summary,df) %>%
    arrange(month)

# write_csv(tw_SARS_summary,"./unemployment.disease/data/SARS_cases.csv")
    
    
    # mutate(month=ifelse(month %in% 1:9 ,paste(year,"/0",month,sep = ""),paste(year,"/",month,sep = "")))
    