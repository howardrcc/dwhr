library(assertive)

x <- c()
for (i in 20120101:20201212) { if (is_date_string(as.character(i),format = '%Y%m%d')) x <- c(x,i)}


z <- data.frame(
    dateId = x,
    level1Code = x %/% 10000,
    level1Label = as.character(x %/% 10000),
    level2Code = x %/% 100,
    level2Label = as.character(x %/% 100),
    level3Code = x,
    level3Label = as.character(as.Date(as.character(x),format = '%Y%m%d'), format = '%Y-%m-%d'),
    stringsAsFactors = FALSE)

saveRDS(z,file = 'dates.rds')