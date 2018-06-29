library(assertive)

x <- c()
for (i in 20120101:20201212) { if (is_date_string(as.character(i),format = '%Y%m%d')) x <- c(x,i)}

z <- data.frame(
    dateId = x,
    level1Label = x %/% 10000,
    level2Label = x %/% 100,
    level3Label = x)

saveRDS(z,file = 'dates.rds')