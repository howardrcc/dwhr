score <- data.table(
    kostenplaats = kpl$level2Code,
    periodemaandId = 201902,
    score = 'Groen',
    lastUpdateDate = Sys.time(),
    updatedBy = 'dev')

lapply(per$periodemaandId[per$periodemaandId > 201902], function(x){
    
    score <<- rbind(
        score,
        data.table(
            kostenplaats = kpl$level2Code,
            periodemaandId = x,
            score = 'Groen',
            lastUpdateDate = Sys.time(),
            updatedBy = 'dev')
    )
    
})


file <- paste0(getwd(),'/tmp/score')
saveRDS(score,file)