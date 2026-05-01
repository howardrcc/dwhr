library(data.table)

comments <- NULL

for (file in Sys.glob(paste0(getwd(),'/tmp/*Comments'))) {
    
    tmp <- readRDS(file)
    
    if (!'verzameld' %in% names(tmp)) {
        tmp$verzameld <- FALSE
        saveRDS(tmp,file)
    }
    
    if (basename(file) != 'allComments') {
        file.rename(file,paste0(getwd(),'/archief/',basename(file)))
    }
    
    comments <- rbind(
        comments,
        tmp
    )    
    
}

comments <- comments[
    comments[, list(lastUpdateDate = max(lastUpdateDate)), 
             by = c('kpiCode','kostenplaats','periodemaandId','volgnr','subVolgnr','type')
             ],
    on = c('kpiCode','kostenplaats','periodemaandId','volgnr','subVolgnr','type','lastUpdateDate'), 
    nomatch = 0
    ]

comments$verzameld <- TRUE
comments <- unique(comments[trimws(txt) != '',])
comments$endDate[comments$status == 'Gesloten'] <- '201802' 

file <- paste0(getwd(),'/tmp/allComments')

saveRDS(comments,file)
