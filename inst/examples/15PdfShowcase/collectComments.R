library(data.table)

commentCheck <- function(zz) {
    
    if (!'gsCode' %in% names(zz)) {
        zz$gsCode <- '0'
        zz$kpiCode[grep('^X',zz$kpiCode, invert = TRUE)] <- paste0(zz$kpiCode[grep('^X',zz$kpiCode, invert = TRUE)],'|kpi')
    }
    
    if ('periodemaandId' %in% names(zz)) {
        zz$periodemaandId <- as.character(zz$periodemaandId)
        names(zz)[names(zz) == 'periodemaandId'] <- 'perCode'
    }
    
    if (!'perType' %in% names(zz)) {
        zz$perType <- 'maand'
    }
    
    zz$kostenplaats <- sprintf('%06d',as.integer(zz$kostenplaats))
    
    zz <- zz[!(type == 'conc' & perCode >= 202001)]
    zz <- zz[!(perCode < 201900)]
    
    zz
}

comments <- NULL
dir <- paste0(getwd(),'/archief/', as.character(Sys.Date()))

if (!dir.exists(dir)) {
    
    if (length(Sys.glob(paste0(getwd(),'/tmp/*Comments'))) > 1) {
        
        dir.create(dir)
        
        for (file in Sys.glob(paste0(getwd(),'/tmp/*Comments'))) {
            
            tmp <- readRDS(file)

            file.rename(file,paste0(getwd(),'/archief/',as.character(Sys.Date()),'/',basename(file)))
            
            comments <- rbind(
                comments,
                commentCheck(tmp)
            )    
            
        }
        
        comments <- comments[
            comments[, list(lastUpdateDate = max(lastUpdateDate)), 
                     by = c('kpiCode','kostenplaats','perCode','perType','gsCode','volgnr','subVolgnr','type')
                     ],
            on = c('kpiCode','kostenplaats','perCode','perType','gsCode','volgnr','subVolgnr','type','lastUpdateDate'), 
            nomatch = 0
            ]
        
        comments$verzameld <- TRUE
        comments <- unique(comments[trimws(txt) != '',])
        
        file <- paste0(getwd(),'/tmp/allComments')
        
        saveRDS(comments,file)
        
        print('ok')
        
    } else {
        
        print('Niets te doen')
    }
    
} else {
    print(paste0(dir,' bestaat al'))
}