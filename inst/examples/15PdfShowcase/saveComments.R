library(data.table)
library(RODBC)

args = commandArgs(trailingOnly = TRUE)

credFile <- paste0(getwd(),'/data/dbCred.rds')
dbCred <- readRDS(credFile)

source('global.R')
source('func.R')

comments <- initComments()
comments <- unique(comments[trimws(txt) != '',])

if (!(length(args) > 0 && args[1] == 'ALL')) {
    comments <- comments[year(comments$lastUpdateDate) == year(Sys.Date()),]    
}

print(paste0('Omgeving:', omgeving))
print(paste0('nrows:', nrow(comments)))

if (nrow(comments) > 0) {
    
    handle <- RODBC::odbcDriverConnect(paste0("DSN=",dbCred[[omgeving]]$dsn,";DATABASE=R;UID=",dbCred[[omgeving]]$user,";PWD=",dbCred[[omgeving]]$pwd))
    
    sql <- 'truncate table R.dbo.import_kpirvb_comments'
    importProc <- 'exec R.dbo.insert_kpirvb_comment'
    #mergeProc <- "exec ETL_DM.dbo.merge_fact @inc_db  = 'R', @inc_tab = 'import_kpirvb_comments' ,@cum_db  = 'R' ,@cum_tab = 'kpirvb_comments' ,@no_delete = 1"
    mergeProc <- 'exec R.dbo.merge_kpirvb_comment'
    
    z <- sqlQuery(channel = handle, query = sql, as.is = TRUE)
    
    if (is.character(z) && length(z) > 0) {
        stop(paste(z,collapse = '\n'))
    }
    
    for (i in 1:nrow(comments)) {
        
        sql <- paste0(importProc, " ",
                      "@kpi_code = '", comments$kpiCode[i], "', ",
                      "@kostenplaats = '", comments$kostenplaats[i], "', ",
                      "@periodemaand_id = '", comments$periodemaandId[i], "', ",
                      "@gs_code = '", comments$gsCode[i], "', ",
                      "@volgnr = '", comments$volgnr[i], "', ",
                      "@type = '", comments$type[i], "', ",
                      "@status = '", comments$status[i], "', ",
                      "@txt = '", gsub("'","''",comments$txt[i]), "', ",
                      "@creation_date = '", comments$creationDate[i], "', ",
                      "@lastupdate_date = '", comments$lastUpdateDate[i], "', ",
                      "@createdby = '", comments$createdBy[i], "', ",
                      "@updatedby = '", comments$updatedBy[i], "', ",
                      "@subvolgnr = '", comments$subVolgnr[i], "', ",
                      "@enddate = '", comments$endDate[i], "'")
        
        z <- sqlQuery(channel = handle, query = sql, as.is = TRUE)
        
        if (is.character(z) && length(z) > 0) {
            stop(paste(z,collapse = '\n'))
        }
        
        if (length(z) > 0)
            print(z)
        
    }
    
    if (is.character(z) && length(z) > 0) {
        stop(paste(z,collapse = '\n'))
    }
    
    z <- sqlQuery(channel = handle, query = mergeProc, as.is = TRUE)
    
    if (is.character(z) && length(z) > 0) {
        stop(paste(z,collapse = '\n'))
    }
    
    odbcClose(handle)
}

# x <- comments[month(comments$lastUpdateDate) == 4 & month(comments$creationDate) == 3 & type %in% c('maatr','maatrOpm') & periodemaandId == 201803,]
# z <- comments[month(comments$creationDate) == 3 & type %in% c('maatr','maatrOpm') & periodemaandId == 201802,]
# y <- x[z,on = c('kpiCode','kostenplaats','volgnr','type'), nomatch = 0]
# zz <- y[txt != i.txt,c('txt','i.txt')]