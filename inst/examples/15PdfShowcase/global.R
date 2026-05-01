debug <- FALSE

batchMode <- FALSE
batchArgs <- c('MP','figOnly')

# debounceBackgroundColor <- "#41b6c4"
# debounceOpacity <- 0.1
debounceTimeout <- 10000

if (isTRUE(getOption("shiny.testmode"))) {
    securityModel <- 'none'
    
    if (!file.exists(paste0(getwd(),'/script/batchArgs.rds'))) {
        stop('missing batchArgs.rds')
    }
    
    batchArgs <- readRDS(paste0(getwd(),'/script/batchArgs.rds'))
    batchMode <- TRUE
    
}



