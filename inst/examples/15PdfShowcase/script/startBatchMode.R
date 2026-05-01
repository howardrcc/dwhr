args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop('Missend script argument', call. = FALSE)
}

if (!args[1] %in% c('MR','MP','CACHE')) {
    stop('Eerste argument mag zijn: MR, MP')
}

#
#  MR: Maandrapportage
#  MP: Meetplan
#

saveRDS(args,paste0(getwd(),'/batchArgs.rds'))

library(shinytest)

setwd(paste0(getwd(),'/../'))
app <- ShinyDriver$new('.')

app$waitFor('batchStateFinished;',checkInterval = 1000, timeout = 4e6)
log <- app$getDebugLog("shiny_console")
print(log)

app$stop()





