# set package global variables in pkg.env

pkg.env <- new.env(parent = emptyenv())

.onLoad <- function(libname,pkgname) {

    isDefinedGlobal <- function(var,default) {

        if (exists(var, envir = .GlobalEnv, inherit = FALSE)) {
            pkg.env[[var]] <- .GlobalEnv[[var]]
        } else {
            pkg.env[[var]] <- default
        }
    }

    isDefinedGlobal('securityModel','none')
    isDefinedGlobal('omg','ACC')
    isDefinedGlobal('hasODBC',FALSE)
    isDefinedGlobal('debug',FALSE)
    isDefinedGlobal('debugDims',NULL)
    isDefinedGlobal('debugDumpReactive',FALSE)

    pkg.env$dbhandle <- NULL
    pkg.env$sessionCount <- 0
    pkg.env$dashboardName <- basename(getwd())
    pkg.env$dimUiIds <- c()
    pkg.env$globalCache <- list()
    pkg.env$reservedColumnPatterns <- c('*_fc','*_org','*_tooltip','*_text','*_sort')

    # account data (set in global.R?)

    if (pkg.env$omg == 'PRD') {
        pkg.env$odbcDsn <- 'PRD1'
        pkg.env$odbcUser <- 'sa'
        pkg.env$odbcPwd <- 'saPRD3'
    } else {
        pkg.env$odbcDsn <- 'ACC1'
        pkg.env$odbcUser <- 'etl'
        pkg.env$odbcPwd <- 'etlACC1'
    }

    if (pkg.env$hasODBC) {
        sql <- paste0("exec R.dbo.get_startpunt '",pkg.env$omg,"'")
        pkg.env$dbhandle <- RODBC::odbcDriverConnect(paste0("DSN=",pkg.env$odbcDsn,";DATABASE=R;UID=",pkg.env$odbcUser,";PWD=",pkg.env$odbcPwd))
        pkg.env$portalUrl <- RODBC::sqlQuery(pkg.env$dbhandle, sql)$startpunt
    } else {
        pkg.env$portalUrl <- 'http://www.example.com'
    }

    pkg.env$partialMatchOld <- options(warnPartialMatchDollar = TRUE)

}

# .onUnload <- function(libPath) {
#     options(pkg.env$partialMatchOld)
# }
