#'
#' Initialiseer dwhr shiny UI
#'
#' Deze functie moet als eerste aangeroepen worden binnen de shiny app's UI. Alleen te gebruiken in UI.R
#'
#' @export
dwhrInit <- function() {
    
    initGlob()
    shiny::addResourcePath('dwhRs',system.file('www', package = 'dwhr'))
    glob.env$dimUiIds <- c()
    shiny::tagList(
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(script = system.file('www/starExtend.js', package = 'dwhr')),
        
        # Loading message

        div(
            id = "loading-content",
            h2("Laden dashboard ...")
        ),
        
        shiny::tags$head(
            shiny::tags$link(rel = "stylesheet", type = "text/css", href = "dwhRs/app.css"),
            shiny::tags$script(src = "dwhRs/jquery.blockUI.js"),
            shiny::tags$link(rel = "stylesheet", type = "text/css", href = "dwhRs/introjs.min.css"),
            shiny::tags$script(src = "dwhRs/intro.min.js")
        )
    )
}

#'
#'  Creeer dimView object in the UI
#'
#' Creeer de html voor het dimView object in de shiny app's UI.
#'
#' @param starId, uniek id van het sterschema. Dit id moet overeenkomen met de parameter \code{starId} in \code{\link{new.star}}
#' @param dim string, uniek id van the dimView. Dit id moet overeenkomen met de parameter \code{dim} in
#' \code{\link{addDimView}}
#' @param skipTopRow boolean, Als TRUE: eerste regel met Naam, links en presentatie wordt overgeslagen.
#' @param maxHeight an integer, maximum hoogte in pixels voor dit dimView object
#' @param overflowX string, css overflow propery in X richting, bepaalt of er wel of niet een horizontale scrollbar getoond wordt bij overflow. 
#' @param accordion boolean, Als TRUE: dimensie kan in UI verkleint worden via een icon.
#'
#' @export
getDimUI <- function(starId, dim, skipTopRow = FALSE, maxHeight = NULL, overflowX = 'hidden', accordion = FALSE) {
    
    withCallingHandlers({
        assert_is_a_string(starId)
        assert_is_a_string(dim)
        assert_is_a_bool(skipTopRow)
        assert_is_a_number(isNull(maxHeight,0))
        maxHeight <- as.integer(maxHeight)
        assert_is_a_string(overflowX)
        assert_is_subset(overflowX,domains[['cssOverflow']])
        assert_is_a_bool(accordion)
        
        gdim <- getGlobalId(starId,dim)
        gdim %in% glob.env$dimUiIds && stop('duplicate dims')    
    },
    
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    glob.env$dimUiIds <- c(glob.env$dimUiIds,gdim)
    
    if (!is.null(maxHeight)) {
        style <- paste0("overflow-x:", overflowX, "; overflow-y:hidden; height: ", maxHeight, "px;")
    } else {
        style <- paste0("overflow-x:", overflowX, "; overflow-y:hidden;")
    }
    
    acc <- paste0('<td class="db-header" style="width: 16px; cursor: pointer;" onclick="toggleAccordion(this,\'', gdim, 'DwhrPanel\');">', 
                  shiny::icon('chevron-down',lib = 'glyphicon'), 
                  '</td>')
    
    shiny::div(
        id = paste0(gdim,'Dimensie'),
        div(class = 'dwhrAccordion',
            if (!skipTopRow)
                HTML(paste0(
                    '<table width = "100%">'
                    , '<tbody>'
                    , '<tr>'
                    , ifelse(accordion,acc,'')
                    , '<td class="db-header">', shiny::uiOutput(paste0(gdim,"DimName")), '</td>'
                    , '<td class="db-header">', shiny::uiOutput(paste0(gdim,"DimLinks")), '</td>'
                    , '<td class="db-header">', shiny::uiOutput(paste0(gdim,"DimPresList")), '</td>'
                    , '<td class="db-header" style="padding-top: 42px"></td>'
                    , '</tr></tbody></table>')
                ),
            shiny::uiOutput(paste0(gdim,"DimHeader"))),
        div(class = 'dwhrPanel', id = paste0(gdim,'DwhrPanel'),
            shiny::uiOutput(paste0(gdim,"DimBody")),
            shiny::uiOutput(paste0(gdim,'DimFooter'))),
        style = style
    )

}

initGlob <- function() {
    
    options(warnPartialMatchDollar = TRUE)
    
    if (!exists('glob.env', envir = .GlobalEnv, inherit = FALSE)) {
        
        glob.env <- new.env(parent = emptyenv())
        .GlobalEnv$glob.env <- glob.env
        
        isDefinedGlobal <- function(var,default) {
            
            if (exists(var, envir = .GlobalEnv, inherit = FALSE)) {
                glob.env[[var]] <- .GlobalEnv[[var]]
            } else {
                glob.env[[var]] <- default
            }
        }
        
        isDefinedGlobal('securityModel','none')
        isDefinedGlobal('omgeving','ACC')
        isDefinedGlobal('debug',FALSE)
        isDefinedGlobal('debugDims',NULL)
        isDefinedGlobal('debugDumpReactive',FALSE)
        isDefinedGlobal('debounceTimeout',1000)
        isDefinedGlobal('debounceBackgroundColor',"#ffffff")
        isDefinedGlobal('debounceOpacity',0)
        
        glob.env$sessionCount <- 0
        glob.env$dashboardName <- basename(getwd())
        glob.env$dimUiIds <- c()
        glob.env$globalCache <- list()
        glob.env$reservedColumnPatterns <- c('*_fc','*_org','*_tooltip','*_text','*_sort')
        
        glob.env$securityModel %in% c('none','proxy') || dwhrStop('Invalid securityModel')
        
        # account data
        
        credFile <- paste0(getwd(),'/data/dbCred.rds')
        
        if (glob.env$securityModel == 'proxy') {
            
            if (is.na(file.info(credFile)$mtime)) {
                
                dwhrStop(paste0('credentials file: ', credFile, ' not found'))
            }
            
            dbCred <- readRDS(credFile)
            
            omg <- glob.env$omgeving
            
            sql <- paste0("exec R.dbo.get_startpunt '",omg,"'")
            handle <- RODBC::odbcDriverConnect(paste0("DSN=",dbCred[[omg]]$dsn,";DATABASE=R;UID=",dbCred[[omg]]$user,";PWD=",dbCred[[omg]]$pwd))
            glob.env$portalUrl <- RODBC::sqlQuery(handle, sql)$startpunt
    
            glob.env$dbCred <- dbCred
            glob.env$dbCred[[omg]]$handle <- handle
            
        } else {
            if (!is.na(file.info(credFile)$mtime)) {
                
                dbCred <- readRDS(credFile)
                glob.env$dbCred <- dbCred
            }

            glob.env$portalUrl <- 'http://www.example.com'
        }
        
     
        adUserFile <- paste0(getwd(),"/../data/userInfo/ds_ad_user.txt")
        
        if (file.exists(adUserFile)) {
            
            glob.env$adUser <- as.data.table(read.csv(  
                file = paste0(getwd(),"/../data/userInfo/ds_ad_user.txt"),
                header = FALSE,
                sep = ";",
                encoding = 'UTF-8',
                col.names = 
                    c("usr",
                      "functie",
                      "naam"), 
                stringsAsFactors = FALSE))
        } else {
            warning('ad-user file not found')
            
            glob.env$adUser <- data.table(
                usr = character(0),
                functie = character(0),
                naam = character(0),
                stringsAsFactors = FALSE
            )
            
        }
        
    }
    
}

#'
#' get database-handle voor omgeving
#' 
#' database-handle kan worden gebruikt voor sql-queries op de betreffende omgeving. Het bestand dbCred.rds moet aanwezig
#' zijn in de data directory van de shiny app.
#' 
#' @param omg string, naam van te benaderen omgeving.
#' 
#' @return database-handle 
#' @export
getDbHandle <- function(omg) {
    
    withCallingHandlers({
        assert_is_a_string(omg)
    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    if (is.null(glob.env$dbCred))
        return()
    
    omg %in% names(glob.env$dbCred) || dwhrStop(paste0('No credentials for omg:', omg))
    
    if (is.null(glob.env$dbCred[[omg]]$handle)) {
        
        dbCred <- glob.env$dbCred
        glob.env$dbCred[[omg]]$handle <- RODBC::odbcDriverConnect(paste0("DSN=",dbCred[[omg]]$dsn,";DATABASE=R;UID=",dbCred[[omg]]$user,";PWD=",dbCred[[omg]]$pwd))
        
    }
    
    glob.env$dbCred[[omg]]$handle
    
}
 

#' authenticate dwhr session.
#' 
#' authenticeren van de gebruiker. Wordt gestuurd door de globale variable securityModel. 
#' 
#' @return als geauthenticiteerd dan TRUE ander FALSE.
#' @export
authenticate <- function(session) {
    
    'ShinySession' %in% class(session) || stop('session is not of class shinySession')
    
    ce <- parent.frame() 
    
    session$onSessionEnded(function() {
        glob.env$sessionCount <- glob.env$sessionCount - 1
        print(paste0('exit: ',glob.env$sessionCount))    
        
        if(exists(paste0('sessionEndHook'),envir = ce)) {
            do.call(paste0('sessionEndHook'),list(session = session),envir = ce)
        }
        
        if (glob.env$sessionCount == 0) {
            
            print('Closing ODBC connections')
            RODBC::odbcCloseAll()
            
            if (exists('globalCache', env = glob.env)) {
                for (id in names(glob.env$globalCache)) {
                    saveRDS(glob.env$globalCache[[id]],getCacheFile(id))
                }
            }
            
            rm(glob.env, envir = .GlobalEnv)
            stopApp()
        }
        
    })
    
    ses <- session$userData
    
    if (exists('authenticated', envir = ses))
        return(ses$authenticated)
    
    ses$authenticated <- FALSE
    ses$cdata <- session$clientData
    ses$urlQuery <- parseQueryString(shiny::isolate(ses$cdata$url_search))
    ses$baseUrl <- ''
    
    glob.env$sessionCount <- glob.env$sessionCount + 1
    
    if (glob.env$securityModel == 'none') {
        ses$authenticated <- TRUE
        ses$dashUser <- 'dev'
        ses$dashUserName <- 'dev'
        ses$dashUserFunc <- 'ontwikkelaar'
        return(TRUE)
    }

    if(!shiny::serverInfo()[[1]]) {
        user <- 'dev'
        hash <- '123456'
        ts <- 0
    } else {
        user <- ses$urlQuery$u
        hash <- ses$urlQuery$h
        ts <- ses$urlQuery$t
    }

    omg <- glob.env$omgeving
    sql <- paste0("exec R.dbo.check_hash '",user,"','",glob.env$dashboardName,"','",ts,"','",hash,"','",omg,"'")
    res <- RODBC::sqlQuery(glob.env$dbCred[[omg]]$handle, sql)
    
    if (res$status %in% c(3,4,5)) {
        
        if (is.na(res$redir) || res$redir == '') {
            shinyjs::runjs(paste0('window.top.location.replace("',glob.env$portalUrl,'");'))
        } else {
            shinyjs::runjs(paste0('window.top.location.replace("',res$redir,'");'))
        }
        ses$authenticated <- FALSE
        ses$dashUser <- 'dev'
        ses$dashUserName <- 'dev'
        ses$dashUserFunc <- 'ontwikkelaar'
        return(FALSE)
    }
    
    if (shiny::serverInfo()$edition != 'OS') {
        ses$baseUrl <- shiny::isolate(paste0(
            ses$cdata$url_protocol,'//',
            ses$cdata$url_hostname,':',
            ses$cdata$url_port,
            '/boRedir?docId=',res$docid,'&server=',res$server))
        
        updateQueryString(paste0('/boRedir?docId=',res$docid,'&server=',res$server),'replace')
    }
    
    ses$authenticated <- TRUE
    ses$dashUser <- user
    adU <- glob.env$adUser[glob.env$adUser$usr == toupper(user),]
    ses$dashUserName = adU$naam
    ses$dashUserFunc = adU$functie
    return(TRUE)

}

#' portalUrl
#' 
#' Geeft de url van de portal waarin dit dashboard gepubliceerd wordt
#' 
#' @export
#' 
portalUrl <- function(){
    glob.env$portalUrl
}
