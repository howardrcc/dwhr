#'
#' Initialiseer dwhr shiny UI
#'
#' Deze functie moet als eerste aangeroepen worden binnen de shiny app's UI
#'
#' @export
dwhrInit <- function() {
    initGlob()
    shiny::addResourcePath('dwhRs',system.file('www', package = 'dwhr'))
    glob.env$dimUiIds <- c()
    shiny::tagList(
        shinyjs::useShinyjs(),
        rintrojs::introjsUI(includeOnly = TRUE),
        shinyjs::extendShinyjs(script = system.file('www/starExtend.js', package = 'dwhr')),
        shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "dwhRs/app.css")),
        
        #includeCSS(system.file('www/app.css', package = 'dwhr')),

        # Loading message

        div(
            id = "loading-content",
            h2("Laden dashboard ...")
        ),
        shiny::tags$head(shiny::tags$script(src = "dwhRs/jquery.blockUI.js"))
    )
}

#'
#' Creeer dimView object in the UI
#'
#' Creeer de html voor het dimView object in de shiny app's UI.
#'
#' @param starId, uniek id van het sterschema. Dit id moet overeenkomen met de parameter \code{starId} in \code{\link{new.star}}
#' @param dim string, uniek id van the dimView. Dit id moet overeenkomen met de parameter \code{dim} in
#' \code{\link{addDimView}}
#' @param skipTopRow boolean, Als TRUE: eerste regel met Naam, links en presentatie wordt overgeslagen.
#' @param maxHeight an integer, maximum hoogte in pixels voor dit dimView object
#' @param overflowX string, css overflow propery in X richting, bepaalt of er wel of niet een horizontale scrollbar getoond wordt bij overflow. 
#'
#' @export
getDimUI <- function(starId, dim, skipTopRow = FALSE, maxHeight = NULL, overflowX = 'hidden') {
    
    withCallingHandlers({
        assert_is_a_string(starId)
        assert_is_a_string(dim)
        assert_is_a_bool(skipTopRow)
        assert_is_a_number(isNull(maxHeight,0))
        maxHeight <- as.integer(maxHeight)
        assert_is_a_string(overflowX)
        assert_is_subset(overflowX,domains[['cssOverflow']])
        
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
    
    shiny::div(
        id = paste0(gdim,'Dimensie'),
        if (!skipTopRow)
            HTML(paste0(
                '<table width = "100%">'
                , '<tbody>'
                , '<tr>'
                , '<td class="db-header">', shiny::uiOutput(paste0(gdim,"DimName")), '</td>'
                , '<td class="db-header">', shiny::uiOutput(paste0(gdim,"DimLinks")), '</td>'
                , '<td class="db-header">', shiny::uiOutput(paste0(gdim,"DimPresList")), '</td>'
                , '<td class="db-header" style="padding-top: 30px"></td>'
                , '</tr></tbody></table>')
            ),
        shiny::uiOutput(paste0(gdim,"DimHeader")),
        shiny::uiOutput(paste0(gdim,"DimBody")),
        shiny::uiOutput(paste0(gdim,'DimFooter')),
        style = style
    )

}

wrapUp <- function() {
    glob.env$sessionCount <- glob.env$sessionCount - 1
    print(paste0('exit: ',glob.env$sessionCount))    
    
    if (glob.env$sessionCount == 0) {
        
        if (!is.null(glob.env$dbhandle)) {
            print('Closing ODBC connections')
            RODBC::odbcCloseAll()
        }
        
        if (exists('globalCache', env = glob.env)) {
            for (id in names(glob.env$globalCache)) {
                saveRDS(glob.env$globalCache[[id]],getCacheFile(id))
            }
        }
        
        rm(glob.env, envir = .GlobalEnv)
        stopApp()
    }

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
        isDefinedGlobal('hasODBC',FALSE)
        isDefinedGlobal('debug',FALSE)
        isDefinedGlobal('debugDims',NULL)
        isDefinedGlobal('debugDumpReactive',FALSE)
        
        glob.env$dbhandle <- NULL
        glob.env$sessionCount <- 0
        glob.env$dashboardName <- basename(getwd())
        glob.env$dimUiIds <- c()
        glob.env$globalCache <- list()
        glob.env$reservedColumnPatterns <- c('*_fc','*_org','*_tooltip','*_text','*_sort')
        
        # account data

        if (glob.env$omgeving == 'PRD') {
            glob.env$odbcDsn <- 'PRD1'
            glob.env$odbcUser <- 'sa'
            glob.env$odbcPwd <- 'saPRD3'
        } else {
            glob.env$odbcDsn <- 'ACC1'
            glob.env$odbcUser <- 'etl'
            glob.env$odbcPwd <- 'etlACC1'
        }
        
        if (glob.env$hasODBC) {
            sql <- paste0("exec R.dbo.get_startpunt '",glob.env$omgeving,"'")
            glob.env$dbhandle <- RODBC::odbcDriverConnect(paste0("DSN=",glob.env$odbcDsn,";DATABASE=R;UID=",glob.env$odbcUser,";PWD=",glob.env$odbcPwd))
            glob.env$portalUrl <- RODBC::sqlQuery(glob.env$dbhandle, sql)$startpunt
        } else {
            glob.env$portalUrl <- 'http://www.example.com'
        }
    }
    
}

#'
#' authenticate dwhr session
#'
#' @export
authenticate <- function(session) {

    ses <- session$userData
    
    if (exists('authenticated', envir = ses))
        return(ses$authenticated)
    
    ses$authenticated <- FALSE
    session$onSessionEnded(wrapUp)
    
    ses$cdata <- session$clientData
    ses$urlQuery <- parseQueryString(shiny::isolate(ses$cdata$url_search))
    ses$baseUrl <- ''
    
    glob.env$sessionCount <- glob.env$sessionCount + 1
    
    if (glob.env$securityModel == 'none' || !glob.env$hasODBC) {
        ses$authenticated <- TRUE
        ses$dashUser <- 'unknown'
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

    sql <- paste0("exec R.dbo.check_hash '",user,"','",glob.env$dashboardName,"','",ts,"','",hash,"','",glob.env$omgeving,"'")
    res <- RODBC::sqlQuery(glob.env$dbhandle, sql)
    
    if (res$status %in% c(3,4,5)) {
        
        if (is.na(res$redir) || res$redir == '') {
            shinyjs::runjs(paste0('window.top.location.replace("',glob.env$portalUrl,'");'))
        } else {
            shinyjs::runjs(paste0('window.top.location.replace("',res$redir,'");'))
        }
        ses$authenticated <- FALSE
        ses$dashUser <- 'none'
        return(FALSE)
    }
    
    ses$baseUrl <- shiny::isolate(paste0(
        ses$cdata$url_protocol,'//',
        ses$cdata$url_hostname,':',
        ses$cdata$url_port,
        '/boRedir?docId=',res$docid,'&server=',res$server))
    
    updateQueryString(paste0('/boRedir?docId=',res$docid,'&server=',res$server),'replace')
    
    ses$authenticated <- TRUE
    ses$dashUser <- user
    return(TRUE)

}

#'
#' @export
#' 
portalUrl <- function(){
    glob.env$portalUrl
}
