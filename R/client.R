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
        #shinyjqui::includeJqueryUI(),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(
            script = 'dwhRs/starExtend.js',
            functions = c('dumpToConsole',
                          'updateTitle',
                          'updateSeriesData',
                          'updateSeriesOpts',
                          'updateXAxis',
                          'updateYAxis',
                          'updateXPlotBands',
                          'redraw',
                          'blockUI',
                          'tooltip',
                          'popover',
                          'hideDim',
                          'showDim',
                          'searchDT',
                          'updateDT',
                          'init',
                          'hcSetHeight'))
        ,
        
        # Loading message

        shiny::div(
            id = "loading-content",
            shiny::h2("Laden dashboard ...")
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
#' @param checkDups boolean, Als TRUE: controleer of dimensie als in ui is opgenomen.
#'
#' @export
getDimUI <- function(starId, dim, skipTopRow = FALSE, maxHeight = NULL, overflowX = 'hidden', accordion = FALSE, checkDups = TRUE, bodyStyle = NULL ) {
    
    assert_is_a_string(starId)
    assert_is_a_string(dim)
    assert_is_a_bool(skipTopRow)
    assert_is_a_number(isNull(maxHeight,0))
    maxHeight <- as.integer(maxHeight)
    assert_is_a_string(overflowX)
    assert_is_subset(overflowX,domains[['cssOverflow']])
    assert_is_a_bool(accordion)
    assert_is_a_bool(checkDups)
    assert_is_a_string(isNull(bodyStyle,''))
    
    gdim <- getGlobalId(starId,dim)
    gdim %in% glob.env$dimUiIds && checkDups && stop('duplicate dims')    
    
    glob.env$dimUiIds <- c(glob.env$dimUiIds,gdim)
    
    if (!is.null(maxHeight)) {
        style <- paste0("overflow-x:", overflowX, "; overflow-y:hidden; height: ", maxHeight, "px;")
    } else {
        style <- paste0("overflow-x:", overflowX, "; overflow-y:hidden;")
    }
    
    acc <- paste0('<td id="', gdim, 'AccordionId" class="db-header2" style="width: 16px; cursor: pointer;" onclick="toggleAccordion(this,\'', gdim, 'DwhrPanel\');">', 
                  shiny::icon('chevron-down',lib = 'glyphicon'), 
                  '</td>')
    if (!skipTopRow) 
        
        shiny::div(
            id = paste0(gdim,'Dimensie'),
            shiny::div(class = 'dwhrAccordion',
                shiny::HTML(paste0(
                    '<table width = "100%">'
                    , '<tbody>'
                    , '<tr>'
                    , ifelse(accordion,acc,'')
                    , '<td class="db-header2">', shiny::uiOutput(paste0(gdim,"DimName")), '</td>'
                    , '<td class="db-header2">', shiny::uiOutput(paste0(gdim,"DimLinks")), '</td>'
                    , '<td class="db-header2">', shiny::uiOutput(paste0(gdim,"DimPresList")), '</td>'
                    , '</tr></tbody></table>')
                ),
                shiny::uiOutput(paste0(gdim,"DimHeader"))),
            shiny::div(class = 'dwhrPanel', id = paste0(gdim,'DwhrPanel'),
                       shiny::uiOutput(paste0(gdim,"DimBody"), style = bodyStyle),
                       shiny::uiOutput(paste0(gdim,'DimFooter'))),
            style = style)
    else
        shiny::div(
            id = paste0(gdim,'Dimensie'),
            shiny::uiOutput(paste0(gdim,"DimHeader")),
            shiny::uiOutput(paste0(gdim,"DimBody"), style = bodyStyle),
            shiny::uiOutput(paste0(gdim,'DimFooter')),
            style = style)

}

initGlob <- function() {
    
    options(
        warnPartialMatchDollar = TRUE,
        warnPartialMatchArgs = TRUE,
        warnPartialMatchAttr = TRUE
    )
    
    if (!exists('glob.env', envir = .GlobalEnv, inherits = FALSE)) {
        
        glob.env <- new.env(parent = emptyenv())
        .GlobalEnv$glob.env <- glob.env
        
        isDefinedGlobal <- function(var,default) {
            
            if (exists(var, envir = .GlobalEnv, inherits = FALSE)) {
                glob.env[[var]] <- .GlobalEnv[[var]]
            } else {
                glob.env[[var]] <- default
            }
        }
        
        glob.env$securityModel <- 'none'
        shinyProxy <- Sys.getenv('SHINYPROXY')
        rHome <- Sys.getenv('R_PROJECT_HOME')

        if (rHome == '') 
            rHome = paste0(getwd(),'/..')

        if (shinyProxy != '') {
            shinyProxy %in% c('PRD','ACC','LOCAL','NONE') || stop('Invalid value Environment variable SHINYPROXY') 
            isDefinedGlobal('omgeving',shinyProxy)
            glob.env$securityModel <- 'shinyproxy'
        } else {
            isDefinedGlobal('omgeving','NONE')
        }

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
        glob.env$rHome <- rHome
        
        glob.env$restart <- FALSE
        
        # account data
        
        credFile <- paste0(rHome,'/admin/data/dbCred.rds')
        
        if (glob.env$omgeving != 'NONE') {
            
            is.na(file.info(credFile)$mtime) && stop(paste0('credentials file: ', credFile, ' not found'))
            
            dbCred <- readRDS(credFile)
            omg <- glob.env$omgeving
            omg %in% names(dbCred) || stop(paste0(omg,'  missing in dbCred file'))  
            
            sql <- paste0("exec R.dbo.get_startpunt '",omg,"'")
            handle <- RODBC::odbcDriverConnect(paste0("DSN=",dbCred[[omg]]$dsn,";DATABASE=R;UID=",dbCred[[omg]]$user,";PWD=",dbCred[[omg]]$pwd))
            
            assert_is_all_of(handle,'RODBC')

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
        
     
        adUserFile <- paste0(rHome,"/admin/data/ds_ad_user.txt")
        
        if (file.exists(adUserFile)) {
            
            glob.env$adUser <- as.data.table(read.csv(  
                file = adUserFile,
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
#' @param db string, database naam, default R.
#' 
#' @return database-handle 
#' @export
getDbHandle <- function(omg,db = 'R') {
    
    assert_is_a_string(omg)
    assert_is_a_string(db)
    
    if (!exists('glob.env')) 
        stop('glob.env does not exist. Run dwhrInit()?')
    
    if (is.null(glob.env$dbCred))
        stop('No credentials')
    
    omg %in% names(glob.env$dbCred) || stop(paste0('No credentials for omg:', omg))
    
    if (is.null(glob.env$dbCred[[omg]]$handle)) {
        
        dbCred <- glob.env$dbCred
        handle <- RODBC::odbcDriverConnect(paste0("DSN=",dbCred[[omg]]$dsn,";DATABASE=",db,";UID=",dbCred[[omg]]$user,";PWD=",dbCred[[omg]]$pwd))
        
        if (class(handle) != 'RODBC') {
            warning(paste0('Failed to get dbHandle for omg = ', omg))
            return(NULL)
        }

        glob.env$dbCred[[omg]]$handle <- handle
    }
    
    glob.env$dbCred[[omg]]$handle
    
}
 

#' authenticate dwhr session.
#' 
#' authenticeren van de gebruiker. Wordt gestuurd door de globale variable securityModel. 
#' 
#' @return als geauthenticiteerd dan TRUE ander FALSE.
#' @export
authenticate <- function(session, sessionTimeout = 0) {
    
    assert_is_all_of(session,'ShinySession')
    
    ce <- parent.frame() 
    
    session$onSessionEnded(function() {
        glob.env$sessionCount <- glob.env$sessionCount - 1
        print(paste0('exit: ',glob.env$sessionCount))    
        
        if(exists(paste0('sessionEndHook'),envir = ce)) {
            do.call(paste0('sessionEndHook'),list(session = session),envir = ce)
        }
        
#        if (glob.env$sessionCount == 0 && !glob.env$restart) {
 
#           print('Closing ODBC connections')
#           RODBC::odbcCloseAll()

#           if (length(glob.env$globalCache) > 0) {
#               saveRDS(glob.env$globalCache, getCacheFile())
#           }

#           rm(glob.env, envir = .GlobalEnv)
#           stopApp()
#       }
        
        glob.env$restart <- FALSE
        
    })

    ses <- session$userData
    
    if (exists('authenticated', envir = ses))
        return(ses$authenticated)


    # session timeout observer

    if (sessionTimeout > 0) {
        
        shinyjs::runjs(paste0("function idleTimer() {
                var t = setTimeout(logout, ", 1000 *  sessionTimeout,");
                window.onmousemove = resetTimer; // catches mouse movements
                window.onmousedown = resetTimer; // catches mouse movements
                window.onclick = resetTimer;     // catches mouse clicks
                window.onscroll = resetTimer;    // catches scrolling
                window.onkeypress = resetTimer;  //catches keyboard actions

                function logout() {
                    Shiny.setInputValue('timeOut', '", sessionTimeout,"s')
                }

                function resetTimer() {
                    clearTimeout(t);
                    t = setTimeout(logout, ", 1000 * sessionTimeout,");  // time is in milliseconds (1000 is 1 second)
                }
            }
            idleTimer();"))

        observeEvent(session$input$timeOut, { 
            print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
            showModal(modalDialog(
              title = "Timeout",
              paste("Session timeout due to", session$input$timeOut, "inactivity -", Sys.time()),
              footer = NULL
            ))
            session$close()
        })
    }

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

    dashUser <- session$request[['HTTP_X_SP_USERID']]
    dashGroups <- session$request[['HTTP_X_SP_USERGROUPS']]

    (!is.null(dashUser) && substr(dashUser,1,1) == 'z') || stop('Invalid username')

    ses$authenticated <- TRUE
    ses$dashUser <- dashUser
    ses$dashGroups <- dashGroups
    ses$schema <- session$request[['HTTP_X_FORWARDED_PROTO']]
    ses$port <- session$request[['HTTP_X_FORWARDED_PORT']]
    ses$server <- session$request[['HTTP_X_FORWARDED_SERVER']]
    adU <- glob.env$adUser[glob.env$adUser$usr == toupper(dashUser),]
    ses$dashUserName = adU$naam
    ses$dashUserFunc = adU$functie

    omg <- glob.env$omgeving
    sql <- paste0("exec R.dbo.log_dash '",dashUser,"','",glob.env$dashboardName,"','",dashGroups,"'")
    res <- RODBC::sqlQuery(glob.env$dbCred[[omg]]$handle, sql)

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
