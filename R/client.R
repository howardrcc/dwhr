#'
#' Initialize dwhr shiny UI
#'
#' This function must be called first from within the shiny app's UI
#'
#' @export
dwhrInit <- function() {
    shiny::addResourcePath('dwhRs',system.file('www', package = 'dwhr'))
    pkg.env$dimUiIds <- c()
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
#' Create dimView object in the UI
#'
#' Create html for dimView object in the shiny app's UI.
#'
#' @param dim string, unique id of the dimView. This id should correspond to the parameter \code{dim} in
#' \code{\link{addDimView}}
#' @param useWellPanel boolean, if TRUE a wellPanel is drawn around the dimView object
#' @param maxHeight an integer, maxheight in pixels for this dimView
#' @param overflowX ?
#'
#' @export
getDimUI <- function(dim, useWellPanel = FALSE, maxHeight = NULL, overflowX = 'hidden') {

    ui2 <- function() {

        if (!is.null(maxHeight)) {
            style <- paste0("overflow-x:", overflowX, "; overflow-y:hidden; height: ", maxHeight, "px;")
            # style <- paste0("overflow-x:", overflowX, "; overflow-y:hidden; max-height = ", maxHeight, "px; min-height: ", maxHeight, "px; height: ", maxHeight, "px;")
        } else {
            style <- paste0("overflow-x:", overflowX, "; overflow-y:hidden;")
        }

        shiny::div(id = paste0(dim,'Dimensie'),
                   shiny::uiOutput(paste0(dim,"DimHeader")),
                   shiny::uiOutput(paste0(dim,"DimBody")),
                   shiny::uiOutput(paste0(dim,'DimFooter')),
                   style = style
        )
    }

    dim %in% pkg.env$dimUiIds && dwhrStop('duplicate dims')
    pkg.env$dimUiIds <- c(pkg.env$dimUiIds,dim)

    ui2()

}

#'
#' authenticate dwhr session
#'
#' @export
authenticate <- function(session) {

    session$onSessionEnded(wrapUp)

    pkg.env$sessionCount <- pkg.env$sessionCount + 1
    cdata <- session$clientData
    urlQuery <- parseQueryString(shiny::isolate(cdata$url_search))
    ses <- session$userData
    ses$authenticated <- FALSE

    if (pkg.env$securityModel == 'none' || !pkg.env$hasODBC) {
        ses$authenticated <- TRUE
        ses$dashUser <- 'unknown'
        return(TRUE)
    }

    if(!shiny::serverInfo()[[1]]) {
        user <- 'dev'
        hash <- '123456'
        ts <- 0
    } else {
        user <- urlQuery$u
        hash <- urlQuery$h
        ts <- urlQuery$t
    }

    sql <- paste0("exec R.dbo.check_hash '",user,"','",pkg.env$dashboardName,"','",ts,"','",hash,"','",pkg.env$omg,"'")
    res <- RODBC::sqlQuery(pkg.env$dbhandle, sql)
    
    if (res$status %in% c(3,4,5)) {
        
        if (is.na(res$redir) || res$redir == '') {
            shinyjs::runjs(paste0('window.top.location.replace("',pkg.env$portalUrl,'");'))
        } else {
            shinyjs::runjs(paste0('window.top.location.replace("',res$redir,'");'))
        }
        ses$authenticated <- FALSE
        ses$dashUser <- 'none'
        return(FALSE)
    }
    
    ses$authenticated <- TRUE
    ses$dashUser <- user
    return(TRUE)

}

#'
#' @export
#' 
portalUrl <- function(){
    pkg.env$portalUrl
}
