defaultAfd <- data.frame(
    parent = '',
    label = 'Alle afdelingen',
    level = 0,
    stringsAsFactors = FALSE
)

defaultKpi <- data.frame(
    parent = 'Realm Overview',
    label = 'Iron Throne Tribute (x 1000)',
    level = 2,
    stringsAsFactors = FALSE
)

defaultPer <- data.frame(
    parent = laatstGeslotenT,
    label = laatstGesloten,
    level = 2,
    stringsAsFactors = FALSE
)

defaultPerT <- data.frame(
    parent = 'Alle Periodes',
    label = laatstGeslotenT,
    level = 1,
    stringsAsFactors = FALSE
)

defaultMnd <- data.frame(
    parent = 'All',
    label = 'Up to the Moon',
    level = 1,
    stringsAsFactors = FALSE
)

# default instellingen dashboard

defaultOpts <- list(
    spark = TRUE,
    kplSel = defaultAfd,
    kpiSel = defaultKpi,
    mndSel = defaultMnd,
    perState = 'maand',
    start = 'Realm Overview',
    kpls = ''
)

sparkVis <- function(env) { 
    session$userData$dashOpts$spark
}

initDashOpts <- function(userData) {
    
    file <- paste0(getwd(),'/tmp/',userData$dashUser,'Prev')  
    
    if (file.exists(file)) {
        dashOpts <- readRDS(file)
    } else {
        dashOpts <- defaultOpts
    }

    userData$dashOpts <- dashOpts
    
}


observeEvent(input[['pref']],{
    
    userData$dashOptsSave <- userData$dashOpts
    showModal(prefModal(userData$dashOpts))
    
})

getAfdChoices <- function(start) {
 
    if (start %in% c("Realm Overview","Iron Bank Ledger",'Bannerman Levies',"Houses Pact Compliance")) {
        kpls <- unique(ovzMRHulp[perCode == laatstGeslotenId & level1Label == start]$kostenplaats)
    } else {
        kpls <- unique(ovzMRHulp[perCode == laatstGeslotenId & level2Label == start]$kostenplaats)
    }    
    
    kpls <- as.data.table(kpl[kpl$level2Code %in% kpls,c('level2Code','level2Label')])[order(level2Label)]
    
    ret <- kpls$level2Code
    names(ret) <- kpls$level2Label 
    
    ret <- c("Kies Afdelng" = "",ret)
}

getStartChoices <- function(kpls) {

    if (kpls == '')
        sort(unique(union(
            ovzMRHulp[perCode == laatstGeslotenId & level1Label != 'Battle Conduct']$level1Label,
            ovzMRHulp[perCode == laatstGeslotenId & level1Label == 'Battle Conduct']$level2Label)))
    else 
        sort(unique(union(
            ovzMRHulp[perCode == laatstGeslotenId & kostenplaats == kpls & level1Label != 'Battle Conduct']$level1Label,
            ovzMRHulp[perCode == laatstGeslotenId & kostenplaats == kpls & level1Label == 'Battle Conduct']$level2Label)))
    
}

getFirstKpi <- function(start,kpls) {
    
    if (start %in% c("Realm Overview","Iron Bank Ledger",'Bannerman Levies',"Houses Pact Compliance")) {
        if (kpls == '')
            xx <- unique(ovzMRHulp[perCode == laatstGeslotenId & level1Label == start])
        else
            xx <- unique(ovzMRHulp[perCode == laatstGeslotenId & kostenplaats == kpls & level1Label == start])
        parent <- xx[order(level2Sort)][1]$level1Label
        label <- xx[order(level2Sort)][1]$level2Label
        level <- 2
        
    } else {
        if (kpls == '')
            xx <- unique(ovzMRHulp[perCode == laatstGeslotenId & level2Label == start])
        else
            xx <- unique(ovzMRHulp[perCode == laatstGeslotenId & kostenplaats == kpls & level2Label == start])
        
        parent <- xx[order(level3Sort)][1]$level2Label
        label <- xx[order(level3Sort)][1]$level3Label
        level <- 3
    }
    
    data.frame(
        parent = parent,
        label = label,
        level = level,
        stringsAsFactors = FALSE
    )
}

prefModal <- function(dashOpts) {

    choices <- getAfdChoices(dashOpts$start)
    choices2 <- getStartChoices(dashOpts$kpls)

    if (any(dashOpts$kplSel$level == 1))
        afd <- dashOpts$kpls
    else 
        afd <- ""
    
    modalDialog(
        
        div(style = "font-size: 85%; width: 100%",
            h4('Instellen voorkeuren'),
            hr(),
            checkboxInput('prefSpark','Tonen sparklines in tabellen',  dashOpts$spark),
            selectizeInput(
                inputId = 'prefAfd', 
                label = 'Voorkeurs afdeling:',
                choices = choices,
                multiple = FALSE,
                selected = afd),
            selectizeInput(
                inputId = 'prefStart', 
                label = 'Startscherm indicatoren:',
                choices = choices2,
                multiple = FALSE,
                selected = userData$dashOpts$start),
            radioButtons('prefMnd','Start met periode als:',choices = c('Within the Moon','Up to the Moon'),selected = dashOpts$mndSel$label, inline = TRUE)
        ),
        
        footer = tagList(
            actionButton("prefCancel", "Cancel"),
            actionButton("prefOk", "OK")
        ),
        
        size = 's',
        fade = FALSE
    )
    
}

observeEvent(input$prefCancel, {
    removeModal()
    userData$dashOpts <- userData$dashOptsSave 
})

observeEvent(input$prefOk, {
    removeModal()
    
    change <- FALSE
    restart <- FALSE
    
    if (userData$dashOpts$start != userData$dashOptsSave$start) {
        change <- TRUE
        restart <- TRUE
    }
    
    if (!restart) {
        if (userData$dashOpts$spark != userData$dashOptsSave$spark) {
            change <- TRUE
            
            for (d in c('kpi','kpl','gs')) {
                s1$dims[[d]]$reactive$visChange <- s1$dims[[d]]$reactive$visChange + 1  
            }
            if (!is.null(s1$clones$q2)) {
                s1$clones$q2$dims[['kpi']]$reactive$visChange <-  s1$clones$q2$dims[['kpi']]$reactive$visChange + 1
            }
            if (!is.null(s1$clones$q5)) {
                s1$clones$q5$dims[['kpi']]$reactive$visChange <-  s1$clones$q5$dims[['kpi']]$reactive$visChange + 1
            }
        }
        
        if (userData$dashOpts$kpls != userData$dashOptsSave$kpls) {
            change <- TRUE
            
            setSelection(s1,'kpl',userData$dashOpts$kplSel)
            setSelection(s1,'kpi',getFirstKpi(userData$dashOpts$start,userData$dashOpts$kpls))
            
            if (perLinkState$link == 'maand') 
                setSelection(s1,'per',defaultPer)
            else 
                setSelection(s1,'per',defaultPerT)
            
            for (d in c('gs'))
                setSelection(s1,d,s1$dims[[d]]$rootSelected)
            
        }
        
        if (userData$dashOpts$mndSel$label != userData$dashOptsSave$mndSel$label) {
            change <- TRUE
            setSelection(s1,'mnd',data.frame(level = 1, label = userData$dashOpts$mndSel$label, stringsAsFactors = FALSE)) 
        }
    }
    
    if (change) {
        file <- paste0(getwd(),'/tmp/',userData$dashUser,'Prev')  
        if (identical(defaultOpts,userData$dashOpts)) {
            file.remove(file)    
        } else {
            saveRDS(userData$dashOpts,file)
        }
    }
    
    if (restart) {
        glob.env$restart <- TRUE
        shinyjs::runjs('history.go(0);')
    }
    
})

observeEvent(input$prefSpark,{
    userData$dashOpts$spark <- input$prefSpark
})

observeEvent(input$prefAfd,{
  
    if (userData$dashOpts$kpls != input$prefAfd) {
        
        if (input$prefAfd == '') {
            
            userData$dashOpts$kplSel <- defaultAfd
            userData$dashOpts$kpls <- ''
            
        } else {
            
            userData$dashOpts$kpls <- input$prefAfd
            label <- kpl$level2Label[kpl$level2Code == input$prefAfd][1]
            
            sel <- data.frame(
                parent = "Alle afdelingen", 
                label = label,
                level = 1,
                stringsAsFactors = FALSE)
            
            userData$dashOpts$kplSel <- sel
            
        }
      
        updateSelectizeInput(session,inputId = 'prefStart',selected = userData$dashOpts$start, choices = getStartChoices(input$prefAfd))
    }
}, ignoreInit = TRUE, ignoreNULL = FALSE)

observeEvent(input$prefStart,{
    
    if (input$prefStart == '') {
        updateSelectizeInput(session,inputId = 'prefStart',selected = userData$dashOpts$start)
        return()
    }
  
    if (userData$dashOpts$start != input$prefStart) {
        
        userData$dashOpts$start <- input$prefStart
        userData$dashOpts$kpiSel <- getFirstKpi(input$prefStart,userData$dashOpts$kpls)
        
        if (userData$dashOpts$start == 'Houses Pact Compliance') {
            userData$dashOpts$perState <- 'tertiaal'    
            updateRadioButtons(session,inputId = 'prefMnd',selected = 'Within the Moon')
        } else {
            userData$dashOpts$perState <- 'maand'
            updateRadioButtons(session,inputId = 'prefMnd',selected = 'Up to the Moon')
        }
        updateSelectizeInput(session,inputId = 'prefAfd', selected = userData$dashOpts$kpls, choices = getAfdChoices(input$prefStart))
        
    }
})

observeEvent(input$prefMnd,{
    
    if (userData$dashOpts$mndSel$label != input$prefMnd) {
        userData$dashOpts$mndSel$label <- input$prefMnd
    }
    
    
    
    
})


