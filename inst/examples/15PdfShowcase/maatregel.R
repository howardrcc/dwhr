
maxMaatr <- 10
toek <- as.character(per$periodemaandId[per$jaarLabel == ditJaar & per$periodemaandId >= laatstGeslotenId])
currentPeriod <- NULL
modalObs <- list()

observeEvent(input$maatr,{
    
    if (input$maatr == '')
        return()
    
    currentPeriod <<- s1$dims[['perAfd']]$selected
    
    if (input$maatr == 'MS') 
        return()
    
    kplSel <- s1$dims[['kpl']]$selected$label
    kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
    
    perInfo <- getPerInfo(s1)
    
    kpiRec <- ovzMRItems[ovzMRItems$ovzCode == input$maatr,][1,]
    
    kpiParent <- kpiRec$kpiParent
    kpiLabel <- kpiRec$kpiLabel
    kpiKey <- kpiRec$ovzCode
    
    cmntKey <- paste0(kpiKey,'|',kplKey,'|',perInfo$key,'|0|maatr')
    
    if(setLock(s1,cmntKey,dashUser)) {
        comments <<- updateComments()
        
        maatr <- list(
            data = NULL,
            kpiParent = kpiParent,
            kpiLabel = kpiLabel,
            kplSel = kplSel,
            perInfo = perInfo,
            kpiCode = kpiKey,
            kostenplaats = kplKey,
            ovzCode = input$maatr)
        
        data <- comments[kpiCode == kpiKey &
                             kostenplaats == kplKey &
                             perCode == perInfo$key &
                             perType == 'maand' &
                             gsCode == 0 &
                             trimws(txt) != '' &
                             type %in% c('maatr','alg','maatrOpm'),]
        
        data <- data[!volgnr %in% data$volgnr[type == 'maatr' & endDate < perInfo$key & status == 'Gesloten'],]
        
        for (v in sort(unique(data$volgnr))) {
            if (!(0 %in% data$subVolgnr[data$volgnr == v]) || trimws(data$txt[data$volgnr == v & data$subVolgnr == 0]) == '')
                data <- data[!(volgnr == v),]
        }
        
        data$nw <- FALSE
        maatr$data <- data
        
        now <- Sys.time()
        
        if (is.null(maatr$data) || nrow(maatr$data[type == 'alg',]) == 0) {
            
            maatr$data <- rbind(
                maatr$data,
                data.table(
                    kpiCode = kpiKey,
                    kostenplaats = kplKey,
                    perCode = perInfo$key,
                    perType = perInfo$type,
                    gsCode = 0,
                    volgnr = 0,
                    type = 'alg',
                    status = '',
                    txt = '',
                    creationDate = now,
                    lastUpdateDate = now,
                    createdBy = dashUser,
                    updatedBy = dashUser,
                    subVolgnr = 0,
                    endDate = '',
                    verzameld = FALSE,
                    nw = TRUE
                )
            )    
        }
        
        if (input$maatr != '20000|kpi' && nrow(maatr$data[type == 'maatr',]) == 0) {
            
            maatr$data <- rbind(
                maatr$data,
                data.table(
                    kpiCode = kpiKey,
                    kostenplaats = kplKey,
                    perCode = perInfo$key,
                    perType = perInfo$type,
                    gsCode = 0,
                    volgnr = 1,
                    type = 'maatr',
                    status = 'Open',
                    txt = '',
                    creationDate = now,
                    lastUpdateDate = now,
                    createdBy = dashUser,
                    updatedBy = dashUser,
                    subVolgnr = 0,
                    endDate = 100 * ditJaar + 12,
                    verzameld = FALSE,
                    nw = TRUE
                )
            )
            
        }
        
        if (input$maatr != '20000|kpi' && nrow(maatr$data[type == 'maatrOpm' & subVolgnr == 1,]) == 0) {
            
            maatr$data <- rbind(
                maatr$data,
                data.table(
                    kpiCode = kpiKey,
                    kostenplaats = kplKey,
                    perCode = perInfo$key,
                    perType = perInfo$type,
                    gsCode = 0,
                    volgnr = min(maatr$data$volgnr[maatr$data$type == 'maatr']),
                    type = 'maatrOpm',
                    status = '',
                    txt = '',
                    creationDate = now,
                    lastUpdateDate = now,
                    createdBy = dashUser,
                    updatedBy = dashUser,
                    subVolgnr = 1,
                    endDate = '',
                    verzameld = FALSE,
                    nw = TRUE
                )
            )    
            
        }
        
        if (input$maatr == '20002|kpi' && nrow(maatr$data[type == 'maatrOpm' & subVolgnr == 2,]) == 0) {
            
            maatr$data <- rbind(
                maatr$data,
                data.table(
                    kpiCode = kpiKey,
                    kostenplaats = kplKey,
                    perCode = perInfo$key,
                    perType = perInfo$type,
                    gsCode = 0,
                    volgnr = min(maatr$data$volgnr[maatr$data$type == 'maatr']),
                    type = 'maatrOpm',
                    status = '',
                    txt = '',
                    creationDate = now,
                    lastUpdateDate = now,
                    createdBy = dashUser,
                    updatedBy = dashUser,
                    subVolgnr = 2,
                    endDate = '',
                    verzameld = FALSE,
                    nw = TRUE
                )
            )    
            
        }
        
        progress <- Progress$new(session)
        progress$set(message = "Verzamelen data", value = 0)
        userData$progress <- progress
        
        maatr$cmntKey <- cmntKey
        maatr$data <- maatr$data[order(volgnr,subVolgnr),]
        maatr$chg <- 0
        userData$maatr <- maatr
        
        showModal(maatrModal())
        shinyjs::js$popover(trigger = 'click')
        
        userData$progress$set(message = "Finished", value = 1)
        userData$progress$close() 
        
    }
    
    # trigger reset dropDown
    
    s1$dims[['kpi']]$reactive$linksChange <- s1$dims[['kpi']]$reactive$linksChange + 1
    
})

addMutInfo <- function(data) {
    for (i in 1:nrow(data)) {
        data$mutInfo[i] <- paste0('Aangemaakt:', format(data$creationDate[i],format='%Y-%m-%d %H:%M'),
                               '\ndoor:',isNa(data$createdByName[i],data$createdBy[i]),
                               '\nGemuteerd:', format(data$lastUpdateDate[i],format='%Y-%m-%d %H:%M'),
                               '\ndoor:',isNa(data$updatedByName[i],data$updatedBy[i])) 
    }
    data
}


maatrDataState <- reactiveValues(link = '')

output[['maatrDataChoices']] <- renderUI({

    shinyjs::runjs(paste0('$(".modal-lg").block({ message: null, timeout: 1000, overlayCSS: { backgroundColor: "#ffffff", opacity:0}});'))
    
    if (maatrDataState$link == '')
        return()
    
    q2 <- s1$clones[['q2']]
    q2$dims[['kpi']]$reactive$selectedIdsChange
    
    maatr <- userData$maatr
    
    selStyle <- 'font-weight:bold; background-color: #e0e0e0; padding-left:10px; padding-right:10px; padding-top:1px; padding-bottom:4px;'
    
    if (maatr$ovzCode %in% c('XXXXX3','XXXXX4','20000|kpi')) {
        
        if (nrow(q2$dims[['gs']]$membersFiltered) == 2)
            return(
                tagList(
                    span(style = 'font-weight: bold;','Kies geldstroom:'),
                    HTML('&nbsp&nbsp&nbsp'),
                    if (maatrDataState$link == 'Alle geldstromen') {
                        span('Alle geldstromen',style = selStyle )
                    } else {
                        actionLink('gsLink0','Alle geldstromen')
                        
                    },
                    HTML('&nbsp&nbsp&nbsp'),
                    if (maatrDataState$link == 'Crown') {
                        span('Crown',style = selStyle)
                    } else {
                        actionLink('gsLink1','Crown')
                    },
                    HTML('&nbsp&nbsp&nbsp'),
                    if (maatrDataState$link == 'Bannermen') {
                        span('Bannermen',style = selStyle)
                    } else {
                        actionLink('gsLink2','Bannermen')
                    }))
        else {
            gs <- q2$dims[['gs']]$membersFiltered$memberKey
            
            return(
                tagList(
                    if (gs == 'E') {
                        span('Crown',style = selStyle)
                    },
                    if (gs == 'O') {
                        span('Bannermen',style = selStyle)
                    }))
        }
    }
    

    if (maatr$ovzCode %in% c('30001|kpi')) {

      return(
          tagList(
              span(style = 'font-weight: bold;','Kies plan:'),
              HTML('&nbsp&nbsp&nbsp'),
              if (maatrDataState$link == 'Sieges by Burden') {
                  span('Sieges by Burden', style = selStyle )
              } else {
                  actionLink('planLink0','Sieges by Burden')
                  
              },
              HTML('&nbsp&nbsp&nbsp'),
              if (maatrDataState$link == 'Sieges by Tactic') {
                  span('Sieges by Tactic', style = selStyle)
              } else {
                  actionLink('planLink1','Sieges by Tactic')
              },
              HTML('&nbsp&nbsp&nbsp'),
              if (maatrDataState$link == 'Sieges by Region') {
                  span('Sieges by Region',style = selStyle)
              } else {
                  actionLink('planLink2','Sieges by Region')
              }))
      
    }
    
    return()
    
})

observeEvent(input[['gsLink0']] ,{
    q2 <- s1$clones[['q2']]
    setSelection(q2,'gs', q2$dims[['gs']]$rootSelected, dimRefresh = FALSE)
    maatrDataState$link <- 'Alle geldstromen'
})

observeEvent(input[['gsLink1']] ,{
    q2 <- s1$clones[['q2']]
    setSelection(q2,'gs', data.frame(level = 1, parent = 'Alle geldstromen', label = 'Crown', stringsAsFactors = FALSE), dimRefresh = FALSE)
    maatrDataState$link <- 'Crown'
})

observeEvent(input[['gsLink2']] ,{
    q2 <- s1$clones[['q2']]
    setSelection(q2,'gs', data.frame(level = 1, parent = 'Alle geldstromen', label = 'Bannermen', stringsAsFactors = FALSE), dimRefresh = FALSE)
    maatrDataState$link <- 'Bannermen'
})

observeEvent(input[['planLink0']] ,{
    q2 <- s1$clones[['q2']]
    navigate(q2,'kpi',3,'Sieges by Burden', 'Battle Plans')
    maatrDataState$link <-'Sieges by Burden'
})

observeEvent(input[['planLink1']] ,{
    q2 <- s1$clones[['q2']]
    navigate(q2,'kpi',3,'Sieges by Tactic', 'Battle Plans')
    maatrDataState$link <- 'Sieges by Tactic'
})

observeEvent(input[['planLink2']] ,{
    q2 <- s1$clones[['q2']]
    navigate(q2,'kpi',3,'Sieges by Region', 'Battle Plans')
    maatrDataState$link <- 'Sieges by Region'
})



maatrModal <- function(selected = 1, failed = FALSE, foutText = '') {
    
    maatr <- userData$maatr
    data <- maatr$data[order(volgnr,subVolgnr),]

    kpiLabel <- maatr$kpiLabel
    kpiParent <- maatr$kpiParent
    kplSel <- maatr$kplSel
    perSel <- maatr$perInfo$sel
    mndSel <- as.character(s1$dims[['mnd']]$selected$label)
    
    readOnly <- (laatstGesloten != perSel)
    
    knooppunt <- kpiLabel
    
    if (kpiLabel == 'Iron Bank Ledger')
        knooppunt <- 'Realm Balance Notes'
    
    if (kpiLabel == 'Sieges by Burden')
        knooppunt <- 'Battle Plans'
    
    header <- paste0('Knooppunt:', knooppunt ,'\nKostenplaats:', kplSel,'\nPeriode: ',perSel,'\nSoort Periode: ', mndSel)

    data$usr <- data$createdBy
    data[glob.env$adUser, createdByName := paste0(naam,' (',functie,')'), on = 'usr']
    
    data$usr <- data$updatedBy
    data[glob.env$adUser, updatedByName := paste0(naam,' (',functie,')'), on = 'usr']
    
    data$usr <- NULL
    
    volgnrs <- sort(unique(data$volgnr))
    data <- addMutInfo(data)
  
    getTabs <- function() {
       
        i <- 0

        l <- lapply(volgnrs,function(x) {

            df <- data[volgnr == x,]
            res <- NULL
            
            if (x == 0) {
                res <- tabPanel(
                    'Toelichting',
                    HTML('&nbsp'),
                    fluidRow(
                        column(
                            width = 11, {
                                xx <- textAreaInput(
                                    inputId = paste0('opmAlg'), 
                                    label = NULL, 
                                    placeholder = if (readOnly) '' else 'Type hier de toelichtingstekst', 
                                    width = '900px', 
                                    height = '120px', 
                                    value = df$txt[1], 
                                    resize = "none")
                                
                                if (readOnly) 
                                    xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                                xx
                            }
                        ),
                        column(
                            width = 1,
                            if (!df$nw)
                                img(src="dwhRs/info-sign.png", 
                                    height="16", 
                                    'data-toggle' = "popover", 
                                    'data-placement' = "right", 
                                    'data-content' = pre(df$mutInfo[1]), 
                                    title = 'Recordhistorie')
                        )
                    )
                )
            } else {
                
                if ((maatr$ovzCode != '20000|kpi')) {
                    
                    state <- paste0('maatrState',i)
                    eindDatum <- paste0('maatrEnd',i)
                    
                    res <- tabPanel(
                        paste0('Maatregel ',i),
                        HTML('&nbsp'),
                        fluidRow(
                            column(
                                width = 9, {
                                    xx <- textAreaInput(
                                        inputId = paste0('maatrTxt',i), 
                                        label = NULL, 
                                        placeholder = if (readOnly) '' else 'Type hier de maatregeltekst', 
                                        width = '800px', 
                                        height = '120px', 
                                        value = df$txt[df$subVolgnr == 0], 
                                        resize = "none")
                                    
                                    if (readOnly) 
                                        xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                                    xx
                                }
                            ),
                            column(
                                width = 2,
                                selectInput(
                                    inputId = state,
                                    selected = df$status[df$subVolgnr == 0], 
                                    label = 'Status', 
                                    choices = {
                                        if (readOnly)
                                            df$status[df$subVolgnr == 0]
                                        else
                                            c('Open','Gesloten')
                                    }),
                                selectInput(
                                    inputId = eindDatum,
                                    selected = df$endDate[df$subVolgnr == 0], 
                                    label = 'Datum gereed', 
                                    choices = {
                                        if (readOnly)
                                            df$endDate[df$subVolgnr == 0]
                                        else
                                            sort(unique(c(toek,df$endDate[df$subVolgnr == 0])))
                                    })
                            ),
                            column(
                                width = 1,
                                if (!df$nw[df$subVolgnr == 0])
                                    img(src="dwhRs/info-sign.png", 
                                        height="16", 
                                        'data-toggle' = "popover", 
                                        'data-placement' = "right", 
                                        'data-content' = pre(df$mutInfo[df$subVolgnr == 0]), 
                                        title = 'Recordhistorie')
                            )
                        ),
                        fluidRow(
                            column(
                                width = 1
                            ),
                            column(
                                width = 7, {
                                    xx <- textAreaInput(
                                        inputId = paste0('maatrOpmTxt',i), 
                                        label = 'Opmerking BC', 
                                        width = '790px', 
                                        height = '80px', 
                                        value = df$txt[df$subVolgnr == 1], 
                                        resize = "none")
                                    
                                    if (readOnly) 
                                        xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                                    xx
                                }
                            ),
                            column(
                                width = 3
                            ),
                            column(
                                width = 1,
                                if (length(df$nw[df$subVolgnr == 1]) > 0 && !df$nw[df$subVolgnr == 1])
                                    img(src="dwhRs/info-sign.png", 
                                        height="16", 
                                        'data-toggle' = "popover", 
                                        'data-placement' = "right", 
                                        'data-content' = pre(df$mutInfo[df$subVolgnr == 1]), 
                                        title = 'Recordhistorie')
                                
                            )
                        ),
                        if (maatr$ovzCode == '20002|kpi'){
                            fluidRow(
                                column(
                                    width = 1
                                ),
                                column(
                                    width = 7, {
                                        xx <- textAreaInput(
                                            inputId = paste0('maatrOpmTxtHR',i), 
                                            label = 'Opmerking HR Adviseur', 
                                            width = '790px', 
                                            height = '80px', 
                                            value = df$txt[df$subVolgnr == 2], 
                                            resize = "none")
                                        
                                        if (readOnly) 
                                            xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                                        xx
                                    }
                                ),
                                column(
                                    width = 3
                                ),
                                column(
                                    width = 1,
                                    if (length(df$nw[df$subVolgnr == 2]) > 0 && !df$nw[df$subVolgnr == 2])
                                        img(src="dwhRs/info-sign.png", 
                                            height="16", 
                                            'data-toggle' = "popover", 
                                            'data-placement' = "right", 
                                            'data-content' = pre(df$mutInfo[df$subVolgnr == 2]), 
                                            title = 'Recordhistorie')
                                    
                                )
                            )
                        })
                    
                    obs <- observeEvent(input[[state]],{
                        if (!readOnly) {
                            if (input[[state]] == 'Gesloten') {
                                updateSelectInput(session = session, inputId = eindDatum, choices = laatstGeslotenId, selected = laatstGeslotenId)
                            } else {
                                updateSelectInput(session = session, inputId = eindDatum, choices = sort(unique(c(toek,df$endDate[df$subVolgnr == 0]))), selected = df$endDate[df$subVolgnr == 0])
                            }
                        }
                    })
                    
                    modalObs[[length(modalObs) + 1]] <<- obs 
                    
                }
            }

            i <<- i + 1
            res
        })
        
        l$id = 'maatrTabs'
        
        if (maatr$ovzCode == '20000|kpi') {
            l$selected = 'Toelichting'
        } else {
            
            if (length(l) <= (maxMaatr + 1) && !readOnly) {
                l[[length(l) + 1]] <- tabPanel(
                    paste0('+'),div())
            }
            
            l$selected = paste0('Maatregel ',selected)
        }
  
        l
        
    }
    
    minLvl <- 2
    kpiLvl <- 3
    
    if (maatr$kpiCode %in% c('20000|kpi','20002|kpi')){
        minLvl <- 1
        kpiLvl <- 2
    }
    
    if (maatr$ovzCode %in% c('XXXXX3','XXXXX4','20000|kpi')) {
        maatrDataState$link <- 'Alle geldstromen'
    } else {
        if (maatr$ovzCode %in% c('30001|kpi')) {
            maatrDataState$link <- 'Sieges by Burden'
        } else {
            maatrDataState$link <- ''
        }
    }

    selFun <- function(env,kplSel,perSel,mndSel,gsSel,selectChange = TRUE) {
        setSelection(env,'kpl',kplSel, dimRefresh = FALSE,selectChange = selectChange)
        setSelection(env,'perAfd',perSel, dimRefresh = FALSE,selectChange = selectChange)
        setSelection(env,'mnd',mndSel, dimRefresh = FALSE,selectChange = selectChange)
        setSelection(env,'gs', gsSel, dimRefresh = FALSE,selectChange = selectChange)
        env
    }                   
    
    perSel <-s1$dims[['perAfd']]$selected
    kplSel <- s1$dims[['kpl']]$selected
    gsSel <- s1$dims[['gs']]$rootSelected
    mndSel <- s1$dims[['mnd']]$selected
    
    if (is.null(s1$clones[['q2']])) {
        
        q2 <- clone.star(
            from = s1, 
            toId = 'q2', 
            print = FALSE,
            dimViews = list( 
                kpi = list(),
                kpl = list(measures = FALSE),
                perAfd = list(measures = FALSE),
                gs = list(measures = FALSE),
                mnd = list(measures = FALSE))) %>%
            selFun(kplSel,perSel,mndSel,gsSel,FALSE) %>%
            navigate('kpi',kpiLvl,kpiLabel,kpiParent, levelChange = FALSE) %>%
            renderDims(input,output)   
        
        q2$dims[['kpi']]$print <- FALSE
        q2$dims[['kpi']]$presList$dataTable1$dataTableOpts$pageLength <- 20
        q2$dims[['kpi']]$presList$dataTable1$dataTableOpts$pageLengthList <- 20
        q2$dims[['kpi']]$pageLength <- 20
        q2$dims[['kpi']]$selectableLevels <- NULL
        
        observeEvent(q2$dims[['kpi']]$reactive$levelChange,{
            dd <- q2$dims[['kpi']]
            lvl <- dd$level
            parent <- dd$parent
            gparent <- rev(dd$ancestors)[2]

            if (dd$ancestors[3] == 'Iron Bank Ledger')
                setSelection(q2,'kpi',data.frame(level = lvl - 1 , parent = gparent , label = parent, stringsAsFactors = FALSE))
        
        })

        s1$clones[['q2']] <- q2
    } else {
        q2 <- s1$clones[['q2']]
    }

    q2$dims[['kpi']]$presList$dataTable1$navOpts$minBreadCrumbLevel = minLvl
    
    q2 <- q2 %>%
        selFun(kplSel,perSel,mndSel,gsSel) %>%
        navigate('kpi',kpiLvl,kpiLabel,kpiParent)

    modalDialog(
        
        div(style = "font-size: 85%; width: 100%",
            h4(ifelse(readOnly,'Maatregelen/Toelichting voor dit knooppunt (alleen lezen)','Bewerk maatregelen/toelichting voor dit knooppunt')),
            hr(),
            fluidRow( 
                style = "display:flex;",
                column(
                    width = 4,
                    pre(header, style = "height:100%;")),
                column(
                    width = 8,
                    uiOutput('maatrDataChoices',style = 'text-align:right'),
                    getDimUI(starId = 'q2', dim = 'kpi', skipTopRow = TRUE, checkDups = FALSE))
            ),
            HTML('&nbsp'),
            
            if (nrow(data[trimws(txt) != '',]) > 0 || !readOnly) {
                do.call(tabsetPanel,getTabs())
            }
            else 
                if (maatr$ovzCode == '20000|kpi') {
                    pre('Er is geen toelichting op het resultaat voor deze periode', 
                        class="shiny-text-output noplaceholder")
                } else {
                    pre('Er zijn geen maatregelen voor dit knooppunt', 
                        class="shiny-text-output noplaceholder")
                }
        ),
          
        if (failed)
            div(tags$b(foutText, style = "color: red;")),
        
        footer = tagList(
            if (!readOnly) actionButton("maatrCancel", "Cancel"),
            actionButton("maatrOk", "OK")
        ),
        
        size = 'l',
        fade = FALSE
    )
    
}

observeEvent(input$maatrCancel, {
    lapply(modalObs,function(o) {o$destroy()})
    modalObs <<- list()
    removeModal()
    cmntKey <- userData$maatr$cmntKey 
    releaseLock(s1,cmntKey)
})

observeEvent(input$maatrOk, {
    lapply(modalObs,function(o) {o$destroy()})
    modalObs <<- list()
    removeModal()
    cmntKey <- userData$maatr$cmntKey 
    releaseLock(s1,cmntKey)

    if (laatstGesloten != userData$maatr$perInfo$sel) 
        return()
    
    maatr <- saveMaatr()

    if (maatr$chg == 0) 
        return()
    
    data <- maatr$data

    for (v in sort(unique(data$volgnr))) {
        if (trimws(data$txt[data$volgnr == v & data$subVolgnr == 0]) == '') {
            if (data$nw[data$volgnr == v & data$subVolgnr == 0]) 
                data <- data[!(volgnr == v),]
            else 
                data$txt[data$volgnr == v] <- ''
        }
    }
    
    data <- data[!(trimws(txt) == '' & data$nw),]
    
    data$volgnr[data$type == 'alg'] <- 0
    data$subVolgnr[data$type == 'alg'] <- 0
    
    tmp <- copy(data[type %in% c('maatr','maatrOpm')])

    if (nrow(tmp) > 0) {
        for (p in setdiff(toek,laatstGeslotenId)) {
            tmp[,perCode := p]
            data <- rbind(
                data,
                tmp
            )
        }
    }

    comments <<- comments[!(kpiCode %in% data$kpiCode &
                                kostenplaats %in% data$kostenplaats &
                                perCode %in% c(toek,data$perCode) &
                                perType == 'maand' &
                                gsCode == 0 & 
                                type %in% c('alg','maatr','maatrOpm')),]
    data$nw <- NULL
    comments <<- rbind(comments,unique(data))
    
    file <- paste0(getwd(),'/tmp/',dashUser,'Comments')
    
    # save alleen het deel aangepast/aangemaakt door deze user
    # hierdoor kunnen er duplicates ontstaan, dit lossen we op in updateComments (func.R)
    
    saveRDS(comments[updatedBy == dashUser & !verzameld,],file)

})



observeEvent(input[[paste0('q2Kpi_dt_ready')]],{
    shinyjs::runjs('$(".modal-lg").unblock();')
})


observeEvent(input$maatrTabs,{

    maatr <- saveMaatr()
    
    if (input$maatrTabs == '+') {
        
        now <- Sys.time()
        n <- max(maatr$data$volgnr[maatr$data$type == 'maatr'])
        
        maatr$data <- rbind(
            maatr$data,
            data.table(
                kpiCode = maatr$kpiCode,
                kostenplaats = maatr$kostenplaats,
                perCode = maatr$perInfo$key,
                perType = maatr$perInfo$type,
                gsCode = 0,
                volgnr = n + 1,
                type = 'maatr',
                status = 'Open',
                txt = '',
                creationDate = now,
                lastUpdateDate = now,
                createdBy = dashUser,
                updatedBy = dashUser,
                subVolgnr = 0,
                endDate = 100 * ditJaar + 12,
                verzameld = FALSE,
                nw = TRUE
                
            )
        )
        
        maatr$data <- rbind(
            maatr$data,
            data.table(
                kpiCode = maatr$kpiCode,
                kostenplaats = maatr$kostenplaats,
                perCode = maatr$perInfo$key,
                perType = maatr$perInfo$type,
                gsCode = 0,
                volgnr = n + 1,
                type = 'maatrOpm',
                status = '',
                txt = '',
                creationDate = now,
                lastUpdateDate = now,
                createdBy = dashUser,
                updatedBy = dashUser,
                subVolgnr = 1,
                endDate = '',
                verzameld = FALSE,
                nw = TRUE
            )
        )
        
        if (maatr$ovzCode == '20002|kpi') {
            
            maatr$data <- rbind(
                maatr$data,
                data.table(
                    kpiCode = maatr$kpiCode,
                    kostenplaats = maatr$kostenplaats,
                    perCode = maatr$perInfo$key,
                    perType = maatr$perInfo$type,
                    gsCode = 0,
                    volgnr = n + 1,
                    type = 'maatrOpm',
                    status = '',
                    txt = '',
                    creationDate = now,
                    lastUpdateDate = now,
                    createdBy = dashUser,
                    updatedBy = dashUser,
                    subVolgnr = 2,
                    endDate = '',
                    verzameld = FALSE,
                    nw = TRUE
                )
            )
        }
            
        userData$maatr <- maatr
        
        showModal(maatrModal(selected = nrow(maatr$data[maatr$data$type == 'maatr',])))
       
    }
    
    shinyjs::js$popover(trigger = 'click')
})

maatrVis <- function(env) {
    !checkMs(env) & 
    any(env$dims[['kpl']]$selected$level > 0) & 
    perState(env) == 'maand' &
    env$dims[['kpi']]$level > 1 &
    !(!is.na(env$dims[['kpi']]$ancestors[3]) && env$dims[['kpi']]$ancestors[3] == 'Houses Pact Compliance')
}


rvbVis <- function(env) {
    !checkMs(env) & 
    all(env$dims[['kpl']]$selected$level == 0) & 
    perState(env) == 'maand' & 
    env$dims[['kpi']]$level > 1 &
    !(!is.na(env$dims[['kpi']]$ancestors[3]) && env$dims[['kpi']]$ancestors[3] == 'Houses Pact Compliance')
}


getMaatrChoices <- function(env) {
    
    kplSel <- s1$dims[['kpl']]$selected$label
    kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
    
    perInfo <- getPerInfo(s1)
    
    level1Codes <- unique(ovzMRHulp$level1Code[ovzMRHulp$perCode == perInfo$key & ovzMRHulp$kostenplaats == kplKey])
    level2Codes <- unique(ovzMRHulp$level2Code[ovzMRHulp$perCode == perInfo$key & ovzMRHulp$kostenplaats == kplKey])
    
    choiceNames <- maatrDropDown[ovzCode %in% c(level1Codes,level2Codes) | ovzCode == 'MS',]$dropDownChoice
    choiceCodes <- maatrDropDown[ovzCode %in% c(level1Codes,level2Codes) | ovzCode == 'MS',]$ovzCode
    
    names(choiceCodes) <- choiceNames
    
    l <- list()
    
    l[['Input maandrapportage']] = as.list(choiceCodes)

    l
    
}

saveMaatr <- function() {
    
    maatr <- userData$maatr
    
    now <- Sys.time()
    
    i <- 0
    
    for (x in unique(maatr$data$volgnr)) {
        
        rw0 <- maatr$data[volgnr == x & subVolgnr == 0,]
        rw1 <- maatr$data[volgnr == x & subVolgnr == 1,]
        rw2 <- maatr$data[volgnr == x & subVolgnr == 2,]
        
        if (i == 0) {
            tekst <- input$opmAlg
            state <- ''
            eindDatum <- ''
            opm <- ''
            opmHR <- ''
        } else {
            tekst <- eval(parse(text = paste0('input$maatrTxt',i)))
            state <- eval(parse(text = paste0('input$maatrState',i)))
            eindDatum <- eval(parse(text = paste0('input$maatrEnd',i)))
            opm <- eval(parse(text = paste0('input$maatrOpmTxt',i)))
            opmHR <- eval(parse(text = paste0('input$maatrOpmTxtHR',i)))
        }
        
        if (!is.null(tekst) && (tekst != rw0$txt || state != rw0$status || eindDatum != rw0$endDate)) {
            
            maatr$data[volgnr == x & subVolgnr == 0 , c('txt','status','endDate','updatedBy','lastUpdateDate','verzameld') := list(
                tekst,
                state,
                eindDatum,
                dashUser,
                now,
                FALSE
            )]
            
            maatr$chg <- 1
        }
        
        if (i > 0 && nrow(rw1) == 0) {
            
            maatr$data <- rbind(
                maatr$data,
                data.table(
                    kpiCode = rw0$kpiCode,
                    kostenplaats = rw0$kostenplaats,
                    perCode = rw0$perCode,
                    perType = rw0$perType,
                    gsCode = 0,
                    volgnr = x,
                    type = 'maatrOpm',
                    status = '',
                    txt = '',
                    creationDate = now,
                    lastUpdateDate = now,
                    createdBy = dashUser,
                    updatedBy = dashUser,
                    subVolgnr = 1,
                    endDate = '',
                    verzameld = FALSE,
                    nw = TRUE
                )
            )
            
            rw1 <- maatr$data[volgnr == x & subVolgnr == 1,]
            
        }
        
        if (i > 0 && !is.null(opm) && opm != rw1$txt) {
            
            maatr$data[volgnr == x & subVolgnr == 1 , c('txt','status','endDate','updatedBy','lastUpdateDate','verzameld') := list(
                opm,
                '',
                '',
                dashUser,
                Sys.time(),
                FALSE)]
            
            maatr$chg <- 1
        }
        
        if (i > 0 && maatr$ovzCode == '20002|kpi') {
            
            if (nrow(rw2) == 0) {
                
                maatr$data <- rbind(
                    maatr$data,
                    data.table(
                        kpiCode = rw0$kpiCode,
                        kostenplaats = rw0$kostenplaats,
                        perCode = rw0$perCode,
                        perType = rw0$perType,
                        gsCode = 0,
                        volgnr = x,
                        type = 'maatrOpm',
                        status = '',
                        txt = '',
                        creationDate = now,
                        lastUpdateDate = now,
                        createdBy = dashUser,
                        updatedBy = dashUser,
                        subVolgnr = 2,
                        endDate = '',
                        verzameld = FALSE,
                        nw = TRUE
                    )
                )
                
                rw2 <- maatr$data[volgnr == x & subVolgnr == 2,]
                
            }
            
            if (!is.null(opmHR) && opmHR != rw2$txt) {
                
                maatr$data[volgnr == x & subVolgnr == 2 , c('txt','status','endDate','updatedBy','lastUpdateDate','verzameld') := list(
                    opmHR,
                    '',
                    '',
                    dashUser,
                    Sys.time(),
                    FALSE)]
                
                maatr$chg <- 1
                
            }
            
        }
        
        i <- i + 1
    }
    
    userData$maatr <- maatr
    maatr
}

