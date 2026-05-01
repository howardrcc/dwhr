
toekT <- per$tertiaalCode[per$jaarLabel == ditJaar & per$tertiaalCode >= laatstGeslotenIdT]

meetplanTypes <- data.frame(
    code = c('meetp1','meetp2','meetp3','meetp4'),
    label = c('Aanleiding','Doelstelling','Voortgang','Bijstellen (Act)'),
    stringsAsFactors = FALSE
)

mpCount <- 0


observeEvent(input$meetp,{
    
    if (input$meetp == '')
        return()
    
    if (input$meetp == 'MS') 
        return()
    
    kplSel <- s1$dims[['kpl']]$selected$label
    kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
    
    perInfo <- getPerInfo(s1)
    
    kpiParent <- "Alle kpi's"
    kpiLabel <- 'Houses Pact Compliance'
    kpiKey <- input$meetp
    
    cmntKey <- paste0(input$meetp,'|',kplKey,'|',perInfo$key,'|0|meetp')
    
    if(setLock(s1,cmntKey,dashUser)) {
        
        comments <<- updateComments()
        
        meetp <- list(
            data = NULL,
            kpiParent = kpiParent,
            kpiLabel = kpiLabel,
            kplSel = kplSel,
            perInfo = perInfo,
            kpiCode = kpiKey,
            kostenplaats = kplKey,
            ovzCode = input$meetp)
        
        data <- comments[kpiCode == kpiKey &
                             kostenplaats == kplKey &
                             perCode == perInfo$key &
                             perType == 'tertiaal' &
                             gsCode == 0 &
                             trimws(txt) != '' &
                             type %in% c(meetplanTypes$code,'mpScore'),]

        data$nw <- FALSE
        meetp$data <- data

        now <- Sys.time()
     
        for (mpt in meetplanTypes$code) {
            if (is.null(meetp$data) || nrow(meetp$data[type == mpt,]) == 0) {
               
                meetp$data <- rbind(
                    meetp$data,
                    data.table(
                        kpiCode = kpiKey,
                        kostenplaats = kplKey,
                        perCode = perInfo$key,
                        perType = perInfo$type,
                        gsCode = 0,
                        volgnr = 0,
                        type = mpt,
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
        }
        
        if (is.null(meetp$data) || nrow(meetp$data[type == 'mpScore',]) == 0) {
            
            meetp$data <- rbind(
                meetp$data,
                data.table(
                    kpiCode = kpiKey,
                    kostenplaats = kplKey,
                    perCode = perInfo$key,
                    perType = perInfo$type,
                    gsCode = 0,
                    volgnr = 0,
                    type = 'mpScore',
                    status = '',
                    txt = 'Geen',
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
        
        progress <- Progress$new(session)
        progress$set(message = "Verzamelen data", value = 0)
        userData$progress <- progress
        
        meetp$cmntKey <- cmntKey
        meetp$chg <- 0
        userData$meetp <- meetp
        
        showModal(meetpModal())
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

meetpModal <- function(selected = 1, failed = FALSE, foutText = '') {
    
    meetp <- userData$meetp
    data <- meetp$data

    kpiLabel <- meetp$kpiLabel
    kpiParent <- meetp$kpiParent
    kplSel <- meetp$kplSel
    perSel <- meetp$perInfo$sel
    realNaam <- ifelse(meetp$perInfo$future,'Prognose','Realisatie')
    
    readOnly <- (laatstGeslotenT != perSel)
    
    knooppunt <- kpiLabel
    
    knooppunt <- kvGroups$oms[match(meetp$ovzCode,kvGroups$code)]
    mndSel <- 'Within the Moon'
    
    header <- paste0('Knooppunt:', knooppunt ,'\nKostenplaats:', kplSel,'\nPeriode: ',perSel,'\nSoort Periode: ', mndSel)

    data$usr <- data$createdBy
    data[glob.env$adUser, createdByName := paste0(naam,' (',functie,')'), on = 'usr']
    
    data$usr <- data$updatedBy
    data[glob.env$adUser, updatedByName := paste0(naam,' (',functie,')'), on = 'usr']
    
    data$usr <- NULL
    data <- addMutInfo(data)
  
    getTabs <- function() {
       
        l <- lapply(meetplanTypes$code,function(x) {

            df <- data[type == x,]
            tabLabel <- meetplanTypes$label[meetplanTypes$code == x]
            res <- NULL
            
            res <- tabPanel(
                tabLabel,
                HTML('&nbsp'),
                fluidRow(
                    column(
                        width = 11, {
                            xx <- textAreaInput(
                                inputId = paste0(x,'Txt'), 
                                label = NULL, 
                                placeholder = if (readOnly) '' else 'Type hier de tekst', 
                                width = '1000px', 
                                height = '140px', 
                                value = df$txt, 
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
                                'data-content' = pre(df$mutInfo), 
                                title = 'Recordhistorie')
                    )
                )
            )
            
            res
        })
        
        l$id = 'meetpTabs'
        l$selected = meetplanTypes$label[1]
        
        l
        
    }
    
    if (is.null(s1$clones[['q5']])) {
        timeout <- 5000
    } else {
        timeout <- 1000
    }
    
    shinyjs::runjs(paste0('$(".modal-lg").block({ message: null, timeout: ', timeout, ', overlayCSS: { backgroundColor: "#ffffff", opacity:0}});'))
    
    if (is.null(s1$clones[['q5']])) {

        q5 <- clone.star(
            from = s1, 
            toId = 'q5', 
            facts = s1$facts[inDeMaand == 1 & kpiId %in% kpi$kpiId[kpi$level1Label == 'Houses Pact Compliance'],],
            print = FALSE,
            dimViews = list( 
                kpi = list(
                    initParent = kpiLabel, 
                    selectMode = 'none'),
                kpl = list(
                    selectLevel = 2, 
                    selectLabel = s1$dims[['kpl']]$selected$label, 
                    measures = FALSE),
                perAfd = list(
                    selectLevel = 2, 
                    selectLabel = s1$dims[['perAfd']]$selected$label, 
                    measures = FALSE),
                mnd = list(
                    selectLabel = 'Within the Moon', 
                    measures = FALSE))) %>%
            setColumnName('kpi', viewColFrom = 'waardeReal', colTo = realNaam) %>%
            renderDims(input,output) 

        s1$clones[['q5']] <- q5
    } else {
        q5 <- s1$clones[['q5']]
    }
    
    q5$dims[['kpi']]$print <- FALSE
    q5$dims[['kpi']]$presList$dataTable1$navOpts$minBreadCrumbLevel = 1
    q5$dims[['kpi']]$presList$dataTable1$dataTableOpts$pageLength <- 200
    q5$dims[['kpi']]$presList$dataTable1$dataTableOpts$pageLengthList <- 200
    
    q5$dims[['kpi']]$pageLength <- 200
    
    setSelection(q5,'kpl',s1$dims[['kpl']]$selected, dimRefresh = FALSE)
    setSelection(q5,'perAfd',s1$dims[['perAfd']]$selected, dimRefresh = FALSE)
    setSelection(q5,'mnd',data.frame(level = 1, label = 'Within the Moon', stringsAsFactors = FALSE), dimRefresh = FALSE)
    
    filt1 <- isNull(q5$dims[['kpi']]$filterXtra,'') 
    filt2 <- paste0('level2Groups == "',kvGroups$oms[match(meetp$ovzCode,kvGroups$code)],'"')
    
    if (filt1 != filt2) {
        q5$dims[['kpi']]$filterXtra <- filt2
        q5$dims[['kpi']]$reactive$visChange <- q5$dims[['kpi']]$reactive$visChange + 1    
    }
    
    navigate(q5,'kpi',2,kpiLabel,kpiParent)
    
    choices <- c('Geen','Groen','Oranje','Rood')
    mpScore <- data$txt[data$type == 'mpScore']
    
    if (readOnly)
        choices <- mpScore
    
    if (mpCount %% 2 == 0) {
        mpScore <- paste0(' ', mpScore)
        choices <- paste0(' ', choices)
    } 
    
    mpCount <<- mpCount + 1
    
    modalDialog(
        
        div(style = "font-size: 85%; width: 100%",
            h4(ifelse(readOnly,'Voortgang voor dit knooppunt (alleen lezen)','Bewerk voortgang voor dit knooppunt')),
            hr(),
            fluidRow( 
                style = "display:flex;",
                column(
                    width = 4,
                    pre(header, style = "height:100%;")),
                column(
                    width = 8,
                    getDimUI(starId = q5$id, dim = 'kpi', skipTopRow = TRUE, checkDups = FALSE)
                )
            ),
            HTML('&nbsp'),
            
            if (nrow(data[trimws(txt) != '',]) > 0 || !readOnly) {
                
                tagList(
                    do.call(tabsetPanel,getTabs()),
                    fluidRow(
                        column(
                            width = 3,
                            selectInput(
                                inputId = 'meetpState',
                                selected = mpScore,
                                label = 'Aandacht', 
                                choices = choices)
                        ),
                        column(
                            width = 1,
                            if (!data$nw[data$type == 'mpScore'])
                                img(src="dwhRs/info-sign.png", 
                                    height="16", 
                                    'data-toggle' = "popover", 
                                    'data-placement' = "right", 
                                    'data-content' = pre(data$mutInfo[data$type == 'mpScore']), 
                                    title = 'Recordhistorie')
                        )
                    )
                )
            }
            else 
                pre('Er is geen voorgang ingevoerd voor dit knooppunt', 
                    class="shiny-text-output noplaceholder")
            
        ),
          
        if (failed)
            div(tags$b(foutText, style = "color: red;")),
        
        footer = tagList(
            if (!readOnly) actionButton("meetpCancel", "Cancel"),
            actionButton("meetpOk", "OK")
        ),
        
        size = 'l',
        fade = FALSE
    )
    
}


observeEvent(input$meetpCancel, {
    removeModal()
    cmntKey <- userData$meetp$cmntKey 
    releaseLock(s1,cmntKey)
})

observeEvent(input$meetpOk, {
    removeModal()
    cmntKey <- userData$meetp$cmntKey 
    releaseLock(s1,cmntKey)

    if (laatstGeslotenT != userData$meetp$perInfo$sel) 
        return()
    
    meetp <- saveMeetp()

    if (meetp$chg == 0) 
        return()
    
    data <- meetp$data

    data <- data[!(trimws(txt) == '' & data$nw),]
    
    # tmp <- copy(data)
    # 
    # if (nrow(tmp) > 0) {
    #     for (p in setdiff(toekT,laatstGeslotenIdT)) {
    #         tmp[,perCode := p]
    #         data <- rbind(
    #             data,
    #             tmp
    #         )
    #     }
    # }

    comments <<- comments[!(kpiCode %in% data$kpiCode &
                                kostenplaats %in% data$kostenplaats &
                                perCode %in% c(toek,data$perCode) &
                                perType == 'tertiaal' &
                                gsCode == 0 & 
                                type %in% c(meetplanTypes$code,'mpScore')),]
    data$nw <- NULL
    comments <<- rbind(comments,unique(data))
    
    file <- paste0(getwd(),'/tmp/',dashUser,'Comments')
    
    # save alleen het deel aangepast/aangemaakt door deze user
    # hierdoor kunnen er duplicates ontstaan, dit lossen we op in updateComments (func.R)
    
    saveRDS(comments[updatedBy == dashUser & !verzameld,],file)

})

observeEvent(input[[paste0('q5Kpi_dt_ready')]],{
    shinyjs::runjs('$(".modal-lg").unblock();')
})

meetpVis <- function(env) {
  
    if (!(!is.na(env$dims[['kpi']]$ancestors[3]) && env$dims[['kpi']]$ancestors[3] == 'Houses Pact Compliance'))
        return(FALSE)
    
    if (length(getMeetpChoices(env)) == 0)
        return(FALSE)
    !checkMs(env) & any(env$dims[['kpl']]$selected$level > 0 & perState(env) == 'tertiaal')
}


getMeetpChoices <- function(env) {
    
    kplSel <- s1$dims[['kpl']]$selected$label
    kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
    
    perInfo <- getPerInfo(s1)
    
    kvGrps <- unique(ovzMPHulp$level2Groups[ovzMPHulp$perCode == perInfo$key & ovzMPHulp$kostenplaats == kplKey])
    l <- list()
    
    if (length(kvGrps) > 0) {
        kvCodes <- kvGroups$code[kvGroups$oms %in% kvGrps]

        choiceNames <- kvGroups$oms[kvGroups$code %in% kvCodes | kvGroups$code == 'MS']
        choiceCodes <- kvGroups$code[kvGroups$code %in% kvCodes | kvGroups$code == 'MS']

        names(choiceCodes) <- choiceNames

        l[['Input meetplan']] = as.list(choiceCodes)
    }

    l
    
}

saveMeetp <- function() {
    
    meetp <- userData$meetp
    
    now <- Sys.time()
    state <- trimws(input[['meetpState']])
    newState <- meetp$data$txt[meetp$data$type == 'mpScore']
    
    for (x in meetplanTypes$code) {

        rw0 <- meetp$data[meetp$data$type == x,]        
        tekst <- eval(parse(text = paste0('input$',x,'Txt')))

        if (!is.null(tekst) && tekst != rw0$txt) {
            
            meetp$data[meetp$data$type == x, c('txt','updatedBy','lastUpdateDate','verzameld') := list(
                tekst,
                dashUser,
                now,
                FALSE
            )]
            
            meetp$chg <- 1
        }
    }
    
    if (state != newState) {
        
        meetp$data[meetp$data$type == 'mpScore', c('txt','updatedBy','lastUpdateDate','verzameld') := list(
            state,
            dashUser,
            now,
            FALSE
        )]
        
        meetp$chg <- 1
    }
    
    userData$meetp <- meetp
    meetp
}

observeEvent(input$meetpState,{

    scr <- trimws(input$meetpState)
    
    color <- switch(
        scr,
        Geen = 'white',
        Groen = 'lightgreen',
        Oranje = '#FECE00',
        Rood = 'red')
    
    shinyjs::runjs(paste0('$(".item[data-value = \'', input$meetpState, '\'").parent()[0].style.background = "', color, '"'))
    
})

