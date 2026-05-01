concCount <- 0

concModal <- function(failed = FALSE, foutText = '') {
    
    conc <- userData$concData
    
    
    if (length(conc$updatedBy) > 0) {
        adU <- glob.env$adUser[usr == conc$createdBy,]
        conc$createdByName = paste0(adU$naam,' (',adU$functie,')')
        
        adU <- glob.env$adUser[usr == conc$updatedBy,]
        conc$updatedByName = paste0(adU$naam,' (',adU$functie,')')
        
        mutInfo <- paste0('Aangemaakt:', format(conc$creationDate,format='%Y-%m-%d %H:%M'))
        mutInfo <- paste0(mutInfo, '\ndoor:',conc$createdByName) 
        mutInfo <- paste0(mutInfo, '\nGemuteerd:', format(conc$lastUpdateDate,format='%Y-%m-%d %H:%M'))
        mutInfo <- paste0(mutInfo, '\ndoor:',conc$updatedByName) 
    } else {
        mutInfo <- ''
    }
    
    header <- paste0('kostenplaats: ', conc$kplSel, '\nPeriode:', conc$perInfo$sel)
    readOnly <- (laatstGesloten != conc$perInfo$sel)
    
    scr <- conc$score
    
    choices <- c('Geen','Groen','Oranje','Rood')
    if(laatstGesloten == conc$perInfo$sel) {
        choices <- c('Geen','Groen','Oranje','Rood')
    } else {
        choices <- scr
    }
    
    if (concCount %% 2 == 0) {
        scr <- paste0(' ',scr)
        choices <- paste0(' ', choices)
    } 
    
    concCount <<- concCount + 1
    
    modalDialog(
        div(h4('Counsel of the Small Council'),
            pre(header, 
                class="shiny-text-output noplaceholder"),
            fluidRow(
                if (conc$perInfo$key < conc3Dat)
                    column(
                        width = 10, {
                            xx <- textAreaInput(
                                inputId = 'concTxt',
                                label = 'Advies Staven tekst:', 
                                width = '900px', 
                                height = '200px', 
                                value = conc$conc, 
                                resize = "none")
                            if (readOnly) 
                                xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                            xx
                        }
                    )
                else {
                    column(
                        width = 10, {
                            xx <- textAreaInput(
                                inputId = 'concTxt1',
                                label = 'Advies:', 
                                width = '900px', 
                                height = '100px', 
                                value = conc$conc1, 
                                resize = "none")
                            if (readOnly) 
                                xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                            xx
                        }, {
                            xx <- textAreaInput(
                                inputId = 'concTxt2',
                                label = 'Argumenten:', 
                                width = '900px', 
                                height = '100px', 
                                value = conc$conc2, 
                                resize = "none")
                            if (readOnly) 
                                xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                            xx
                        }
                        , {
                            xx <- textAreaInput(
                                inputId = 'concTxt3',
                                label = 'Risico\'s:', 
                                width = '900px', 
                                height = '100px', 
                                value = conc$conc3, 
                                resize = "none")
                            if (readOnly) 
                                xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                            xx
                        }
                    )
                    
                },
                column(
                    width = 1,
                    selectInput(
                        inputId = 'score',
                        label = 'Score',
                        choices = choices,
                        selected = scr,
                        width = "100px")),
                column(
                    width = 1,
                    if (mutInfo != '')
                        img(src="dwhRs/info-sign.png", 
                            height="16", 
                            'data-toggle' = "popover", 
                            'data-placement' = "right", 
                            'data-content' = pre(mutInfo), 
                            title = 'Recordhistorie'))
            )
        ),
        
        if (failed)
            div(tags$b(foutText, style = "color: red;")),
        
        footer = tagList(
            {if (!readOnly) actionButton("concCancel","Cancel")},
            actionButton("concOk","Ok") 
        ),
        
        size = 'l',
        fade = FALSE
    )
    
}

observeEvent(input$score,{
    
    scr <- trimws(input$score)
    
    color <- switch(
        scr,
        Geen = 'white',
        Groen = 'lightgreen',
        Oranje = '#FECE00',
        Rood = 'red')
    
    shinyjs::runjs(paste0('$(".item[data-value = \'', input$score, '\'").parent()[0].style.background = "', color, '"'))
    
    conc <- userData$concData
    
    if(laatstGesloten != conc$perInfo$sel)
        return()
    
    if (conc$kplLvl == 0) 
        return()
    
    userData$concData$score <- scr
    
})


observeEvent(input$maatr,{
    
    if (isNull(input$maatr,'') != 'MS')
        return()
    
    kplLvl <- s1$dims[['kpl']]$selected$level
    kplSel <- s1$dims[['kpl']]$selected$label
    
    if (kplLvl == 0) {
        kplKey <- '000000'
    } else {
        kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
    }
    
    perInfo <- getPerInfo(s1)
    cmntKey <- paste0('MR|',kplKey,'|',perInfo$key,'|0|conc')
    
    if(setLock(s1,cmntKey,dashUser)) {
        
        comments <<- updateComments()
        score <<- initScore()
        
        cmnt <- comments[kpiCode == 0 &
                             kostenplaats == kplKey &
                             perCode == perInfo$key &
                             perType == 'maand' &
                             gsCode == 0 &
                             type %in% c('conc','conc1','conc2','conc3'),]
        
        scr <- score$score[score$kostenplaats == kplKey & score$perCode == perInfo$key]
        
        if (length(scr) == 0)
            scr <- 'Geen'
        
        userData$concData <- list(
            perInfo = perInfo,
            kplSel = kplSel,
            kplKey = kplKey,
            conc = ifelse(length(trimws(cmnt$txt[cmnt$type == 'conc'])) == 0,'',cmnt$txt[cmnt$type == 'conc']),
            conc1 = ifelse(length(trimws(cmnt$txt[cmnt$type == 'conc1'])) == 0,'',cmnt$txt[cmnt$type == 'conc1']),
            conc2 = ifelse(length(trimws(cmnt$txt[cmnt$type == 'conc2'])) == 0,'',cmnt$txt[cmnt$type == 'conc2']),
            conc3 = ifelse(length(trimws(cmnt$txt[cmnt$type == 'conc3'])) == 0,'',cmnt$txt[cmnt$type == 'conc3']),
            kplLvl = kplLvl, 
            score = scr,
            nw = sum(length(trimws(cmnt$txt))) == 0,
            creationDate =  min(cmnt$creationDate),
            lastUpdateDate = max(cmnt$lastUpdateDate),
            updatedBy = cmnt$updatedBy[cmnt$lastUpdateDate ==  max(cmnt$lastUpdateDate)][1],
            createdBy = cmnt$createdBy[cmnt$creationDate == min(cmnt$creationDate)][1],
            cmntKey = cmntKey)

        showModal(concModal())
        shinyjs::js$popover(trigger = 'click')
        
    }
    
    # trigger reset dropDown
    
    s1$dims[['kpi']]$reactive$linksChange <- s1$dims[['kpi']]$reactive$linksChange + 1    
    
})



observeEvent(input$concCancel, {
    removeModal()
    cmntKey <- userData$concData$cmntKey 
    releaseLock(s1,cmntKey)
})


observeEvent(input$concOk,{
    
    conc <- userData$concData
    
    if (isNull(input$concOk,0) == 0) 
        return()
    
    removeModal()
    cmntKey <- conc$cmntKey 
    releaseLock(s1,cmntKey)
    
    if (laatstGesloten != conc$perInfo$sel) 
        return()
    
    nw <- conc$nw
    perKey <- conc$perInfo$key
    kplKey <- conc$kplKey
    
    now <- Sys.time()
    change <- FALSE
    
    if (perKey < conc3Dat) {
        tekst <- trimws(input[['concTxt']])
        if (length(tekst) > 0 && conc$conc != tekst)
            change <- TRUE
    } else {
        tekst1 <- trimws(input[['concTxt1']])
        tekst2 <- trimws(input[['concTxt2']])
        tekst3 <- trimws(input[['concTxt3']])
        
        if (length(tekst1) > 0 && conc$conc1 != tekst1 ||
            length(tekst2) > 0 && conc$conc2 != tekst2 ||
            length(tekst3) > 0 && conc$conc3 != tekst3)
            change <- TRUE
    }
    
    for (ct in c('','1','2','3')) {
        
        if ((ct == '' && perKey < conc3Dat) || (ct != '' && perKey >= conc3Dat)) {
            
            txtInp <- paste0('concTxt',ct)
            txtOut <- paste0('conc',ct)
            tekst <- trimws(input[[txtInp]])
            
            if (change) {
                
                if (nrow(comments[kpiCode == 0 & 
                                  kostenplaats == kplKey & 
                                  perCode == perKey &
                                  perType == 'maand' &
                                  gsCode == 0 &
                                  volgnr == 0 &
                                  subVolgnr == 0 &
                                  type == txtOut]) > 0) { 
                    # update
                    
                    comments[kpiCode == 0 & 
                                 kostenplaats == kplKey & 
                                 perCode == perKey &
                                 perType == 'maand' &
                                 gsCode == 0 &
                                 volgnr == 0 &
                                 subVolgnr == 0 &
                                 type == txtOut,
                             c('txt', 'updatedBy','lastUpdateDate','verzameld') := list(
                                 tekst,
                                 dashUser,
                                 now,
                                 FALSE
                             )]
                } else {
                    
                    
                    if (tekst != '' || !nw) {
                        
                        # insert
                        
                        comments <<- rbind(comments,data.table(
                            kpiCode = 0,
                            kostenplaats = kplKey,
                            perCode = perKey,
                            perType = 'maand',
                            gsCode = 0 ,
                            volgnr = 0,
                            type = txtOut,
                            status = '',
                            txt = tekst,
                            creationDate = now,
                            lastUpdateDate = now,
                            createdBy = dashUser,
                            updatedBy = dashUser,
                            subVolgnr = 0,
                            endDate = '',
                            verzameld = FALSE) 
                        )
                        
                    }
                }
            }
        }
    }
    
    file <- paste0(getwd(),'/tmp/',dashUser,'Comments')
    
    # save alleen het deel aangepast/aangemaakt door deze user
    # hierdoor kunnen er duplicates ontstaan, dit lossen we op in updateComment (func.R)
    
    saveRDS(comments[updatedBy == dashUser & !verzameld,],file)
    
    score <<- initScore()
    curScore <- score$score[score$kostenplaats == conc$kplKey & score$perCode == conc$perInfo$key]
    scr <- conc$score
    
    if (length(curScore) == 0)
        curScore <- ''
    
    if (scr == curScore)
        return()
    
    score <<- score[!(kostenplaats == conc$kplKey & perCode >= conc$perInfo$key)]
    
    if (scr != 'Geen') {
        
        score <<- rbind(
            score,
            data.table(
                kostenplaats = conc$kplKey,
                perCode = per$periodemaandId[per$jaarCode == ditJaar & per$periodemaandId >= conc$perInfo$key],  
                score = scr,
                lastUpdateDate = Sys.time(),
                updatedBy = dashUser))
        
    }
    
    file <- paste0(getwd(),'/tmp/score')
    saveRDS(score,file)
    
    
})


concMPModal <- function(failed = FALSE, foutText = '') {
    
    conc <- userData$concMPData
    
    if (length(conc$updatedBy) > 0) {
        adU <- glob.env$adUser[usr == conc$createdBy,]
        conc$createdByName = paste0(adU$naam,' (',adU$functie,')')
        
        adU <- glob.env$adUser[usr == conc$updatedBy,]
        conc$updatedByName = paste0(adU$naam,' (',adU$functie,')')
        
        mutInfo <- paste0('Aangemaakt:', format(conc$creationDate,format='%Y-%m-%d %H:%M'))
        mutInfo <- paste0(mutInfo, '\ndoor:',conc$createdByName) 
        mutInfo <- paste0(mutInfo, '\nGemuteerd:', format(conc$lastUpdateDate,format='%Y-%m-%d %H:%M'))
        mutInfo <- paste0(mutInfo, '\ndoor:',conc$updatedByName) 
    } else {
        mutInfo <- ''
    }
    
    header <- paste0('kostenplaats: ', conc$kplSel, '\nPeriode:', conc$perInfo$sel)
    readOnly <- (laatstGeslotenT != conc$perInfo$sel)
    
    modalDialog(
        div(h4('Management Samenvatting Kwaliteit en Veiligheid'),
            pre(header, 
                class="shiny-text-output noplaceholder"),
            fluidRow(
                column(
                    width = 10, {
                        xx <- textAreaInput(
                            inputId = 'concMPTxt',
                            label = 'Management Samenvatting tekst:', 
                            width = '900px', 
                            height = '200px', 
                            value = conc$value, 
                            resize = "none")
                        if (readOnly) 
                            xx$children[[2]] <- tagAppendAttributes(xx$children[[2]],readOnly = TRUE)
                        xx
                    }),
                column(
                    width = 1,
                    if (mutInfo != '')
                        img(src="dwhRs/info-sign.png", 
                            height="16", 
                            'data-toggle' = "popover", 
                            'data-placement' = "right", 
                            'data-content' = pre(mutInfo), 
                            title = 'Recordhistorie'))
            )
        ),
        
        if (failed)
            div(tags$b(foutText, style = "color: red;")),
        
        footer = tagList(
            {if (!readOnly) actionButton("concMPCancel","Cancel")},
            actionButton("concMPOk","Ok") 
        ),
        
        size = 'l',
        fade = FALSE
    )
    
}


observeEvent(input$meetp,{
    
    if (isNull(input$meetp,'') != 'MS')
        return()
    
    kplLvl <- s1$dims[['kpl']]$selected$level
    kplSel <- s1$dims[['kpl']]$selected$label
    
    if (kplLvl == 0) {
        kplKey <- '000000'
    } else {
        kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
    }
    
    perInfo <- getPerInfo(s1)
    cmntKey <- paste0('MP|',kplKey,'|',perInfo$key,'|0|conc')
    
    if(setLock(s1,cmntKey,dashUser)) {
        
        comments <<- updateComments()
        
        cmnt <- comments[kpiCode == 'MP' &
                             kostenplaats == kplKey &
                             perCode == perInfo$key &
                             perType == 'tertiaal' &
                             gsCode == 0 &
                             type %in% c('conc'),]
        
        userData$concMPData <- list(
            perInfo = perInfo,
            kplSel = kplSel,
            kplKey = kplKey,
            value = ifelse(length(trimws(cmnt$txt)) == 0,'',cmnt$txt),
            kplLvl = kplLvl, 
            nw = length(trimws(cmnt$txt)) == 0,
            creationDate =  cmnt$creationDate,
            lastUpdateDate = cmnt$lastUpdateDate,
            updatedBy = cmnt$updatedBy,
            createdBy = cmnt$createdBy,
            cmntKey = cmntKey)
        
        showModal(concMPModal())
        shinyjs::js$popover(trigger = 'click')
        
    }
    
    # trigger reset dropDown
    
    s1$dims[['kpi']]$reactive$linksChange <- s1$dims[['kpi']]$reactive$linksChange + 1    
    
})



observeEvent(input$concMPCancel, {
    removeModal()
    cmntKey <- userData$concMPData$cmntKey 
    releaseLock(s1,cmntKey)
})


observeEvent(input$concMPOk,{
    
    conc <- userData$concMPData
    
    if (isNull(input$concMPOk,0) == 0) 
        return()
    
    removeModal()
    cmntKey <- conc$cmntKey 
    releaseLock(s1,cmntKey)
    
    if (laatstGeslotenT != conc$perInfo$sel) 
        return()
    
    nw <- conc$nw
    tekst <- trimws(input$concMPTxt)
    
    if (!is.null(tekst) && conc$value != tekst && laatstGeslotenT == conc$perInfo$sel) {
        perKey <- conc$perInfo$key
        kplKey <- conc$kplKey
        now <- Sys.time()
        
        if (nrow(comments[kpiCode == 'MP' & 
                          kostenplaats == kplKey & 
                          perCode == perKey &
                          perType == 'tertiaal' &
                          gsCode == 0 &
                          volgnr == 0 &
                          subVolgnr == 0 &
                          type == 'conc']) > 0) { 
            # update
            
            comments[kpiCode == 'MP' & 
                         kostenplaats == kplKey & 
                         perCode == perKey &
                         perType == 'tertiaal' &
                         gsCode == 0 &
                         volgnr == 0 &
                         subVolgnr == 0 &
                         type == 'conc',
                     c('txt', 'updatedBy','lastUpdateDate','verzameld') := list(
                         tekst,
                         dashUser,
                         now,
                         FALSE
                     )]
        } else {
            
            
            if (tekst != '' || !nw) {
                
                # insert
                
                comments <<- rbind(comments,data.table(
                    kpiCode = 'MP',
                    kostenplaats = kplKey,
                    perCode = perKey,
                    perType = 'tertiaal',
                    gsCode = 0 ,
                    volgnr = 0,
                    type = 'conc',
                    status = '',
                    txt = tekst,
                    creationDate = now,
                    lastUpdateDate = now,
                    createdBy = dashUser,
                    updatedBy = dashUser,
                    subVolgnr = 0,
                    endDate = '',
                    verzameld = FALSE) 
                )
                
            }
        }
        
        file <- paste0(getwd(),'/tmp/',dashUser,'Comments')
        
        # save alleen het deel aangepast/aangemaakt door deze user
        # hierdoor kunnen er duplicates ontstaan, dit lossen we op in updateComment (func.R)
        
        saveRDS(comments[updatedBy == dashUser & !verzameld,],file)
        
    }
    
})



