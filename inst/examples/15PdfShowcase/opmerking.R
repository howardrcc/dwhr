
opmModal <- function(failed = FALSE, foutText = '') {
    
    opm <- userData$opmData
    
    if (length(opm$updatedBy) > 0) {
        adU <- glob.env$adUser[usr == opm$createdBy,]
        opm$createdByName = paste0(adU$naam,' (',adU$functie,')')
        
        adU <- glob.env$adUser[usr == opm$updatedBy,]
        opm$updatedByName = paste0(adU$naam,' (',adU$functie,')')
        
        mutInfo <- paste0('Aangemaakt:', format(opm$creationDate,format='%Y-%m-%d %H:%M'))
        mutInfo <- paste0(mutInfo, '\ndoor:',opm$createdByName) 
        mutInfo <- paste0(mutInfo, '\nGemuteerd:', format(opm$lastUpdateDate,format='%Y-%m-%d %H:%M'))
        mutInfo <- paste0(mutInfo, '\ndoor:',opm$updatedByName) 
    } else {
        mutInfo <- ''
    }
    
    header <- paste0('Knooppunt: ',opm$kpiSel,'\nKostenplaats:', opm$kplSel, '\nGeldstroom:', opm$gsSel, '\nPeriode:', opm$perInfo$sel)
    
    modalDialog(
        div(h4('Voer opmerking in voor dit knooppunt'),
            pre(header, 
                class="shiny-text-output noplaceholder"),
            fluidRow(
                column(
                    width = 11,
                    textAreaInput(inputId = 'newTxt', label = 'Opmerking tekst:', width = '900px', height = '300px', value = opm$value, resize = "both")
                ),
                column(
                    width = 1,
                    if (mutInfo != '')
                        img(src="dwhRs/info-sign.png", 
                            height="16", 
                            'data-toggle' = "popover", 
                            'data-placement' = "right", 
                            'data-content' = pre(mutInfo), 
                            title = 'Recordhistorie')
                )
            )
        ),
        
        if (failed)
            div(tags$b(foutText, style = "color: red;")),
        
        footer = tagList(
            actionButton("opmCancel","Cancel"),
            actionButton("opmOk", "OK")
        ),
        
        size = 'l',
        fade = FALSE
    )
}

observeEvent(input$opmCancel, {
    removeModal()
    cmntKey <- userData$opmData$cmntKey 
    releaseLock(s1,cmntKey)
})
    
observeEvent(input$opmOk, {
    removeModal()
    cmntKey <- userData$opmData$cmntKey 
    nw <- userData$opmData$nw
    releaseLock(s1,cmntKey)

    tekst <- trimws(input$newTxt)

    if (!is.null(tekst) && userData$opmData$value != tekst) {
        perInfo <- userData$opmData$perInfo
        kpiKey <- userData$opmData$kpiKey
        kplKey <- userData$opmData$kplKey
        gsKey <- userData$opmData$gsKey
        
        now <- Sys.time()
     
        if (nrow(comments[kpiCode == kpiKey & 
                          kostenplaats == kplKey & 
                          perCode == perInfo$key &
                          gsCode == gsKey &
                          volgnr == 0 &
                          subVolgnr == 0 &
                          type == 'opm']) > 0) { 
            # update
            
            comments[kpiCode == kpiKey & 
                         kostenplaats == kplKey & 
                         perCode == perInfo$key &
                         gsCode == gsKey &
                         volgnr == 0 &
                         subVolgnr == 0 &
                         type == 'opm',
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
                    kpiCode = kpiKey,
                    kostenplaats = kplKey,
                    perCode = perInfo$key,
                    perType = perInfo$type,
                    gsCode = gsKey, 
                    volgnr = 0,
                    type = 'opm',
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
        # hierdoor kunnen er duplicates ontstaan, dit lossen we op in updateComments (func.R)
        
        saveRDS(comments[updatedBy == dashUser & !verzameld,],file)
        
        s1$dims[['kpi']]$reactive$visChange <- s1$dims[['kpi']]$reactive$visChange + 1 
        s1$dims[['kpl']]$reactive$visChange <- s1$dims[['kpl']]$reactive$visChange + 1 
        s1$dims[['perAfd']]$reactive$visChange <- s1$dims[['perAfd']]$reactive$visChange + 1 
        s1$dims[['per']]$reactive$visChange <- s1$dims[['per']]$reactive$visChange + 1 
        s1$dims[['gs']]$reactive$visChange <- s1$dims[['gs']]$reactive$visChange + 1
    }
    
})



observeEvent({
    s1$dims[['kpi']]$reactive$clickMeasureEvent
    s1$dims[['kpl']]$reactive$clickMeasureEvent
    s1$dims[['gs']]$reactive$clickMeasureEvent
    s1$dims[['per']]$reactive$clickMeasureEvent
},
{
    e1 <- s1$dims[['kpi']]$reactive$clickMeasureEvent
    e2 <- s1$dims[['kpl']]$reactive$clickMeasureEvent
    e3 <- s1$dims[['gs']]$reactive$clickMeasureEvent
    e4 <- s1$dims[['per']]$reactive$clickMeasureEvent
    
    mes <- 'Opmerking plaatsen in combinatie met meerdere geselecteerde items (multi-select) is niet moglijk.'
    
    if (e1$clickViewColumn == 'opm' || e2$clickViewColumn == 'opm' || e3$clickViewColumn == 'opm' || e4$clickViewColumn == 'opm') {
        
        perInfo <- getPerInfo(s1)
        perKey <- perInfo$key
        
        gsSel <- s1$dims[['gs']]$selected$label
        gsLvl <- s1$dims[['gs']]$selected$level
        
        if (gsLvl == 0) {
            gsKey <- 0
        } else {
            gsKey <- gs$gsCode[gs$level1Label == gsSel]
        }
        
        kplSel <- s1$dims[['kpl']]$selected$label
        kplLvl <- s1$dims[['kpl']]$selected$level
        
        if (kplLvl == 0) {
            kplKey <- '000000'
        } else {
            kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
        }
        
        kpiSel <- s1$dims[['kpi']]$selected$label
        kpiLvl <- s1$dims[['kpi']]$selected$level
        kpiParent <- s1$dims[['kpi']]$selected$parent
        
        kpiKey <- unique(kpi$kpiCode[kpi$kpiLabel == kpiSel & kpi$level == kpiLvl & kpi$kpiParent == kpiParent])

        if (e1$clickViewColumn == 'opm') {
            
            # reset
            s1$dims[['kpi']]$reactive$clickMeasureEvent$clickViewColumn <- ''
            
            if (checkMs(s1,'kpi')) {
                alert(mes)
                return()
            }
            
            kpiSel <- e1$clickMember
            kpiKey <- e1$clickMemberKey
            
            
        } else {
            
            if (e2$clickViewColumn == 'opm') {
            
                # reset
                s1$dims[['kpl']]$reactive$clickMeasureEvent$clickViewColumn <- ''
                
                if (checkMs(s1,'kpl')) {
                    alert(mes)
                    return()
                }
                
                kplSel <- e2$clickMember
                kplKey <- e2$clickMemberKey
                
            } else {
                
                if (e3$clickViewColumn == 'opm') {
                
                    # reset
                    s1$dims[['gs']]$reactive$clickMeasureEvent$clickViewColumn <- ''
                    
                    if (checkMs(s1,'gs')) {
                        alert(mes)
                        return()
                    }
                    
                    gsSel <- e3$clickMember
                    gsKey <- gs$gsCode[gs$level1Label == gsSel]
                    
                    
                } else {
                    
                    # reset
                    s1$dims[['per']]$reactive$clickMeasureEvent$clickViewColumn <- ''
                    
                    if (checkMs(s1,'per')) {
                        alert(mes)
                        return()
                    }
                    
                    perInfo$sel <- e4$clickMember
                    perInfo$key <- e4$clickMemberKey
                    
                }
            }
        }
        
        cmntKey <- paste0(kpiKey,'|',kplKey,'|',perInfo$key,'|',gsKey,'|opm')
        
        if(setLock(s1,cmntKey,dashUser)) {

            comments <<- updateComments()

            cmnt <- comments[kpiCode == kpiKey & 
                                 kostenplaats == kplKey & 
                                 perCode == perInfo$key &
                                 gsCode == gsKey &
                                 type == 'opm',]
            
            userData$opmData <- list(
                perInfo = perInfo,
                kpiSel = kpiSel,
                kpiKey = kpiKey,
                kplSel = kplSel,
                kplKey = kplKey,
                gsSel = gsSel,
                gsKey = gsKey,
                value = ifelse(length(trimws(cmnt$txt)) == 0,'',cmnt$txt),
                nw = length(trimws(cmnt$txt)) == 0,
                creationDate =  cmnt$creationDate,
                lastUpdateDate = cmnt$lastUpdateDate,
                updatedBy = cmnt$updatedBy,
                createdBy = cmnt$createdBy,
                cmntKey = cmntKey)
 
            showModal(opmModal())
            shinyjs::js$popover(trigger = 'click')
            
        }
    }
    
})


