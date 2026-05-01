library(knitr)
library(kableExtra)

s1$batchAfd <- c()
batchMPState <- reactiveValues(count = 0)

mpInput <- reactive({input$rapportage4})
mpInput2 <- mpInput %>% shiny::debounce(200)

observeEvent(mpInput2(),{
    printMPInit('meetplan.Rnw')
})


batchMP <- function(args) {
    
    kostenplaatsen <- unique(ovzMPHulp$kostenplaats[ovzMPHulp$perCode == laatstGeslotenIdT  & ovzMPHulp$level2Groups %in% kvGroups$oms])
    s1$batchAfd <- sort(kpl$level2Label[kpl$level2Code %in% kostenplaatsen])
    
    batchMPState$count <- 1
    
}

observeEvent(batchMPState$count,{
    
    figOnly <- batchMode && !is.na(batchArgs[2]) && batchArgs[2] == 'figOnly'
    
    if (batchMPState$count == 0)
        return()
    
    if (batchMPState$count == 1) {
        file.remove(Sys.glob(paste0(getwd(),'/out/*.pdf')))
        printMPInit(reportFileName = 'meetplan.Rnw', perSel = laatstGeslotenT, kplSel =  s1$batchAfd[1], kplLvl = 1) 
        return()
    }
    
    if (batchMPState$count <= length(s1$batchAfd)) {
        printMPInit(reportFileName = 'meetplan.Rnw', perSel = laatstGeslotenT, kplSel =  s1$batchAfd[batchMPState$count], kplLvl = 1) 
        return()
    }
    
    if (batchMPState$count == length(s1$batchAfd) + 1) {
        
        if (!figOnly) {
            zipFile <- paste0(getwd(),'/out/meetplan-',laatstGeslotenT,'.zip') 
            file.remove(zipFile)
            zip(zipFile, Sys.glob(paste0(getwd(),'/out/*.pdf')), flags = '-j9X')
            
            file.remove(Sys.glob(paste0(getwd(),'/out/*.pdf')))
        }
        
        shinyjs::runjs('batchStateFinished = true;')
    }
    
}, priority = -2)

printMPState <- new.env()
printMPState$count = 0
printMPState$state = 'get'
printMPState$prevCount = 0 
printMPState$prevState = 'get'

printMPForce <- reactiveValues(count = 0)

printMPInit <- function(reportFileName, perSel = NULL, kplSel = NULL, kplLvl = NULL) {

    if (!file.exists(paste0(getwd(),'/',reportFileName))) {
        stop(paste0('reporFileName: ',reportFileName, ' does not exist'))
    }
    
    if (is.null(perSel)) {
        perSel <- s1$dims[['perAfd']]$selected$label    
    }
    
    realNaam <- ifelse(any(per$tertiaalCode[per$tertiaalLabel %in% perSel] > laatstGeslotenT),'Prognose','Realisatie')
                       
    if (is.null(kplSel) || is.null(kplLvl)) {
        kplLvl <- s1$dims[['kpl']]$selected$level
        kplSel <- s1$dims[['kpl']]$selected$label
    }
    
    pdfFileName <- paste0('Meetplan ',kplSel,' ',perSel,'.pdf')

    # reset de print loop
    
    printMPState$count = 0
    printMPState$state = 'get'
    
    progress <- Progress$new(session)
    
    # init overzicht
    
    kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
    perKey <- unique(per$tertiaalCode[per$tertiaalLabel == perSel])
    
    codes <- unique(ovzMPHulp$kpiCode[ovzMPHulp$perCode == perKey & ovzMPHulp$kostenplaats == kplKey])
    
    kpis <-  kpi[kpi$kpiCode %in% codes,]

    ovzItems = rbind(
        data.table(
            ovzGrp = kpis$level2Groups,
            ovzCode = kpis$kpiCode,
            kpiParent = kpis$kpiParent,
            kpiLabel = kpis$kpiLabel,
            pres = 'highCharts1',
            kpiLvl = 2),
        data.table(
            ovzGrp = kpis$level2Groups,
            ovzCode = kpis$kpiCode,
            kpiParent = kpis$kpiParent,
            kpiLabel = kpis$kpiLabel,
            pres = 'highCharts2',
            kpiLvl = 2))
    
    ovzItems <- ovzItems[order(ovzGrp,ovzCode,pres)]
    ovzItems$ovzGrpCode <- kvGroups$code[match(ovzItems$ovzGrp,kvGroups$oms)]
    
    s1$overzicht <- list(
        perSel = perSel,
        perKey = perKey,
        kplLvl = kplLvl,
        kplSel = kplSel,
        kplKey = kplKey,
        reportFileName = reportFileName,
        pdfFileName = pdfFileName,
        outDir = paste0(getwd(),'/out'),
        tab = NULL,
        ovzItems = ovzItems,
        rapData = list(),
        comments = list(),
        htmls = c(),
        pngs = c(),
        realNaam = realNaam
    )
    
    if (is.null(s1$clones[['smp']])) {
        progress$set(message = "Initialisatie printen", value = 0)
        
        smp <- clone.star(
            from = s1, 
            toId = 'smp', 
            facts = s1$facts[inDeMaand == 1 & kpiId %in% kpi$kpiId[kpi$level1Label == 'Houses Pact Compliance'],],
            print = TRUE,
            dimViews = list( 
                kpi = list(
                    initLevel = 2,
                    initParent = 'Houses Pact Compliance',
                    selectLevel = ovzItems$kpiLevel[1],
                    selectLabel = ovzItems$kpiLabel[1],
                    selectParent = 'Houses Pact Compliance'),
                kpl = list(
                    selectLevel = 2, 
                    selectLabel = kplSel, 
                    measures = FALSE),
                perAfd = list(
                    initLevel = 2,
                    selectLevel = 2, 
                    selectLabel = perSel,
                    state = 'enabled',
                    presentations = c('mixedChart1','mixedChart2')))) %>%
            setColumnName('kpi', viewColFrom = 'waardeReal', colTo = realNaam) %>%
            renderDims(input,output)
        
        smp$dims[['perAfd']]$presList$highCharts1$highChartsOpts$legend$layout <- 'horizontal'
        smp$dims[['perAfd']]$presList$highCharts1$highChartsOpts$series[['opm']]$visible <- FALSE
        smp$dims[['perAfd']]$presList$highCharts2$highChartsOpts$legend$layout <- 'horizontal'
        smp$dims[['perAfd']]$presList$highCharts2$highChartsOpts$series[['opm']]$visible <- FALSE
        
        s1$clones[['smp']] <- smp
        
    }
    
    s1$printProgress <- progress
    s1$progressCount <- 0
    
    # start print loop
    printMPState$count <- printMPState$count + 1
}

observeEvent({
    printMPForce$count
    autoInvalidate()
}, {

    if (printMPState$count == 0 || (printMPState$count == printMPState$prevCount && printMPState$state == printMPState$prevState))
        return()
    
    printMPState$prevCount <- printMPState$count
    printMPState$prevState <- printMPState$state
    
    kplLvl <- s1$overzicht$kplLvl
    kplSel <- s1$overzicht$kplSel
    kplKey <- s1$overzicht$kplKey
    perSel <- s1$overzicht$perSel
    perKey <- s1$overzicht$perKey
    
    smp <- s1$clones[['smp']]
    realNaam <- s1$overzicht$realNaam
    
    ovzItem <- s1$overzicht$ovzItems[printMPState$count,]
    md5 <- digest::digest(ovzItem$kpiLabel,algo = 'md5')
    
    figOnly <- batchMode && !is.na(batchArgs[2]) && batchArgs[2] == 'figOnly'
    
    pngDir <- gsub(' ','_',paste0(getwd(),'/tmp/meetplan/',laatstGesloten,'/',kplSel))
    
    if (!dir.exists(pngDir)) 
        dir.create(pngDir, recursive = TRUE, mode = "0777")
    
    pngFile <- paste0(pngDir,'/',ovzItem$pres,'-',md5,'.png')
    
    
    if (printMPState$state == 'get') {
        
        if (printMPState$count == 1)
            shinyjs::runjs('$.blockUI({ message: null, overlayCSS: { backgroundColor: "#ffffff", opacity:0 }});')
        
        doSet <- TRUE
        
        if (file.exists(pngFile)) {
            
            s1$overzicht$rapData[[printMPState$count]] <- pngFile
            
            if (printMPState$count > 1 || figOnly) {
                printMPForce$count <- printMPForce$count + 1 
                printMPState$count <- printMPState$count + 1
                s1$progressCount <- s1$progressCount + 1
                doSet <- FALSE
            }
        }
        
        if (doSet) {    
            
            setSelection(smp, 'kpl', data.frame(level = 1, label = kplSel, stringsAsFactors = FALSE) , dimRefresh = FALSE)
            setSelection(smp, 'perAfd', data.frame(level = 1, label = perSel, stringsAsFactors = FALSE), dimRefresh = FALSE)  
            setSelection(smp, 'kpi',data.frame(level = 2, parent = ovzItem$kpiParent, label = ovzItem$kpiLabel, stringsAsFactors = FALSE), dimRefresh = FALSE)
            navigate(smp,'kpi',2,'Houses Pact Compliance',"Alle kpi's")
            navigate(smp,'perAfd', level = 1, parent = 'Alle Periodes')
            smp$dims[['perAfd']]$pres <- ovzItem$pres
            smp$hcRenderers[['perAfd']]$count <- smp$hcRenderers[['perAfd']]$count + 1
            
            printMPState$state <- 'set'
        }
        
    } else {
        
        if (printMPState$state == 'set') {
            
            if (printMPState$count == 1) 
                s1$overzicht$tab <- smp$dtPrep[['kpi']]$tab   
            
            if (!file.exists(pngFile)) {
                html <- paste0(pngDir,'/',ovzItem$pres,'-',md5,'.html')
                htmlwidgets::saveWidget(smp$hcPrev[['perAfd']]$widget, file = html, libdir = 'html_support_files')
                s1$overzicht$htmls <- c(s1$overzicht$htmls,html)
                s1$overzicht$rapData[[printMPState$count]] <- pngFile
                s1$overzicht$pngs <- c(s1$overzicht$pngs,pngFile)
            }
            
            printMPState$state <- 'get'
            printMPState$count <- printMPState$count + 1
            s1$progressCount <- s1$progressCount + 1
            
        } else {
            
            if (printMPState$state == 'done') {
                
                if (length(s1$overzicht$htmls) == 0) {
                    printMPState$state <- 'print'
                } else {
                    s1$progressCount <- 0
                    
                    htmls <- s1$overzicht$htmls
                    pngs <- s1$overzicht$pngs
                    
                    s1$g <- future({ 
                        webshot::webshot(htmls, file = pngs, delay = 0, vwidth = 800, vheight = 250, zoom = 1)
                        unlink(htmls)
                        unlink(paste0(pngDir,'/html_support_files'), recursive = TRUE)
                        TRUE
                    }, 
                    globals = list(htmls = htmls,pngs = pngs,pngDir = pngDir),
                    seed = TRUE) %plan% multisession
                }
            }
            
            if (printMPState$state == 'print') {
                
                printMPState$count <- 0
                printMPState$state <- 'get'
                
                if (!figOnly) {
                    
                    tbl <- s1$overzicht$tab[,c('rowGroupColumn','Naam', realNaam, 'Aantal Groen','Aantal Rood', 'Begroting / Norm', 'Verschil', 'posneg')]
                    tbl$rowGroupColumn <- gsub('\\(|\\)','',tbl$rowGroupColumn)
                    
                    reportVars <- new.env(parent = emptyenv())
                    
                    reportVars$afdeling <- s1$overzicht$kplSel
                    reportVars$periode <- s1$overzicht$perSel
                    reportVars$tbl <- tbl
                    reportVars$realNaam <- realNaam
                    reportVars$ovzItems <- s1$overzicht$ovzItems
                    reportVars$rapData <- s1$overzicht$rapData
                    
                    reportVars$wwwDir <- paste0(getwd(),'/www/')
                    reportVars$src <- normalizePath(s1$overzicht$reportFileName)
                    
                    mcmnts <- comments[kpiCode %in% c(kvGroups$code,'MP') &
                                           kostenplaats == kplKey &
                                           perCode == perKey &
                                           perType == 'tertiaal' &
                                           gsCode == 0 & 
                                           trimws(txt) != '' &
                                           type %in% c(meetplanTypes$code,'mpScore','conc'),c('kpiCode','type','txt','lastUpdateDate','updatedBy')]
                    
                    scr <- mcmnts[mcmnts$type == 'mpScore',c('kpiCode','txt')]
                    
                    scr$txt <- ifelse(is.na(scr$txt) | scr$txt == 'Geen','white',
                                          ifelse(scr$txt == 'Rood','red',
                                                 ifelse(scr$txt == 'Groen', 'light-green','orange2')))
                    
                    names(scr) <- c('kpiCode','score')
                    names(mcmnts) <- c('kpiCode','type','txt','lastUpdateDate','usr')
                    
                    # mcmnts[glob.env$adUser, updatedBy := latexEscape(paste0(naam,' (',functie,')')), on = 'usr']
                    # mcmnts$txt <- paste0('\\hl{\\textit{\\scriptsize ',mcmnts$updatedBy,' / ',as.character(mcmnts$lastUpdateDate,format = '%Y-%m-%d'),':}} ',latexEscape(mcmnts$txt))
                
                    reportVars$mcmnts <- mcmnts
                    reportVars$scr <- scr
                    reportVars$printDatum <- format(Sys.time(), format = '%Y-%m-%d %H:%M:%S')
                    s1$progressCount <- 0
    
                    s1$f <- future({
                        library(knitr)
                        library(kableExtra)
                        library(dwhr)
                        
                        tmpDir <- paste(tempdir(), Sys.getpid(), sep='_') # apart process na fork: zelfde tempdir maar andere pid!
                        dir.create(tmpDir, FALSE, TRUE, "0700")
                        
                        setwd(tmpDir)
                        
                        file.copy(from = reportVars$src, to = 'tmp.Rnw', overwrite = TRUE)
                        
                        ret <- tryCatch(
                            {
                                x <- knit2pdf(input = 'tmp.Rnw')
                                list(
                                    status = 'goed',
                                    txt = x,
                                    tmpDir = tmpDir)
                            },
                            error = function(cond) {
                                return(list(
                                    status = 'fout', 
                                    txt = as.character(cond),
                                    tmpDir = tmpDir))
                            }
                        )
                        ret
                    }, 
                    globals = list(reportVars = reportVars),
                    seed = TRUE) %plan% multisession
                    
                    print('ok')
                } else {
                    
                    s1$printProgress$set(message = "Finished", value = 1)
                    s1$printProgress$close() 
                    batchMPState$count <- batchMPState$count + 1
                    shinyjs::runjs('$.unblockUI();')
                    
                }
            }
        }
    }
    
    if (nrow(s1$overzicht$ovzItems) >= printMPState$count && printMPState$count != 0) {
        s1$printProgress$set(message = "Verzamelen data", value = s1$progressCount/nrow(s1$overzicht$ovzItems))
        
        if (ovzItem$pres == 'highCharts1') 
            s1$overzicht$title <- 'Relatief'
        else
            s1$overzicht$title <- 'Absoluut'
        
    } else {
        printMPState$state <- 'done'
        s1$progressCount <- 0
    }
    
}, priority = -1)





