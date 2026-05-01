
printState <- new.env()
printState$count = 0
printState$prevCount = 0 

mrInput <- reactive({input$rapportage})
mrInput2 <- mrInput %>% shiny::debounce(200)

observeEvent(mrInput2(),{
    printInit('maandrapportage.Rnw')
})

batchAfd <- c()
rvbLid <- ''
batchMRState <- reactiveValues(count = 0)

batchMR <- function(args) {
    
    batchAfd <<- bestuurder$afdeling[bestuurder$rvbLid == args[2]]
    rvbLid <<- args[2]
    length(batchAfd) > 0 || stop('Geen Afdelingen gevonden voor dit rvbLid')
    batchMRState$count <- 1
    
}

observeEvent(batchMRState$count,{
    
    if (batchMRState$count == 0)
        return()
    
    if (batchMRState$count == 1) {
        file.remove(Sys.glob(paste0(getwd(),'/out/*.pdf')))
        s1$batchAfd <- intersect(s1$dims[['kpl']]$membersFiltered$member,batchAfd)
        printInit(reportFileName = 'maandrapportage.Rnw', perSel = laatstGesloten, kplSel =  s1$batchAfd[1], kplLvl = 1) 
        return()
    }
    
    if (batchMRState$count <= length(s1$batchAfd)) {
        printInit(reportFileName = 'maandrapportage.Rnw', perSel = laatstGesloten, kplSel =  s1$batchAfd[batchMRState$count], kplLvl = 1) 
        return()
    }
    
    if (batchMRState$count == length(s1$batchAfd) + 1) {
        printRvbInit('samenvatting.Rnw',perSel = laatstGesloten,rvbLid)
        return()
    }
    
    if (batchMRState$count == length(s1$batchAfd) + 2) {
        zipFile <- paste0(getwd(),'/out/',rvbLid,'-',laatstGesloten,'.zip') 
        file.remove(zipFile)
        zip(zipFile, Sys.glob(paste0(getwd(),'/out/*.pdf')), flags = '-j9X')
        
        file.remove(Sys.glob(paste0(getwd(),'/out/*.pdf')))
        
        shinyjs::runjs('batchStateFinished = true;')
    }
    
}, priority = -2)


getText <- function(itemNr,kpiKey,kplKey,perKey,textType) {
    
    cc <- comments[kpiCode == kpiKey &
                       kostenplaats == kplKey &
                       perCode == perKey &
                       gsCode == 0 & 
                       trimws(txt) != '' &
                       type %in% textType,]
    
    cc <- cc[!volgnr %in% cc$volgnr[type == 'maatr' & endDate < perKey & status == 'Gesloten'],]
    cc[,itemNr := itemNr]
}

printInit <- function(reportFileName,pdfFileName = NULL,perSel = NULL,kplSel = NULL, kplLvl = NULL) {
    
    if (!file.exists(paste0(getwd(),'/',reportFileName))) {
        stop(paste0('reporFileName: ',reportFileName, ' does not exist'))
    }
    
    if (is.null(perSel)) {
        perSel <- s1$dims[['perInst']]$selected$label    
    }
    
    if (is.null(kplSel) || is.null(kplLvl)) {
        kplLvl <- s1$dims[['kpl']]$selected$level
        kplSel <- s1$dims[['kpl']]$selected$label
    }
    
    if (is.null(pdfFileName)) {
        if (kplSel == "Financi\U00EBn")  # downloadHandler heeft problemen met diacritics in filenaam
            kplTxt = 'Iron Bank Ledger'
        else 
            kplTxt = kplSel
        
        pdfFileName <- paste0(kplTxt,' ',perSel,'.pdf')
    }
    
    # reset de print loop
    
    printState$count = 0
    progress <- Progress$new(session)
    
    # check of ster-schema voor printen geinitialiseerd is
    
    if (is.null(s1$clones[['smr']])) {
        
        progress$set(message = "Initialisatie printen", value = 0)
        
        smr <- clone.star(
            from = s1, 
            toId = 'smr', 
            print = TRUE,
            dimViews = list( 
                kpi = list(),
                kpl = list(measures = FALSE),
                perAfd = list(state = 'enabled',presentations = c('mixedChart1')), 
                mnd = list(measures = FALSE))) %>%
            renderDims(input,output)
        
        smr$dims[['perAfd']]$presList$highCharts1$highChartsOpts$legend$layout <- 'horizontal'
        smr$dims[['perAfd']]$presList$highCharts1$highChartsOpts$series[['opm']]$visible <- FALSE
        smr$dims[['perAfd']]$presList$highCharts2$highChartsOpts$legend$layout <- 'horizontal'
        smr$dims[['perAfd']]$presList$highCharts2$highChartsOpts$series[['opm']]$visible <- FALSE
        
        s1$clones[['smr']] <- smr

    }
    
    # init overzicht
    
    s1$overzicht <- list(
        kplSel = kplSel,
        kplLvl = kplLvl,
        perSel = perSel,
        reportFileName = reportFileName,
        pdfFileName = pdfFileName,
        outDir = paste0(getwd(),'/out'),
        widgets = list(),
        statusOk = list(),
        kpiLvl = list()
    )
    
    s1$printProgress <- progress
    s1$progressCount <- 0
    
    # start print loop
    printState$count <- printState$count + 1
}

observeEvent(autoInvalidate(),{
    
    if (printState$count == 0 || (printState$count == printState$prevCount))
        return()
    
    printState$prevCount <- printState$count
    
    smr <- s1$clones[['smr']]

    kplLvl <- s1$overzicht$kplLvl
    kplSel <- s1$overzicht$kplSel
    
    if (kplLvl == 0) {
        kplKey <- '000000'
        kplParent <- ''
    } else {
        kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
        kplParent <- 'Alle afdelingen'
    }
    
    perSel <- s1$overzicht$perSel
    perKey <- per$periodemaandId[per$maandLabel == perSel]
    perParent <- per$tertiaalLabel[per$maandLabel == perSel]
    
    ovzItem <- ovzMRItems[printState$count,]

    if (printState$count == 1) {
        shinyjs::runjs('$.blockUI({ message: null, overlayCSS: { backgroundColor: "#ffffff", opacity:0 }});')
        s1$progressCount = 1
        s1$printProgress$set(message = "Verzamelen data", value = s1$progressCount/10)
        
        setSelection(smr, 'kpl', data.frame(level = kplLvl, parent = kplParent, label = kplSel, stringsAsFactors = FALSE) , dimRefresh = FALSE)
        setSelection(smr, 'perAfd', data.frame(level = 2, label = perSel, stringsAsFactors = FALSE), dimRefresh = FALSE)  
        setSelection(smr, 'mnd',data.frame(level = 1, label = 'Up to the Moon', stringsAsFactors = FALSE), dimRefresh = FALSE)
        setSelection(smr, 'kpi',data.frame(level = 2, parent = 'Iron Bank Ledger', label = 'Realm Balance (x 1000)', stringsAsFactors = FALSE), dimRefresh = FALSE)
        navigate(smr,'perAfd',2,perParent)
        
    } 
    
    if (printState$count < 9) {
        
        if (printState$count > 1) {
            if (smr$dtPrep[['kpi']]$level == s1$overzicht$kpiLvl[[printState$count - 1]]) {  # er is daadwerkelijk naar het gevraagde nivo genavigeerd
                
                s1$overzicht$widgets[[printState$count - 1]] <- smr$dtPrep[['kpi']] 
                
                if (printState$count == 2) {
                    s1$overzicht$widgets[[printState$count - 1]]$hc <- smr$hcPrev[['perAfd']]$widget 
                }
                
                if (printState$count == 6) {
                    zz <- dwhr:::topx(smr,'kpi',5,'waardeNorm','Rest')
                    zz$tab$posneg[zz$tab$Naam == 'Rest'] <- ifelse(zz$tab$Verschil_org[zz$tab$Naam == 'Rest'] >= 0, 1,-1)
                    s1$overzicht$widgets[[printState$count - 1]] <- zz
                }
                if (printState$count == 7) {
                    zz <- dwhr:::topx(smr,'kpi',10,'waardeNorm','Rest')
                    zz$tab$posneg[zz$tab$Naam == 'Rest'] <- ifelse(zz$tab$Verschil_org[zz$tab$Naam == 'Rest'] >= 0, 1,-1)
                    s1$overzicht$widgets[[printState$count - 1]] <- zz
                }
                
            } else {
                s1$overzicht$statusOk[[printState$count - 1]] <- FALSE   
            }
        }
        
        kpiParent <- ovzItem$kpiParent
        kpiLabel <- ovzItem$kpiLabel
        kpiLvl <- ovzItem$kpiLvl
        kpiKey <- ovzItem$ovzCode

        s1$overzicht$data <- rbind(s1$overzicht$data,getText(printState$count,kpiKey,kplKey,perKey,c('maatr','alg','maatrOpm')))
        s1$overzicht$statusOk[[printState$count]] <- navigate(smr,'kpi',kpiLvl,kpiLabel,kpiParent)$navigateOK
        s1$overzicht$kpiLvl[[printState$count]] <- kpiLvl
        printState$count <- printState$count + 1
        
    } else {
     
        if (smr$dtPrep[['kpi']]$level == s1$overzicht$kpiLvl[[printState$count - 1]]) {  # er is daadwerkelijk naar het gevraagde nivo genavigeerd
            s1$overzicht$widgets[[printState$count - 1]] <- smr$dtPrep[['kpi']] 
        } else {
            s1$overzicht$statusOk[[printState$count - 1]] <- FALSE  
        }
        
        s1$overzicht$data <- rbind(s1$overzicht$data,getText(printState$count,0,kplKey,perKey,c('conc','conc1','conc2','conc3')))
        printState$count <- 0

        reportVars <- new.env(parent = emptyenv())

        # rapport data
        reportVars$maand <- 'Up to the Moon'
        reportVars$afdeling <- s1$overzicht$kplSel
        reportVars$periode <- s1$overzicht$perSel
        reportVars$laatstGeslotenId <- laatstGeslotenId
        reportVars$overzicht <- s1$overzicht
        reportVars$src <- normalizePath(s1$overzicht$reportFileName)
        reportVars$adUser <- glob.env$adUser
        
        reportVars$wwwDir <- paste0(getwd(),'/www/')
        reportVars$printDatum <- format(Sys.time(), format = '%Y-%m-%d %H:%M:%S')

        s1$f <- future({
            library(knitr)
            library(kableExtra)
            library(data.table)   # symmetry with samenvatting.R; future-safe for any data.table fn used inside .Rnw chunks
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
        
    } 
        
}, priority = -1)

