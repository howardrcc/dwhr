library(knitr)
library(kableExtra)
source('latexSparklines.R', local = TRUE)

printRvbState <- new.env()
printRvbState$count = 0
printRvbState$prevCount = 0 

observeEvent(input$rvb,{
    showModal(rvbChoice())
})


rvbChoice <- function() {
    modalDialog(
        
        selectizeInput('rvbdd','Kies bestuurder', choices = unique(bestuurder$rvbLid), selected = 'Buren'),
        
        footer = tagList(
            actionButton("rvbCancel","Cancel"),
            actionButton("rvbPdf", "pdf")
        ),
        
        size = 's',
        fade = FALSE
    )
}

observeEvent(input$rvbCancel,{
    removeModal()
})

observeEvent(input$rvbPdf,{
    removeModal()
    
    printRvbInit('samenvatting.Rnw',NULL,input$rvbdd)
    
})

printRvbInit <- function(reportFileName, perSel = NULL, rvbLid) {
    
    if (!file.exists(paste0(getwd(),'/',reportFileName))) {
        stop(paste0('reporFileName: ',reportFileName, ' does not exist'))
    }
    
    if (is.null(perSel)) {
        perSel <- s1$dims[['perInst']]$selected$label    
    }
    
    perParent <- per$tertiaalLabel[per$maandLabel == perSel]
    pdfFileName <- paste0('Samenvatting ',rvbLid,' ',perSel,'.pdf')

    # reset de print loop
    
    printRvbState$count = 0
    progress <- Progress$new(session)
    
    if (is.null(s1$clones[['s5']])) {
        
        progress$set(message = "Initialisatie printen", value = 0)
        
        s5 <- clone.star(
            from = s1, 
            toId = 's5', 
            print = TRUE,
            dimViews = list( 
                kpi = list(measures = FALSE),
                kpl = list(presentations = c('tabel1')),
                perAfd = list(measures = FALSE), 
                mnd = list(measures = FALSE))) %>%
            setSelection('kpi',data.frame(level = 2, parent = 'Iron Bank Ledger', label = 'Realm Balance (x 1000)', stringsAsFactors = FALSE), dimRefresh = FALSE) %>%
            setSelection('kpl', s1$dims[['kpl']]$rootSelected , dimRefresh = FALSE, selectChange = FALSE) %>%
            setSelection('perAfd', data.frame(level = 2, label = perSel, parent = perParent, stringsAsFactors = FALSE), dimRefresh = FALSE, selectChange = FALSE) %>%
            setSelection('mnd',data.frame(level = 1, label = 'Up to the Moon', stringsAsFactors = FALSE), dimRefresh = FALSE, selectChange = FALSE) %>%
            renderDims(input,output)
        
        s1$clones[['s5']] <- s5

    }
    
    # init overzicht
    
    s1$overzicht <- list(
        perSel = perSel,
        perParent = perParent,
        rvbLid = rvbLid,
        reportFileName = reportFileName,
        pdfFileName = pdfFileName,
        outDir = paste0(getwd(),'/out'),
        tab = list()
    )
    
    s1$printProgress <- progress
    s1$progressCount <- 0
    
    # start print loop
    printRvbState$count <- printRvbState$count + 1
}

observeEvent(autoInvalidate(),{
    
    if (printRvbState$count == 0 || (printRvbState$count == printRvbState$prevCount))
        return()
    
    printRvbState$prevCount <- printRvbState$count
    
    s5 <- s1$clones[['s5']]
    
    perSel <- s1$overzicht$perSel
    perParent <- s1$overzicht$perParent
    perKey <- per$periodemaandId[per$maandLabel == perSel]
    
    if (printRvbState$count == 1) {
        shinyjs::runjs('$.blockUI({ message: null, overlayCSS: { backgroundColor: "#ffffff", opacity:0 }});')
        s1$progressCount = 1
        s1$printProgress$set(message = "Verzamelen data", value = s1$progressCount/10)
        setSelection(s5, 'kpi',data.frame(level = 2, parent = 'Iron Bank Ledger', label = 'Realm Balance (x 1000)', stringsAsFactors = FALSE), dimRefresh = FALSE) 
        setSelection(s5, 'perAfd', data.frame(level = 2, label = perSel, parent = perParent, stringsAsFactors = FALSE), dimRefresh = FALSE) 
    } 
    
    
    if (printRvbState$count < 4) {
        
        if (printRvbState$count == 2) {
            s1$overzicht$tab[[printRvbState$count - 1]] <- s5$dtPrep[['kpl']]$tab 
            setSelection(s5, 'kpi',data.frame(level = 2, parent = 'Bannerman Levies', label = 'Crown Bannermen (FTE)', stringsAsFactors = FALSE), dimRefresh = FALSE)
        }
        
        if (printRvbState$count == 3) {
            s1$overzicht$tab[[printRvbState$count - 1]] <- s5$dtPrep[['kpl']]$tab
            setSelection(s5, 'kpi',data.frame(level = 5, parent = 'Iron Throne Tariffs (excl. grants)', label = 'Tribute Cap Breach', stringsAsFactors = FALSE), dimRefresh = FALSE)
        }
        
        printRvbState$count <- printRvbState$count + 1
        
    } else {
        
        s1$overzicht$tab[[printRvbState$count - 1]] <- s5$dtPrep[['kpl']]$tab 
        
        printRvbState$count <- 0
    
        
        tab1 <- as.data.table(s1$overzicht$tab[[1]][,c('Naam','Verschil','Verschil_org','Trend')])
        tab1 <- tab1[tab1$Naam %in% bestuurder$afdeling[bestuurder$rvbLid == s1$overzicht$rvbLid],]

        tab1 <- rbind(
            tab1,
            data.table(
                Naam = "Totaal",
                Verschil = paste0('\U20AC ', formatC(digits = 0, format = 'f', sum(tab1$Verschil_org), big.mark='.',decimal.mark = ',')),
                Verschil_org = sum(tab1$Verschil_org),
                Trend = "0")
        )

        tab2 <- as.data.table(s1$overzicht$tab[[2]][,c('Naam','Verschil','Verschil_org','Trend')])
        tab2 <- tab2[tab2$Naam %in% bestuurder$afdeling[bestuurder$rvbLid == s1$overzicht$rvbLid],]
        tab2 <- rbind(
            tab2,
            data.table(
                Naam = "Totaal",
                Verschil = formatC(digits = 1, format = 'f', sum(tab2$Verschil_org), big.mark='.',decimal.mark = ','),
                Verschil_org = sum(tab2$Verschil_org),
                Trend = "0")
        )
        
        tab3 <- as.data.table(s1$overzicht$tab[[3]][,c('Naam','Verschil','Verschil_org','Trend')])
        tab3 <- tab3[tab3$Naam %in% bestuurder$afdeling[bestuurder$rvbLid == s1$overzicht$rvbLid],]
        
        # tekenwisseling voor productie
        
        tab3$Verschil_org <- -1 * tab3$Verschil_org
        tab3$Verschil <- paste0('\U20AC ', formatC(digits = 0, format = 'f', tab3$Verschil_org, big.mark='.',decimal.mark = ','))
        
        tab3 <- rbind(
            tab3,
            data.table(
                Naam = "Totaal",
                Verschil = paste0('\U20AC ', formatC(digits = 0, format = 'f', sum(tab3$Verschil_org), big.mark='.',decimal.mark = ',')),
                Verschil_org = sum(tab3$Verschil_org),
                Trend = "0")
        )
        
        keys <- union(tab1$Naam,union(tab2$Naam,tab3$Naam))
        
        fnRap <- function(tbl,pos,addXaxis,flip) {
            
            tbl$posneg <- 0
            
            if (pos == 1)
                tbl$posneg <- ifelse(tbl$Verschil_org < 0, -1, 1)
            if (pos == -1)
                tbl$posneg <- ifelse(tbl$Verschil_org > 0, -1, 1)
            
            if (!all(tbl$posneg == 0)) {
                
                tbl$Verschil <- cell_spec(
                    tbl$Verschil, 
                    "latex", 
                    color = ifelse(tbl$posneg == 1, "red", "black"))
            }
            
            tbl$posneg <- NULL
            
            tbl$Trend <- unlist(lapply(tbl$Trend,function(q) {
                
                zz <- tail(eval(parse(text = paste0('c(',q,')'))),4)
                if (flip) {
                    zz <- -1 * round(zz,2)
                } else {
                    zz <- round(zz,2)
                }
                
                if (all(zz == 0)) 
                    return('')
                
                x <- 1:length(zz)
                fn <- function(x,a,b) {(a*x) + b}
                co <- coef(lm(zz~x))
                
                if (sign(co[2] > 0)) {
                    return('\\vcenteredincludeup{arrow-up.png}')
                }
                
                if (sign(co[2] < 0)) {
                    return('\\vcenteredincludedown{arrow-down.png}')
                }
                
                return('')
                                         
                # if (addXaxis)
                #     latexSparkline(
                #         x = rep(0.01,length(zz)), 
                #         x2 = zz,
                #         ylim = c(min(zz,0.01),max(zz,0.01)),
                #         lineColor = 'gray', 
                #         lineColor2 = 'blue')
                # else
                #     latexSparkline(x = zz)
            }))
            
            tbl[,c('Naam','Verschil','Trend')]
            
        }
        
        tab1 <- fnRap(tab1,-1,TRUE,FALSE)    
        names(tab1) <- c('Naam','Verschil1','Trend1')
        
        tab2 <- fnRap(tab2,1,TRUE,FALSE)    
        names(tab2) <- c('Naam','Verschil2','Trend2')
        
        tab3 <- fnRap(tab3,0,TRUE,TRUE)    
        names(tab3) <- c('Naam','Verschil3','Trend3')
        
        tbl <- tab1[tab2[tab3[keys, on = c("Naam")], on = c("Naam")], on = c("Naam")]

        score <<- initScore()
        scr <- score[perCode == perKey,c('kostenplaats','score')]
        
        names(scr) <- c('level2Code','Score')
        scr <- scr[kpl, on = c('level2Code'), nomatch = 0][,c('level2Label','Score')]
        names(scr) <- c('Naam','Score')
        scr <- scr[scr$Naam %in% bestuurder$afdeling[bestuurder$rvbLid == s1$overzicht$rvbLid],]
        
        nm <- names(tbl)
        tbl <- scr[tbl, on = "Naam"][, c(nm,'Score'), with = FALSE]
        
        tbl$Score <- ifelse(is.na(tbl$Score),'white',
                                ifelse(tbl$Score == 'Rood','red',
                                       ifelse(tbl$Score == 'Groen', 'light-green','orange2')))
        
        tbl$Naam <- latexEscape(tbl$Naam)
    
        tbl$Verschil1[is.na(tbl$Verschil1)] <- ""
        tbl$Trend1[is.na(tbl$Trend1)] <- ""
        
        tbl$Verschil2[is.na(tbl$Verschil2)] <- ""
        tbl$Trend2[is.na(tbl$Trend2)] <- ""
        
        tbl$Verschil3[is.na(tbl$Verschil3)] <- ""
        tbl$Trend3[is.na(tbl$Trend3)] <- ""
        
        cmnts <- comments[type %in% c('conc','conc1','conc2','conc3') & perCode == perKey, c('kostenplaats','txt','updatedBy','lastUpdateDate','type')]
       
        names(cmnts) <- c('level2Code','txt','usr','lastUpdateDate','type')
        cmnts <- cmnts[kpl, on = c('level2Code'), nomatch = 0][,c('level2Label','txt','usr','lastUpdateDate','type')]
        names(cmnts) <- c('Naam','txt','usr','lastUpdateDate','type')
        cmnts <- cmnts[cmnts$Naam %in% bestuurder$afdeling[bestuurder$rvbLid == s1$overzicht$rvbLid],]
        
        cmnts[glob.env$adUser, updatedBy := latexEscape(paste0(naam,' (',functie,')')), on = 'usr']
        
        cmnts$txt <- latexEscape(cmnts$txt)
        
        reportVars <- new.env(parent = emptyenv())
        reportVars$periode <- s1$overzicht$perSel
        reportVars$laatstGeslotenId <- laatstGeslotenId
        reportVars$tbl <- tbl
        reportVars$cmnts <- cmnts
        reportVars$scr <- scr
        reportVars$wwwDir <- paste0(getwd(),'/www/')
        reportVars$printDatum <- format(Sys.time(), format = '%Y-%m-%d %H:%M:%S')
        reportVars$bestuurder <- s1$overzicht$rvbLid
        reportVars$src <- normalizePath(s1$overzicht$reportFileName)

        s1$f <- future({
            library(knitr)
            library(kableExtra)
            library(data.table)   # samenvatting.Rnw uses bare dcast()
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



