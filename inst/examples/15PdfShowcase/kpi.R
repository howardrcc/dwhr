#prevFolder <- initParent
inDeMaand <-'Up to the Moon'

showHjTotaal <- function(env) { if (env$id %in% c('smr','q2')) TRUE else FALSE }
showAantal <- function(env) { if (env$id %in% c('q5')) TRUE else FALSE }
showOpm <- function(env) { if (env$id == 's1' && env$dims[['kpi']]$parent != 'Battle Conduct') TRUE else FALSE }
heeftAbsoluutKpi <- function(env) { if (env$id %in% c('smp','q5')) TRUE else FALSE }

sparkVisKpi <- function(env) {
    if (env$id == 's1' && env$dims[['kpi']]$parent == 'Battle Conduct') FALSE else sparkVis(env) 
}

s1 <- s1 %>%
    addDimView(
        dim = 'kpi',
        name = 'Indicatoren',
        data = kpi,
        levelNames = c('Alle kpi\'s', 'Hoofdgroepen', 'Subgroepen',' Categorie','SubCategorie','kpi'),
        selectMode = 'single',
        initLevel = initLvl,
        initParent = initParent,
        selectLevel = initLvl,
        keepUnused = FALSE,
        selectLabel = initKpi,
        selectParent = initParent,
        selectableLevels = c(2,3,4,5),
        footerLevels = { 
            if (userData$dashOpts$start %in% c('Sieges by Tactic', 'Sieges by Region'))
                c(3,4,5) 
            else 
                c()
        },
        ignoreDims = c('perInst')) %>%
    addMeasure( 
        dim = 'kpi',
        factColumn = c('tellerReal','noemerReal','tellerNorm','noemerNorm','kpiId',
                       'tellerRealVj','noemerRealVj','tellerProgHjT','noemerProgHjT','tellerNormHjT','noemerNormHjT','ids','ids'),
        fun = c('sum','sum','sum','sum','max','sum','sum','sum','sum','sum','sum','opnCount','opnCountVj'),
        as = c('tellerReal','noemerReal','tellerNorm','noemerNorm','kpiId',
               'tellerRealVj','noemerRealVj','tellerProgHjT','noemerProgHjT','tellerNormHjT','noemerNormHjT','Aantal Opnames','Aantal Opnames VJ'),
        levels = c(2,3,4,5)) %>%
    addMeasureDerrived( 
        dim = 'kpi', 
        userFunc = c('waardeRealProg','waardeBegroting','waardeVerschilRealProg','waardeRealVj','waardeProgHjT','waardeNormHjT'),
        viewColumn = c('waardeReal','waardeNorm','waardeVerschil','waardeRealVj','waardeProgHjT','waardeNormHjT'),
        as = c(initRealNaam,'Begroting / Norm','Verschil','Realisatie VJ', 'Prognose dit jaar', 'Begroting dit jaar'), 
        formatColumn = 'format',
        levels = c(2,3,4,5)) %>%
    addMeasureDerrived( 
        dim = 'kpi', 
        userFunc = c('setPosNeg','getOpmerkingTekst','getOpmerking','getSpark','aantalRoodRealProg','aantalGroenRealProg'), 
        viewColumn = c('posneg','opmTekst','opm','waardeTrend','aantalRood','aantalGroen'),
        as = c('posneg','OpmTekst','Opm','Trend','Aantal Rood','Aantal Groen'),
        format = c('standard','standard','standard','standard','integer','integer'),
        levels = c(2,3,4,5)) %>%
    addTextColumn(dim = 'kpi', textColumn = 'url', viewColumn = 'wiki', as = 'Wiki', levels = c(2,3)) %>%
    addTextColumn(dim = 'kpi', textColumn = 'link', viewColumn = 'lnk', as = ' ', levels = c(4)) %>%
    addSortColumn(dim = 'kpi', sortColumn = 'level1Sort', levels = c(1)) %>%
    addSortColumn(dim = 'kpi', sortColumn = 'level2Sort', levels = c(2)) %>%
    addSortColumn(dim = 'kpi', sortColumn = 'level3Sort', levels = c(3)) %>%
    addSortColumn(dim = 'kpi', sortColumn = 'level4Sort', levels = c(4)) %>%
    addSortColumn(dim = 'kpi', sortColumn = 'level5Sort', levels = c(5)) %>%
    addTooltipColumn(dim = 'kpi', tooltipColumn = 'level2Tooltip', levels = c(2)) %>%
    addTooltipColumn(dim = 'kpi', tooltipColumn = 'level3Tooltip', levels = c(3)) %>%
    addTooltipColumn(dim = 'kpi', tooltipColumn = 'level4Tooltip', levels = c(4)) %>%
    addTooltipColumn(dim = 'kpi', tooltipColumn = 'level5Tooltip', levels = c(5)) %>%
    addRowGroupColumn(dim = 'kpi', rowGroupColumn = 'level2Groups', levels = c(2)) %>%
    addPresentation(
        dim = 'kpi',
        type = 'dataTable',
        as = 'tabel1',
        isDefault = TRUE,
        checkUiId = FALSE,
        navOpts = list(
            hideAll = TRUE,
            links = list(
                list(
                    id = 'maatr',
                    label = NULL,
                    placeholder = 'Input maandrapportage..',
                    choiceFun = 'getMaatrChoices',
                    visFun = 'maatrVis',
                    type = 'dropDown',
                    width = 8),
                list( 
                    id = 'rapportage',
                    label = 'maandrapportage',
                    type = 'actionLink',
                    visFun = 'maatrVis',
                    width = 4),
                list(
                    id = 'meetp',
                    label = NULL,
                    placeholder = 'Input 4 maands rapportage..',
                    choiceFun = 'getMeetpChoices',
                    visFun = 'meetpVis',
                    type = 'dropDown',
                    width = 8),
                list( 
                    id = 'rapportage4',
                    label = 'meetplan',
                    type = 'actionLink',
                    visFun = 'meetpVis',
                    width = 4),
                list( 
                    id = 'rvb',
                    label = 'Realm Overview',
                    type = 'actionLink',
                    visFun = 'rvbVis',
                    width = 4)
            )),
        dataTableOpts = list(
            pageLength = 10,
            pageLengthList = c(5,10,20),
            measures = list(
                list(viewColumn = 'lnk',
                     width = 10,
                     print = FALSE,
                     orderable = FALSE),
                list(viewColumn = 'waardeRealVj',
                     width = 40),
                list(viewColumn = 'waardeReal',
                     width = 50),
                list(viewColumn = 'aantalGroen', 
                     width = 20, 
                     visible = heeftAbsoluutKpi),
                list(viewColumn = 'aantalRood', 
                     width = 20, 
                     visible = heeftAbsoluutKpi),
                list(viewColumn = 'waardeNorm',
                     width = 40),
                list(viewColumn = 'waardeVerschil',
                     bgStyle = list(
                         cuts = c(-0.5,0.5),
                         values = c('pink','rgba(0,0,0,0)','lightgreen'),
                         valueColumn = 'posneg'),
                     width = 50),
                list(viewColumn = 'opmTekst', visible = FALSE),
                list(viewColumn = 'waardeProgHjT',
                     visible = showHjTotaal,
                     width = 110),
                list(viewColumn = 'waardeNormHjT',
                     visible = showHjTotaal,
                     width = 40),
                list(viewColumn = 'wiki',
                     align = 'center',
                     width = 20,
                     print = FALSE),
                list(viewColumn = 'waardeTrend', 
                     format = 'sparkline', 
                     visible = sparkVisKpi,
                     align = 'center',
                     width = 40,
                     print = FALSE,
                     orderable = FALSE,
                     sparkOpts = list(
                         type = 'line',
                         addXaxis = TRUE,
                         lineWidth = 1.5,
                         defaultPixelsPerValue = 4,
                         tooltipPrefix = 'Verschil: ',
                         spotColor = FALSE,
                         minSpotColor = FALSE,
                         maxSpotColor = FALSE,
                         fillColor = 'rgba(0,0,0,0)')),
                list(viewColumn = 'opm',
                     format = 'paperclip',
                     align = 'center',
                     tooltip = 'opmTekst',
                     visible = showOpm,
                     bgStyle = list(
                         cuts = c(0),
                         values = c('rgba(0,0,0,0)','yellow')),
                     width = 10,
                     cursor = 'pointer',
                     print = FALSE),
                list(viewColumn = 'posneg',
                     visible = FALSE)
            )
        )
    )

kpiClickMeasureHook <- function(env,event) {
    
    if (!env$id %in% c('s1','q2'))
        return()
    
    if (event$clickViewColumn == 'lnk' && event$clickMemberKey %in% c('XXX800','XXX700')) {
        
        kplSel <- env$dims[['kpl']]$selected$label
        kplLvl <- env$dims[['kpl']]$selected$level
        
        if (kplLvl == 0) {
            kplKey <- '000000'
        } else {
            kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
        }
        
        mndSel <- env$dims[['mnd']]$selected$label
        mndKey <- inMaand$inDeMaand[inMaand$level1Label == mndSel]
        perKey <- getPerInfo(env)$key
        
        if (glob.env$securityModel == 'none' && dashUser == 'dev') {
            command <- paste0('shiny::runApp(appDir = "', getwd(), '/../imd/", port = 3030)')
            system(paste0("R -e '", command,"'"), wait = FALSE)

            linkTarget <- 'http://127.0.0.1:3030?'    
        } else {
            schema <- userData$schema
            server <- userData$server
            port <- userData$port

            linkTarget <- paste0(schema,'://',server,':',port,'/app/imd')
            
            if (length(linkTarget) == 0) {
                stop('Link naar imd-dashboard onbekend')
            }
            
            linkTarget <- paste0(linkTarget,'?')
        }
        
        if (event$clickMemberKey == 'XXX800') 
            shinyjs::runjs(paste0('window.open("', linkTarget, 'aanv=',kplKey,'&mnd=',mndKey,'&per=',perKey,'","Sq 003 Imd dashboard");'))
        if (event$clickMemberKey == 'XXX700') 
            shinyjs::runjs(paste0('window.open("', linkTarget, 'uitv=',kplKey,'&mnd=',mndKey,'&per=',perKey,'","Sq 003 Imd dashboard");'))
        
    } 
    
}

kpiSelectChangeHook <- function(env) {
    
    if (length(getKpiProps(env)) > 0) {
        
        format <- env$kpiProps$format
        sort <- env$kpiProps$sort
        
        if ('kpl' %in% names(env$dims) && env$dims[['kpl']]$pres != 'stub') {
            setColumnName(env,'kpl', viewColFrom ='waardeNorm', colTo = env$kpiProps$normNaam)
            changeFormatMeasure(env,dim = 'kpl', viewColumn = c('waardeReal','waardeNorm','waardeVerschil','waardeRealVj'), format = format)
            
            if (env$kpiProps$isBegroot) {
                
                if (sort == 'asc') {
                    changeDtFgStyle(env,'kpl','tabel1','waardeVerschil',fgStyle = list(cuts = c(0),values = c('red','green')))
                }
                else {
                    changeDtFgStyle(env,'kpl','tabel1','waardeVerschil',fgStyle = list(cuts = c(0),values = c('green','red')))
                }
                
            } else {
                changeDtFgStyle(env,'kpl','tabel1','waardeVerschil',fgStyle = NULL)
            }
            
        }
        
        fmt <- format  # voor de extra details van de trend (er wordt afgerond in highcharts)
        if (substr(format,1,4) == 'perc')
            fmt <- 'perc2'
        if (substr(format,1,7) == 'decimal' || format == 'integer')
            fmt <- 'decimal3'
        if (substr(format,1,4) == 'euro')
            fmt <- 'euro2'
        
        if ('perInst' %in% names(env$dims) && env$dims[['perInst']]$pres != 'stub') {
            setColumnName(env,'perInst',viewColFrom = 'waardeNorm', colTo = env$kpiProps$normNaam)
            changeFormatMeasure(env,dim = 'perInst', viewColumn = c('waardeReal','waardeNorm','waardeProg','waardeVerschil'), format = format)
            changeFormatMeasure(env,dim = 'perInst', viewColumn = c('waardeTrend'), format = fmt)
            if (!env$kpiProps$heeftAbsoluut) {
                env$dims[['perInst']]$pres = 'highCharts1'
            }
        }
        
        if ('kpl2' %in% names(env$dims) && env$dims[['kpl2']]$pres != 'stub') {
            if (!env$kpiProps$heeftAbsoluut) {
                env$dims[['kpl2']]$pres = 'highCharts1'
            }
        }
        
        if ('perAfd' %in% names(env$dims) && env$dims[['perAfd']]$pres != 'stub') {
            setColumnName(env,'perAfd', viewColFrom = 'waardeNorm', colTo = env$kpiProps$normNaam)
            changeFormatMeasure(env,dim = 'perAfd', viewColumn = c('waardeReal','waardeNorm','waardeProg','waardeVerschil') , format = format)
            changeFormatMeasure(env,dim = 'perAfd', viewColumn = c('waardeTrend'), format = fmt)
            if (!env$kpiProps$heeftAbsoluut) {
                env$dims[['perAfd']]$pres = 'highCharts1'
            }
        }   
        
        if ('per' %in% names(env$dims) && env$dims[['per']]$pres != 'stub') {
            setColumnName(env,'per', viewColFrom = 'waardeNorm', colTo = env$kpiProps$normNaam)
            changeFormatMeasure(env,dim = 'per', viewColumn = c('waardeRealProg','waardeNorm','waardeRealVj','waardeVerschil') , format = format)
            
            if (env$kpiProps$isBegroot) {
                
                if (sort == 'asc') {
                    changeDtFgStyle(env,'per','tabel1','waardeVerschil',fgStyle = list(cuts = c(0),values = c('red','green')))
                }
                else {
                    changeDtFgStyle(env,'per','tabel1','waardeVerschil',fgStyle = list(cuts = c(0),values = c('green','red')))
                }
                
            } else {
                changeDtFgStyle(env,'per','tabel1','waardeVerschil',fgStyle = NULL)
            }
            
        }   
        
        if ('gs' %in% names(env$dims) && env$dims[['gs']]$pres != 'stub') {    
            changeFormatMeasure(env,dim = 'gs', viewColumn = c('waardeReal','waardeNorm','waardeVerschil','waardeRealVj'), format = format)
            
            if (env$kpiProps$isBegroot) {
                
                if (sort == 'asc') {
                    changeDtFgStyle(env,'gs','tabel1','waardeVerschil',fgStyle = list(cuts = c(0),values = c('red','green')))
                }
                else {
                    changeDtFgStyle(env,'gs','tabel1','waardeVerschil',fgStyle = list(cuts = c(0),values = c('green','red')))
                }
                
            } else {
                changeDtFgStyle(env,'gs','tabel1','waardeVerschil',fgStyle = NULL)
            }
        }
    }
    
    # if (env$id == 's1') {
    # 
    #     if (env$dims[['kpi']]$selected$label != 'Unique Smallfolk' && env$dims[['mnd']]$selected$label == 'Within the Moon') {
    #         env$dims[['per']]$selectMode <- 'multi'
    #         env$dims[['per']]$reactive$presChange <- env$dims[['per']]$reactive$presChange + 1
    #     } else {
    #         env$dims[['per']]$selectMode <- 'single'
    #         env$dims[['per']]$reactive$presChange <- env$dims[['per']]$reactive$presChange + 1
    #     }
    #     
    #     if (prevFolder != env$dims[['kpi']]$ancestors[3]) {
    #         
    #         if (env$dims[['kpi']]$ancestors[3] == 'Houses Pact Compliance') {
    #             inDeMaand <<- env$dims[['mnd']]$selected$label
    #             setSelection(env, 'mnd',data.frame(level = 1, label = 'Within the Moon', stringsAsFactors = FALSE))
    #         } 
    #         
    #         if (prevFolder == 'Houses Pact Compliance') {
    #             setSelection(env, 'mnd',data.frame(level = 1, label = inDeMaand, stringsAsFactors = FALSE))
    #         }
    #         
    #         prevFolder <<- env$dims[['kpi']]$ancestors[3]
    #     }
    #     
    # }
    
}

kpiLevelChangeHook <- function(env) {
    
    dd <-  env$dims[['kpi']]
    
    dd$reactive$linksChange <- dd$reactive$linksChange + 1
    
    if (dd$parent == 'Battle Conduct') {
        setDtVisible(env = env, dim = 'kpi', 'tabel1',c('waardeReal','waardeNorm','waardeVerschil','waardeRealVj','wiki'), FALSE) 
    } else {
        setDtVisible(env = env, dim = 'kpi', 'tabel1',c('waardeReal','waardeNorm','waardeVerschil','waardeRealVj','wiki'), TRUE) 
    }

    setDtVisible(env = env, dim = 'kpi', 'tabel1',c('waardeTrend'), sparkVisKpi)
    
    if (dd$level > 1) {
        
        if (dd$ancestors[3] == 'Iron Bank Ledger') {
            if (env$id == 's1')
                dd$selectableLevels = c(2,3,4,5) 
            dd$footerLevels = c(3,4,5)
            setDtVisible(env = env, dim = 'kpi', 'tabel1','wiki', FALSE) 
        }
          
        if (dd$ancestors[3] == 'Battle Conduct') {
            
            if (is.na(dd$ancestors[4])) {
                if (env$id == 's1')
                    dd$selectableLevels = c(3) 
                dd$footerLevels = c()
            }
            
            if (dd$ancestors[4] %in% c('Other Battle Burdens','Sieges by Burden')) {
                if (env$id == 's1')
                    dd$selectableLevels = c(3) 
                dd$footerLevels = c()
            } 
            
            if (dd$ancestors[4] %in% c('Sieges by Tactic','Sieges by Region')) {
                if (env$id == 's1')
                    dd$selectableLevels = c(3,4,5) 
                dd$footerLevels = c(3,4,5)
                setDtVisible(env = env, dim = 'kpi', 'tabel1','wiki', FALSE) 
            }
            
        }
        
        if (!is.na(dd$ancestors[4]) && dd$ancestors[4] == 'Septon Cleansing Compliance') {
            if (env$id == 's1')
                dd$selectableLevels = c(2,3,4) 
        } else {
            if (!(dd$ancestors[3] %in% c('Iron Bank Ledger','Battle Conduct'))) {
                if (env$id == 's1')
                    dd$selectableLevels = c(2) 
                dd$footerLevels = c()
            }
        }
    }
}


observeEvent(input[['s1KpiRowGroupEvent']],{
    
    dd <- s1$dims[['kpi']]
    grp <- input[['s1KpiRowGroupEvent']]$rowGroup
    
    if (grp %in% c('Iron Bank Ledger','Bannerman Levies')) {
        par <- 'Alle kpi\'s'
        lvl <- 2
    } else {
        par <- 'Battle Conduct'
        lvl <- 3
    }
    
    parent <- kpi$kpiParent[kpi$kpiLabel == dd$selected$label & kpi$kpiParent != 'Realm Overview']
    
    navigate(s1,'kpi',lvl, grp,par)
    
    if (!is.null(parent) && parent == grp) {
        setSelection(s1, 'kpi',data.frame(level = lvl, parent = grp, label = dd$selected$label, stringsAsFactors = FALSE))
    }
    
    
})

observeEvent(s1$dims[['kpi']]$reactive$selectChange,{

    if (unique(kpi[kpi$kpiLabel == s1$dims[['kpi']]$selected$label,]$gs)) {
        shinyjs::show(selector = "li a[data-value=Geldstroom]")
    } else {
        if (isNull(input$afdgs,'') == 'Geldstroom') {
            updateTabsetPanel(session,'afdgs','Afdeling')
        }
        shinyjs::hide(selector = "li a[data-value=Geldstroom]")
    }
})


s1 <- s1  %>% 
    addDimView(
        dim = 'nunq',
        name = 'nunq',
        data = nunq,
        type = 'input',
        levelNames = c('All','Kies'),
        useLevels = 1,
        selectLabel = 'nunq',
        selectLevel = 1,
        state = ifelse(userData$dashOpts$perState == 'tertiaal','enabled','disabled'))
