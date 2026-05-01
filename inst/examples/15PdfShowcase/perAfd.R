heeftAbsoluutPer <- function(env) { 
  if(env$id == 's1' && all(kpi$level1Label[kpi$kpiLabel == env$dims[["kpi"]]$selected$label] %in% c('Houses Pact Compliance'))) TRUE else FALSE }

s1 <- s1 %>% 
    addDimView( 
        dim = 'perAfd',
        name = '',
        data = per,
        levelNames = c('Alle Periodes', 'jaar', 'tertiaal', 'periode'),
        selectMode = 'single',
        ignoreDims = c('perInst'),
        ignoreParent = TRUE,
        orderBy = 'key',
        useLevels = c(2,3),
        initLevel = ifelse(userData$dashOpts$perState == 'tertiaal',2,3),
        selectLabel = ifelse(userData$dashOpts$perState == 'tertiaal',laatstGeslotenT,laatstGesloten),
        selectLevel = ifelse(userData$dashOpts$perState == 'tertiaal',2,3),
        selectableLevels = c(2,3),
        footerLevels = c(),
      #  state = 'hidden',
        fixedMembers = TRUE) %>%
    addMeasure( 
        dim = 'perAfd',
        factColumn = c('kpiId', 'periodemaandId', 'tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','tellerRealVj','noemerRealVj','ids','ids'),
        fun = c('max','mean','sum','sum','sum','sum','sum','sum','opnCount','opnCountVj'),
        as = c('kpiId','maand','tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','tellerRealVj','noemerRealVj','Aantal Opnames','Aantal Opnames VJ')) %>%
    addMeasureDerrived( 
        dim = 'perAfd',  
        userFunc = c('waardeRealisatie','waardeBegroting','waardePrognose','waardeVerschil', 'waardeTrend','aantalRood','aantalRoodProg','aantalGroen','aantalGroenProg','waardeRealProg','waardeRealVj'),
        as = c('Realisatie', initNormNaam, 'Prognose', 'Verschil', 'Trend','Aantal Rood', 'Prognose Rood','Aantal Groen', 'Prognose Groen','Realisatie/Prognose', 'Real VJ'), 
        viewColumn = c('waardeReal','waardeNorm','waardeProg','waardeVerschil','waardeTrend','aantalRood','aantalRoodProg','aantalGroen','aantalGroenProg','waardeRealProg','waardeRealVj'),
        format = c(initFormat,initFormat,initFormat,initFormat,initFormat,'integer','integer','integer','integer',initFormat,initFormat)) %>%
    addMeasureDerrived( 
        dim = 'perAfd', 
        userFunc = c('getOpmerkingTekst','getOpmerking'), 
        viewColumn = c('opmTekst','opm'),
        as = c('Opmerking','Opm'), 
        levels = c(2,3))

perAfdVjVis <- function(env) {
    if (isKV(env)) {
        if (env$id == 'smp')
            return(TRUE)
        if (is.null(env$hcPrev[['perAfd']]))
            return(FALSE)
        else 
            return(env$hcPrev[['perAfd']]$seriesOpts[[1]]$visible)
    }
    else 
        return(FALSE)
}

setTitlePerAfd <- function(env) {
    if (env$id == 'smp')
        return(isNull(s1$overzicht$title,''))
    if (unique(kpi[kpi$kpiLabel == env$dims[['kpi']]$selected$label,]$gs)) {
        paste0('Afdeling ', env$dims[['kpl']]$selected$label, ': ', env$dims[['mnd']]$selected$label, ' / ', env$dims[['gs']]$selected$label, ' / ', env$dims[['kpi']]$selected$label)
    } else {
        paste0('Afdeling ', env$dims[['kpl']]$selected$label, ': ', env$dims[['mnd']]$selected$label, ' / ', env$dims[['kpi']]$selected$label)
    }
}

hcOpts1 <- hcOptsBaseColumn

hcOpts1$yAxis[[2]] <- list(
    opposite = TRUE,
    reversed = TRUE,
    max = 10,
    visible = FALSE)

hcOpts1$title$text <- setTitlePerAfd
hcOpts1$series <- list(
    # waardeRealVj = list(
    #     viewColumn = 'waardeRealVj',
    #     type = 'column',
    #     visible = perAfdVjVis,
    #     showInLegend = isKV,
    #     grouping = TRUE,
    #     dataLabels = list(enabled = TRUE),
    #     color = '#b7e2b6',
    #     pointPadding = 0,
    #     ttXtraData = fn_ttXtraData1,
    #     yAxis = 0,
    #     stack = 0),
    opm = list(
        viewColumn = 'opm',
        type = 'column',
        visible = TRUE,
        showInLegend = FALSE,
        ttXtraData = list(
            includeSelf = FALSE,
            enabled = TRUE,
            viewColumns = c('opmTekst')),
        dataLabels = list(enabled = FALSE),
        color = 'yellow',
        borderRadius = 0,
        groupPadding = 0.0,
        pointPadding = 0.0,
        yAxis = 1),
    waardeReal = list(
        viewColumn = 'waardeReal',
        type = 'column',
        visible = TRUE,
        grouping = TRUE,
        dataLabels = list(enabled = TRUE),
        color = '#41b6c4',
        pointPadding = 0,
        ttXtraData = fn_ttXtraData1,
        yAxis = 0,
        stack = 0), 
    waardeProg = list(
        viewColumn = 'waardeProg',
        type = 'column',
        visible = TRUE,
        dataLabels = list(enabled = TRUE),
        color = '#41b6c4',
        pattern = 'stripe1',
        pointPadding = 0,
        ttXtraData = fn_ttXtraData2,
        yAxis = 0,
        stack = 0,
        linkedTo = ':previous'),
    waardeVerschil = list(
        viewColumn = 'waardeVerschil',
        type = 'column',
        visible = FALSE,
        dataLabels = list(enabled = TRUE),
        color = 'red',
        pointPadding = 0.1,
        yAxis = 0,
        stack = 1),
    waardeNorm = list( 
        viewColumn = 'waardeNorm',
        type = 'line',
        visible = TRUE,
        dataLabels = list(enabled = FALSE),
        color = 'blue',
        yAxis = 0),
    waardeTrend = list(
        viewColumn = 'waardeTrend',
        type = 'line',
        visible = FALSE,
        showInLegend = userData$dashOpts$mndSel$label == 'Within the Moon',
        dataLabels = list(enabled = FALSE),
        color = 'lightgreen',
        yAxis = 0)
)

s1 <- s1 %>% 
    addPresentation( 
        dim = 'perAfd',
        type = 'highCharts',
        as = 'mixedChart1',
        isDefault = TRUE,
        checkUiId = FALSE,
        navOpts = list(
            hideBreadCrumb = TRUE,
            hideNoFilter = TRUE,
            noDrill = TRUE
        ),
        highChartsOpts = hcOpts1)


hcOpts2 <- hcOptsBaseColumn

hcOpts2$yAxis[[2]] <- list(
    opposite = TRUE,
    reversed = TRUE,
    max = 10,
    visible = FALSE)

hcOpts2$title$text <- setTitlePerAfd

hcOpts2$series <- list(
    opm = list(
        viewColumn = 'opm',
        type = 'column',
        visible = TRUE,
        showInLegend = FALSE,
        ttXtraData = list(
            includeSelf = FALSE,
            enabled = TRUE,
            viewColumns = c('opmTekst')),
        dataLabels = list(enabled = FALSE),
        color = 'yellow',
        borderRadius = 0,
        groupPadding = 0.0,
        pointPadding = 0.0,
        yAxis = 1),
    aantalRood = list(
        viewColumn = 'aantalRood',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        showInLegend = TRUE,
        dataLabels = list(enabled = TRUE),
        color = 'tomato',
        ttXtraData = fn_ttXtraData3,
        enableMouseTracking = TRUE,
        pointPadding = 0),
    aantalRoodProg = list(
        viewColumn = 'aantalRoodProg',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        dataLabels = list(enabled = TRUE),
        color = 'tomato',
        ttXtraData = fn_ttXtraData4,
        pattern = 'stripe1',
        enableMouseTracking = TRUE,
        pointPadding = 0,
        linkedTo = ':previous'),
    aantalGroen = list(
        viewColumn = 'aantalGroen',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        showInLegend = TRUE,
        dataLabels = list(enabled = TRUE),
        color = 'green',
        ttXtraData = fn_ttXtraData3,
        enableMouseTracking = TRUE,
        pointPadding = 0),
    aantalGroenProg = list(
        viewColumn = 'aantalGroenProg',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        dataLabels = list(enabled = TRUE),
        color = 'green',
        ttXtraData = fn_ttXtraData4,
        pattern = 'stripe1',
        enableMouseTracking = TRUE,
        pointPadding = 0,
        linkedTo = ':previous')
)

s1 <- s1 %>%
    addPresentation(
        dim = 'perAfd',
        type = 'highCharts',
        as = 'mixedChart2',
        isDefault = FALSE,
        checkUiId = FALSE,
        navOpts = list(
            hideBreadCrumb = TRUE,
            hideNoFilter = TRUE,
            noDrill = TRUE
        ),
        highChartsOpts = hcOpts2)

if (max(userData$dashOpts$kplSel$level) == 0)
    s1$dims[['perAfd']]$state <- 'hidden'

perAfdSelectChangeHook <- function(env) {

    perInfo <- getPerInfo(env)
        
    if (env$id != 's5') {
        setColumnName(env, 'kpi', viewColFrom = 'waardeReal', colTo = perInfo$realNaam)
    }
    
    if (env$id == 's1') {
        
        env$dims[['kpi']]$reactive$linksChange <- env$dims[['kpi']]$reactive$linksChange + 1
        
        setColumnName(env, 'kpl', viewColFrom = 'waardeReal', colTo = perInfo$realNaam)
        setColumnName(env, 'gs', viewColFrom = 'waardeReal', colTo = perInfo$realNaam)
    }
    
    if (!is.null(env$dims[['perInst']]))
        setSelection(env,'perInst',env$dims[['perAfd']]$selected)
        
}


s1 <- s1 %>%
    addPresentation(
        dim = 'perAfd',
        uiId = 'per',
        name = 'Periode',
        type = 'dataTable',
        as = 'tabel1',
        state = 'hidden',
        isDefault = FALSE,
        #selectMode = 'multi',
        checkUiId = FALSE,
        navOpts = list(
            hideBreadCrumb = TRUE,
            syncNav = FALSE,
            noDrill = TRUE
        ),
        dataTableOpts = list(
            pageLength = 12,
            pageLengthList = c(3,6,12,24),
            measures = list(
                list(viewColumn = 'waardeRealVj'),  
                list(viewColumn = 'waardeRealProg'),
                list(viewColumn = 'aantalGroen', 
                     width = 20, 
                     visible = heeftAbsoluutPer),
                list(viewColumn = 'aantalRood', 
                     width = 20, 
                     visible = heeftAbsoluutPer),
                list(viewColumn = 'waardeNorm'),
                list(viewColumn = 'waardeVerschil',
                     fgStyle = list(
                         cuts = c(0),
                         values = c('red','green')),
                     fontWeight = 'bold'),
                list(viewColumn = 'opmTekst', visible = FALSE),
                list(viewColumn = 'opm',
                     format = 'paperclip',
                     align = 'center',
                     tooltip = 'opmTekst',
                     bgStyle = list(
                         cuts = c(0),
                         values = c('rgba(0,0,0,0)','yellow')),
                     width = 10)
                )))

observeEvent(s1$dims[['perAfd']]$reactive$selectedIdsChange,{
    if (nrow(s1$dims[['perAfd']]$selected) > 1) {
        dimChangeState(s1,'mnd','hidden')
        dimChangeState(s1,'nunq','hidden')
    } else {
        dimChangeState(s1,'mnd','enabled')
        if (s1$dims[['perAfd']]$selected$level == 2)
            dimChangeState(s1,'nunq','disabled')
        
        shinyjs::js$showDim(dim = 's1Mnd')  
        s1$dims[['mnd']]$visible <- TRUE
    }
})

observeEvent(s1$dims[['perAfd']]$reactive$isFiltered,{
    
    if (s1$dims[['perAfd']]$reactive$isFiltered) {
        shinyjs::addCssClass(class = 'tabRed', selector = "#afdgs li a[data-value=Periode]")
    } else {
        shinyjs::removeCssClass(class = 'tabRed', selector = "#afdgs li a[data-value=Periode]")
    }
})

perLinkState <- reactiveValues(link = userData$dashOpts$perState)

output[['perLink']] <- renderUI({
  
  s1$dims[['kpi']]$reactive$selectedIdsChange
  kpiSel <- s1$dims[['kpi']]$selected$label
  
  tagList(
    span(style = 'font-weight: bold;','Indeling:'),
    HTML('&nbsp&nbsp&nbsp'),
    if (perLinkState$link == 'maand') {
          span('Per maand',style = 'font-weight:bold; background-color: #e0e0e0; padding-left:10px; padding-right:10px; padding-top:1px; padding-bottom:4px;')
    } else {
        if (!isNull(input[['s1PerTDimWait']],FALSE)) {
            actionLink('perMaand','Per maand')
        } 
    },
    HTML('&nbsp&nbsp&nbsp'),
    if (perLinkState$link == 'tertiaal' || kpiSel == 'Unique Smallfolk') {
        if (kpiSel == 'Unique Smallfolk') 
            span('Per 4 maanden')  
        else     
            span('Per 4 maanden',style = 'font-weight:bold; background-color: #e0e0e0; padding-left:10px; padding-right:10px; padding-top:1px; padding-bottom:4px;')
    } else {
        if (!isNull(input[['s1perDimWait']],FALSE)) {
            actionLink('per4Maand','Per 4 maanden')
        }
    })
  
})

setPerMaand <- function(perSel = NULL ) {
    
    if (is.null(perSel)) {
        perInfo <- getPerInfo(s1)
        
        sel <- tail(per$maandLabel[per$tertiaalCode == perInfo$key],1)
        parent <- max(perInfo$sel)
    } else {
        sel <- perSel$label
        parent <- perSel$parent
    }
    
    setSelection(s1,'perAfd',data.frame(level = 2, label = sel, stringsAsFactors = FALSE))
    navigate(env = s1, dim = 'perAfd', level = 2, parent = parent)
    navigate(env = s1, dim = 'perInst', level = 2, parent = parent)
    navigate(env = s1, dim = 'per', level = 2, parent = parent)
    
    dimChangeState(s1,'mnd4','disabled')
    dimChangeState(s1,'nunq','disabled')
    
    perLinkState$link <- 'maand'  
    
}
observeEvent(input[['perMaand']],{
    setPerMaand()
})

setPer4Maand <- function(perSel = NULL) {
    parent <- 'Alle Periodes'
    
    if (is.null(perSel)) {
        perInfo <- getPerInfo(s1)
        sel <- per$tertiaalLabel[per$maandCode == perInfo$key]
        
    } else {
        sel <- perSel$label
        parent <- perSel$parent
    }

    setSelection(s1,'perAfd',data.frame(level = 1, label = sel, stringsAsFactors = FALSE))
    navigate(env = s1, dim = 'perAfd', level = 1, parent = parent)
    navigate(env = s1, dim = 'perInst', level = 1, parent = parent)
    navigate(env = s1, dim = 'per', level = 1, parent = parent)
    
    dimChangeState(s1,'mnd4','hidden')
    dimChangeState(s1,'nunq','hidden')
    
    perLinkState$link <- 'tertiaal'
    
}
observeEvent(input[['per4Maand']],{
    setPer4Maand()
})




