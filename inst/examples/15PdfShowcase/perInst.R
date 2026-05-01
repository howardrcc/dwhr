s1 <- s1 %>%
    addDimView( 
        dim = 'perInst',
        name = '',
        data = per,
        levelNames = c('Alle Periodes', 'jaar', 'tertiaal', 'periode'),
        selectMode = 'single',
        ignoreDims = c('kpl','perAfd'),
        ignoreParent = TRUE,
        orderBy = 'key',
        useLevels = c(2,3),
        initLevel = ifelse(userData$dashOpts$perState == 'tertiaal',2,3),
        selectLabel = ifelse(userData$dashOpts$perState == 'tertiaal',laatstGeslotenT,laatstGesloten),
        selectLevel = ifelse(userData$dashOpts$perState == 'tertiaal',2,3),
        selectableLevels = c(2,3),
        footerLevels = c(),
        fixedMembers = TRUE) %>%
    addMeasure( 
        dim = 'perInst',
        factColumn = c('periodemaandId', 'tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','tellerRealVj','noemerRealVj','ids'),
        fun = c('mean','sum','sum','sum','sum','sum','sum','opnCount'),
        as = c('maand','tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','tellerRealVj','noemerRealVj','Aantal Opnames')) %>%
    addMeasureDerrived( 
        dim = 'perInst',  
        userFunc = c('waardeRealisatie','waardeBegroting','waardePrognose','waardeVerschil', 'waardeTrend', 'aantalRood','aantalRoodProg','aantalGroen','aantalGroenProg','waardeRealVj','aantalRoodVj','aantalGroenVj'),
        as = c('Realisatie', initNormNaam, 'Prognose', 'Verschil', 'Trend','Aantal Rood', 'Prognose Rood','Aantal Groen', 'Prognose Groen','Real VJ','Aantal Rood VJ','Aantal Groen VJ'), 
        viewColumn = c('waardeReal','waardeNorm','waardeProg','waardeVerschil','waardeTrend','aantalRood','aantalRoodProg','aantalGroen','aantalGroenProg','waardeRealVj','aantalRoodVj','aantalGroenVj'),
        format = c(initFormat,initFormat,initFormat,initFormat,initFormat,'integer','integer','integer','integer',initFormat,'integer','integer'))

setTitlePerInst <- function(env) {
    if (unique(kpi[kpi$kpiLabel == env$dims[['kpi']]$selected$label,]$gs)) {
        paste0('Realm Total: ', env$dims[['mnd']]$selected$label, ' / ', env$dims[['gs']]$selected$label, ' / ', env$dims[['kpi']]$selected$label) 
    } else {
        paste0('Realm Total: ', env$dims[['mnd']]$selected$label, ' / ', env$dims[['kpi']]$selected$label) 
    }
}

setExportPerInst <- function(env,type) {

    if (type == 'abs') {
        value <- 'abs'
        text <- 'Absolute waarden'
    } else {
        value <- 'rel'
        text <- 'Relatieve waarden'
    }
    
    if (env$kpiProps$heeftAbsoluut) {
        list(
            enabled = TRUE,
            buttons = list(
                contextButton = list(enabled = FALSE),
                customButton = list(
                    enabled = TRUE,
                    y = 15,
                    text = text,
                    onclick = highcharter::JS(paste0("function () {
                var number = Math.random();
                Shiny.onInputChange('perInstPresChange',{r: number, value: '",value,"'});
            }")),
                    theme = list(
                        style = list(
                            color = 'blue',
                            textDecoration = 'underline'
                        )
                    )
                )
            )
        ) 
    } else {
        NULL
    }
}

fn_ttXtraData <- function(env,vc,nm) {

    if (env$id != 's1')
        return(NULL)
    
    p <- env$kpiProps
    
    if (p$heeftAbsoluut) {
        list(
            includeSelf = TRUE,
            enabled = TRUE,
            name = nm,
            viewColumns = vc)
    } else 
        NULL
}

perInstVjVis <- function(env) {
    if (isKV(env)) {
        if (is.null(env$hcPrev[['perInst']]))
            FALSE
        else 
            env$hcPrev[['perInst']]$seriesOpts[[1]]$visible
    }
    else 
        FALSE
}


fn_ttXtraData1 <- function(env) { fn_ttXtraData(env,c('aantalGroen','aantalRood'),c('Aantal Groen','Aantal Rood')) }
fn_ttXtraData2 <- function(env) { fn_ttXtraData(env,c('aantalGroenProg','aantalRoodProg'),c('Aantal Groen','Aantal Rood')) }
fn_ttXtraData3 <- function(env) { fn_ttXtraData(env,c('waardeReal'),'Realisatie') }
fn_ttXtraData4 <- function(env) { fn_ttXtraData(env,c('waardeProg'),'Prognose') }
fn_ttXtraData5 <- function(env) { fn_ttXtraData(env,c('aantalGroenVj','aantalRoodVj'),c('Aantal Groen VJ','Aantal Rood VJ')) }
fn_ttXtraData6 <- function(env) { fn_ttXtraData(env,c('waardeRealVj'),'Realisatie VJ') }

setExportPerInstAbs <- function(env) { setExportPerInst(env,'abs') }

hcOpts1 <- hcOptsBaseColumn

hcOpts1$exporting <- setExportPerInstAbs
hcOpts1$title$text <- setTitlePerInst

hcOpts1$series <- list(
    # waardeRealVj = list(
    #     viewColumn = 'waardeRealVj',
    #     type = 'column',
    #     visible = perInstVjVis,
    #     showInLegend = isKV,
    #     grouping = TRUE,
    #     dataLabels = list(enabled = TRUE),
    #     color = '#b7e2b6',
    #     pointPadding = 0,
    #     ttXtraData = fn_ttXtraData5,
    #     yAxis = 0,
    #     stack = 0),
    waardeReal = list(
        viewColumn = 'waardeReal',
        type = 'column',
        grouping = TRUE,
        visible = TRUE,
        dataLabels = list(enabled = TRUE),
        ttXtraData = fn_ttXtraData1,
        color = '#41b6c4',
        pointPadding = 0,
        yAxis = 0), 
    waardeProg = list(
        viewColumn = 'waardeProg',
        type = 'column',
        visible = TRUE,
        dataLabels = list(enabled = TRUE),
        color = '#41b6c4',
        pattern = 'stripe2',
        pointPadding = 0,
        ttXtraData = fn_ttXtraData2,
        yAxis = 0,
        linkedTo = ':previous'),
    waardeVerschil = list(
        viewColumn = 'waardeVerschil',
        type = 'column',
        visible = FALSE,
        dataLabels = list(enabled = TRUE),
        color = 'red',
        pointPadding = 0.1,
        yAxis = 0),
    waardeNorm = list( 
        viewColumn = 'waardeNorm',
        type = 'line',
        visible = TRUE,
        dataLabels = list(enabled = FALSE),
        #marker = list(enabled = FALSE),
        color = 'blue',
        yAxis = 0),
    waardeTrend = list(
        viewColumn = 'waardeTrend',
        type = 'line',
        visible = FALSE,
        showInLegend = userData$dashOpts$mndSel$label == 'Within the Moon',
        dataLabels = list(enabled = FALSE),
        color = 'lightgreen',
        yAxis = 0))

s1 <- s1 %>%
    addPresentation(
        dim = 'perInst',
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

setExportPerInstRel <- function(env) {
    setExportPerInst(env,'rel')
}

hcOpts2 <- hcOptsBaseColumn

hcOpts2$exporting <- setExportPerInstRel
hcOpts2$title$text <- setTitlePerInst

hcOpts2$series <- list(
    # aantalRoodVj = list(
    #     viewColumn = 'aantalRoodVj',
    #     type = 'column',
    #     visible = FALSE,
    #     stacking = 'normal',
    #     stack = 1,
    #     showInLegend = TRUE,
    #     dataLabels = list(enabled = TRUE),
    #     color = 'orange',
    #     ttXtraData = fn_ttXtraData6,
    #     enableMouseTracking = TRUE,
    #     pointPadding = 0,
    #     grouping = TRUE),
    # aantalGroenVj = list(
    #     viewColumn = 'aantalGroenVj',
    #     type = 'column',
    #     visible = FALSE,
    #     stacking = 'normal',
    #     stack = 1,
    #     showInLegend = TRUE,
    #     dataLabels = list(enabled = TRUE),
    #     color = '#b7e2b6',
    #     ttXtraData = fn_ttXtraData6,
    #     enableMouseTracking = TRUE,
    #     pointPadding = 0,
    #     grouping = TRUE),
    aantalRood = list(
        viewColumn = 'aantalRood',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        stack = 0,
        showInLegend = TRUE,
        dataLabels = list(enabled = TRUE),
        color = 'tomato',
        ttXtraData = fn_ttXtraData3,
        enableMouseTracking = TRUE,
        pointPadding = 0,
        grouping = TRUE),
    aantalRoodProg = list(
        viewColumn = 'aantalRoodProg',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        stack = 0,
        dataLabels = list(enabled = TRUE),
        color = 'tomato',
        ttXtraData = fn_ttXtraData4,
        pattern = 'stripe1',
        enableMouseTracking = TRUE,
        pointPadding = 0,
        linkedTo = ':previous',
        grouping = TRUE),
    aantalGroen = list(
        viewColumn = 'aantalGroen',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        stack = 0,
        showInLegend = TRUE,
        dataLabels = list(enabled = TRUE),
        color = 'green',
        ttXtraData = fn_ttXtraData3,
        enableMouseTracking = TRUE,
        pointPadding = 0,
        grouping = TRUE),
    aantalGroenProg = list(
        viewColumn = 'aantalGroenProg',
        type = 'column',
        visible = TRUE,
        stacking = 'normal',
        stack = 0,
        dataLabels = list(enabled = TRUE),
        color = 'green',
        ttXtraData = fn_ttXtraData4,
        pattern = 'stripe1',
        enableMouseTracking = TRUE,
        pointPadding = 0,
        linkedTo = ':previous',
        grouping = TRUE)
)

s1 <- s1 %>%
    addPresentation(
        dim = 'perInst',
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

perInstSelectChangeHook <- function(env) {
    setSelection(env, 'perAfd',env$dims[['perInst']]$selected)
}

observeEvent(input[['perInstPresChange']],{
    
    dd <- s1$dims[['perInst']]
    da <- s1$dims[['perAfd']]
    dk <- s1$dims[['kpl2']]
    
    if (input[['perInstPresChange']]$value == 'abs') {
        dd$pres = 'highCharts2'
        da$pres = 'highCharts2'
        dk$pres = 'highCharts2'
    } else {
        dd$pres = 'highCharts1'
        da$pres = 'highCharts1'
        dk$pres = 'highCharts1'
    }

    dd$reactive$presChange <- dd$reactive$presChange + 1
    s1$hcRenderers[['perInst']]$count <- s1$hcRenderers[['perInst']]$count + 1
    
    if (s1$hcRenderers[['perAfd']]$count > 0) {
        da$reactive$presChange <- da$reactive$presChange + 1
        s1$hcRenderers[['perAfd']]$count <- s1$hcRenderers[['perAfd']]$count + 1
    }
    
    dk$reactive$presChange <- dk$reactive$presChange + 1
    s1$hcRenderers[['kpl2']]$count <- s1$hcRenderers[['kpl2']]$count + 1
    
})
