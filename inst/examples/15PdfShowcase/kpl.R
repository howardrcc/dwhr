heeftAbsoluutKpl <- function(env,test) { 
    getKpiProps(env) 
    env$kpiProps$heeftAbsoluut 
}

sparkKplVisible <- function(env) {
    if (session$userData$dashOpts$spark) {
        getKpiProps(env)
        !env$kpiProps$kpiId %in% c(opnameKpiId,patKpiId)
    } else {
        FALSE
    }
}

typeFun <- function(env) {
    if (isKV(env)) 
        'line'
    else 
        'column'
}



s1 <- s1 %>%
    addDimView( 
        dim = 'kpl',
        name = 'Afdelingen',
        data = kpl,
        levelNames = c('Alle afdelingen', 'Maatschappij', 'Afdeling','SubAfdeling'),
        selectMode = 'single',
        selectLabel = userData$dashOpts$kplSel$label,
        selectLevel = max(userData$dashOpts$kplSel$level) + 1,
        useLevels = c(2),
        initLevel = 2,
        presListType = 'links',
        ignoreDims = c('perInst')) %>%
    addMeasure( 
        dim = 'kpl',
        factColumn = c('tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','kostenplaatsId','tellerRealVj','noemerRealVj','ids','ids'),
        fun = c('sum','sum','sum','sum','mean','sum','sum','opnCount','opnCountVj'),
        as = c('tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','kostenplaatsId','tellerRealVj','noemerRealVj','Aantal Opnames','Aantal Opnames VJ')) %>%
    addMeasureDerrived( 
        dim = 'kpl', 
        userFunc = c('waardeRealProg', 'waardeBegroting', 'waardeVerschilRealProg','waardeRealVj'),
        sort = c(10,20,30,9),
        as = c(initRealNaam, initNormNaam, initVerschilNaam,'Real VJ'), 
        viewColumn = c('waardeReal', 'waardeNorm', 'waardeVerschil','waardeRealVj'),
        format = initFormat) %>%
    addMeasureDerrived( 
        dim = 'kpl', 
        userFunc = c('getOpmerkingTekst','getOpmerking','getSpark','aantalRoodRealProg','aantalGroenRealProg'), 
        as = c('OpmTekst','Opm','Trend','Aantal Rood','Aantal Groen'), 
        viewColumn = c('opmTekst','opm','waardeTrend','aantalRood','aantalGroen'),
        format = c('standard','standard','standard','integer','integer'),
        levels = c(2)) %>%
    addPresentation(
        dim = 'kpl' ,
        type = 'dataTable',
        as = 'tabel1',
        isDefault = TRUE,
        checkUiId = FALSE,
        navOpts = list(
            hideBreadCrumb = TRUE,
            hideAll = TRUE
        ),
        dataTableOpts = list(
            pageLength = 100,
            pageLengthList = c(20,50,100),
            measures = list(
                list(viewColumn = 'waardeRealVj'),
                list(viewColumn = 'aantalGroen', 
                     width = 20, 
                     visible = call('heeftAbsoluutKpl',quote(s1),'ok')),
                list(viewColumn = 'aantalRood', 
                     width = 20, 
                     visible = call('heeftAbsoluutKpl',quote(s1),'ok')),
                list(viewColumn = 'waardeReal'),
                list(viewColumn = 'waardeNorm'),
                list(viewColumn = 'waardeVerschil',
                     fgStyle = list(
                         cuts = c(0),
                         values = c('red','green')),
                     fontWeight = 'bold'),
                list(viewColumn = 'waardeTrend', 
                     format = 'sparkline', 
                     orderable = FALSE,
                     print = FALSE,
                     width = 60,
                     visible = sparkKplVisible,
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
                     bgStyle = list(
                         cuts = c(0),
                         values = c('rgba(0,0,0,0)','yellow')),
                     width = 10),
                list(viewColumn = 'opmTekst', visible = FALSE)))) 

setOrdering(s1, dim = 'kpl', as = initVerschilNaam, sort = initSort)  

setTitleKpl2 <- function(env) {
    if (unique(kpi[kpi$kpiLabel == env$dims[['kpi']]$selected$label,]$gs)) {
        paste0('Per Afdeling: ', 
               env$dims[['mnd']]$selected$label, ' / ',
               paste0(env$dims[['perInst']]$selected$label, collapse = ','), ' / ', 
               env$dims[['gs']]$selected$label, ' / ', 
               env$dims[['kpi']]$selected$label) 
    } else {
        paste0('Per Afdeling: ', 
               env$dims[['mnd']]$selected$label, ' / ', 
               paste0(env$dims[['perInst']]$selected$label, collapse = ','), ' / ', 
               env$dims[['kpi']]$selected$label) 
    }
}

hcOpts <- hcOptsBaseColumn
hcOpts$title$text <- setTitleKpl2
hcOpts$series <- list(
    waardeReal = list(
        viewColumn = 'waardeReal',
        type = 'column',
        visible = FALSE,
        dataLabels = list(enabled = TRUE),
        ttXtraData = fn_ttXtraData1,
        color = '#41b6c4',
        pointPadding = 0), 
    waardeVerschil = list(
        viewColumn = 'waardeVerschil',
        type = 'column',
        visible = TRUE,
        dataLabels = list(enabled = TRUE),
        color = 'red',
        pointPadding = 0.2),
    waardeNorm = list(
        viewColumn = 'waardeNorm',
        type = typeFun,
        visible = FALSE,
        dataLabels = list(enabled = isNotKV),
        color = 'blue',
        pointPadding = 0.10))

hcOpts$xAxis$scrollbar <- list(
    enabled = TRUE,
    showFull = FALSE,
    liveRedraw = FALSE
)
hcOpts$xAxis$max = 19
hcOpts$xAxis$min = 0

hcOpts$xAxis$labels$formatter <- JS("function() {
                if (typeof this.value == 'number') {
                    return '';
                } else {
                    return this.value;
                }
            }")

s1 <- s1 %>%    
    addPresentation( 
        dim = 'kpl',
        uiId = 'kpl2',             
        type = 'highCharts',
        as = '',
        isDefault = TRUE,
        checkUiId = FALSE,
        navOpts = list(
            syncNav = TRUE,
            hideBreadCrumb = TRUE,
            hideNoFilter = TRUE
        ),
        highChartsOpts = hcOpts)

hcOpts <- hcOptsBaseColumn
hcOpts$title$text <- setTitleKpl2

hcOpts$xAxis$scrollbar <- list(
    enabled = TRUE,
    showFull = FALSE,
    liveRedraw = FALSE
)
hcOpts$xAxis$max = 19
hcOpts$xAxis$min = 0
hcOpts$xAxis$labels$formatter <- JS("function() {
                if (typeof this.value == 'number') {
                    return '';
                } else {
                    return this.value;
                }
            }")

hcOpts$series <- list(
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
        pointPadding = 0))

s1 <- s1 %>%
    addPresentation(
        dim = 'kpl2',
        type = 'highCharts',
        as = 'chart2',
        isDefault = FALSE,
        checkUiId = FALSE,
        navOpts = list(
            hideBreadCrumb = TRUE,
            hideNoFilter = TRUE
        ),
        highChartsOpts = hcOpts)



kplSelectChangeHook <- function(env) {
    
    if (env$id != 's1')
        return()
    
    # trigger links
    
    env$dims[['kpi']]$reactive$linksChange <- env$dims[['kpi']]$reactive$linksChange + 1
    
    if (any(env$dims[['kpl']]$selected$level == 0)) {
        dimChangeState(env,'perAfd','hidden')
    } else {
        dimChangeState(env,'perAfd','enabled')
        env$dims[['perAfd']]$reactive$dimRefresh <- env$dims[['perAfd']]$reactive$dimRefresh + 1
    }
}

observeEvent(s1$dims[['kpl']]$reactive$isFiltered,{
    
    if (s1$dims[['kpl']]$reactive$isFiltered) {
        shinyjs::addCssClass(class = 'tabRed', selector = "#afdgs li a[data-value=Afdeling]")
    } else {
        shinyjs::removeCssClass(class = 'tabRed', selector = "#afdgs li a[data-value=Afdeling]")
    }
})


