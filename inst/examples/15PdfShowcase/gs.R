s1 <- s1  %>% 
    addDimView(
        dim = 'gs',
        name = 'Geldstroom',
        data = gs,
        type = 'input',
        levelNames= c('Alle geldstromen','Geldstroom'),
        selectMode = 'single',
        initLevel = 1,
        ignoreDims = c('perInst')) %>%
    addMeasure( 
        dim = 'gs',
        factColumn = c('tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','kostenplaatsId','tellerRealVj','noemerRealVj'),
        fun = c('sum','sum','sum','sum','mean','sum','sum'),
        as = c('tellerReal', 'noemerReal', 'tellerNorm', 'noemerNorm','kostenplaatsId','tellerRealVj','noemerRealVj')) %>%
    addMeasureDerrived( 
        dim = 'gs', 
        userFunc = c('waardeRealProg', 'waardeBegroting', 'waardeVerschilRealProg','waardeRealVj'),
        sort = c(10,20,30,9),
        as = c(initRealNaam, initNormNaam, initVerschilNaam,'Realisatie VJ'), 
        viewColumn = c('waardeReal', 'waardeNorm', 'waardeVerschil','waardeRealVj'),
        format = initFormat) %>%
    addMeasureDerrived( 
        dim = 'gs', 
        userFunc = c('getOpmerkingTekst','getOpmerking','getSpark'), 
        as = c('OpmTekst','Opm','Trend'),
        viewColumn = c('opmTekst','opm','waardeTrend')) %>%
    addPresentation(
        dim = 'gs' ,
        type = 'dataTable',
        as = 'tabel1',
        isDefault = TRUE,
        checkUiId = FALSE,
        navOpts = list(
            hideBreadCrumb = TRUE,
            hideAll = TRUE
        ),
        dataTableOpts = list(
            pageLength = 20,
            pageLengthList = c(5,10,20),
            measures = list(
                list(viewColumn = 'waardeRealVj'),
                list(viewColumn = 'waardeReal'),
                list(viewColumn = 'waardeNorm'),
                list(viewColumn = 'waardeVerschil',
                     fgStyle = list(
                         cuts = c(0),
                         values = c('red','green')),
                     fontWeight = 'bold'),
                list(viewColumn = 'waardeTrend', 
                     format = 'sparkline', 
                     align = 'center',
                     width = 40,
                     print = FALSE,
                     orderable = FALSE,
                     visible = sparkVis,
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

setOrdering(s1, dim = 'gs', as = initVerschilNaam, sort = initSort)  

observeEvent(s1$dims[['gs']]$reactive$isFiltered,{
    
    if (s1$dims[['gs']]$reactive$isFiltered) {
        shinyjs::addCssClass(class = 'tabRed', selector = "#afdgs li a[data-value=Geldstroom]")
    } else {
        shinyjs::removeCssClass(class = 'tabRed', selector = "#afdgs li a[data-value=Geldstroom]")
    }
    
})

