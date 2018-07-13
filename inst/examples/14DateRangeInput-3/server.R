library(shiny)
library(magrittr)
library(dwhr)

# laden data
dwhr::setDebug(TRUE,debugDims = 'leeft')

per <- readRDS(
    file = paste0(getwd(),"/data/dates.rds"))
  
leeft <- read.csv(
    file = paste0(getwd(),"/data/ds_d_leeftijd.txt"),
    header = FALSE,
    sep = ";",
    col.names = c("leeftijd","level1Label","level2Label"),
    stringsAsFactors = FALSE)

leeft$level1Code <- leeft$level1Label
leeft$level2Code <- as.integer(leeft$leeftijd)

facts <- data.frame(
    dateId = as.integer(round(runif(1000000,min(per$dateId),max(per$dateId)),0)),
    leeftijd = as.integer(round(runif(1000000,0,100),0)),
    num1 = runif(1000000,100,200))

function(input, output, session) {
    source('columnChart.R', local = TRUE)
    
    authenticate(session)    # heeft user toegang?

    s1 <- new.star(starId = 's1',
                   session = session,
                   facts = facts,
                   foreignKeyCheck = FALSE) %>%   # maak sterschema object
        addDimView(
            dim = 'per',
            name = 'Periode',                       # getoonde titel van dimensie
            data = per,                             # dimensie data.frame
            initLevel = 3,
            # selectLevel = 1,
            # selectLabel = c('2018'),
            # keepUnused = TRUE,
            
            levelNames = c('Alle perioden','Jaar','maand','Dag'),
            useLevel = c(3)) %>%
        addMeasure(
            dim = 'per',
            factColumn = c('num1','num1','num1'),      # referentie naar fact-column
            viewColumn = c('abc','xyz','pqr'),
            fun = c('sum','dcount','median'),          # toe te passen aggregatie-functie
            as = c('som','distinct','mediaan'),        # te tonen kolomnaam voor de measure
            format = c('decimal2','decimal2','euro2'), # te tonen format van de measure
            levels = c(0,1,2,3)
            ) 
    
    columnChartOpts$series <- list(
        list(
            viewColumn = 'abc',
            type = 'column',                           # columnChart
            visible = TRUE,
            dataLabels = list(enabled = TRUE),
            colorByPoint = TRUE,                       # nodig bij gebruik van colors optie
            colors = 'YlOrRd',                         # colorBrewer sequentieel palette (zie http://colorbrewer2.org)
            pointPadding = 0), 
        list(
            viewColumn = 'xyz',
            type = 'column',
            visible = TRUE,
            dataLabels = list(enabled = TRUE),
            color = '#41b6c4',
            pattern = 'stripe1',                      # stripe pattern
            pointPadding = 0),
        list(
            viewColumn = 'pqr',
            type = 'column',
            visible = FALSE,
            dataLabels = list(enabled = TRUE),
            color = 'red',
            pointPadding = 0.1))
    
    s1 <- s1 %>%  
       
        addPresentation(
            dim = 'per' ,
            checkUiId = FALSE,
            type = 'dateRangeInput',                     # presentatie-vorm is een dataRangeInput
            as = 'dateRange1',                          # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            navOpts = list(hideNoFilter = TRUE),
            rangeOpts = list()) %>%
        addPresentation(
            dim = 'per' ,
            uiId = 'per2',
            name = 'Periode',
            useLevels = c(0,1,2,3),
            type = 'highCharts',                        # presentatie-vorm is een chart
            as = 'chart1',                             # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            height = 250,
            navOpts = list(
                links = list(
                    list(
                        type = 'dim',
                        dim = 'per',
                        width = 10))),
            highChartsOpts = columnChartOpts) %>%
        addPresentation(
            dim = 'per' ,
            uiId = 'per3',
            useLevels = c(0,1,2,3),
            type = 'dataTable',                     # presentatie-vorm is een dataTable
            as = 'per3',                          # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            dataTableOpts = list(                   # opties voor type dataTable
                measures = list(
                    list(viewColumn = 'pqr', colorBarColor1 = '#f7fcb9'),
                    list(viewColumn = 'abc', colorBarColor1 = '#f7fcb9'),
                    list(viewColumn = 'xyz', colorBarColor1 = '#f7fcb9'))))

    
     s1 <- s1 %>% addDimView(
         dim = 'leeft',
         name = 'Leeftijd',                       # getoonde titel van dimensie
         data = leeft,                             # dimensie data.frame
         levelNames = c('Alle leeftijden', 'categorie', 'leeftijd'),  # getoonde namen van nivo's
         initLevel = 2,
         initParent = '41-60',
         orderBy = 'key',
         selectMode = 'single',
         useLevels = c(0,1,2)) %>%
         addMeasure(
             dim = 'leeft',
             factColumn = c('num1','num1','num1'),      # referentie naar fact-column
             viewColumn = c('abc','xyz','pqr'),
             fun = c('sum','dcount','median'),          # toe te passen aggregatie-functie
             as = c('som','distinct','mediaan'),        # te tonen kolomnaam voor de measure
             format = c('decimal2','decimal2','euro2'), # te tonen format van de measure
             levels = c(0,1,2)
         ) %>%
         addPresentation(
             dim = 'leeft' ,
             type = 'dataTable',                     # presentatie-vorm is een dataTable
             as = 'tabel1',                          # te tonen label als er meer presentaties zijn
             isDefault = TRUE,
             navOpts = list(
                 links = list(
                     list(
                         type = 'dim',
                         dim = 'leeft2',
                         width = 10))),
             dataTableOpts = list(                   # opties voor type dataTable
                 measures = list(
                     list(viewColumn = 'pqr', colorBarColor1 = '#f7fcb9'),
                     list(viewColumn = 'abc', colorBarColor1 = '#f7fcb9'),
                     list(viewColumn = 'xyz', colorBarColor1 = '#f7fcb9')))) %>%
         addPresentation(
             dim = 'leeft' ,
             useLevels = c(2),
             uiId = 'leeft2',
             checkUiId = FALSE,
             type = 'rangeSliderInput',                     # presentatie-vorm is een rangeSliderInput
             as = 'Range1',                          # te tonen label als er meer presentaties zijn
             isDefault = TRUE,
             navOpts = list(hideNoFilter = TRUE),
             rangeOpts = list()) 
         
    

        renderDims(s1,input,output)                    # start rendering
}
