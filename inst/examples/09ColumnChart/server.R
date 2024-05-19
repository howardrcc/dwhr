library(shiny)
library(magrittr)
library(dwhr)

dwhr::setDebug(FALSE)

# laden data

per <- read.csv(
    file = paste0(getwd(),"/data/ds_d_periode.txt"),
    header = FALSE,
    sep = ";",
    col.names = c("maandId","level1Code","level1Label","level2Code","level2Label"),
    stringsAsFactors = FALSE)

facts <- data.frame(
    maandId = per$maandId[as.integer(round(runif(10000,1,nrow(per))))],
    num1 = -150  + runif(10000,100,200))

function(input, output, session) {

    shinyjs::runjs('toggleCodePosition();')
    
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
            levelNames = c('Alle perioden', 'Jaar', 'Maand'),  # getoonde namen van nivo's
            initLevel = 2,
            initParent = '2017',
            orderBy = 'key') %>%
        addMeasure(
            dim = 'per',
            factColumn = c('num1','num1','num1'),      # referentie naar fact-column
            viewColumn = c('abc','xyz','pqr'),
            fun = c('sum','dcount','median'),          # toe te passen aggregatie-functie
            as = c('som','distinct','mediaan'),        # te tonen kolomnaam voor de measure
            format = c('decimal2','decimal2','euro2'), # te tonen format van de measure
            levels = c(0,1,2)) 
    
    columnChartOpts$series <- list(
        abc = list(
            viewColumn = 'abc',
            type = 'column',                           # columnChart
            visible = TRUE,
            dataLabels = list(enabled = TRUE),
            colorByPoint = TRUE,                       # nodig bij gebruik van colors optie
            colors = 'YlOrRd',                         # colorBrewer sequentieel palette (zie http://colorbrewer2.org)
            pointPadding = 0), 
        xyz = list(
            viewColumn = 'xyz',
            type = 'column',
            visible = TRUE,
            dataLabels = list(enabled = TRUE),
            color = '#41b6c4',
            pattern = 'stripe1',                      # stripe pattern
            pointPadding = 0),
        pqr = list(
            viewColumn = 'pqr',
            type = 'column',
            visible = FALSE,
            dataLabels = list(enabled = TRUE),
            color = 'red',
            pointPadding = 0.1))
    
    s1 <- s1 %>%
        addPresentation(
            dim = 'per' ,
            type = 'highCharts',                     # presentatie-vorm is highcharts grafiek
            as = 'chart1',                           # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            height = 250,
            highChartsOpts = columnChartOpts) %>%
        renderDims(input,output)                    # start rendering

 
}
