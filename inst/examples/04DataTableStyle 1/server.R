library(shiny)
library(magrittr)
library(dwhr)

dwhr::setDebug(TRUE)

# laden data

per <- read.csv(
    file = paste0(getwd(),"/data/ds_d_periode.txt"),
    header = FALSE,
    sep = ";",
    col.names = c("maandId","level1Code","level1Label","level2Code","level2Label"),
    stringsAsFactors = FALSE)

facts <- data.frame(
    maandId = as.integer(round(runif(10000,min(per$maandId),max(per$maandId)),0)),
    num1 = -150  + runif(10000,100,200))

function(input, output, session) {

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
            levels = c(1,2)
            ) %>%
        addPresentation(
            dim = 'per' ,
            type = 'dataTable',                     # presentatie-vorm is een dataTable
            as = 'tabel1',                          # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            dataTableOpts = list(                   # opties voor type dataTable
                pageLength = 6,
                pageLengthList = c(6,12),
                measures = list(
                    list(viewColumn = 'pqr',
                         colorBarColor1 = '#f7fcb9',
                         colorBarColor2 = 'lightblue',
                         fgStyle = list(
                             cuts = c(-10,0,10),
                             values = c('red','pink','lightseagreen','olivedrab')),
                         fontWeight = 'bold',
                         align = 'center'),
                    list(viewColumn = 'xyz',
                         colorBarColor1 = '#e5c0cb'),
                    list(viewColumn = 'abc',
                         format = 'hidden',
                         bgStyle = list(
                             cuts = c(0),
                             values = c('pink','lightseagreen')),
                         fontWeight = 'bold',
                         width = 10,
                         align = 'center',
                         cursor = 'pointer')
                    ))) %>%
        renderDims(input,output)                    # start rendering

    observeEvent(s1$dims[['per']]$reactive$clickMeasureEvent,{
        e <- s1$dims[['per']]$reactive$clickMeasureEvent

    })
}
