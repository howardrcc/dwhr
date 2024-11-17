library(shiny)
library(magrittr)
library(dwhr)

# laden data

per <- read.csv(
    file = paste0(getwd(),"/data/ds_d_periode.txt"),
    header = FALSE,
    sep = ";",
    col.names = c("maandId","level1Label","level2Label"),
    stringsAsFactors = FALSE)

per$srt1 <- as.character(2018 - as.integer(per$level1Label))
per$srt2 <- as.integer(round(runif(nrow(per),min(per$maandId),max(per$maandId)),0))

facts <- data.frame(
    maandId = per$maandId[as.integer(round(runif(10000,1,nrow(per))))],
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
            initParent = '2017') %>%
        addMeasure(
            dim = 'per',
            factColumn = c('num1','num1','num1'),      # referentie naar fact-column
            viewColumn = c('abc','xyz','pqr'),
            fun = c('sum','dcount','median'),          # toe te passen aggregatie-functie
            as = c('som','distinct','mediaan'),        # te tonen kolomnaam voor de measure
            format = c('decimal2','decimal2','euro2'), # te tonen format van de measure
            sort = c(10,20,30)
            ) %>%
        addSortColumn(
            dim = 'per',
            sortColumn = 'srt1',
            levels = c(1)) %>%
        addSortColumn(
            dim = 'per',
            sortColumn = 'srt2',
            levels = c(2)) %>%
        addPresentation(
            dim = 'per' ,
            type = 'dataTable',                     # presentatie-vorm is een dataTable
            as = 'tabel1',                          # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            dataTableOpts = list(                   # opties voor type dataTable
                measures = list(
                    list(viewColumn = 'pqr',
                         colorBarColor1 = '#f7fcb9',
                         colorBarColor2 = 'lightblue'),
                    list(viewColumn = 'abc',
                    #     format = 'hidden',
                         bgStyle = list(
                             cuts = c(-100,0,100),
                             values = c('red','pink','lightgreen','green'))),
                    list(viewColumn = 'xyz',
                         colorBarColor1 = '#e5c0cb')))) %>%
        renderDims(input,output)                    # start rendering
}
