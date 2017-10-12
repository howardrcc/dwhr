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

per$fx1 <- 'standard'
per$fx1[per$level1Label == "2016"] <- 'perc2'
per$fx2 <- 'standard'
per$fx2[per$level1Label == "2015"] <- 'decimal3'
per$fx2[per$level1Label == "2016"] <- 'euro2'

facts <- data.frame(
    maandId = as.integer(round(runif(10000,min(per$maandId),max(per$maandId)),0)),
    num1 = runif(10000,100,200))

function(input, output, session) {

    authenticate(session)    # heeft user toegang?

    telOp <- function() {
        df$pqr + df$xyz
    }

    trekAf <- function() {
        df$pqr - df$xyz
    }

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
            sort = c(30,20,10)
            ) %>%
        addMeasureDerrived(
            dim = 'per',
            userFunc = c('telOp','telOp'),
            as = c('test1','test2'),
            sort = c(15,16),
            levels = c(2),
            formatColumn = c('fx1','fx2')
        ) %>%
        addMeasureDerrived(
            dim = 'per',
            viewColumn = 'whatever',
            userFunc = c('trekAf'),
            as = c('test'),
            sort = c(15),
            levels = c(1),
            formatColumn = 'fx2'
        ) %>%
        addPresentation(
            dim = 'per' ,
            type = 'dataTable',                     # presentatie-vorm is een dataTable
            as = 'tabel1',                          # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            dataTableOpts = list(                   # opties voor type dataTable
                measures = list(
                    list(viewColumn = 'pqr', colorBarColor1 = '#e5c0cb'),
                    list(viewColumn = 'd1'),
                    list(viewColumn = 'd2'),
                    list(viewColumn = 'abc', colorBarColor1 = '#f7fcb9'),
                    list(viewColumn = 'xyz', colorBarColor1 = '#f7fcb9')))) %>%
        renderDims(input,output)                    # start rendering
}
