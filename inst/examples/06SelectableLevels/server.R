library(shiny)
library(magrittr)
library(dwhr)

# laden data

per <- read.csv(
    file = paste0(getwd(),"/data/ds_d_periode.txt"),
    header = FALSE,
    sep = ";",
    col.names = c("maandId","level1Label","level2Label","level3Label"),
    stringsAsFactors = FALSE)

facts <- data.frame(
    maandId = as.integer(round(runif(10000,min(per$maandId),max(per$maandId)),0)),
    num1 = runif(10000,100,200))

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
            levelNames = c('Alle perioden', 'Jaar', 'Kwartaal', 'Maand'),  # getoonde namen van nivo's
            initLevel = 3,
            selectMode = 'multi',
            selectLevel = 2,
            selectLabel = '2017Q1',
            initParent = '2017Q1',
            selectableLevels = c(2,3),
            footerLevels = c(3),
            useLevels = c(0,1,2,3)) %>%
        addMeasure(
            dim = 'per',
            factColumn = c('num1','num1','num1'),      # referentie naar fact-column
            viewColumn = c('abc','xyz','pqr'),
            fun = c('sum','dcount','median'),          # toe te passen aggregatie-functie
            as = c('som','distinct','mediaan'),        # te tonen kolomnaam voor de measure
            format = c('decimal2','decimal2','euro2'), # te tonen format van de measure
            sort = c(30,20,10),
            levels = c(1,2,3)
            ) %>%
        addPresentation(
            dim = 'per' ,
            type = 'dataTable',                     # presentatie-vorm is een dataTable
            as = 'tabel1',                          # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            dataTableOpts = list(                   # opties voor type dataTable
                measures = list(
                    list(viewColumn = 'pqr', colorBarColor1 = '#f7fcb9'),
                    list(viewColumn = 'abc', colorBarColor1 = '#f7fcb9'),
                    list(viewColumn = 'xyz', colorBarColor1 = '#f7fcb9')))) %>%
        renderDims(input,output)                    # start rendering
}
