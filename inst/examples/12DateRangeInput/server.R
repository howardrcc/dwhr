library(shiny)
library(magrittr)
library(dwhr)

# laden data

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
    dateId = as.integer(round(runif(10000,min(per$dateId),max(per$dateId)),0)),
    leeftijd = as.integer(round(runif(10000,0,100),0)),
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
            levelNames = c('Alle perioden', 'Jaar', 'Maand','Dag'),  # getoonde namen van nivo's
            initLevel = 2,
            initParent = '2017',
            useLevels = c(0,1,2,3)) %>%
        addMeasure(
            dim = 'per',
            factColumn = c('num1','num1','num1'),      # referentie naar fact-column
            viewColumn = c('abc','xyz','pqr'),
            fun = c('sum','dcount','median'),          # toe te passen aggregatie-functie
            as = c('som','distinct','mediaan'),        # te tonen kolomnaam voor de measure
            format = c('decimal2','decimal2','euro2'), # te tonen format van de measure
            levels = c(0,2,3)
            ) %>%
        addPresentation(
            dim = 'per' ,
            type = 'dateRangeInput',                     # presentatie-vorm is een dataRangeInput
            as = 'dateRange1',                          # te tonen label als er meer presentaties zijn
            isDefault = TRUE,
            dateRangeOpts = list())
            
    
     s1 <- s1 %>% addDimView(
         dim = 'leeft',
         name = 'Leeftijd',                       # getoonde titel van dimensie
         data = leeft,                             # dimensie data.frame
         levelNames = c('Alle leeftijden', 'categorie', 'leeftijd'),  # getoonde namen van nivo's
         initLevel = 2,
         initParent = '41-60',
         orderBy = 'key',
         useLevels = c(0,1,2)) %>%
         addMeasure(
             dim = 'leeft',
             factColumn = c('num1','num1','num1'),      # referentie naar fact-column
             viewColumn = c('abc','xyz','pqr'),
             fun = c('sum','dcount','median'),          # toe te passen aggregatie-functie
             as = c('som','distinct','mediaan'),        # te tonen kolomnaam voor de measure
             format = c('decimal2','decimal2','euro2'), # te tonen format van de measure
             levels = c(0,2)
         ) %>%
         addPresentation(
             dim = 'leeft' ,
             type = 'dataTable',                     # presentatie-vorm is een dataTable
             as = 'tabel1',                          # te tonen label als er meer presentaties zijn
             isDefault = TRUE,
             dataTableOpts = list(                   # opties voor type dataTable
                 measures = list(
                     list(viewColumn = 'pqr', colorBarColor1 = '#f7fcb9'),
                     list(viewColumn = 'abc', colorBarColor1 = '#f7fcb9'),
                     list(viewColumn = 'xyz', colorBarColor1 = '#f7fcb9')))) 
         
    

        renderDims(s1,input,output)                    # start rendering
}
