library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),
    getDimUI(starId = 's1', dim = 'per', accordion = TRUE),
    getDimUI(starId = 's1', dim = 'leeft', accordion = TRUE)
)
