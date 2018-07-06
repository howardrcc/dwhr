library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),
    getDimUI(starId = 's1', dim = 'per2', accordion = TRUE),
    getDimUI(starId = 's1', dim = 'leeft', accordion = TRUE)
)
