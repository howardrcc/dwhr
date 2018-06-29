library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),
    getDimUI(starId = 's1', dim = 'per2'),
    getDimUI(starId = 's1', dim = 'per'),
    getDimUI(starId = 's1', dim = 'leeft')
)
