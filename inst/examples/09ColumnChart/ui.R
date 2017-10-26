library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),
    getDimUI(starId = 's1', dim = 'per')
)
