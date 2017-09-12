library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),
    getDimUI(dim = 'per'),
    getDimUI(dim = 'perChart')
)
