library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),
    getDimUI(dim = 's1Per'),
    getDimUI(dim = 's2Per')
)
