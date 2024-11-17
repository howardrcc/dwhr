library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),
    conditionalPanel(
        condition = "output.browser",
        actionButton(inputId = 'browser','browser()')),
    getDimUI(starId = 's1', dim = 'per', accordion = TRUE),
    getDimUI(starId = 's1', dim = 'per2', accordion = TRUE),
    getDimUI(starId = 's1', dim = 'leeft', accordion = TRUE)
)
