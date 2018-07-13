renderRangeSliderDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$gdim
    outputRangeSlider <- paste0(gdim,'DimRangeSlider')
    
    vr <- reactive({input[[outputRangeSlider]]})
    vr2 <- vr %>% shiny::throttle(1000)
    
    shiny::observeEvent(vr2(),{
        sel <- vr2()

        if (is.null(sel))
            return()

        dd <- env$dims[[dim]]
        
        minVal <- min(dd$data[['level1Label']])
        maxVal <- max(dd$data[['level1Label']])
        
        if (sel[1] <= minVal && sel[2] >= maxVal) {
            s <- dd$rootSelected
        } else {
            s <- makeRangeSelection(env,dim,sel[1],sel[2])
            if (is.null(s) || !(any(s$label %in% dd$membersFiltered$member[dd$membersFiltered$cnt > 0]))) {
                shinyjs::alert('Geen data!')
                return()
            }
            if (!dd$fixedMembers) {
                s <- s[s$label %in% dd$membersFiltered$member[dd$membersFiltered$cnt > 0],]
            }
        }

        if(!identical(s,dd$selected)) {
            dd$selected <- s
            
            if (dd$selectSource != 'observeEvent') {
                dd$selectSource <- 'rangeSliderClick'
                dd$reactive$selectChange <- dd$reactive$selectChange + 1
            } else {
                dd$selectSource <- ''    
            } 
        } else {
            dd$selectSource <- ''
        }
    })
}

