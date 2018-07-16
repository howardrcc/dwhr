renderRangeSliderDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$gdim
    outputRangeSlider <- paste0(gdim,'DimRangeSlider')
    
    dd <- env$dims[[dim]]
    opts <- dd$presList[[dd$pres]]$rangeOpts
    
    vr <- reactive({input[[outputRangeSlider]]})
    
    if (!is.null(opts$throttle)) {
        vr2 <- vr %>% shiny::throttle(opts$throttle)
    }
    
    if (!is.null(opts$debounce)) {
        vr2 <- vr %>% shiny::debounce(opts$debounce)
    }
    
    if (is.null(opts$throttle) && is.null(opts$debounce)) {
        vr2 <- vr %>% shiny::throttle(1000)
    }
    
    shiny::observeEvent(vr2(),{
        sel <- vr2()

        if (is.null(sel))
            return()

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
        }
        
        dd$selected$level <- as.numeric(dd$selected$level)
        s$level <- as.numeric(s$level)

        if(!identical(s,dd$selected)) {
            dd$selected <- s
            
            if (dd$selectSource != 'observeEvent') {
                dd$selectSource <- 'rangeSliderClick'
                dd$reactive$selectChange <- dd$reactive$selectChange + 1
                return()
            } 
        } 

        dd$selectSource <- ''
        
    })
}

