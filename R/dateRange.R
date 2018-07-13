renderDateRangeDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$gdim
    outputDateRange <- paste0(gdim,'DimDateRange')
    
    dd <- env$dims[[dim]]
    opts <- dd$presList[[dd$pres]]$rangeOpts
    
    dr <- reactive({input[[outputDateRange]]})
    
    if (!is.null(opts$throttle)) {
        dr2 <- dr %>% shiny::throttle(opts$throttle)
    }

    if (!is.null(opts$debounce)) {
        dr2 <- dr %>% shiny::debounce(opts$debounce)
    }
    
    if (is.null(opts$throttle) && is.null(opts$debounce)) {
        dr2 <- dr %>% shiny::throttle(1000)
    }
        
    shiny::observeEvent(dr2(),{
        
        sel <- dr2()
        
        if (is.null(sel))
            return()
        
        minDate <- min(dd$data[['level1Label']])
        maxDate <- max(dd$data[['level1Label']])
        
        if (sel[1] <= minDate && sel[2] >= maxDate) {
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
                dd$selectSource <- 'dateRangeClick'
                dd$reactive$selectChange <- dd$reactive$selectChange + 1
            } else {
                dd$selectSource <- ''
            }
        }
    })
}
