renderDateRangeDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$gdim
    outputDateRange <- paste0(gdim,'DimDateRange')
    
    dr <- reactive({input[[outputDateRange]]})
    dr2 <- dr %>% shiny::throttle(1000)
    
    shiny::observeEvent(dr2(),{
        
        sel <- dr2()
        
        if (is.null(sel))
            return()
        
        dd <- env$dims[[dim]]
        
        minDate <- min(dd$data[['level1Label']])
        maxDate <- max(dd$data[['level1Label']])
        
        if (sel[1] <= minDate && sel[2] >= maxDate) {
            s <- dd$rootSelected
        } else {
            s <- makeDateRangeSelection(env,dim,sel[1],sel[2])
            if (is.null(s) || !(any(s$label %in% dd$membersFiltered$member[dd$membersFiltered$cnt > 0]))) {
                shinyjs::alert('Geen data!')
                return()
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

makeDateRangeSelection <- function(env,dim,from,to) {
    
    dd <- env$dims[[dim]]
    root <- dd$rootLabel
    
    dt <- dd$data[['level1Label']]
    dt <- dt[dt >= from & dt <= to]
    if (length(dt) > 0)
        data.frame(level = 1, parent = root, label = dt, stringsAsFactors = FALSE)
    else 
        NULL

}