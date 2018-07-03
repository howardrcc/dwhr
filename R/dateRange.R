renderDateRangeDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$gdim
    outputDateRange <- paste0(gdim,'DimDateRange')
    
    observeEvent(input[[outputDateRange]],{
        
        sel <- input[[outputDateRange]]
        dd <- env$dims[[dim]]
        
        s <- makeDateRangeSelection(env,dim,sel[1],sel[2])

        if (is.null(s) || !(any(s$label %in% dd$membersFiltered$member[dd$membersFiltered$cnt > 0]))) {
            shinyjs::alert('Geen data!')
        } else {
            if(!identical(s,dd$selected)) {
                dd$selected <- s
                
                if (dd$selectSource != 'observeEvent') {
                    dd$selectSource <- 'dateRangeClick'
                    dd$reactive$selectChange <- dd$reactive$selectChange + 1
                } else {
                    dd$selectSource <- ''
                }
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