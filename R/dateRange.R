renderDateRangeDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$gdim
    outputDateRange <- paste0(gdim,'DimDateRange')
    
    observeEvent(input[[outputDateRange]],{
        
        sel <- input[[outputDateRange]]
        dd <- env$dims[[dim]]
        
        s <- makeDateRangeSelection(env,dim,sel[1],sel[2])
          
        if(!identical(s,dd$selected)) {
            dd$selected <- s
            dd$reactive$selectChange <- dd$reactive$selectChange + 1
        }
        
    })
}

makeDateRangeSelection <- function(env,dim,from,to) {
    
    dd <- env$dims[[dim]]
    root <- dd$rootLabel
    
    dt <- dd$data[['level1Label']]
    dt <- dt[dt >= from & dt <= to]
    data.frame(level = 1, parent = root, label = dt, stringsAsFactors = FALSE)

}