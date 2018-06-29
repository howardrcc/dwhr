renderDateRangeDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$gdim
    outputDateRange <- paste0(gdim,'DimDateRange')
    
    observeEvent(input[[outputDateRange]],{
        sel <- input[[outputDateRange]]
        browser()
        root <- env$dims[[dim]]$rootLabel
        
       
        s <- data.frame(level = 1, parent = root, label = sel, stringsAsFactors = FALSE)
        
        
        
        if(!identical(s,env$dims[[dim]]$selected)) {
            env$dims[[dim]]$selected <- s
            env$dims[[dim]]$reactive$selectChange <- env$dims[[dim]]$reactive$selectChange + 1
        }
        
    })
    
}