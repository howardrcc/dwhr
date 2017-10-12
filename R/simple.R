renderSimpleDim <- function(env,dim,input,output)  {
    
    gdim <- env$dims[[dim]]$globalDim
    outputSimple <- paste0(gdim,'DimSimple')
    
    observeEvent(input[[outputSimple]],{
        sel <- input[[outputSimple]]
        root <- env$dims[[dim]]$rootLabel
        
        if (env$dims[[dim]]$leafOnly) {
            s <- data.frame(level = 1, parent = root, label = sel, stringsAsFactors = FALSE)
        } else {
            if (sel == root) {
                s <- data.frame(level = 0, parent = '', label = sel, stringsAsFactors = FALSE)
            } else {
                s <- data.frame(level = 1, parent = root, label = sel, stringsAsFactors = FALSE)
            }
        }

        if(!identical(s,env$dims[[dim]]$selected)) {
            env$dims[[dim]]$selected <- s
            env$dims[[dim]]$reactive$selectChange <- env$dims[[dim]]$reactive$selectChange + 1
        }
        
    })
    
}