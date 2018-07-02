
startObserversData <- function(env,dim) {

    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    obs <- dd$observers
    inp <- env$session$input
    userData <- env$session$userData
    

    if (!('levelChange' %in% obs)) {
        shiny::observeEvent(dd$reactive$levelChange,{

            if(dd$reactive$levelChange == 0)
                return()
            
            printDebug(env = env, dim, eventIn = 'levelChange', info = paste0('new level:',dd$level))

            dd$searchTxt <- ""

            if(exists(paste0(dim,'LevelChangeHook'),envir = env$ce)) {
                do.call(paste0(dim,'LevelChangeHook'),list(env = env),envir = env$ce)
            }

            if (isNull(dd$syncNav,FALSE)) {

                pd <- dd$parentDim

                if (!is.null(pd))
                    dms <- union(pd,env$dims[[pd]]$childDims)
                else
                    dms <- dd$childDims

                for (d in dms) {

                    if (env$dims[[d]]$level != dd$level) {

                        env$dims[[d]]$level <- dd$level
                        env$dims[[d]]$parent <- dd$parent
                        env$dims[[d]]$ancestors <- dd$ancestors
                        env$dims[[d]]$reactive$levelChange <- env$dims[[d]]$reactive$levelChange + 1
                        printDebug(env = env, d, eventIn = 'levelChangeSync', eventOut = 'levelChange')
                    }

                }
            }

        })
        obs <- c(obs,'levelChange')
    }

    if (!('orderChange' %in% obs)) {
        shiny::observeEvent(dd$reactive$orderChange,{

            if(dd$reactive$orderChange == 0)
                return()
            
            printDebug(env = env, dim, eventIn = 'orderChange')

           # dd$searchTxt <- ""

            orderColumn <- dd$orderColumn
            orderViewColumn <- dd$orderViewColumn
            orderViewColumn2 <- dd$orderViewColumn2
            orderColumnDir <- dd$orderColumnDir

            if(exists(paste0(dim,'OrderChangeHook'),envir = env$ce)) {
                do.call(paste0(dim,'OrderChangeHook'),list(env = env, orderColumn,orderColumnDir),envir = env$ce)
            }

            if (isNull(dd$syncNav,FALSE)) {

                pd <- dd$parentDim

                if (!is.null(pd))
                    dms <- union(pd,env$dims[[pd]]$childDims)
                else
                    dms <- dd$childDims

                for (d in dms) {

                    if (env$dims[[d]]$orderColumn != orderColumn || env$dims[[d]]$orderColumnDir != orderColumnDir) {

                        env$dims[[d]]$orderColumn <- orderColumn
                        env$dims[[d]]$orderViewColumn <- orderViewColumn
                        env$dims[[d]]$orderViewColumn2 <- orderViewColumn2
                        env$dims[[d]]$orderColumnDir <- orderColumnDir
                        env$dims[[d]]$goToPage <- 1
                        env$dims[[d]]$reactive$dimRefresh <- env$dims[[d]]$reactive$dimRefresh + 1
                        printDebug(env = env, d, eventIn = 'orderChangeSync', eventOut = 'dimRefresh')
                    }

                }
            }

        })
        obs <- c(obs,'orderChange')
    }

    if (!('pageChange' %in% obs)) {
        shiny::observeEvent(dd$reactive$pageChange,{

            if(dd$reactive$pageChange == 0)
                return()
            
            printDebug(env = env, dim, eventIn = 'pageChange')

            if(exists(paste0(dim,'PageChangeHook'),envir = env$ce)) {
                do.call(paste0(dim,'PageChangeHook'),list(env = env),envir = env$ce)
            }

            if (isNull(dd$syncNav,FALSE)) {

                pd <- dd$parentDim

                if (!is.null(pd))
                    dms <- union(pd,env$dims[[pd]]$childDims)
                else
                    dms <- dd$childDims

                for (d in dms) {

                    if (env$dims[[d]]$currentPage != dd$currentPage) {
                        env$dims[[d]]$goToPage <- dd$currentPage
                        env$dims[[d]]$reactive$dimRefresh <- env$dims[[d]]$reactive$dimRefresh + 1
                        printDebug(env = env, d, eventIn = 'pageChangeSync', eventOut = 'dimRefresh')
                    }

                }
            }

        })
        obs <- c(obs,'pageChange')
    }

    if (!('pageLengthChange' %in% obs)) {
        shiny::observeEvent(dd$reactive$pageLengthChange,{

            if(dd$reactive$pageLengthChange == 0)
                return()

            printDebug(env = env, dim, eventIn = 'pageLengthChange')

            if(exists(paste0(dim,'PageLengthChangeHook'),envir = env$ce)) {
                do.call(paste0(dim,'PageLengthChangeHook'),list(env = env),envir = env$ce)
            }

            if (isNull(dd$syncNav,FALSE)) {

                pd <- dd$parentDim

                if (!is.null(pd))
                    dms <- union(pd,env$dims[[pd]]$childDims)
                else
                    dms <- dd$childDims

                for (d in dms) {

                    if (isNull(env$dims[[d]]$pageLength,0) != isNull(dd$pageLength,0)) {
                        env$dims[[d]]$pageLength <- dd$pageLength
                        env$dims[[d]]$goToPage <- dd$currentPage
                        env$dims[[d]]$reactive$dimRefresh <- env$dims[[d]]$reactive$dimRefresh + 1
                        printDebug(env = env, d, eventIn = 'pageLengthChangeSync', eventOut = 'dimRefresh')
                    }

                }
            }

        })
        obs <- c(obs,'pageLengthChange')
    }

    if (!('selectChange' %in% obs)) {
        shiny::observeEvent(dd$reactive$selectChange,{

            if(dd$reactive$selectChange > 0 && !dd$wait) {

                if(exists(paste0(dim,'SelectChangeHook'),envir = env$ce)) {
                    do.call(paste0(dim,'SelectChangeHook'),list(env = env),envir = env$ce)
                }

                if (any(dd$selected$level == 0)) {
                    dd$reactive$isFiltered <- FALSE
                } else {
                    dd$reactive$isFiltered <- TRUE
                }

                ids <- getSelectedIds(env,dim)

                if (!(identical(ids,dd$selectedIds))) {
                    if (dd$debounce) {
                        shinyjs::js$blockUI(
                            timeout = glob.env$debounceTimeout,
                            backgroundColor = glob.env$debounceBackgroundColor,
                            opacity = glob.env$debounceOpacity)
                    } else {
                        dd$debounce <- TRUE
                    }

                    dd$selectedIds <- ids
                    dd$reactive$selectedIdsChange <- dd$reactive$selectedIdsChange + 1

                    printDebug(env = env, dim = dim,
                               eventIn = 'selectChange',
                               eventOut = 'selectedIdsChange',
                               info = paste0('selected level:',dd$selected$level,
                                             '|selected label:',dd$selected$label))
                }

                if (dd$selectSource != 'observeEvent') {

                    if (!is.null(dd$childDims)) {
                        env$proxyDims <- unique(setdiff(c(env$proxyDims,dd$childDims),dim))
                    }

                    if (!is.null(dd$parentDim)) {
                        par <- dd$parentDim
                        env$proxyDims <- unique(setdiff(c(par,env$proxyDims,env$dims[[par]]$childDims),dim))
                    }

                    if (!is.null(dd$parentDim) || !is.null(dd$childDims)) {

                        if (!is.null(dd$parentDim))
                            dms <- union(dd$parentDim,env$dims[[dd$parentDim]]$childDims)
                        else
                            dms <- dd$childDims

                        #orgLevel <- dd$useLevels[dd$selected$level + 1]
                        #selIds <- dd$selectedIds
                        

                        for (d in dms) {

                            #dLevel <- which(env$dims[[d]]$useLevels == orgLevel) - 1

                            # if (length(dLevel) == 1) {
                            #     sel$level <- dLevel
                            #     pc <- env$dims[[d]]$pc
                            #     sel$parent <- pc$parentLabel[pc$level == dLevel & pc$label == sel$label]
                            # } else {
                            #     sel <- env$dims[[d]]$rootSelected
                            # }

                            setSelection2(env,d,dd$selected,dd$selectedIds, source = 'observeEvent')

                        }
                    }

                }

            }
        })
        obs <- c(obs,'selectChange')
    }

    if (!('memberChange' %in% obs)) {

        observeEvent({
            dd$memberChangeLevel()
            dd$memberChangeVis()
            dd$memberChangeOther()
        },
        {

            if (!(dim %in% visibleDims(env))) {  
                return()
            }

            if(dd$memberChangeLevel() > 0 || dd$memberChangeVis() > 0 || dd$memberChangeOther() > 0) {

                if (any(dd$selected$level == 0)) {
                    dd$rowLastAccessed$value <- ""
                }

                lst <- getMembers(env,dim)
                body <- lst$body
                footer <- lst$footer

                if (nrow(body) > 0) {
                    if (!identical(body,dd$membersFiltered)) {

                        dd$membersFiltered <- body
                        dd$footer <- footer
                        dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1

                        printDebug(env = env, dim = dim,
                                   eventIn = 'memberChange',
                                   eventOut = 'dimRefresh')

                        if(exists(paste0(dim,'MembersChangedHook'),envir = env$ce)) {
                            do.call(paste0(dim,'MembersChangedHook'),list(env = env),envir = env$ce)
                        }

                    } else {

                        if (dd$state == 'enabled' && !dd$visible) {
                            shinyjs::js$showDim(dim = gdim)
                            dd$visible <- TRUE
                        }
                    }
                } else {

                    dwhrStop('Lege dataset')

                }

            } 
        })
        obs <- c(obs,'memberChange')
    }

    dd$observers <- obs
}

startObserversPres <- function(env,dim,pres) {

    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    obs <- dd$observers
    presListType <- dd$presListType
    presList <- dd$presList
    hideBreadCrumb <- presList[[pres]]$navOpts$hideBreadCrumb
    hideAll <- presList[[pres]]$navOpts$hideAll
    hideNoFilter <- presList[[pres]]$navOpts$hideNoFilter
    inp <- env$session$input

    #
    # observers voor checkboxes
    #

    dimMs <- paste0(gdim,'DimMs')

    if (!(dimMs %in% obs)) {
        shiny::observeEvent(inp[[dimMs]], {

            if(inp[[dimMs]] == FALSE)
                if(dimCorrectSelectionInfo(inp,env,dim))
                    dimSetHasSubselect(env,dim)

            if (dd$msState != inp[[dimMs]]) {
                dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1

                dd$msState <- inp[[dimMs]]
                printDebug(env = env, dim = dim,
                           eventIn = 'multSelect',
                           eventOut = 'dimRefresh',
                           info = paste0('checkboxValue: ', dd$msState))

            }
        })
        obs <- c(obs,dimMs)
    }

    dimWait <- paste0(gdim,'DimWait')

    if (!(dimWait %in% obs)) {

        shiny::observeEvent(inp[[dimWait]], {

            for (d in setdiff(inputDims(env),dim)) {
                g <- env$dims[[d]]$gdim
                if(!inp[[dimWait]]) {
                    shinyjs::runjs(paste0('$("#',g,'Dimensie").unblock()'))
                } else {
                    shinyjs::runjs(paste0('$("#',g,'Dimensie").block({ message: null, overlayCSS: { backgroundColor: "#f2f2f2"} })'))
                }
            }

            if(!inp[[dimWait]]) {
                if (dd$wait) {
                    dd$wait <- FALSE
                    dd$selectSource <- 'dimWait'
                    dd$reactive$selectChange <- dd$reactive$selectChange + 1

                    printDebug(env = env, dim = dim,
                               eventIn = 'dimWait',
                               eventOut = 'selectChange')
                }
            } else {

                dd$wait <- TRUE
                printDebug(env = env, dim = dim,
                           eventIn = 'dimWait',
                           eventOut = 'blockUI')
            }


            if(exists(paste0('waitHook'),envir = env$ce)) {
                do.call(paste0('waitHook'),list(env = env, block = inp[[dimWait]]),envir = env$ce)
            }
        })

        obs <- c(obs,dimWait)
    }

    #
    # observers voor actionlinks
    #

    maxLevel <- dd$maxLevel
    dimLink0 <- paste0(gdim,'DimLink0')

    if (!(dimLink0 %in% obs) && !hideBreadCrumb && !hideAll) {

        shiny::observeEvent(inp[[dimLink0]], {
            dd$level <- 0
            dd$parent <- ''
            dd$ancestors <- c('')
            dd$reactive$levelChange <- dd$reactive$levelChange + 1
            printDebug(env = env, dim, eventIn = 'dimLink0', eventOut = 'levelChange')
        })

        obs <- c(obs,dimLink0)
    }

    dimLink1 <- paste0(gdim,'DimLink1')

    if (!(dimLink1 %in% obs) && !hideBreadCrumb) {

        shiny::observeEvent(inp[[dimLink1]] , {
            dd$level <- 1
            dd$parent <- dd$rootLabel
            dd$ancestors <- dd$ancestors[1:2]
            dd$reactive$levelChange <- dd$reactive$levelChange + 1
            printDebug(env = env, dim, eventIn = 'dimLink1', eventOut = 'levelChange')
        })

        obs <- c(obs,dimLink1)
    }

    dimLink2 <- paste0(gdim,'DimLink2')

    if (!(dimLink2 %in% obs) && !hideBreadCrumb && maxLevel >= 2) {

        shiny::observeEvent(inp[[dimLink2]] , {
            dd$level <- 2
            dd$parent <- dd$ancestors[3]
            dd$ancestors <- dd$ancestors[1:3]
            dd$reactive$levelChange <- dd$reactive$levelChange + 1
            printDebug(env = env, dim, eventIn = 'dimLink2', eventOut = 'levelChange')
        })

        obs <- c(obs,dimLink2)
    }

    dimLink3 <- paste0(gdim,'DimLink3')

    if (!(dimLink3 %in% obs) && !hideBreadCrumb && maxLevel >= 3) {

        shiny::observeEvent(inp[[dimLink3]] , {
            dd$level <- 3
            dd$parent <- dd$ancestors[4]
            dd$ancestors <- dd$ancestors[1:4]
            dd$reactive$levelChange <- dd$reactive$levelChange + 1
            printDebug(env = env, dim, eventIn = 'dimLink3', eventOut = 'levelChange')
        })

        obs <- c(obs,dimLink3)
    }
    
    dimLink4 <- paste0(gdim,'DimLink4')
    
    if (!(dimLink4 %in% obs) && !hideBreadCrumb && maxLevel >= 4) {
        
        shiny::observeEvent(inp[[dimLink4]] , {
            dd$level <- 4
            dd$parent <- dd$ancestors[5]
            dd$ancestors <- dd$ancestors[1:5]
            dd$reactive$levelChange <- dd$reactive$levelChange + 1
            printDebug(env = env, dim, eventIn = 'dimLink4', eventOut = 'levelChange')
        })
        
        obs <- c(obs,dimLink4)
    }

    noFilter <- paste0(gdim,'NoFilter')

    if (!(noFilter %in% obs) && !hideNoFilter) {

        shiny::observeEvent(inp[[noFilter]] , {
            dd$selected <- dd$rootSelected
            dimSetHasSubselect(env,dim)
            dd$selectSource <- 'NoFilter'
            dd$reactive$selectChange <- dd$reactive$selectChange + 1
            printDebug(env = env, dim, eventIn = 'NoFilter', eventOut = 'selectChange')
            dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
            printDebug(env = env, dim, eventIn = 'NoFilter', eventOut = 'dimRefresh')
        })

        obs <- c(obs,noFilter)
    }
    
    if (!('dimRefresh' %in% obs)) {

        shiny::observeEvent(dd$reactive$dimRefresh, {

            if (!(dim %in% visibleDims(env))) {  
                return()
            }
            
            presList <- dd$presList
            presType <- presList[[dd$pres]]$type

            printDebug(env = env, dim, eventIn = 'dimRefresh', info = paste0('presType: ',presType))

            if (presType == 'highCharts') {
                processHighCharts(env,dim,dd$pres)
            }
            if (presType == 'dataTable') {
                processDataTable(env,dim,dd$pres)
            }
            if (presType %in% c('radioButton','selectInput')) {
                 dd$reactive$presChange <- dd$reactive$presChange + 1
            }
            if (presType %in% c('dateRangeInput')) {
                
                minDate <- min(dd$membersFiltered$member)
                
                if (minDate > min(dd$selected$label))
                    minDate <- min(dd$selected$label)
                
                maxDate <- max(dd$membersFiltered$member)
                
                if (maxDate > max(dd$selected$label))
                    maxDate <- max(dd$selected$label)
                
                dd$presList[[dd$pres]]$dateRangeOpts$min <- minDate
                dd$presList[[dd$pres]]$dateRangeOpts$max <- maxDate
                
                dd$reactive$presChange <- dd$reactive$presChange + 1
            }
        })

        obs <- c(obs,'dimRefresh')
    }

    if (presListType == 'dropdown') {
        
        dimPres <- paste0(gdim,'Pres')
        
        if (!(dimPres %in% obs)) {
            
            shiny::observeEvent(inp[[dimPres]],{

                if (!(dim %in% visibleDims(env)) || dd$pres == inp[[dimPres]]) {  
                    return()
                }
               
                dd$reactive$presChange <- dd$reactive$presChange + 1
                
                printDebug(env = env, dim, eventIn = dimPres, eventOut = 'presChange')
                
                presList <- dd$presList
                dd$pres <- inp[[dimPres]]
                presType <- presList[[dd$pres]]$type
                
                if (presType == 'highCharts') {
                    env$hcRenderers[[dim]]$count <- env$hcRenderers[[dim]]$count + 1
                }
                if (presType == 'dataTable' ) {
                    env$dtRenderers[[dim]]$count <- env$dtRenderers[[dim]]$count + 1
                }
                
            })
            
            obs <- c(obs,dimPres)
            
        }
    }
    
    if (presListType == 'links') {
        
        presNum <- length(dd$presList)
        dimPresLink <- paste0(gdim,'PresLink',presNum)
        
        if (!(dimPresLink %in% obs)) {
            
            shiny::observeEvent(inp[[dimPresLink]],{
                
                if (!(dim %in% visibleDims(env))) {  
                    return()
                }
                
                printDebug(env = env, dim, eventIn = dimPresLink, eventOut = 'presChange')
                
                dd$reactive$presChange <- dd$reactive$presChange + 1
                
                presList <- dd$presList

                dd$pres <- names(presList)[presNum]
                presType <- presList[[dd$pres]]$type
                
                if (presType == 'highCharts') {
                    env$hcRenderers[[dim]]$count <- env$hcRenderers[[dim]]$count + 1
                }
                if (presType == 'dataTable' ) {
                    env$dtRenderers[[dim]]$count <- env$dtRenderers[[dim]]$count + 1
                }
                
            })
            
            obs <- c(obs,dimPresLink)
            
        }
        
        
    }
    
    
    dd$observers <- obs

}

