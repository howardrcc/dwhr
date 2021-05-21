#'
#'@export
#'
renderDims <- function(env,input,output) {

    #
    # render functions for dims
    #
    
    for (ddim in names(env$dims)) {

        presList <- env$dims[[ddim]]$presList
        presTypes <- sapply(presList,function(x) {return(x$type)})
        
        if (length(presList) > 0) {

            local({

                dim <- ddim
                dd <- env$dims[[dim]]
                gdim <- dd$gdim
                
                # simple

                if(length(intersect(c('radioButton','selectInput'),presTypes)) >= 1) {
                    renderSimpleDim(env,dim,input,output)
                }

                # dataTable

                if ('dataTable' %in% presTypes) {
                    renderDataTableDim(env,dim,input,output)
                }

                # highCharts

                if('highCharts' %in% presTypes) {
                    renderHighchartDim(env,dim,input,output)
                }
                
                if('dateRangeInput' %in% presTypes) {
                    renderDateRangeDim(env,dim,input,output)
                }
                
                if('rangeSliderInput' %in% presTypes) {
                    renderRangeSliderDim(env,dim,input,output)
                }
                
                
                # dimension preslist
                
                outputName = paste0(gdim,'DimPresList')
                
                output[[outputName]] <- shiny::renderUI({
                    
                    printDebug(env = env, dim, eventIn = 'renderPresList')
                    
                    dd$reactive$presListChange
                    
                    if (!is.null(dd$presVisFun)) {
                        presVec <- do.call(dd$presVisFun,list(env = env, dim = dim, vec = dd$presVec),envir = env$ce)
                    } else {
                        presVec <- dd$presVec
                    }
                    
                    if (length(presVec) > 1) {
                        
                        
                        if (dd$presListType == 'dropdown') {
                            
                            txt <- '<span style = "font-size:90%; display: inline-block; margin-top:2px; float:right;">'
                            
                            txt <- paste0(
                                txt,
                                shiny::selectInput(
                                    inputId = paste0(gdim,'Pres'),
                                    label = NULL,
                                    choices = presVec,
                                    selectize = FALSE,
                                    width = "150px",
                                    selected = dd$defPres))
                        }
                        
                        if (dd$presListType == 'links') {
                        
                            txt <- '<span style = "font-size:120%; float:right;">'
                            
                            for (i in 1:length(presVec)) {
                                
                                input[[paste0(gdim,'PresLink',i)]]
                                
                                if (dd$pres == presVec[i]) {
                                    txt <- paste0(txt,span(names(presVec)[i],style = 'background-color: #f4f4f4', title = 'representatie'))
                                } else {
                                    txt <- paste0(txt,shiny::actionLink(inputId = paste0(gdim,'PresLink',i), label = names(presVec)[i], title = 'representatie'))
                                }
                                
                                if (length(presVec) > i)
                                    txt <- paste0(txt,'&nbsp&nbsp&nbsp&nbsp')
                            }
                        }
                        
                        txt <- paste0(txt, '</span>') 
                        
                        HTML(txt)
                    }
                    
                })
                
                # dimension Name
                
                outputName = paste0(gdim,'DimName')
                
                output[[outputName]] <- shiny::renderUI({
                    
                    printDebug(env = env, dim, eventIn = 'renderName')
                    
                    dd$reactive$presChange
                    dd$reactive$nameChange
                    
                    presList <- dd$presList
                    presType <- presList[[dd$pres]]$type
                 
                    if (presType %in% c('selectInput','radioButton') || length(dd$name) == 0) {
                        name <- ''
                    } else {
                        name <- switch(
                            as.character(dd$headerSize),
                            '1' = h1(dd$name, style = 'margin-top: 0;margin-bottom: 0;'),
                            '2' = h2(dd$name, style = 'margin-top: 0;margin-bottom: 0;'),
                            '3' = h3(dd$name, style = 'margin-top: 0;margin-bottom: 0;'),
                            '4' = h4(dd$name, style = 'margin-top: 0;margin-bottom: 0;'),
                            '5' = h5(dd$name, style = 'margin-top: 0;margin-bottom: 0;'),
                            '6' = h6(dd$name, style = 'margin-top: 0;margin-bottom: 0;'))
                    }
                    
                    name
                    
                })
                
                # dimension Links
                
                outputName = paste0(gdim,'DimLinks')
                
                output[[outputName]] <- shiny::renderUI({
                    
                    printDebug(env = env, dim, eventIn = 'renderLinks')
                    
                    dd$reactive$presChange
                    dd$reactive$linksChange
                    
                    presList <- dd$presList
                    links <- presList[[dd$pres]]$navOpts$links
                    
                    elems <- list()
                    
                    totWidth <- 0
                    
                    for (ll in links) {
                   
                        if(ifelse(!is.null(ll$visFun),do.call(ll$visFun,list(env = env),envir = env$ce),TRUE)) {
                            
                            eleObject <- list(width = isNull(ll$width,12))
                            totWidth <- totWidth + isNull(ll$width,12)
                            
                            if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'actionLink') {
                                ele <- shiny::actionLink(inputId = ll$id, label = ll$label)
                            }
                            
                            if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'downloadLink') {
                                ele <- shiny::downloadLink(outputId = ll$id, label = ll$label)
                            }
                            
                            if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'downloadButton') {
                                ele <- shiny::downloadButton(outputId = ll$id, label = ll$label)
                            }
                            
                            if (!is.null(ll$id) && ll$type == 'uiOutput') {
                                ele <- shiny::uiOutput(outputId = ll$id, inline = isNull(ll$inline,FALSE))
                            }
                            
                            if (!is.null(ll$id) && ll$type == 'dropDown') {
                                
                                if (!is.null(ll$choiceFun)) {
                                    choices <- do.call(ll$choiceFun,list(env = env),envir = env$ce) 
                                    if (!is.null(ll$placeholder)) {
                                        nm <- isNull(names(choices),rep('',length(choices)))
                                        choices <- c('',choices)
                                        names(choices) <- c(ll$placeholder,nm) 
                                    }
                                } else {
                                    choices <- c(a = '?')
                                }
                                
                                ele <- shiny::selectizeInput(
                                    inputId = ll$id, 
                                    label = ll$label, 
                                    choices = choices,
                                    width = ll$ddWidth,
                                    options = list(dropdownParent = 'body'))
                            }
                            
                            if (ll$type == 'dim') {
                                ele <- getDimUI(starId = env$id, dim = ll$dim, skipTopRow = TRUE, checkDups = FALSE)
                            }
                        
                            eleObject$ele <- ele
                            elems[[length(elems) + 1]] <- eleObject
                            
                        } 
                        
                    }
                    
                    if (totWidth < 12) 
                        preElm <- shiny::column(width = 12 - totWidth)
                    else 
                        preElm <- NULL
          
                    shiny::div(shiny::fluidRow(if(!is.null(preElm)) {preElm}, lapply(elems,function(x) {
                        shiny::column(width = x$width, div(style = 'float:right;padding-right:20px',x$ele))
                    })), class = paste0(gdim,'Links'))
                    
                })
                
                    
                # dimension Header

                outputHeader = paste0(gdim,'DimHeader')

                output[[outputHeader]] <- shiny::renderUI({
                    
                    printDebug(env = env, dim, eventIn = 'renderHeader')

                    dd$reactive$levelChange
                    dd$reactive$isFiltered
                    dd$reactive$presChange

                    presList <- dd$presList
                  
                    level <- dd$level
                    ancestors <- dd$ancestors

                    hideNoFilter <- presList[[dd$pres]]$navOpts$hideNoFilter
                    hideBreadCrumb <- presList[[dd$pres]]$navOpts$hideBreadCrumb
                    hideAll <- presList[[dd$pres]]$navOpts$hideAll
                    minBreadCrumbLevel <- isNull(presList[[dd$pres]]$navOpts$minBreadCrumbLevel,0)
                    
                    selLinks <- presList[[dd$pres]]$navOpts$selLinks
                    
                    if (minBreadCrumbLevel > level)
                        hideBreadCrumb <- TRUE
                    
                    elems <- list()
                    
                    txt <- ''
                    lnks <- ''
                    
                    if(!hideBreadCrumb) {
                    
                        txt <- paste0(txt,'<div style="padding-bottom:4px;" id="',gdim, 'Breadcrumb">')

                        if (!hideAll && minBreadCrumbLevel == 0) {
                            if (level == 0) {
                                txt <- paste0(txt, 'Top')
                            } else {
                                txt <- paste0(txt, shiny::actionLink(inputId = paste0(gdim,'DimLink0'), label = 'Top'))
                            }
                        }

                        if (level >= 1) {
                            for (lvl in 0:(level - 1)) {
                                if (lvl >= minBreadCrumbLevel) {
                                    if (lvl == 0) {
                                        if (!hideAll) 
                                            txt <- paste0(txt,'&nbsp>&nbsp')
                                        if (level == 1)
                                            txt <- paste0(txt,dd$levelNames[2])
                                        else 
                                            txt <- paste0(txt,shiny::actionLink(inputId = paste0(gdim,'DimLink1'), label = dd$levelNames[2]))    
                                    } else {
                                        if (lvl == (level - 1)) {
                                            if (lvl == minBreadCrumbLevel)
                                                txt <- paste0(txt,'&nbsp')
                                            else
                                                txt <- paste0(txt,'&nbsp>&nbsp',substr(ancestors[lvl + 2],1,30))
                                            
                                        } else {
                                            if (lvl == minBreadCrumbLevel)
                                                txt <- paste0(txt,shiny::actionLink(inputId = paste0(gdim,'DimLink',lvl + 1), label = substr(ancestors[lvl + 2],1,30)))
                                            else
                                                txt <- paste0(txt,'&nbsp>&nbsp',shiny::actionLink(inputId = paste0(gdim,'DimLink',lvl + 1), label = substr(ancestors[lvl + 2],1,30)))
                                        }    
                                    }
                                }
                            }
                        }

                    } else {

                        txt <- paste0(txt,'<div style="padding-bottom:4px;">')
                    }
        
                    for (ll in selLinks) {
        
                        if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'actionLink') {
                            ele <- shiny::actionLink(inputId = paste0(gdim, ll$id, 'selLink'), label = ll$label)
                        }
                        
                        lnks <- paste0(lnks, '&nbsp&nbsp<span>', ele,'</span>')
                        

                    }
                    
                    if (nchar(lnks) > 1) {
                        txt <- paste0(txt, lnks)
                    }
                    
                    if (any(dd$selected$level > 0) && !hideNoFilter) {

                        txt <- paste0(txt,'&nbsp<span style="float:right;">',actionLink(inputId = paste0(gdim,'NoFilter'), label = 'Verwijder Filter', style='color:red'),'</span>')
                    } else {
                        if (!hideNoFilter)
                            txt <- paste0(txt,'&nbsp')
                    }

                    txt <- paste0(txt,"</div>")
                    HTML(txt)

                })

                # dimension Body

                outputBody = paste0(gdim,'DimBody')

                output[[outputBody]] <- shiny::renderUI({

                    printDebug(env = env, dim, eventIn = 'renderBody')
                    
                    dd$reactive$presChange
                    
                    presList <- dd$presList

                    presType <- presList[[dd$pres]]$type
                    height <- presList[[dd$pres]]$height
                    width <- presList[[dd$pres]]$width

                    leafOnly <- dd$leafOnly

                    outputSimple <- paste0(gdim,'DimSimple')
                    outputDim <- paste0(gdim,'Dim')
                    outputChart <- paste0(gdim,'DimChart')
                    outputDateRange <- paste0(gdim,'DimDateRange')
                    outputRangeSlider <- paste0(gdim,'DimRangeSlider')
                    
                    if (is.null(height) || length(height) == 0) { height <- '300px'}

                    if (presType %in% c('radioButton','selectInput')) {

                        if (leafOnly) choices <- c() else choices <- dd$rootLabel

                        choices <- c(choices,dd$membersFiltered$member)
                        selected <- dd$selected$label
                        inline <- presList[[dd$pres]]$simpleOpts$inline

                    }
                    
                    if (presType %in% c('dateRangeInput','rangeSliderInput')) {
                       
                        minVal <- presList[[dd$pres]]$rangeOpts$min
                        maxVal <- presList[[dd$pres]]$rangeOpts$max
                        
                        rLabel <- isNull(presList[[dd$pres]]$rangeOpts$label,'')

                        if (any(dd$selected$level == 0)) {
                            startVal <- min(dd$data[['level1Label']])
                            endVal <- max(dd$data[['level1Label']])
                        } else {
                            startVal <- min(dd$selected$label)
                            endVal <- max(dd$selected$label)
                        }
                        
                    }

                    switch(
                        presType,
                        dataTable = DT::dataTableOutput(outputDim),
                        highCharts = highcharter::highchartOutput(outputChart, height = height),
                        radioButton = shiny::radioButtons(
                            inputId = outputSimple,
                            choices = choices,
                            label = dd$name,
                            selected = selected,
                            inline = inline),
                        selectInput = shiny::selectInput(
                            inputId = outputSimple,
                            choices = choices,
                            label = dd$name,
                            selected = selected,
                            selectize = FALSE),
                        dateRangeInput = shiny::dateRangeInput(
                            inputId = outputDateRange,
                            label = rLabel,
                            min = minVal,
                            max = maxVal,
                            start = startVal,
                            end = endVal,
                            language = 'nl',
                            separator = ' tm '),
                        rangeSliderInput = shiny::sliderInput(
                            inputId = outputRangeSlider,
                            label = rLabel,
                            min = minVal,
                            max = maxVal,
                            value = c(startVal, endVal)))
                })

                # dimension Footer

                outputFooter <- paste0(gdim,'DimFooter')

                output[[outputFooter]] <- shiny::renderUI({

                    printDebug(env = env, dim, eventIn = 'renderFooter')
                    
                    dd$reactive$presChange
                    
                    if (is.null(input[[paste0(gdim,'DimMs')]]))
                        val <- dd$msState
                    else 
                        val <- input[[paste0(gdim,'DimMs')]]

                    presList <- dd$presList
                    presType <- presList[[dd$pres]]$type
                    noWait <- isNull(presList[[dd$pres]]$navOpts$noWait,FALSE)

                    if (dd$selectMode == 'multi' && presType == 'dataTable') {

                        txt <- '<table><tr><td>'

                        txt <- paste0(
                            txt,
                            shiny::checkboxInput(
                                inputId = paste0(gdim,'DimMs'),
                                label = 'Meerdere items selecteren',
                                value = val),
                            '</td>')


                        if (length(val) > 0 && val && !noWait) {
                            txt <- paste0(
                                txt,
                                '<td>',
                                shiny::checkboxInput(
                                    inputId = paste0(gdim,'DimWait'),
                                    label = 'Wachten met bijwerken',
                                    value = NULL),
                                '</td></tr></table>')
                        } else {
                            txt <- paste0(
                                txt,
                                '<td hidden>',
                                shiny::checkboxInput(
                                    inputId = paste0(gdim,'DimWait'),
                                    label = 'Wachten met bijwerken',
                                    value = NULL),
                                '</td></tr></table>')
                        }

                        HTML(txt)

                    }

                })

            })
            
        }
        
        # init dims
        
        if (ddim %in% visibleDims(env)) {
            
            lst <- getMembers(env,ddim)
            
            if (!is.null(lst)) {
                env$dims[[ddim]]$membersFiltered <- lst$body
                env$dims[[ddim]]$footer <- lst$footer
            }
        }

        if (glob.env$debug) {
            print(paste0(env$dims[[ddim]]$gdim,'|observers: ',paste0(env$dims[[ddim]]$observers,collapse = '|')))
        }

    }
    
    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade",time = 2)
    env
}
