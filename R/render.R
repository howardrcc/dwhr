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
                gdim <- dd$globalDim
                
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
                
                # dimension preslist
                
                outputName = paste0(gdim,'DimPresList')
                
                output[[outputName]] <- shiny::renderUI({
                    
                    printDebug(env = env, dim, eventIn = 'renderPresList')
                    
                    presVec <- dd$presVec
                    
                    if (length(presVec) > 1) {
                        
                        
                        if (dd$presListType == 'dropdown') {
                            
                            txt <- '<span style = "font-size:90%; display: inline-block; margin-right:20px; margin-top:2px; float:right;">'
                            
                            txt <- paste0(
                                txt,
                                shiny::selectizeInput(
                                    inputId = paste0(gdim,'Pres'),
                                    label = NULL,
                                    choices = presVec,
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
                    
                    presList <- dd$presList
                    presType <- presList[[dd$pres]]$type
                    
                    if (presType %in% c('selectInput','radioButton') || length(dd$name) == 0) {
                        name <- ''
                    } else {
                        name <- h4(dd$name)
                    }
                    
                    name
                    
                })
                
                # dimension Links
                
                outputName = paste0(gdim,'DimLinks')
                
                output[[outputName]] <- shiny::renderUI({
                    
                    printDebug(env = env, dim, eventIn = 'renderLinks')
                    
                    dd$reactive$presChange
                    
                    presList <- dd$presList
                    
                    presType <- presList[[dd$pres]]$type
                    links <- presList[[dd$pres]]$navOpts$links
                    
                    txt <- paste0('<div>')
                    
                    for (ll in links) {
                        
                        if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'actionLink') {
                            txt <- paste0(txt,shiny::actionLink(inputId = ll$id, label = ll$label))
                        }
                        
                        if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'downloadLink') {
                            txt <- paste0(txt,shiny::downloadLink(outputId = ll$id, label = ll$label))
                        }
                        
                        if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'downloadButton') {
                            txt <- paste0(txt,shiny::downloadButton(outputId = ll$id, label = ll$label))
                        }
                    }
                    
                    txt <- paste0(txt,'</div>')
                    HTML(txt)
                
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
            
                    txt <- ''

                    if(!hideBreadCrumb) {
                    
                        txt <- paste0(txt,'<div style="padding-bottom:4px;" id="',dim, 'Breadcrumb">')

                        if (!hideAll) {
                            if (level == 0) {
                                txt <- paste0(txt, 'Top')
                            } else {
                                txt <- paste0(txt, shiny::actionLink(inputId = paste0(gdim,'DimLink0'), label = 'Top'))
                            }
                        }

                        if (level >= 1) {
                            for (lvl in 0:(level - 1)) {
                                if (lvl == 0) {
                                    if (!hideAll) 
                                        txt <- paste0(txt,'&nbsp>&nbsp')
                                    if (level == 1)
                                        txt <- paste0(txt,dd$levelNames[2])
                                    else 
                                        txt <- paste0(txt,shiny::actionLink(inputId = paste0(gdim,'DimLink1'), label = dd$levelNames[2]))    
                                } else {
                                    if (lvl == (level - 1)) {
                                        txt <- paste0(txt,'&nbsp>&nbsp',substr(ancestors[lvl + 2],1,30))
                                    } else {
                                        txt <- paste0(txt,'&nbsp>&nbsp',shiny::actionLink(inputId = paste0(gdim,'DimLink',lvl + 1), label = substr(ancestors[lvl + 2],1,30)))
                                    }    
                                }
                            }
                        }

                    } else {

                        txt <- paste0(txt,'<div style="padding-bottom:4px;">')
                    }

                    if (any(dd$selected$level > 0) && !hideNoFilter) {

                        txt <- paste0(txt,'<span style="float:right;">',actionLink(inputId = paste0(gdim,'NoFilter'), label = 'Verwijder Filter', style='color:red'),'</span>')
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


                    if (is.null(height)) { height <- '300'}

                    if (presType %in% c('radioButton','selectInput')) {

                        if (leafOnly) choices <- c() else choices <- dd$rootLabel

                        choices <- c(choices,dd$membersFiltered$member)
                        selected <- dd$selected$label
                        inline <- presList[[dd$pres]]$simpleOpts$inline

                    }

                    switch(
                        presType,
                        dataTable = DT::dataTableOutput(outputDim),
                        highCharts = highcharter::highchartOutput(outputChart,height = height),
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
                            selectize = FALSE))
                })

                # dimension Footer

                outputFooter <- paste0(gdim,'DimFooter')

                output[[outputFooter]] <- shiny::renderUI({

                    printDebug(env = env, dim, eventIn = 'renderFooter')
                    
                    dd$reactive$presChange
                    val <- input[[paste0(gdim,'DimMs')]]

                    presList <- dd$presList
                    presType <- presList[[dd$pres]]$type

                    if (dd$selectMode == 'multi' && presType == 'dataTable') {

                        txt <- '<table><tr><td>'

                        txt <- paste0(
                            txt,
                            shiny::checkboxInput(
                                inputId = paste0(gdim,'DimMs'),
                                label = 'Meerdere items selecteren',
                                value = val),
                            '</td>')


                        if (length(val) > 0 && val) {
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

                # init dims
                
                if (dim %in% visibleDims(env)) {
                    
                    lst <- getMembers(env,dim)
                    
                    if (!is.null(lst)) {
                        dd$membersFiltered <- lst$body
                        dd$footer <- lst$footer
                    }
                }

            })
        }

        if (glob.env$debug) {
            print(paste0(ddim,'|observers: ',paste0(env$dims[[ddim]]$observers,collapse = '|')))
        }

    }

    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade",time = 2)
    env
}
