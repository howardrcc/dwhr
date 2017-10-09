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

                # dimension Header

                outputHeader = paste0(dim,'DimHeader')

                output[[outputHeader]] <- shiny::renderUI({

                    printDebug(env = env, dim, eventIn = 'renderHeader')

                    pres <- isolate(input[[paste0(dim,'Pres')]])
                    if(is.null(pres)) {
                        pres <- dd$defPres
                    }

                    dd$reactive$levelChange
                    dd$reactive$isFiltered

                    presList <- dd$presList
                    presType <- presList[[pres]]$type


                    if (presType %in% c('selectInput','radioButton') || length(dd$name) == 0) {
                        name <- ''
                    } else {
                        name <- h4(dd$name)
                    }

                    level <- dd$level
                    ancestors <- dd$ancestors

                    hideNoFilter <- presList[[pres]]$navOpts$hideNoFilter
                    hideBreadCrumb <- presList[[pres]]$navOpts$hideBreadCrumb
                    hideAll <- presList[[pres]]$navOpts$hideAll
                    links <- presList[[pres]]$navOpts$links

                    txt <- paste0('<table style="width:100%"><tr><td style="width:100%">',name,'</td>')

                    for (ll in links) {

                        if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'actionLink') {
                            txt <- paste0(txt,'<td>',shiny::actionLink(inputId = ll$id, label = ll$label),'</td>')
                        }
                        
                        if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'downloadLink') {
                            txt <- paste0(txt,'<td>',shiny::downloadLink(outputId = ll$id, label = ll$label),'</td>')
                        }

                        if (!is.null(ll$label) && !is.null(ll$id) && ll$type == 'downloadButton') {
                            txt <- paste0(txt,'<td>',shiny::downloadButton(outputId = ll$id, label = ll$label),'</td>')
                        }
                    }

                    presVec <- dd$presVec

                    if (length(presVec) > 1) {
                        txt <- paste0(txt, '<td><span style = "font-size:90%; display: inline-block; margin-right:10px; margin-top:2px">')
                    } else {
                        txt <- paste0(txt, '<td hidden><span>')
                    }

                    txt <- paste0(
                        txt,
                        shiny::selectizeInput(
                            inputId = paste0(dim,'Pres'),
                            label = NULL,
                            choices = presVec,
                            width = "150px",
                            selected = pres),
                        '</span></td></tr></table>')

                    if(!hideBreadCrumb) {
                        
                        #browser(expr = {dim == 'kpi'})

                        txt <- paste0(txt,'<div style="padding-bottom:4px;" id="',dim, 'Breadcrumb">')

                        if (!hideAll) {
                            if (level == 0) {
                                txt <- paste0(txt, 'Top')
                            } else {
                                txt <- paste0(txt, shiny::actionLink(inputId = paste0(dim,'DimLink0'), label = 'Top'))
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
                                        txt <- paste0(txt,shiny::actionLink(inputId = paste0(dim,'DimLink1'), label = dd$levelNames[2]))    
                                } else {
                                    if (lvl == (level - 1)) {
                                        txt <- paste0(txt,'&nbsp>&nbsp',substr(ancestors[lvl + 2],1,30))
                                    } else {
                                        txt <- paste0(txt,'&nbsp>&nbsp',shiny::actionLink(inputId = paste0(dim,'DimLink',lvl + 1), label = substr(ancestors[lvl + 2],1,30)))
                                    }    
                                }
                            }
                        }

                    } else {

                        txt <- paste0(txt,'<div style="padding-bottom:4px;">')
                    }

                    if (any(dd$selected$level > 0) && !hideNoFilter) {

                        txt <- paste0(txt,'<span style="float:right;">',actionLink(inputId = paste0(dim,'NoFilter'), label = 'Verwijder Filter', style='color:red'),'</span>')
                    }

                    txt <- paste0(txt,"</div>")
                    HTML(txt)

                })

                # dimension Body

                outputBody = paste0(dim,'DimBody')

                output[[outputBody]] <- shiny::renderUI({

                    shiny::req(input[[paste0(dim,'Pres')]])

                    printDebug(env = env, dim, eventIn = 'renderBody')

                    presList <- dd$presList

                    pres <- input[[paste0(dim,'Pres')]]
                    presType <- presList[[pres]]$type
                    height <- presList[[pres]]$height
                    width <- presList[[pres]]$width

                    leafOnly <- dd$leafOnly

                    outputSimple <- paste0(dim,'DimSimple')
                    outputDim <- paste0(dim,'Dim')
                    outputChart <- paste0(dim,'DimChart')


                    if (is.null(height)) { height <- '300'}

                    if (presType %in% c('radioButton','selectInput')) {

                        if (leafOnly) choices <- c() else choices <- dd$rootLabel

                        choices <- c(choices,dd$membersFiltered$member)
                        selected <- dd$selected$label
                        inline <- presList[[pres]]$simpleOpts$inline

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

                outputFooter <- paste0(dim,'DimFooter')

                output[[outputFooter]] <- shiny::renderUI({

                    req(input[[paste0(dim,'Pres')]])
                    
                    printDebug(env = env, dim, eventIn = 'renderFooter')
                    
                    pres <- input[[paste0(dim,'Pres')]]
                    val <- input[[paste0(dim,'DimMs')]]

                    if(is.null(pres)) {
                        pres <- dd$defPres
                    }

                    presList <- dd$presList
                    presType <- presList[[pres]]$type

                    if (dd$selectMode == 'multi' && presType == 'dataTable') {

                        txt <- '<table><tr><td>'

                        txt <- paste0(
                            txt,
                            shiny::checkboxInput(
                                inputId = paste0(dim,'DimMs'),
                                label = 'Meerdere items selecteren',
                                value = val),
                            '</td>')


                        if (length(val) > 0 && val) {
                            txt <- paste0(
                                txt,
                                '<td>',
                                shiny::checkboxInput(
                                    inputId = paste0(dim,'DimWait'),
                                    label = 'Wachten met bijwerken',
                                    value = NULL),
                                '</td></tr></table>')
                        } else {
                            txt <- paste0(
                                txt,
                                '<td hidden>',
                                shiny::checkboxInput(
                                    inputId = paste0(dim,'DimWait'),
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
