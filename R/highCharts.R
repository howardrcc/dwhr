
plotBandSingleSelectJS <- function(env,dim,label,color,serieType) {
    
    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    
    selectable <- 'true'
    unSelectable <- 'true'
    drillable <- 'true'

    if (dd$leafOnly) {
        selectable <- ifelse((dd$level == dd$maxLevel),'true','false')
        unSelectable <- 'false'
    }

    if (serieType %in% c('gauge','solidgauge')) {
        selectable <- 'false'
        unSelectable <- 'false'
        drillable <- 'false'
    }

    if (dd$selectMode != 'single') {
        selectable <- 'false'
        unSelectable <- 'false'
    }

    if (dd$level == 0) {
        unSelectable <- 'false'
    }

    if (dd$level == dd$maxLevel) {
        drillable <- 'false'
    }

    if (label == ' ') {
        selectable <- 'false'
        unSelectable <- 'false'
        drillable <- 'false'
    }

    highcharter::JS(paste0("function(event){
    plotBandSingleSelect('",gdim,"',this, event,",selectable,",",unSelectable,",",drillable,",'",color,"');
}"))
}

pointSingleSelectJS <- function(env,dim,color,serieType) {
    
    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    
    selectable <- 'true'
    unSelectable <- 'true'
    drillable <- 'true'

    if (dd$leafOnly) {
        selectable <- ifelse((dd$level == dd$maxLevel),'true','false')
        unSelectable <- 'false'
    }

    if (serieType %in% c('gauge','solidgauge')) {
        selectable <- 'false'
        unSelectable <- 'false'
        drillable <- 'false'
    }

    if (dd$selectMode != 'single') {
        selectable <- 'false'
        unSelectable <- 'false'
    }

    if (dd$level == 0) {
        unSelectable = 'false'
    }

    if (dd$level == dd$maxLevel) {
        drillable <- 'false'
    }

     highcharter::JS(paste0("function(event){
    pointSingleSelect('",gdim,"',this, event,",selectable,",",unSelectable,",",drillable,",'",color,"');
}"))
}

fixCreditsJS <- function() {
    highcharter::JS("function() {
    this.credits.element.onclick = function() {
        window.open('http://umcdotsaprd02:85/BI_startpunt.aspx','_blank');
    };}")
}

readyJS <- function(dim) {
    highcharter::JS(paste0("function(){
    var number = Math.random();
    $.unblockUI();
    Shiny.onInputChange('",dim,"HighchartReady',{r: number});
    }"))
}


getCustomPattern <- function(env,dim,stroke,width) {
    # door base tag in shinyserver-pro werken relatieve urls binnen svg niet -> absolute url gebruiken
    userData <- env$session$userData
    
    id <- paste0('custom-',dim,'-',gsub('#','',stroke),'-',width)
    env$customPatterns[[dim]][[id]] <- list( id = id
                                  , path = list( d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11'
                                               , stroke = stroke
                                               , strokeWidth = width))

    paste0('url(',userData$baseUrl,'#',id,')')
}


getFormat <- function(format) {
    switch(
        isNull(format,'standard'),
        euro = '\U20AC {point.y:,.0f}',
        euro2 = '\U20AC {point.y:,.2f}',
        perc = '{point.y:.0f} %',
        perc1 = '{point.y:.1f} %',
        perc2 = '{point.y:.2f} %',
        integer = '{point.y:,.0f}',
        decimal1 = '{point.y:,.1f}',
        decimal2 = '{point.y:,.2f}',
        decimal3 = '{point.y:,.3f}',
        standard = '{point.y}')
}

getMemberValue <- function(env, dim, memberLevel, memberValue, viewColumn) {
    
    dd <- env$dims[[dim]]
    
    pc <- dd$pc
    memberParent <- pc$parentLabel[pc$level == memberLevel & pc$label == memberValue]
    length(memberParent) > 0 || dwhrStop('member not found')

    meas <- getMeasList(env,dim)
    if (dd$level != memberLevel || dd$parent != memberParent) {
        df <- getMembers(env = env, dim = dim,level = memberLevel, parent = memberParent)$body
    } else {
        df <- data.frame(dd$membersFiltered)
    }

    format <- meas$format[meas$viewColumn == viewColumn]
    formatRef <- meas$formatRef[meas$viewColumn == viewColumn]

    if(!is.na(formatRef)) {
        format <- df[df$member == memberValue,c(formatRef)]
    }

    y <- df[df$member == memberValue,c(viewColumn)]

    if (length(format) > 0 && format %in% c('perc','perc1','perc2')) {
        y <- y * 100
    }

    return(list(value = y, format = format))

}

getAdhocSlice <- function(env, dim,level,parent,selected) {

    dd <- env$dims[[dim]]
    
    is.null(selected) || length(setdiff(c('dim', 'level','parent','label'),names(selected))) == 0 || dwhrStop('Invalid selected parameter')
    is.null(selected) || 'data.frame' %in% class(selected) || dwhrStop('Invalid selected parameter')

    pc <- dd$pc
    memberParent <- pc$parentLabel[pc$level == memberLevel & pc$label == memberValue]
    length(memberParent) > 0 || dwhrStop('member not found')

    meas <- getMeasList(env,dim)
    if (dd$level != memberLevel || dd$parent != memberParent) {
        df <- getMembers(env = env, dim = dim,level = memberLevel, parent = memberParent)$body
    } else {
        df <- data.frame(dd$membersFiltered)
    }

    format <- meas$format[meas$viewColumn == viewColumn]
    formatRef <- meas$formatRef[meas$viewColumn == viewColumn]

    if(!is.na(formatRef)) {
        format <- df[df$member == memberValue,c(formatRef)]
    }

    y <- df[df$member == memberValue,c(viewColumn)]

    if (length(format) > 0 && format %in% c('perc','perc1','perc2')) {
        y <- y * 100
    }

    return(list(value = y, format = format))

}

makeHcWidget <- function(env,dim,prep){
    
    print <- prep$print
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    hcoptslang$decimalPoint <- ','
    options(highcharter.lang = hcoptslang)
    
    a <- highcharter::highchart()

    if (!is.null(prep$chartOpts)) {
        prep$chartOpts$hc = a
        prep$chartOpts$events$redraw <- readyJS(dim)
        a <- do.call(eval(parse(text = 'highcharter::hc_chart')), prep$chartOpts)
    }
    
    patterns <- lapply(env$customPatterns[[dim]],function(x) {return (x)})
    
    if (length(patterns) > 0) {
        
        names(patterns) <- NULL  # het moet een unnamed list zijn
        
        defsOpts <- list(
            hc = a,
            patterns = patterns)
        
        a <- do.call(eval(parse(text = 'highcharter::hc_defs')), defsOpts)
    }
    
    if (!is.null(prep$titleOpts)) {
        prep$titleOpts$hc <- a
        a <- do.call(eval(parse(text = 'highcharter::hc_title')), prep$titleOpts)
    }
    
    if(!is.null(prep$xAxisOpts)) {
        prep$xAxisOpts$hc <- a
        a <- do.call(eval(parse(text = 'highcharter::hc_xAxis')), prep$xAxisOpts)
    }
    
    if(!is.null(prep$yAxisOpts)) {
        prep$yAxisOpts$hc <- a
        a <- do.call(eval(parse(text = 'highcharter::hc_yAxis_multiples')), prep$yAxisOpts)
    }
    
    if(!is.null(prep$tooltipOpts)) {
        prep$tooltipOpts$hc <- a
        a <- do.call(eval(parse(text = 'highcharter::hc_tooltip')), prep$tooltipOpts)
    }
    
    if (!is.null(prep$legendOpts)) {
        prep$legendOpts$hc <- a
        a <- do.call(eval(parse(text = 'highcharter::hc_legend')), prep$legendOpts)
    }
    
    if (!is.null(prep$paneOpts)) {
        prep$paneOpts$hc <- a
        a <- do.call(eval(parse(text = 'highcharter::hc_pane')), prep$paneOpts)
    }
    
    if (!is.null(prep$plotOptionsOpts)) {
        prep$plotOptionsOpts$hc <- a
        a <- do.call(eval(parse(text = 'highcharter::hc_plotOptions')), prep$plotOptionsOpts)
    }
    
    a <- a %>%
        highcharter::hc_add_series_list(prep$seriesOpts) %>%
        highcharter::hc_add_theme(highcharter::hc_theme_smpl())
    
    
    # hc_credits( enabled = TRUE
    #           , position = list(
    #                 align = 'right',
    #                 x = -10,
    #                 verticalAlign = 'top',
    #                 y = 20)
    #           , style =  list(color = 'black', fontSize= '12px')
    #             , text = "Business Intelligence and Analytics") %>%
    
    a
    
}

#'
#' @export
#'
prepHc <- function(env, dim, pres, print = FALSE) {

    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    
    presList <- dd$presList
    presType <- presList[[pres]]$type

    mode <- dd$selectMode
    followPager <- isNull(dd$syncNav,FALSE) && isNull(dd$pageLength,FALSE)

    expandList <- function(l){
        lapply(l, function(x) if(class(x) == 'function') do.call(x,list()) else if(is.list(x)) expandList(x) else x)
    }

    highChartsOpts <- expandList(presList[[pres]]$highChartsOpts)

    chartOpts <- highChartsOpts$chart
    tooltipOpts <- highChartsOpts$tooltip
    xAxisOpts <- highChartsOpts$xAxis
    yAxisOpts <- highChartsOpts$yAxis
    legendOpts <- highChartsOpts$legend
    seriesOpts <- highChartsOpts$series
    plotOptionsOpts <- highChartsOpts$plotOptions
    titleOpts <- highChartsOpts$title
    plotBandColor <- highChartsOpts$dashboard$plotBandColor
    paneOpts <- highChartsOpts$pane

    if (is.null(plotBandColor)) {
        plotBandColor <- 'lightGrey'
    }

    presCols <- sapply(seriesOpts,function(x) {return(x$viewColumn)})
    serieTypes <- sapply(seriesOpts,function(x) {return(x$type)})
    palList <- lapply(seriesOpts,function(x) {return(x$colors)})
    patternList <- sapply(seriesOpts,function(x) {return(x$pattern)})
    clickable <- sapply(seriesOpts,function(x) {return(ifelse(is.null(x$clickable),TRUE,x$clickable))})

    level <- dd$level
    parent <- dd$parent
    selected <- dd$selected
    meas <- getMeasList(env,dim)
    currentPage <- dd$currentPage
    pageLength <- dd$pageLength
    goToPage <- dd$goToPage
    
    presCols <- intersect(presCols,meas$viewColumn)

    tab <- data.frame(dd$membersFiltered)

    fc = meas$viewColumn[meas$format %in% c('perc','perc1','perc2')]
    if (length(fc) > 0) {
        tab[,fc] <- tab[,fc] * 100
    }

    #
    # set firstrow & page
    #

    firstRow <- getFirstRow(env,dim,tab)

    if (is.null(goToPage)) {
        currentPage <- ((firstRow - 1) %/% pageLength) + 1
    } else {
        currentPage <- goToPage
        dd$goToPage <- NULL
    }

    dd$currentPage <- currentPage

    measCols <- meas$viewColumn[order(meas$sort, method = 'radix')]
    measColNames <- meas$as[order(meas$sort, method = 'radix')]

    allCols <- c('member',measCols)
    orderViewColumn <- dd$orderViewColumn
    orderColumnDir <- dd$orderColumnDir
    orderViewColumn2 <- dd$orderViewColumn2

    if (!is.null(orderViewColumn2)) {
        tab <- tab[order(tab[,orderViewColumn],tab[,orderViewColumn2],tab$member,method = 'radix', decreasing = (orderColumnDir == 'desc')),]
    } else {
        tab <- tab[order(tab[,orderViewColumn],tab$member,method = 'radix', decreasing = (orderColumnDir == 'desc')),]
    }

    labels <- tab$member

    sel <- NULL

    if (mode == 'single') {
        sel <- selected$label[selected$level == level & selected$parent == parent]
    }

    if (is.null(sel) || length(sel) == 0) {
        sel <- ''
    }

    dd$highchartsClickEvent <- -1

    series <- list()

    pageStart <- 1
    pageEnd <- nrow(tab)

    if (followPager) {
        pageStart <- ((currentPage - 1) * dd$pageLength) + 1
        pageEnd <- pageStart + dd$pageLength - 1

        if(!isNull(xAxisOpts$fillPage,FALSE) && pageEnd > nrow(tab))
            pageEnd <- nrow(tab)
    }

    plotBands <- list()

    if (length(presCols) > 0) {
        
        for (serieNum in 1:length(presCols)) {
            
            serieType <- serieTypes[[serieNum]]
            pal <- palList[[serieNum]]
            pattern <- patternList[[serieNum]]
            presCol <- presCols[[serieNum]]
            
            colName <- measColNames[which(measCols == presCol)]
            format <- meas$format[meas$as == colName]
            
            fmt <- getFormat(format)
            
            fmtTooltip <- paste0('{series.name}:',fmt,'<br/>')
            dt <- tab[pageStart:pageEnd,c(presCol)]
            
            labelsPage <- as.character(labels[pageStart:pageEnd])
            labelsPage[is.na(labelsPage)] <- ' '
            
            if (serieType %in% c('pie','treemap')) {
                labelsPage <- labelsPage[dt > 0 & !is.na(dt)]
                dt <- dt[dt > 0 & !is.na(dt)]
            }
            
            seriesData <- list()
            
            # check datalabel instelling (formatter?)
            
            dl <- seriesOpts[[serieNum]]$dataLabels$enabled
            
            if (is.null(dl)) {
                dl <- plotOptionsOpts$series$dataLabels$enabled
            }
            
            if (is.null(dl)) {
                dl <- plotOptionsOpts[[serieType]]$dataLabels$enabled
            }
            
            if (is.null(dl)) {
                dl <- FALSE
            }
            
            seriesList <- seriesOpts[[serieNum]]
            
            if (!is.null(seriesList[['color']])) {
                if (length(pattern) > 0 && pattern == 'stripe1') {
                    seriesList[['color']] <- getCustomPattern(env,dim,seriesList[['color']],2)
                }
            }
            
            colorsPage <- getColors(dt, pal,seriesList$colorTrans)
            seriesList$colorTrans <- NULL
            
            if (!is.null(colorsPage)) {
                if (length(colorsPage) == 1) {
                    seriesList$colors <- list(colorsPage)
                } else {
                    seriesList$colors <- colorsPage
                }
            }
            
            if (serieType %in% c('gauge','solidgauge')) {      # 1D
                
                memberValue <- seriesOpts[[serieNum]]$member
                
                if (!is.null(memberValue)) {
                    memberLevel <- isNull(seriesOpts[[serieNum]]$memberLevel,dd$maxLevel)
                    memberLevel %in% dd$useLevels || dwhrStop('Invalid memberLevel')
                } else {
                    sel <- dd$selected
                    memberLevel <- sel$level
                    memberValue <- sel$label
                }
                
                y <- getMemberValue(env = evn, dim = dim, memberLevel = memberLevel, memberValue = memberValue, viewColumn = presCol)
                
                if (length(y$value) > 0) {
                    
                    seriesData[[1]] <- list(
                        name = colName,
                        id = colName,
                        y = y$value)
                    
                    fmt <- getFormat(y$format)
                    fmtTooltip <- paste0('{series.name}:',fmt,'<br/>')
                }
                
            } else {   #2D
                
                i <- 0
                
                if (length(dt) > 0) {
                    for (rec in 1:length(dt)) {
                        point <- list()
                        
                        if (serieType == 'pie') {
                            
                            if (!is.na(dt[rec]) && dt[rec] >= 0) {
                                point$y = dt[rec]
                                point$name = labelsPage[rec]
                                point$sliced = (labelsPage[rec] %in% sel)
                                point$visible = TRUE
                                point$id = labelsPage[rec]
                                i <- i + 1
                                seriesData[[i]] <- point
                            }
                            
                        }
                        
                        if (serieType == 'treemap') {
                            
                            if (!is.na(dt[rec]) && dt[rec] >= 0) {
                                point$value = dt[rec]
                                point$y = dt[rec]
                                point$name = labelsPage[rec]
                                if (labelsPage[rec] %in% sel) {
                                    point$color = plotBandColor
                                }
                                point$orgColor = colorsPage[rec]
                                point$id = labelsPage[rec]
                                i <- i + 1
                                seriesData[[i]] <- point
                            }
                        }
                        
                        if (!(serieType %in% c('pie','treemap'))) {
                            
                            point <- list(
                                name = labelsPage[rec],
                                id = labelsPage[rec],
                                y = dt[rec])
                            
                            if (dl) {
                                if (is.na(dt[rec]) || dt[rec] == 0) {
                                    point$dataLabels <- list(enabled = FALSE)
                                } else {
                                    point$dataLabels <- list(enabled = TRUE)
                                }
                            }
                            
                            seriesData[[rec]] <- point
                        }
                    }
                }
            }
            
            seriesList$id = serieNum
            seriesList$name = colName
            seriesList$data = seriesData
            
            seriesList$tooltip$pointFormat = fmtTooltip
            
            if (is.null(seriesList$dataLabels$format)) {
                seriesList$dataLabels$format = fmt
            }
            
            seriesList$viewColumn <- NULL
            
            if (clickable[serieNum]) {
                seriesList[['point']]$events = list(click = pointSingleSelectJS(env,dim,plotBandColor,serieType))
            }
            
            series[[length(series)+1]] <- seriesList
            
            
            if(length(plotBands) == 0 &&
               !(serieType %in% c('pie','treemap','gauge','solidgauge'))) {
                
                x <- as.data.frame(tab[pageStart:pageEnd,presCols[which(clickable)]])
                
                complete <- rowSums(is.na(x))<length(x)   # true als er tenminste 1 non-na is
                
                for (rec in 1:nrow(tab[pageStart:pageEnd,])) {
                    
                    if (length(complete) >= rec && complete[[rec]]) {
                        lbl <- labelsPage[rec]
                    } else {
                        lbl <- ' '
                    }
                    
                    plotBands[[rec]] <- list(color = ifelse(labelsPage[rec] %in% sel,plotBandColor,'rgba(0,0,0,0)')
                                             , from = rec - 1.5
                                             , to = rec - 0.5
                                             , id = rec - 1
                                             , zIndex = 1
                                             , events = list(click = plotBandSingleSelectJS(env,dim,lbl,plotBandColor,serieType)))
                    
                }
                
                xAxisOpts$plotBands <- plotBands
                xAxisOpts$categories <- sanitizeLabel(labelsPage,10)
            }
            
        }
        
        
        plotOptionsOpts$series$events$hide <- highcharter::JS(paste0("function(){
                                                             var number = Math.random();
                                                             Shiny.onInputChange('",gdim,"HighchartHide',{r: number, data: this.options.id})}"))
        plotOptionsOpts$series$events$show <- highcharter::JS(paste0("function(){
                                                               var number = Math.random();
                                                               Shiny.onInputChange('",gdim,"HighchartShow',{r: number, data: this.options.id})}"))
    } 

    ret <- list(
        legendOpts = legendOpts,
        seriesOpts = series,
        tooltipOpts = tooltipOpts,
        xAxisOpts = xAxisOpts,
        yAxisOpts = yAxisOpts,
        plotOptionsOpts = plotOptionsOpts,
        chartOpts = chartOpts,
        titleOpts = titleOpts,
        paneOpts = paneOpts,
        plotBands = plotBands,
        print = print)

    ret$widget <- makeHcWidget(env,dim,ret)
    
    if (!print)
        env$hcPrep[[dim]] <- ret
    
    ret

}

sanitizeLabel <- function(x, m) {

    ret <- c()
    for (s in x) {
        ss <- ''
        for (t in unlist(strsplit(s,' '))) {
            if (nchar(t) > m) {
                tt <- paste0(substring(t,1,m),'...')
            } else {
                tt <- t
            }

            ss <- paste0(ss,tt,' ')
        }

        ret <- c(ret,trimws(ss,'right'))
    }
    ret

}

removeCallbacks <- function(chart){
    if (length(chart$seriesOpts) > 0) {
        for (serie in 1:length(chart$seriesOpts)) {
            chart$seriesOpts[[serie]][['point']]$events <- NULL
        }
        
        chart$plotOptionsOpts$series$events <- NULL
        
        chart$xAxisOpts$plotBands <- NULL
    }
    chart
}


listDiff <- function(l1,l2) {
    l2[sapply(names(l2),function(x) !identical(l1[[x]],l2[[x]]))]
}

renderHighchartDim <- function(env, dim, input,output) {

    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    
    outputChart <- paste0(gdim,'DimChart')
    env$hcRenderers[[dim]] <- reactiveValues(count=0)
    obs <- dd$observers

    output[[outputChart]] <- highcharter::renderHighchart({

        env$hcRenderers[[dim]]$count

        pres <- dd$pres
        prep <- env$hcPrep[[dim]]
        
        if (env$hcRenderers[[dim]]$count == 0) {
            return()
        }

        printDebug(env, dim, eventIn = 'renderHighCharts', info = paste0('rendercount:', env$hcRenderers[[dim]]$count))
        
        if (is.null(prep))
            prep <- prepHc(env,dim,pres)
        
        env$hcPrev[[dim]] <- removeCallbacks(prep)
        env$hcPrep[[dim]] <- NULL
        
        prep$widget
        
    })
    
    shiny::outputOptions(output,outputChart,suspendWhenHidden = FALSE)
    shiny::outputOptions(output,outputChart,priority = 5)


    #
    # observers voor highcharts
    #

    highchartsClickEvent <- paste0(gdim,'HighchartClick')

    if (!(highchartsClickEvent %in% obs)) {

        observeEvent(input[[highchartsClickEvent]], {
            
            lvl <- dd$level
            parent <- dd$parent
            presList <- dd$presList

            pres <- dd$pres
            presType <- presList[[pres]]$type
            pageLength <- dd$pageLength
            currentPage <- dd$currentPage

            chartType <- input[[highchartsClickEvent]]$type

            event <- input[[highchartsClickEvent]]

            drill <- event$drill
            select <- event$select
            unSelect <- event$unSelect
            id <- event$id

            if(exists(paste0(dim,'HighChartsClickHook'),envir = env$ce)) {
                do.call(paste0(dim,'HighChartsClickHook'),list(env = env, event = event),envir = env$ce)
            }

            if (select || unSelect || drill) {

                tab <- dd$membersFiltered

                e <- which(tab$member == id)
                dd$highchartsClickEvent <- e

                if (drill) {

                    if (dd$level < dd$maxLevel) {
                        dd$parent <- dd$membersFiltered$member[e]
                        dd$ancestors <- c(dd$ancestors,dd$parent)
                        dd$level <- dd$level + 1
                        dd$reactive$levelChange <- dd$reactive$levelChange + 1
                        printDebug(env, dim, eventIn = 'highchartsClick', eventOut = 'levelChange', info = 'drill')
                    }

                } else {

                    l <- data.frame(
                        level = lvl,
                        parent = parent,
                        label = dd$membersFiltered$member[e],
                        stringsAsFactors = FALSE)

                    s <- dd$selected

                    if (identical(s,l)) {
                        dd$selected <- dd$rootSelected
                    } else {
                        dd$selected <- l
                    }

                    dd$rowLastAccessed$value[dd$rowLastAccessed$level == lvl] <- dd$membersFiltered$member[e]

                    dimCorrectSelectionInfo(input,env,dim)
                    dimSetHasSubselect(env,dim)
                    dd$selectSource <- 'highchartsClick'
                    dd$reactive$selectChange <- dd$reactive$selectChange + 1
                    printDebug(env, dim, eventIn = 'highchartsClick', eventOut = 'selectChange', info = 'select')

                }
            }

        })

        obs <- c(obs,highchartsClickEvent)
    }


    highchartsHideEvent <- paste0(gdim,'HighchartHide')

    if (!(highchartsHideEvent %in% obs)) {

        observeEvent(input[[highchartsHideEvent]], {
            
            e <- input[[highchartsHideEvent]]$data
            pres <- dd$pres
            presList <- dd$presList

            seriesOpts <- presList[[pres]]$highChartsOpts$series

            seriesOpts[[e]]$visible <- FALSE
            dd$presList[[pres]]$highChartsOpts$series <- seriesOpts

        })

        obs <- c(obs,highchartsHideEvent)
    }

    highchartsShowEvent <- paste0(gdim,'HighchartShow')

    if (!(highchartsShowEvent %in% obs)) {

        observeEvent(input[[highchartsShowEvent]], {
         
            e <- input[[highchartsShowEvent]]$data

            pres <- dd$pres
            presList <- dd$presList

            seriesOpts <- presList[[pres]]$highChartsOpts$series

            seriesOpts[[e]]$visible <- TRUE
            dd$presList[[pres]]$highChartsOpts$series <- seriesOpts

        })

        obs <- c(obs,highchartsShowEvent)
    }

    highchartsPbClickEvent <- paste0(gdim,'HighchartPbClick')

    if (!(highchartsPbClickEvent %in% obs)) {

        observeEvent(input[[highchartsPbClickEvent]], {
            
            lvl <- dd$level
            parent <- dd$parent
            presList <- dd$presList

            pres <- dd$pres
            followPager <- isNull(dd$syncNav,FALSE) && isNull(dd$pageLength,FALSE)
            pageLength <- dd$pageLength
            currentPage <- dd$currentPage

            event <- input[[highchartsPbClickEvent]]

            drill <- event$drill
            select <- event$select
            unSelect <- event$unSelect

            if(exists(paste0(dim,'HighChartsPbClickHook'),envir = env$ce)) {
                do.call(paste0(dim,'HighChartsPbClickHook'),list(env = env, event = event),envir = env$ce)
            }

            if (select || unSelect || drill) {

                e <- event$data + 0.5

                if(followPager) {
                    e <- e + pageLength * (currentPage - 1)
                }

                tab <- dd$membersFiltered

                if (nrow(tab) < e + 1) {
                    return()
                }

                orderViewColumn <- dd$orderViewColumn
                orderColumnDir <- dd$orderColumnDir
                orderViewColumn2 <- dd$orderViewColumn2

                if (!is.null(orderViewColumn2)) {
                    nm <- tab[
                        order(
                            tab[,orderViewColumn],
                            tab[,orderViewColumn2],
                            tab$member,
                            method = 'radix',
                            decreasing = (orderColumnDir == 'desc')),][e + 1,]$member
                } else {
                    nm <- tab[
                        order(
                            tab[,orderViewColumn],
                            tab$member,
                            method = 'radix',
                            decreasing = (orderColumnDir == 'desc')),][e + 1,]$member
                }

                e <- which(tab$member == nm)
                dd$highchartsPbClickEvent <- e

                if (drill) {

                    if (dd$level < dd$maxLevel) {
                        dd$parent <- dd$membersFiltered$member[e]
                        dd$ancestors <- c(dd$ancestors,dd$parent)
                        dd$level <- dd$level + 1
                        dd$reactive$levelChange <- dd$reactive$levelChange + 1
                        printDebug(env, dim, eventIn = 'highchartsPbClick', eventOut = 'levelChange', info = 'drill')
                    }

                } else {

                    l <- data.frame(
                        level = lvl,
                        parent = parent,
                        label = dd$membersFiltered$member[e],
                        stringsAsFactors = FALSE)

                    s <- dd$selected

                    if (identical(s,l)) {
                        dd$selected <- dd$rootSelected
                    } else {
                        dd$selected <- l
                    }

                    dd$rowLastAccessed$value[dd$rowLastAccessed$level == lvl] <- dd$membersFiltered$member[e]

                    dimCorrectSelectionInfo(input,env,dim)
                    dimSetHasSubselect(env,dim)
                    dd$selectSource <- 'highchartsPbClick'
                    dd$reactive$selectChange <- dd$reactive$selectChange + 1
                    printDebug(env, dim, eventIn = 'highchartsPbClick', eventOut = 'selectChange', info = 'select')
                }
            }
        })

        obs <- c(obs,highchartsPbClickEvent)
    }

    highchartsReadyEvent <- paste0(gdim,'HighchartReady')

    if (!(highchartsReadyEvent %in% obs)) {

        observeEvent(input[[highchartsReadyEvent]], {
            printDebug(env, dim, eventIn = 'highchartsReady')
            if (dd$state == 'enabled' && !dd$visible) {
                shinyjs::js$showDim(dim = dim)
                dd$visible <- TRUE
            }
        })

        obs <- c(obs,highchartsReadyEvent)
    }

    dd$observers <- obs
}

processHighCharts <- function(env,dim,pres){

    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    
    chart <- env$hcPrep[[dim]]
    
    if (is.null(chart))
        chart <- prepHc(env,dim,pres)

    presList <- dd$presList
    useUpdate <- presList[[pres]]$highChartsOpts$dashboard$useUpdate

    #check no of series
    
    prevSeriesCount <- length(env$hcPrev[[dim]]$seriesOpts)
    newSeriesCount <- length(chart$seriesOpts)
    
    if (newSeriesCount > 0 && prevSeriesCount == newSeriesCount) {
        # check length series
        
        prevLength <- c()
        newLength <- c()
        
        for (serie in 1:length(chart$seriesOpts)) {
            prevLength <- c(prevLength,length(env$hcPrev[[dim]]$seriesOpts[[serie]]$data))
            newLength <- c(newLength,length(chart$seriesOpts[[serie]]$data))
        }
        
    } else {
        useUpdate <- FALSE
    }

    if((is.null(useUpdate) || useUpdate) && length(setdiff(newLength,prevLength)) == 0) {

        change <- FALSE
        chart <- removeCallbacks(chart)

        l1 <- list()
        l2 <- list()

        for (serie in 1:length(chart$seriesOpts)) {
            l1[[serie]] <- env$hcPrev[[dim]]$seriesOpts[[serie]]$data
            l2[[serie]] <- chart$seriesOpts[[serie]]$data
        }

        if(!identical(l1,l2)) {
            printDebug(env, dim, eventIn = 'updateHighcharts', info = 'seriesData')

            shinyjs::js$updateSeriesData(
                dim = gdim,
                seriesData = l2,
                redraw = FALSE)
            
            change <- TRUE
        }

        l1 <- env$hcPrev[[dim]]$seriesOpts
        l2 <- chart$seriesOpts

        for (serie in 1:length(chart$seriesOpts)) {
            l1[[serie]]$data <- NULL
            l2[[serie]]$data <- NULL
        }

        if(!identical(l1,l2)) {
            printDebug(env, dim, eventIn = 'updateHighcharts', info = 'seriesOpts')

            shinyjs::js$updateSeriesOpts(
                dim = gdim,
                seriesOpts = lapply(seq_along(l1),function(x) listDiff(l1[[x]],l2[[x]])),
                redraw = FALSE)
            
            change <- TRUE
        }

        l1 <- env$hcPrev[[dim]]$yAxisOpts
        l2 <- chart$yAxisOpts

        if(!identical(l1,l2)) {

            printDebug(env, dim, eventIn = 'updateHighcharts', info = 'yAxisOpts')

            if (class(names(l2)) == 'character') {
                l1 <- list(l1)
                l2 <- list(l2)
            }

            for (axis in seq_along(l2)) {
                
                shinyjs::js$updateYAxis(
                    dim = gdim,
                    axis = axis - 1,
                    yAxisOpts = listDiff(l1[[axis]],l2[[axis]]),
                    redraw = FALSE)
            }

            change <- TRUE
        }

        if(!identical(env$hcPrev[[dim]]$xAxisOpts,chart$xAxisOpts)) {

            printDebug(env, dim, eventIn = 'updateHighcharts', info = 'xAxisOpts')

            shinyjs::js$updateXAxis(
                dim = gdim,
                axis = 0,
                xAxisOpts = listDiff(env$hcPrev[[dim]]$xAxisOpts,chart$xAxisOpts),
                redraw = FALSE)
            
            change <- TRUE
        }

        if(!identical(env$hcPrev[[dim]]$titleOpts,chart$titleOpts)) {
            printDebug(env, dim, eventIn = 'updateHighcharts', info = 'titleOpts')
            shinyjs::js$updateTitle(
                dim = gdim,
                titleOpts = listDiff(env$hcPrev[[dim]]$titleOpts,chart$titleOpts),
                redraw = FALSE)
            change <- TRUE
        }


        if (change) {
            shinyjs::js$redraw(dim = gdim,animate = FALSE)
        }

        if (length(chart$plotBands) > 0) {

            printDebug(env, dim, eventIn = 'updateHighcharts', info = 'xPlotBands')
            shinyjs::js$updateXPlotBands(
                dim = gdim,
                plotBands = chart$plotBands,
                redraw = FALSE)
        }

        env$hcPrev[[dim]] <- chart
        env$hcPrep[[dim]] <- NULL

    } else {

        # trigger render

        env$hcRenderers[[dim]]$count <- env$hcRenderers[[dim]]$count + 1
    }
}


