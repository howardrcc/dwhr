
colorFromMiddle <- function (data, color1,color2) {
    max_val=max(abs(data))
    # (90deg,  transparent 15%, red 15%, red 50%, transparent 50%

    txt1 <-  sprintf("'linear-gradient(90deg, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%, %s 50%%, transparent 50%%)'",
                     max_val,
                     color1,
                     max_val,
                     color1)
    #      transparent 50%, green 50%, green 60%, transparent 60%
    txt2 <-  sprintf("'linear-gradient(90deg, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
                     color2,
                     color2,
                     max_val,
                     max_val)

    DT::JS(paste0("isNaN(parseFloat(value)) || value < 0 ? ", txt1, ' : ', txt2))

}

colorAsImageInterval <- function (cuts, values) {
    n = length(cuts)
    if (n != length(values) - 1)
        stop("length(cuts) must be equal to length(values) - 1")
    values = sprintf("'%s'", values)
    if (n == 0)
        return(values)
    if (!all(cuts == sort(cuts)))
        stop("'cuts' must be sorted increasingly")
    js = ""
    for (i in seq_len(n)) {
        js = paste0(js, sprintf("value <= %s ? %s : ", cuts[i],
                                paste0('linear-gradient(0deg,',values[i],',',values[i],')')))
    }
    DT::JS(paste0(js, "linear-gradient('0deg',",values[n + 1],",",values[n + 1],")"))
}

initCompleteJS <- function(env,dim, row = NULL, pageLength) {
    gdim <- env$dims[[dim]]$gdim
    
    page <- (row - 1) %/% pageLength
    txt <- paste0("function(settings, json) {
    var api = this.api(); api.page(",page,").draw('page');
    var number = Math.random();
    var id = this[0].id
    Shiny.onInputChange('",gdim,"_dt_ready',{r: number, id: id} );
    }")
    DT::JS(txt)
}

callbackJS <- function(env,dim) {
    gdim <- env$dims[[dim]]$gdim
    
    txt <- paste0("table.on('order.dt', function() {
    var order = table.order();
    var data1 = [order[0]];
    var title = table.column( data1[0] ).header();
    var name = $(title).html();
    var number = Math.random();
    Shiny.onInputChange('",gdim,"_ordering',{r: number, name: name, data: data1} );
});
    table.on('page.dt', function() {
    var info = table.page.info();
    var data2 = info.page;
    var number = Math.random();
    Shiny.onInputChange('",gdim,"_paging',{r: number, data: data2} );
});
    table.on('draw.dt', function() {
    var number = Math.random();
    Shiny.onInputChange('",gdim,"_dt_draw',{r: number} );
});
    table.on('length.dt', function(e,setting,len) {
    var number = Math.random();
    Shiny.onInputChange('",gdim,"_page_length',{r: number, data: len} );
    });
")
    DT::JS(txt)
}

renderJS <- function(i) {

    txt <- paste0("function(data, type, row, meta) {
    if (type === 'display') {
        txt = '<div style=\"display: inline-block\" data-toggle=\\\"tooltip\\\" data-container=\\\"body\\\" data-placement=\\\"right\\\" title=\\\"' + row[",i,"] + '\\\">' + data + '</div>';
    } else {
        txt = data;
    }
    return txt;
}
")
    DT::JS(txt)
}

#'
#' @export
#' 
addFormatting <- function(env,dim,df,measures,isFooter = FALSE) {

    measList <- getMeasList(env,dim)  # list of measures for this level
    meas <- measList[(measList$viewColumn %in% measures$viewColumn),]

    if (!is.null(nrow(df))) {

        for (fc in meas$as) {

            formatRef <- meas$formatRef[meas$as == fc]
            vc <- meas$viewColumn[meas$as == fc]

            if (is.numeric(df[[vc]])) {

                if(!is.na(formatRef)) {
                    
                    for (rw in row.names(df)) {

                        if ('format' %in% names(measures) && !is.na(measures$format[measures$viewColumn == vc])) {  # overrule format
                            format <- measures$format[measures$viewColumn == vc]
                        } else {
                            format <- df[rw,formatRef]
                        }
                        
                        if (format == '')
                            format <- 'standard'

                        value <- as.numeric(df[rw,vc])

                        df[rw,paste0(vc,'_fc')] <- switch(
                            format,
                            hidden = '',
                            paperclip = ifelse(isFooter,'',as.character(shiny::icon('paperclip', lib = 'glyphicon'))),
                            euro = paste0('\U20AC ', formatC(digits = 0, format = 'f', value, big.mark='.',decimal.mark = ',')),
                            euro2 = paste0('\U20AC ', formatC(digits = 2, format = 'f', value, big.mark='.',decimal.mark = ',')),
                            perc = paste0(formatC(digits = 0, format = 'f', value * 100, big.mark='.',decimal.mark = ','),' %'),
                            perc1 = paste0(formatC(digits = 1, format = 'f', value * 100, big.mark='.',decimal.mark = ','),' %'),
                            perc2 = paste0(formatC(digits = 2, format = 'f', value * 100, big.mark='.',decimal.mark = ','),' %'),
                            integer = formatC(digits = 0, format = 'f', value, big.mark='.',decimal.mark = ','),
                            decimal1 = formatC(digits = 1, format = 'f', value, big.mark='.',decimal.mark = ','),
                            decimal2 = formatC(digits = 2, format = 'f', value, big.mark='.',decimal.mark = ','),
                            decimal3 = formatC(digits = 3, format = 'f', value, big.mark='.',decimal.mark = ','),
                            standard = as.character(value))
                    }

                } else {

                    if ('format' %in% names(measures) && !is.na(measures$format[measures$viewColumn == vc])) {  # overrule format
                        format <- measures$format[measures$viewColumn == vc]
                    } else {
                        format <- meas$format[meas$viewColumn == vc]
                    }

                    if (format == '')
                        format <- 'standard'

                    df[,paste0(vc,'_fc')] <- switch(
                        format,
                        hidden = '',
                        paperclip = ifelse(isFooter,'',as.character(shiny::icon('paperclip', lib = 'glyphicon'))),
                        euro = paste0('\U20AC ', formatC(digits = 0, format = 'f', df[[vc]], big.mark='.',decimal.mark = ',')),
                        euro2 = paste0('\U20AC ', formatC(digits = 2, format = 'f', df[[vc]], big.mark='.',decimal.mark = ',')),
                        perc = paste0(formatC(digits = 0, format = 'f', df[[vc]] * 100, big.mark='.',decimal.mark = ','),' %'),
                        perc1 = paste0(formatC(digits = 1, format = 'f', df[[vc]] * 100, big.mark='.',decimal.mark = ','),' %'),
                        perc2 = paste0(formatC(digits = 2, format = 'f', df[[vc]] * 100, big.mark='.',decimal.mark = ','),' %'),
                        integer = formatC(digits = 0, format = 'f', df[[vc]], big.mark='.',decimal.mark = ','),
                        decimal1 = formatC(digits = 1, format = 'f', df[[vc]], big.mark='.',decimal.mark = ','),
                        decimal2 = formatC(digits = 2, format = 'f', df[[vc]], big.mark='.',decimal.mark = ','),
                        decimal3 = formatC(digits = 3, format = 'f', df[[vc]], big.mark='.',decimal.mark = ','),
                        standard = as.character(df[[vc]]))
                }

            }
        }
    }

    return (df)

}


getSelectedItems <- function(env,dim){

    dd <- env$dims[[dim]]
    level <- dd$level
    parent <- dd$parent
    member <- dd$membersFiltered$member
    
    s <- dd$selected

    s <- s[(s$level == level & s$parent == parent),]

    m <- NA

    if (nrow(s) > 0) {
        rows <- match(s$label,member)
        m <- matrix(c(rows,rep(1,length(rows))),nrow = length(rows),ncol = 2)
    }
    m
}

makeDtWidget <- function(env,dim,prep) {
    
    dd <- env$dims[[dim]]
    print <- prep$print
    
    height <- NULL
    
    if (print) {
        height <- (nrow(prep$tab) + 1) * 14 
        cursor <- 'default'
        color <- 'black'
    } else {
        cursor <- 'pointer'
        color <- 'blue'
    }
    
    dt <- DT::datatable(
        prep$tab,
        container = prep$container,
        options = prep$options,
        extensions = "RowGroup",
        rownames = FALSE,
        escape = FALSE,
        class = 'compact stripe hover row-border',
        selection = prep$selection,
        callback = callbackJS(env,dim),
        height = height
    ) %>%
        DT::formatStyle(
            columns = 1,
            valueColumns = 'subsel',
            backgroundColor = DT::styleEqual(c(1,0),c('lightGrey','white')),
            cursor = cursor,
            color = color,
            fontWeight = 'bold')
    
    if (dd$type != 'output' && dd$selectMode != 'none' && dd$level %in% dd$selectableLevels) {
        dt <- dt %>%
            DT::formatStyle(
                columns = 2,
                cursor = cursor,
                color = color)
    }
    
    meas <- prep$meas
    
    if (print) {
        meas <- meas[meas$print,]    
    }
    
    if ('colorBarColor1' %in% names(meas)) {
        
        for (fc in meas$as[!is.na(meas$colorBarColor1) & meas$visible]) {
            
            color1 <- meas$colorBarColor1[meas$as == fc]
            color2 <- meas$colorBarColor2[meas$as == fc]
            fontWeight <- isNa(isNull(meas$fontWeight[meas$as == fc],'normal'),'normal')
            cursor <- isNa(isNull(meas$cursor[meas$as == fc],'default'),'default')
            
            if (length(color2) == 0 || is.na(color2)) {
                
                dt <- dt %>%
                    DT::formatStyle(
                        columns = fc,
                        valueColumns = paste0(fc,'_org'),
                        fontWeight = fontWeight,
                        background = DT::styleColorBar(c(range(prep$tab[[paste0(fc,'_org')]]),0),color1),
                        backgroundSize = '90% 70%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'right center',
                        cursor = cursor
                    )
            } else {
                
                dt <- dt %>%
                    DT::formatStyle(
                        columns = fc,
                        valueColumns = paste0(fc,'_org'),
                        fontWeight = fontWeight,
                        background = colorFromMiddle(prep$tab[[paste0(fc,'_org')]],color1,color2),
                        backgroundSize = '90% 70%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'right center',
                        cursor = cursor
                    )
                
            }
            
        }
    }
    
    if ('bgStyle.values' %in% names(meas)) {
        
        for (fc in meas$as[!is.na(meas$bgStyle.values) & meas$visible]) {
            
            cuts <- meas$bgStyle.cuts[meas$as == fc]
            levels <- meas$bgStyle.levels[meas$as == fc]
            values <- eval(parse(text = meas$bgStyle.values[meas$as == fc]))
            fontWeight <- isNa(isNull(meas$fontWeight[meas$as == fc],'normal'),'normal')
            cursor <- isNa(isNull(meas$cursor[meas$as == fc],'default'),'default')
            valueColumn <- meas$bgStyle.valueColumn[meas$as == fc]
            
            if(is.null(valueColumn) || is.na(valueColumn)) {
                valueColumn <- paste0(fc,'_org')
            } else {
                if (paste0(meas$as[meas$viewColumn == valueColumn],'_org') %in% names(prep$tab))
                    valueColumn <- paste0(meas$as[meas$viewColumn == valueColumn],'_org')
                else 
                    valueColumn <- meas$as[meas$viewColumn == valueColumn]
            }
            
            if (length(cuts) > 0) {
                
                cuts <- eval(parse(text = cuts))
                dt <- dt %>%
                    DT::formatStyle(
                        columns = fc,
                        valueColumns = valueColumn,
                        fontWeight = fontWeight,
                        background = DT::styleInterval(cuts,values),
                        #   backgroundSize = '90% 70%',
                        cursor = cursor
                    )
            } else {
                
                levels <- eval(parse(text = levels))
                
                dt <- dt %>%
                    DT::formatStyle(
                        columns = fc,
                        valueColumns = paste0(fc,'_org'),
                        fontWeight = fontWeight,
                        background = DT::styleEqual(levels,values),
                        cursor = cursor
                    )
            }
        }
    }
    
    if ('fgStyle.values' %in% names(meas)) {
        
        for (fc in meas$as[!is.na(meas$fgStyle.values) & meas$visible]) {
            
            cuts <- meas$fgStyle.cuts[meas$as == fc]
            levels <- meas$fgStyle.levels[meas$as == fc]
            values <- eval(parse(text = meas$fgStyle.values[meas$as == fc]))
            fontWeight <- isNa(isNull(meas$fontWeight[meas$as == fc],'normal'),'normal')
            cursor <- isNa(isNull(meas$cursor[meas$as == fc],'default'),'default')
            valueColumn <- meas$fgStyle.valueColumn[meas$as == fc]
            
            if(is.null(valueColumn) || is.na(valueColumn)) {
                valueColumn <- paste0(fc,'_org')
            } else {
                if (paste0(meas$as[meas$viewColumn == valueColumn],'_org') %in% names(prep$tab))
                    valueColumn <- paste0(meas$as[meas$viewColumn == valueColumn],'_org')
                else 
                    valueColumn <- meas$as[meas$viewColumn == valueColumn]
            }
            
            if (length(cuts) > 0) {
                
                cuts <- eval(parse(text = cuts))
                
                dt <- dt %>%
                    DT::formatStyle(
                        columns = fc,
                        valueColumns = valueColumn,
                        fontWeight = fontWeight,
                        color = DT::styleInterval(cuts,values),
                        cursor = cursor
                    )
            } else {
                
                levels <- eval(parse(text = levels))
                
                dt <- dt %>%
                    DT::formatStyle(
                        columns = fc,
                        valueColumns = paste0(fc,'_org'),
                        fontWeight = fontWeight,
                        color = DT::styleEqual(levels,values),
                        cursor = cursor
                    )
            }
        }
    }
    
    dt
}

#'
#' @export
#'
prepDt <- function(env,dim,pres,print = NULL,altData = NULL) {

    dd <- env$dims[[dim]]
    
    print <- isNull(print,isNull(dd$print,FALSE))
    presList <- dd$presList
    opts <- presList[[pres]]$dataTableOpts

    measures <- rlist::list.stack(isNull(opts$measures,list(viewColumn = 'cnt')),fill = TRUE)

    pageLength <- opts$pageLength
    pageLengthList <- opts$pageLengthList

    lvl <- dd$level
    search <- dd$searchTxt
    orderColumn <- dd$orderColumn
    orderColumn2 <- dd$orderColumn2
    orderViewColumn <- dd$orderViewColumn
    orderViewColumn2 <- dd$orderViewColumn2
    orderColumnDir <- dd$orderColumnDir

    measList <- getMeasList(env,dim)
  
    if ('sort' %in% measList$category) {
        orderable <- FALSE
    } else {
        orderable <- TRUE
    }

    meas <- rbind(
        merge(measList, measures,by.x = 'viewColumn', by.y = 'viewColumn'),
        merge(measList[measList$category %in% c('group','tooltip','sort'),], measures,by.x = 'viewColumn', by.y = 'viewColumn', all.x = TRUE))
    
    meas$visible[is.na(meas$visible)] <- FALSE
    meas$print[is.na(meas$print)] <- FALSE

    measures <- measures[measures$viewColumn %in% measList$viewColumn &
                         measures$visible &
                        (measures$print | !print),]

    tab <- data.frame(
        zoom = '+',
        addFormatting(env,dim,isNull(altData$body,dd$membersFiltered),measures,FALSE),
        stringsAsFactors = FALSE)
    
    
    #
    # set firstrow & page
    #

    firstRow <- getFirstRow(env,dim,tab)
    dd$currentPage <- ((firstRow - 1) %/% pageLength) + 1

    #
    # rearange columns
    #

    formattedColumns <- grep('*_fc',names(tab),value = TRUE)
    textColumns <- meas$viewColumn[meas$category == 'text' & (meas$print | !print) & meas$visible]
        
    if (length(textColumns) > 0) {
        for (tc in textColumns) {
            tab[[tc]] <- as.character(tab[[tc]])
        }
    }
    
    measColNames <- c()
    measViewColNames <- c()

    vcs <- c()

    for (cn in meas$as[order(meas$colOrder)]) {

        vc <- meas$viewColumn[meas$as == cn]

        if(paste0(vc,'_fc') %in% names(tab)) {
            vcs <- c(vcs,vc)
            measColNames <- c(measColNames,c(paste0(cn,'_org'),cn))
            measViewColNames <- c(measViewColNames,vc,paste0(vc,'_fc'))
        } else {
            measColNames <- c(measColNames,cn)
            measViewColNames <- c(measViewColNames,vc)
        }

    }
    
    tab <- tab[,c('zoom','member','memberKey',measViewColNames)]
    visCols <- c(0,1,which(names(tab) %in% union(formattedColumns,textColumns)) - 1)

    orgMeasColNrs <- which(names(tab) %in% vcs) - 1
    formattedColNrs <- which(names(tab) %in% formattedColumns) - 1

    #
    # fix zoom
    #

    if(lvl == length(dd$levelNames) - 1) {
        tab$zoom = ''
    }

    tab$zoom[tab$cnt == 0] <- ''
    
    # drie na diepste nivo checken of gelijk aan diepste nivo.
    # dit om hierarchie met variabele diepte te maken
    
    parent <- dd$parent
    if (lvl > 0)
        z <- data.table(dd$data)[eval(expr = parse(text = paste0('level', lvl - 1, 'Label == parent'))),]
    else
        z <- data.table(dd$data)
    
    if(lvl == length(dd$levelNames) - 4) {
        colA <- paste0('level',lvl,'Label')
        colB <- paste0('level',lvl + 1,'Label')
        colC <- paste0('level',lvl + 2,'Label')
        colD <- paste0('level',lvl + 3,'Label')
        
        # selecteer kolommen met gelijke inhoud
        eq1 <- z[[colA]][z[[colA]] == z[[colB]] & z[[colA]] == z[[colC]] & z[[colA]] == z[[colD]]]
        cnts <- z[,eval(parse(text=paste0("length(unique(",colD,"))"))), by = colA]
        # diepste nivo moet evenveel records hebben als parent
        eq2 <- cnts[[colA]][cnts[[colA]] %in% eq1 & cnts$V1 == 1]
        
        tab$zoom[tab$member %in% eq2] <- '' 
        
    }
    
    # twee na diepste nivo checken of gelijk aan diepste nivo.
    
    if(lvl == length(dd$levelNames) - 3) {
        colA <- paste0('level',lvl,'Label')
        colB <- paste0('level',lvl + 1,'Label')
        colC <- paste0('level',lvl + 2,'Label')
        
        # selecteer kolommen met gelijke inhoud
        eq1 <- z[[colA]][z[[colA]] == z[[colB]] & z[[colA]] == z[[colC]]]
        cnts <- z[,eval(parse(text=paste0("length(unique(",colC,"))"))), by = colA]
        # diepste nivo moet evenveel records hebben als parent
        eq2 <- cnts[[colA]][cnts[[colA]] %in% eq1 & cnts$V1 == 1]
        
        tab$zoom[tab$member %in% eq2] <- '' 
        
    }
    
    # een na diepste nivo checken of gelijk aan diepste nivo.
    
    if(lvl == length(dd$levelNames) - 2) {
        colA <- paste0('level',lvl,'Label')
        colB <- paste0('level',lvl + 1,'Label')
        
        # selecteer kolommen met gelijke inhoud
        eq1 <- z[[colA]][z[[colA]] == z[[colB]]]
        cnts <- z[,eval(parse(text=paste0("length(unique(",colB,"))"))), by = colA]
        # diepste nivo moet evenveel records hebben als parent
        eq2 <- cnts[[colA]][cnts[[colA]] %in% eq1 & cnts$V1 == 1]
        tab$zoom[tab$member %in% eq2] <- '' 
        
    }

    levelName = 'Top'
    if(lvl > 0) {
        levelName <- dd$levelNames[lvl + 1]
    }

    #
    # add subsel
    #

    ss <- dd$hasSubselect
    
    ssLabels <- ss$label[ss$level == lvl]
    
    if (lvl > 0)
        ssParents <- ss$label[ss$level == lvl - 1]
    else
        ssParents <- parent
    
    tab$subSel <- as.numeric(tab$member %in% ssLabels & parent %in% ssParents)

    #
    # set selected items
    #

    si <- getSelectedItems(env,dim)

    selection <- list()

    if(dd$selectMode == 'none') {
        selection = list(mode = 'none')
    } else {
        
        if (!is.na(si) && nrow(si) > 1) {
            selection = list(mode = 'multiple', target = 'cell', selected = si)
        } else {
            if(dd$selectMode == 'single') {
                selection = list(mode = 'single', target = 'cell', selected = si)
            } else {
                if (dd$msState) {
                    selection = list(mode = 'multiple', target = 'cell', selected = si)
                } else {
                    selection = list(mode = 'single', target = 'cell', selected = si)
                }
            }
        }
    }

    #
    # other options
    #


    pagingType <- 'simple_numbers'
    paging <- TRUE
    searching <- TRUE

    if(nrow(tab) <= min(pageLengthList)) {
        if (search != '') {
            dom <- 'frt'
        } else {
            dom <- 'rt'
        }
    } else {

        if (length(pageLengthList) == 1) {
            dom <- 'frtp'
        } else {
            dom <- 'flrtp'
        }

    }
    
    #
    # what is hidden?
    #

    hideCols <- setdiff(0:(length(names(tab))-1),visCols)

    if (print) {
        hideCols <- c(0,hideCols)
    }

    if (!lvl %in% dd$selectableLevels) {
        selection = list(mode = 'none')
    }

    #
    # footer stuff
    #

    footer <- NA
    
    if (nrow(tab) > 1 && lvl %in% dd$footerLevels) {
        
        footer <- addFormatting(env,dim,isNull(altData$footer,dd$footer),measures,TRUE)
        
        footer$memberKey <- ''
        footer <- footer[,c(names(footer)[1],'memberKey',measViewColNames)]
        
        if (length(textColumns) > 0) {
            footer[,textColumns] <- ''
        }
        
        if (length(footer) > 1) {
            names(footer) <- c('Totaal',as.character(footer[1,c(2:length(footer))]))
            ft <- DT::tableFooter(c(' ',names(footer),' '))
        } else {
            ft <- ''
        }
    } else {
        ft <- ''
        orderable <- FALSE
    }
    
    if (print) {
        paging <- FALSE
        dom <- 'rt'
        orderable <- FALSE
        selection = list(mode = 'none')
    }

    # set presented column names

    names(tab) = c('  ',dd$itemName,'memberKey',measColNames,'subsel')
    container = htmltools::withTags(table(DT::tableHeader(tab),ft))

    visCols <- setdiff(0:(length(names(tab))-1),hideCols)

    if (!(orderViewColumn %in% c(measures$viewColumn,textColumns,'member','memberKey')) || !orderable) {
        notOrderable <- visCols
    } else {
        notOrderable <- c(0)
    }

    leftAlign <- which(names(tab) %in% union(dd$itemName,meas$as[meas$align == 'left'])) - 1
    centerAlign <- which(names(tab) %in% meas$as[meas$align == 'center']) - 1
    rightAlign <- setdiff(setdiff(union(which(names(tab) %in% meas$as[meas$align == 'right']) - 1,formattedColNrs),centerAlign),leftAlign)

    columnDefs <-  list(
        list(visible=FALSE, targets=hideCols),
        list(searchable=FALSE, targets=hideCols),
        list(orderable=FALSE, targets=notOrderable),
        list(width = '6px', targets=c(0)),
        list(targets=rightAlign, class="dt-right"),
        list(targets=centerAlign, class="dt-center"),
        list(targets=leftAlign, class="dt-left"))

    # width columns

    if ('width' %in% names(meas)) {
        for (fc in meas$as[!is.na(meas$width)]) {
            width <- paste0(meas$width[meas$as == fc],'px')
            wTarget <- which(names(tab) == fc) - 1
            columnDefs[[length(columnDefs) + 1]] <- list(width = width,targets = wTarget)
        }
    }


    orderOpt <- NULL

    if(orderable) {
        if (dd$orderBy == 'name') {
            defaultOrderCol <- 1
        } else {
            defaultOrderCol <- 2
        }

        if (!is.null(dd$orderColumn2))
            c2 <- c(which(names(tab) %in% dd$orderColumn2) - 1, defaultOrderCol)
        else
            c2 <- c(defaultOrderCol)

        for (i in 1:length(formattedColNrs)) {
            columnDefs[[length(columnDefs) + 1]] <- list(targets=formattedColNrs[i], orderData=c(orgMeasColNrs[i],c2))
        }

        if (dd$orderBy == 'key')
            columnDefs[[length(columnDefs) + 1]] <- list(targets=1, orderData=c(2))

        orderOpt <- list(which(names(tab) %in% dd$orderColumn) - 1, dd$orderColumnDir)
    }

    columnDefs[[length(columnDefs) + 1]] <- list(type = 'string', targets = 1)

    if ('member_tooltip' %in% names(tab)) {
        ttcnr <- which(names(tab) %in% 'member_tooltip') - 1
        columnDefs[[length(columnDefs) + 1]] <- list(targets = 1, render = renderJS(ttcnr))
    }
    
    if ('tooltip' %in% names(meas)) {
        tt <- meas[!is.na(meas$tooltip),][,c('viewColumn','as','tooltip')]
        
        if (nrow(tt) > 0) {
            for (i in 1:nrow(tt)) {
                sourceCol <- meas$as[meas$viewColumn == tt$tooltip[i]]
                sourceColnr <- which(names(tab) %in% sourceCol) - 1
                targetColnr <- which(names(tab) %in% tt$as[i]) - 1
                columnDefs[[length(columnDefs) + 1]] <- list(targets = targetColnr, render = renderJS(sourceColnr))
            }
        }
    }
    
    rowGroup <- FALSE
    
    if ('rowGroupColumn' %in% names(tab)) {
        if (any(tab[['Naam']] != tab[['rowGroupColumn']])) {
            rgcnr <- which(names(tab) %in% 'rowGroupColumn') - 1
            rowGroup <- list(dataSrc = rgcnr)
        }
    }
    
    options <- list( dom = dom
                     , lengthChange = TRUE
                     , searching = searching
                     , search = list(regex = FALSE, caseInsensitive = TRUE, search = search)
                     , searchDelay = 800
                     , paging = paging
                     , rowGroup = rowGroup
                     , pagingType = pagingType
                     , lengthMenu = pageLengthList
                     , info = FALSE
                     , pageLength = pageLength
                     , columnDefs = columnDefs
                     , displayStart = firstRow - 1
                     , language = list( paginate = list('first' = '|<', 'next' = '>','previous' = '<', 'last' = '>|')
                                        , search = 'Zoeken'
                                        , lengthMenu = '_MENU_ regels')
                     , initComplete = initCompleteJS(env,dim,firstRow,pageLength))

    if (!is.null(orderOpt)) {
        options$order = list(orderOpt)
    }

    if (any(c('colorBarColor1','fgStyle.values','bgStyle.values') %in% names(meas))) {
        hasFormatting <- TRUE
    } else {
        hasFormatting <- FALSE
    }

    
    if (!print && dd$type != 'output' && dd$selectMode != 'none' && dd$level %in% dd$selectableLevels) {
        tab[,2] <- paste0('<span class = "underline-on-hover">',tab[,2],'</span>')
    }
    
   
    ret <- list(
        tab = tab,
        options = options,
        container = container,
        selection = selection,
        meas = meas,
        print = print,
        level = dd$level,
        parent = dd$parent,
        visCols = visCols,
        hasFormatting = hasFormatting,
        page = (firstRow - 1) %/% pageLength,
        footer = footer)
    
    ret$widget <- makeDtWidget(env,dim,ret)

    if (!print)
        env$dtPrep[[dim]] <- ret
    
    ret
}

renderDataTableDim <- function(env,dim,input,output) {

    dd <- env$dims[[dim]]
    gdim <- dd$gdim
    
    outputDim <- paste0(gdim,'Dim')
    env$dtRenderers[[dim]] <- reactiveValues(count=0)
    serverSide <- isNull(dd$serverSideTable,FALSE)
    
    output[[outputDim]] <- DT::renderDataTable( {
        
        env$dtRenderers[[dim]]$count
        
        prep <- env$dtPrep[[dim]]
        pres <- dd$pres
        
        if (env$dtRenderers[[dim]]$count == 0)
            return()
        
        printDebug(env = env, dim, eventIn = 'renderDT', info = paste0('rendercount:', env$dtRenderers[[dim]]$count))
        
        if (is.null(prep))
            prep <- prepDt(env,dim,pres)
        
        env$dtPrev[[dim]] <- prep
        env$dtPrep[[dim]] <- NULL
        dd$selectSource <- 'init'
        prep$widget
    }, 
    server = serverSide)
    
    shiny::outputOptions(output,outputDim,suspendWhenHidden = FALSE)
    shiny::outputOptions(output,outputDim,priority = 10)

    #
    # observers
    #
    
    cellClicked = paste0(gdim,'Dim_cell_clicked')

    shiny::observeEvent(input[[cellClicked]], {

        info <- input[[cellClicked]]
        dd <- env$dims[[dim]]
    
        if(length(info) > 0) {

            level <- dd$level
            dd$rowLastAccessed$row[dd$rowLastAccessed$level == level] <- info$row
            dd$selectSource <- ''
            
            if((info$value == '+' && info$col == 0)) { 
                cnt <- dd$membersFiltered$cnt[info$row]
               
                if (cnt == 0) {
                    # geen onderliggende data: trigger refresh
                    dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
                    printDebug(env = env, dim, eventIn = 'dataTableCellClicked', eventOut = 'dimRefresh', info = 'no childs')
                } else {
                    dd$parent <- dd$membersFiltered$member[info$row]
                    dd$ancestors <- c(dd$ancestors,dd$parent)
                    dd$level <- dd$level + 1
                    dd$reactive$levelChange <- dd$reactive$levelChange + 1
                    printDebug(env = env, dim, eventIn = 'dataTableCellClicked', eventOut = 'levelChange')

                }
            }

            if (info$col > 1) {
                
                if(exists(paste0(dim,'PageChangeHook'),envir = env$ce)) {
                    do.call(paste0(dim,'PageChangeHook'),list(env = env),envir = env$ce)
                }
                
                dd$reactive$clickMeasureEvent$clickCount <- dd$reactive$clickMeasureEvent$clickCount + 1
                as <- names(env$dtPrev[[dim]]$tab)[info$col + 1]
                meas <- env$dtPrev[[dim]]$meas
                vc <- meas$viewColumn[meas$as == as]
                memberKey <- env$dtPrev[[dim]]$tab[info$row,3]
                dd$reactive$clickMeasureEvent$clickViewColumn <- vc
                dd$reactive$clickMeasureEvent$clickMemberKey <- memberKey
                dd$reactive$clickMeasureEvent$clickMember<- dd$membersFiltered$member[dd$membersFiltered$memberKey == memberKey]
                dd$reactive$clickMeasureEvent$value <- dd$membersFiltered[dd$membersFiltered$memberKey == memberKey,c(vc)]
            }

            if (dim %in% selectableDims(env) || info$value == '+') {
                dd$rowLastAccessed$value[dd$rowLastAccessed$level == level] <- dd$membersFiltered$member[info$row]
            } else {
                dd$rowLastAccessed$value[dd$rowLastAccessed$level == level] <- ''
            }
            
        }

    }, priority = 10)
    
    shiny::observeEvent(env$dims[[dim]]$reactive$clickMeasureEvent,{
        
        if(exists(paste0(dim,'ClickMeasureHook'),envir = env$ce)) {
            do.call(paste0(dim,'ClickMeasureHook'),list(env = env,event = env$dims[[dim]]$reactive$clickMeasureEvent),envir = env$ce)
        }
        
    })

    cellsSelected = paste0(gdim,'Dim_cells_selected')

    shiny::observeEvent(input[[cellsSelected]], {

        m <- input[[cellsSelected]]
        dd <- env$dims[[dim]]
print('cells_selected')
        if (dd$selectSource == 'init') {
            dd$selectSource <- ''
            return()
        }

        lvlChange <- FALSE
        
        selected <- NULL
        level <- dd$level
        parent <- dd$parent
        maxLevel <- dd$maxLevel

        if (nrow(m) > 0) {

            rows <- m[m[,2] == 1,1]

            if (length(rows) > 0) {

                selected <- dd$membersFiltered$member[rows]
            }

            rows <- m[m[,2] == 0,1]

            if (length(rows) == 1) {
                if (env$dtPrev[[dim]]$tab[rows,1] != '+') {
                    # op lege kolom geklikt trigger een refresh
                    dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
                    printDebug(env = env, dim, eventIn = 'dataTableCellsSelected', eventOut = 'dimRefresh', info = 'empty column')
                }
                return()
            }

            rows <- m[m[,2] >= 2,1]

            if (length(rows) == 1) {

                # op measure kolom geklikt trigger een refresh
                dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
                printDebug(env = env, dim, eventIn = 'dataTableCellsSelected', eventOut = 'dimRefresh', info = 'measure column')

                return()
            }

        } else {

            if (!0 %in% dd$selectableLevels || level == 0 ) {

                dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
                printDebug(env = env, dim, eventIn = 'dataTableCellsSelected', eventOut = 'dimRefresh', info = 'not unselectable')

                return()
            }
        }

        l <- dd$selected

        if(!(dd$msState) && nrow(l) <= 1 && length(selected) > 0) {

            # single select

            l <- data.frame( 
                level = level,
                parent = parent,
                label = selected,
                stringsAsFactors = FALSE)


        } else {

            # multi select

            if(nrow(l) > 0) {
                l <- l[!(l$level == level & l$parent == parent),]
            }

            if(length(selected) > 0) {
                s <- data.frame( 
                    level = level,
                    parent = parent,
                    label = selected,
                    stringsAsFactors = FALSE)

                l <- rbind(l,s)

                pc <- dd$pc

                # unselect childs

                delChildSel <- function(lbl,lvl) {

                    del <- pc$label[pc$level == lvl + 1 & pc$parentLabel %in% lbl]

                    if (length(del) > 0) {
                        delChildSel(del,lvl + 1)
                        l <<- l[!(l$level == lvl + 1 & l$parent %in% lbl),]
                    }

                }

                delChildSel(selected,level)

                # unselect parents

                delParentSel <- function(par,lvl) {

                    del <- pc$parentLabel[pc$level == lvl - 1 & pc$label == par]

                    if (length(del) > 0) {
                        delParentSel(del,lvl - 1)
                        l <<- l[!(l$level == lvl - 1 & l$label == par),]
                    }

                }

                delParentSel(parent,level)
            }
        }

        if (nrow(l) == 0) {
            l <- dd$rootSelected
        }

        l <- l[order(l$level,l$label,method = 'radix'),]
        row.names(l) <- NULL

        s <- dd$selected
        dd$selected <- l
        
        if (dimCorrectSelectionInfo(input,env,dim) | dimSetHasSubselect(env,dim)) {
            printDebug(env = env, dim, eventIn = 'subSelectChange/CorrectInfo', eventOut = 'dimRefresh')
            dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
        }
        
        s$level <- as.numeric(s$level)
        l$level <- as.numeric(l$level)
        
        if (!identical(s,l)) {
            dd$reactive$selectChange <- dd$reactive$selectChange + 1
            printDebug(env = env, dim, eventIn = 'dataTableCellsSelected', eventOut = 'selectChange')
        }
            
    })

    orderEvent <- paste0(gdim,'_ordering')

    shiny::observeEvent(input[[orderEvent]], {

        if(length(input[[orderEvent]]$name[[1]]) == 0 ||
           length(input[[orderEvent]]$data[[1]]) == 0 )
            return()

        name <- as.list(input[[orderEvent]]$name[[1]])[[1]]
        srt <- as.list(input[[orderEvent]]$data[[1]])[[2]]
        dd <- env$dims[[dim]]

        if (dd$orderColumn != name || dd$orderColumnDir != srt) {

            dd$orderColumn <- name
            dd$orderColumnDir <- srt
            env$dtPrev[[dim]]$options$order <- NULL

            ml <- getMeasList(env,dim)

            if (name == dd$itemName) {
                if (dd$orderBy =='key') {
                    vc <- 'memberKey'
                } else {
                    vc <- 'member'
                }
            } else {
                vc <- ml$viewColumn[ml$as == name]
            }

            dd$orderViewColumn <- vc
            dd$reactive$orderChange <- dd$reactive$orderChange + 1
            shinyjs::js$tooltip()
            printDebug(env = env, dim, eventIn = 'dataTableOrderChange', eventOut = 'orderChange', info = paste0('column:',name,',dir:',srt))

        }
    })

    readyEvent <- paste0(gdim,'_dt_ready')

    shiny::observeEvent(input[[readyEvent]], {
        printDebug(env = env, dim, eventIn = 'dataTableReady', info = paste0('dtUiId: ', input[[readyEvent]]$id))

        dd <- env$dims[[dim]]

        if (dd$state == 'enabled' && !dd$visible) {
            shinyjs::js$showDim(dim = gdim)
            dd$visible <- TRUE
        }
        
        if (isNull(dd$serverSideTable,FALSE) && dd$searchTxt != '') {
            shinyjs::js$searchDT(id = input[[readyEvent]]$id, txt = dd$searchTxt)
        }
        
        env$dtUiId[[dim]] <- input[[readyEvent]]$id
        shinyjs::js$tooltip()
    })

    drawEvent <- paste0(gdim,'_dt_draw')

    shiny::observeEvent(input[[drawEvent]], {
        printDebug(env = env, dim, eventIn = 'dataTableDraw')
    })

    pagingEvent <- paste0(gdim,'_paging')

    shiny::observeEvent(input[[pagingEvent]], {
        dd <- env$dims[[dim]]

        if (dd$currentPage != input[[pagingEvent]]$data + 1) {

            dd$currentPage <- input[[pagingEvent]]$data + 1
            dd$reactive$pageChange <- dd$reactive$pageChange + 1
            printDebug(env = env, dim, eventIn = 'dataTablePage', eventOut = 'pageChange', info = paste0('page: ', input[[pagingEvent]]$data))

            shinyjs::js$tooltip()

        }
    })

    pageLengthEvent <- paste0(gdim,'_page_length')

    shiny::observeEvent(input[[pageLengthEvent]], {
        dd <- env$dims[[dim]]

        pl <- isNull(dd$pageLength,0)
        cp <- dd$currentPage

        if (pl != input[[pageLengthEvent]]$data) {

            dd$pageLength <- input[[pageLengthEvent]]$data
            dd$currentPage <- ((((pl * (cp - 1)) + 1) %/% dd$pageLength)) + 1
            pres <- dd$pres
            dd$presList[[pres]]$dataTableOpts$pageLength <- input[[pageLengthEvent]]$data

            dd$reactive$pageLengthChange <- dd$reactive$pageLengthChange + 1
            printDebug(env = env, dim, eventIn = 'dataTablePageLength', eventOut = 'pageLengthChange', info = paste0('pageLength: ', input[[pageLengthEvent]]$data))
        }

    })

    outputDim <- paste0(gdim,'Dim')

    searchEvent <- paste0(outputDim,'_search')

    shiny::observeEvent(input[[searchEvent]], {
        dd <- env$dims[[dim]]
        
        txt <- input[[searchEvent]]
        
        dd$prevSearchTxt <- dd$searchTxt
        dd$searchTxt <- txt
        
        printDebug(env = env, dim, eventIn = 'dataTableSearch', info = 'Search Event')
        
        if (txt == '' && dd$prevSearchTxt != txt) { # filter is leeggemaakt
            
            if (any(dd$selected$level > 0)) {
                
                dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
                printDebug(env = env, dim, eventIn = 'dataTableSearch', eventOut = 'dimRefresh', info = 'Search cleared')
                
            } else {
                
                dd$currentPage <- 1
                
                if(exists(paste0(dim,'PageChangeHook'),envir = env$ce)) {
                    do.call(paste0(dim,'PageChangeHook'),list(env = env),envir = env$ce)
                }
            }
            
        }
        
        shinyjs::js$tooltip()

    })

}

processDataTable <- function(env,dim,pres){
    dd <- env$dims[[dim]]

    presList <- dd$presList
    prep <- env$dtPrep[[dim]]

    if (is.null(prep))
        prep <- prepDt(env,dim,pres)
    
    opt1 <- env$dtPrev[[dim]]$options
    opt2 <- prep$options

    opt1$initComplete <- NULL
    opt2$initComplete <- NULL

    if (!(dd$selectMode == 'multi') &&
        identical(opt1,opt2) &&
        identical(env$dtPrev[[dim]]$meas,prep$meas) &&
        !prep$hasFormatting &&
        !isNull(dd$serverSideTable,FALSE) &&
        !prep$print &&
        FALSE    # updates voorlopig uitgezet 
        ) {

        printDebug(env = env, dim, eventIn = 'DatatableUpdate')

        if (prep$selection$mode == 'single') {
            s <- prep$selection$selected[[1]]
        } else {
            s <- NULL
        }

        if (!is.null(s) && is.na(s))
            s <- NULL

        shinyjs::js$updateDT(
            id = env$dtUiId[[dim]],
            container = as.character(prep$container),
            tab = as.matrix(prep$tab),
            selected = s,
            dim = env$dims[[dim]]$gdim,
            page = prep$page)

        env$dtPrev[[dim]] <- prep
        env$dtPrep[[dim]] <- NULL

    } else {

        # trigger render
        env$dtRenderers[[dim]]$count <- env$dtRenderers[[dim]]$count + 1
    }
}
