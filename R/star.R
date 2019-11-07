
dwhrStop <- function(mes) {
    session <- shiny::getDefaultReactiveDomain()
    if (!is.null(session)) {
        shinyjs::logjs(mes)
    }
    stop(mes)
}

printDebug <- function(env, dim, dumpReactive = NULL, eventIn, eventOut = NULL, info = NULL){

    if (glob.env$debug && (is.null(glob.env$debugDims) || dim %in% glob.env$debugDims))  {

        gdim <- env$dims[[dim]]$gdim
        txt <- paste0(gdim, '|eventIn:', eventIn)

        if (!is.null(eventOut)) {
            txt <- paste0(txt,'|eventOut:', eventOut)
        }

        if (!is.null(info)) {
            txt <- paste0(txt,'|', info)
        }

        if (is.null(dumpReactive)) dumpReactive <- glob.env$debugDumpReactive

        if (dumpReactive) {
            shiny::isolate({
                txt <- paste0(txt,
                              '|reactives:',
                              ' (level:',env$dims[[dim]]$reactive$levelChange,
                              ',select:',env$dims[[dim]]$reactive$selectChange,
                              ',ids:',env$dims[[dim]]$reactive$selectedIdsChange,
                              ',refresh: ',env$dims[[dim]]$reactive$dimRefresh,
                              ',vis: ',env$dims[[dim]]$reactive$visChange,
                              ')')
            })
        }

        print(txt)
    }
}


isAuthenticated <- function(session) {
    ses <- session$userData
    exists('authenticated', envir = ses) && ses$authenticated
}


domains <- list(
    aggregateFun = c('sum','dcount','median','mean','min','max','custom'),
    format = c('standard','integer','euro','euro2','keuro','perc','perc1','perc2','decimal1','decimal2','decimal3'),
    ordering = c('HL','LH','asc','desc'),
    presType = c('dataTable','highCharts','radioButton','selectInput','dateRangeInput','rangeSliderInput'),
    selectMode = c('single','multi','none'),
    rangeOpts = c('label','throttle','debounce'),
    dataTableOpts =  c('measures', 'pageLength', 'pageLengthList','serverSideTable','filterRowGroup'),
    dataTableMeasures = c('colorBarColor1','colorBarColor2','viewColumn','format', 'orderable',
                          'bgStyle','fgStyle','width','fontWeight','align','cursor','visible','print','tooltip','sparkOpts'),
    dataTableStyle = c('cuts','levels','values','valueColumn'),
    dataTableFormats = c('standard','integer','euro','euro2','keuro','perc','perc1','perc2','decimal1','decimal2','decimal3','hidden','paperclip','sparkline'),
    fontWeight = c('bold','normal'),
    highChartsOpts = c('type', 'rangeSelector','chart','tooltip','xAxis', 'yAxis', 'legend', 'series', 'plotOptions', 'title','dashboard','pane','navigator','exporting'),
    simpleOpts = c('inline'),
    navOpts = c('syncNav', 'hideNoFilter', 'hideAll', 'hideBreadCrumb', 'links','selLinks','minBreadCrumbLevel'),
    navOptsLinkTypes = c('actionLink','downloadLink','downloadButton','dropDown','dim','uiOutput'),
    orderBy = c('key','name'),
    cssOverflow = c('hidden','visible','scroll','auto'),
    presListType = c('dropdown','links')
)

domainCheck <- function(x,domain,minLength = 0, maxLength = 100000L) {

    if(!length(x) %in% minLength:maxLength) dwhrStop('Invalid length')
    if(length(x) == 0) return()
    if(domain == 'length') return()
    if(is.null(domains[[domain]])) dwhrStop('Unknown domain')
    if(length(setdiff(x, domains[[domain]])) != 0) {
        dwhrStop(paste0('Invalid ', domain))
    } else {
        return()
    }
}

cacheDim <- function(env,dim,dfl) {

    if (!env$caching) {
        return()
    }

    ss <- NULL
    for (d in sort(filteringDims(env))) {

        ss <- rbind(ss,env$dims[[d]]$selected)
    }

    lvl <- as.character(env$dims[[dim]]$level)
    parent <- env$dims[[dim]]$parent

    if (parent == '') {
        parent <- 'x'
    }

    md5 <- digest::digest(ss,algo = 'md5')

    env$globalCache[[env$id]][[dim]][[parent]][[lvl]][[md5]] <- dfl

}

cacheFind <- function(env, dim) {

    if (!env$caching) {
        return()
    }
    
    gdim <- env$dims[[dim]]$gdim

    ss <- NULL
    for (d in sort(filteringDims(env))) {

        ss <- rbind(ss,env$dims[[d]]$selected)
    }

    lvl <- as.character(env$dims[[dim]]$level)
    parent <- env$dims[[dim]]$parent

    if (parent == '') {
        parent <- 'x'
    }

    md5 <- digest::digest(ss,algo = 'md5')

    dfl <- env$globalCache[[env$id]][[dim]][[parent]][[lvl]][[md5]]

    if(!is.null(dfl)) {
        print(paste0(gdim, ': cacheHit!'))
    }

    dfl

}

#'
#' @export
#' 
getSelectedIds <- function(env, dim, selected = NULL) {

    data <- env$dims[[dim]]$data
    keyColumn <- names(data)[1]

    is.null(selected) || length(setdiff(c('level','parent','label'),names(selected))) == 0 || dwhrStop('Invalid selected parameter')

    s <- isNull(selected,env$dims[[dim]]$selected)
    ids <- NULL

    if (any(s$level == 0)) {
        ids <- data[[keyColumn]]
    } else {

        for (lvl in 1:max(s$level)) {

            colsX <- c(paste0('level',lvl - 1,'Label'),paste0('level',lvl,'Label'))
            colsY <- c('parent','label')
            
            ids <- union(ids,merge(data,s[s$level == lvl,],by.x = colsX, by.y = colsY)[[keyColumn]])

        }}

    sort(ids)

}


appendZeroRow <- function(member,dim,df) {

    l <- as.list(member)
    i <- nrow(df)

    for (n in 2:length(names(df))) {

        if (is.numeric(df[,n])) {
            l[[n]] <- 0
        } else {
            l[[n]] <- ''
        }

    }

    df[i + 1,] <- l

    return(df)

}

#'
#' @export
#' 
getMembers <- function(env, dim, level = NULL, parent = NULL, selected = NULL, altData = NULL) {
    dd <- env$dims[[dim]]

    f <- NULL

    data <- isNull(altData,dd$data)
    keyColumn <- names(data)[1]

    tmp <- NULL

    s <- NA
    footer <- NA
    adhoc <- FALSE

    if (!is.null(level) || !is.null(parent) || !is.null(selected)) {
        adhoc <- TRUE
        gcache <- NULL
    } else {
        gcache <- cacheFind(env, dim)
    }

    if (dim %in% selectableDims(env)) {
        s <- dd$selected
    }

    lvl <- isNull(level,dd$level)
    
    if (is.null(parent)) {
        parent <- dd$parent
        if (lvl > 1) {
            gparent <- rev(dd$ancestors)[2]
        } else {
            gparent <- NULL
        }
    } else {
        gparent <- NULL
    }
    

    maxLvl <- dd$maxLevel

    ignoreDims <- union(dimProxySelect(env),dd$ignoreDims)

    ml <- getMeasList(env,dim)

    meas <- ml[ml$type == 'direct',]
    measCols <- meas$viewColumn[order(meas$sort)]
    sortColumn <- meas$viewColumn[grepl('*_sort',meas$viewColumn)]
    ids <- data[[keyColumn]]

    if (is.null(gcache)) {

        condition <- ''

        for (d in setdiff(setdiff(filteringDims(env),dim),ignoreDims)) {
            dkey <- env$dims[[d]]$keyColumn

            if (any(env$dims[[d]]$selected$level > 0)) {
                if (condition != '') {
                    condition <- paste0(condition,' & ')
                }
                condition <- paste0(condition,'env$facts[[\'',dkey,'\']] %in% env$dims[[\'',d,'\']]$selectedIds')
            }
        }
        
        if (condition == '') {
            condition <- TRUE
        }
        
        tmp <- data.table::data.table(env$facts[eval(parse(text = condition)),])
        
        cnt1 <- nrow(tmp)
        
        if (is.null(gparent)) {
            parentFilter <- paste0('level', lvl - 1, 'Label == parent')
        } else {
            parentFilter <- paste0('level', lvl - 1, 'Label == parent & level', lvl - 2, 'Label == gparent')
        }
        
        if (dd$fixedMembers) {
            if (lvl > 1) {
                tmp <- tmp[data.table::data.table(data)[eval(expr = parse(text = parentFilter))], on = keyColumn]
            } else {
                tmp <- tmp[data.table::data.table(data), on = keyColumn]
            }
        } else {
            if (lvl > 1) {
                tmp <- tmp[data.table::data.table(data)[eval(expr = parse(text = parentFilter))], on = keyColumn, nomatch=0]
            } else {
                tmp <- tmp[data.table::data.table(data), on = keyColumn, nomatch=0,allow.cartesian=TRUE]
            }
        }
        
        cnt2 <- nrow(tmp)

        if(cnt1 != 0 && cnt2 == 0 && !(adhoc)) {

            parent = dd$rootLabel
            lvl = 1

            dd$prevLevel <- dd$level
            dd$level <- lvl
            dd$parent <- parent
            dd$ancestors <- c('',parent)
            dd$reactive$levelChange <- dd$reactive$levelChange + 1

            printDebug(env = env, dim, eventIn = 'getMembers', eventOut = 'levelChange', info = 'parent not found')

            return(getMembers(env, dim))
        }

        if (dd$na.rm) {
            narm <- 'na.rm = TRUE' 
        } else {
            narm <- 'na.rm = FALSE'
        }
        
        measFun <- 'list(cnt = .N'

        for (q in meas$as[order(meas$sort)]) {

            fun <- meas$fun[meas$as== q]
            viewColumn <- meas$viewColumn[meas$as == q]
            factColumn <- meas$factColumn[meas$as == q]

            if(factColumn != '*') {

                measFun <- paste0(measFun,", ", viewColumn, "=")

                if(fun == 'dcount') {
                    measFun <- paste0(measFun,"length(unique(",factColumn,"))")
                }

                if (fun == 'sum') {
                    measFun <- paste0(measFun,"sum(",factColumn,",",narm,")")
                }

                if (fun == 'median') {
                    measFun <- paste0(measFun,"median(",factColumn,",",narm,")")
                }

                if (fun == 'mean') {
                    measFun <- paste0(measFun,"mean(",factColumn,",",narm,")")
                }

                if (fun == 'min') {
                    measFun <- paste0(measFun,"min(",factColumn,",",narm,")")
                }

                if (fun == 'max') {
                    measFun <- paste0(measFun,"max(",factColumn,",",narm,")")
                }
                
                if (!fun %in% domains[['aggregateFun']]) {
                    measFun <- paste0(measFun,"custom('",fun,"',",factColumn,")")
                }
            }

        }

        measFun <- paste0(measFun,')')
        
        custom <- function(fun,col) {
            do.call(fun,list(col),envir = env$ce)
        }
        
        
        if (lvl == 0) {
            
            body <- tmp[,eval(expr = parse(text = measFun)),by = level0Label]
            
            if(lvl %in% dd$footerLevels) {
                footer <- body
            }
            
        } else {
            
            parentFilter <- paste0('level', lvl - 1, 'Label == parent')
            byText <- paste0('level',lvl,'Label')
            
            body <- tmp[eval(expr = parse(text = parentFilter)),
                        eval(expr = parse(text = measFun)),
                        by = eval(expr = parse(text = byText))]
            
            if(lvl %in% dd$footerLevels) {
                byText <- paste0('level',lvl - 1,'Label')
                
                footer <- tmp[eval(expr = parse(text = parentFilter)),
                              eval(expr = parse(text = measFun)),
                              by = eval(expr = parse(text = byText))]
                
            }
        }

        names(body) <- c('member',measCols)
        body$member <- as.character(body$member)

        lookup <- data.table(unique(dd$pc[dd$pc$level == lvl & dd$pc$parentLabel == parent & dd$pc$gparentLabel == isNull(gparent,''),][,c('label','code')]))
        names(lookup) <- c('member','memberKey')
    
        body <- as.data.frame(lookup[body,on = 'member'])
        body$memberKey[is.na(body$memberKey)] <- digest::digest(body$member[is.na(body$memberKey)])
        
        #browser(expr = {dim == 'kpi'})
        
        if(lvl %in% dd$footerLevels) {
            footer <- as.data.frame(footer)
        }
        
        # if (dim %in% selectableDims(env) && !(dd$type == 'output')) {
        #     
        #     diff <- setdiff(s$label[s$level == lvl &
        #                                 s$parent == parent],body$member)
        #     
        #     if (length(diff) > 0) {
        #         for (s in diff) {
        #             body <- appendZeroRow(s,dim,body)
        #         }
        #     }
        #     
        # }
        
        if (nrow(body) == 0) {
            body <- appendZeroRow('Onbekend',dim,body)
        }
        
        if(lvl %in% dd$footerLevels && nrow(footer) == 0) {
            footer <- appendZeroRow('Onbekend',dim,footer)
        }

        meas <- ml[ml$type == 'indirect',]

        for (ordr in sort(meas$processingOrder)) {

            vc <- meas$viewColumn[meas$processingOrder == ordr]
            fun <- meas$fun[meas$processingOrder == ordr]

            e <- new.env(parent = env$ce)
            assign(fun,get(fun,envir = env$ce), envir = e)

            e$columnName <- vc
            e$star <- env
            e$dim <- dim

            environment(e[[fun]]) <- e  # set the parent env for userFunc
                
            if(lvl %in% dd$footerLevels) {
                e$type <- 'footer'
                e$df <- footer
                ftr <- do.call(what = fun, args = list(),  envir = e)
                if (is.vector(ftr) && length(ftr) == 1) {
                    footer[[vc]] <- ftr
                } else {
                    warning(paste0('userFunc: ', fun, ' has invalid footer length'))
                    footer[[vc]] <- 0
                }
                e$footer <- footer
            }
            
            e$type <- 'body'
            e$df <- body
            bdy <- do.call(what = fun, args = list(), envir = e)
            if (is.vector(bdy) && length(bdy) == nrow(body)) {
                body[[vc]] <- bdy
            } else {
                warning(paste0('userFunc: ', fun, ' has invalid body length'))
                body[[vc]] <- 0
            }
        }
        
        if (length(sortColumn) != 0) {
            body <- body[order(body[[sortColumn]],method = 'radix'),]
        } else {
            if (dd$orderBy == 'name') {
                body <- body[order(body[['member']],method = 'radix'),]
            } else {
                body <- body[order(body[['memberKey']],method = 'radix'),]
            }
            
            # check bestaan ordercolumns
            
            if (!dd$orderViewColumn %in% names(body)) {
                dd$orderColumn <- dd$itemName
                if (dd$orderBy == 'name')
                    dd$orderViewColumn <- 'member'
                else
                    dd$orderViewColumn <- 'memberKey'
                dd$orderColumnDir <- 'asc'
            }
        }

        res = list(
            body = body,
            footer = footer)

        cacheDim(env,dim,res)

    } else {
        res <- gcache
    }
 
    res
}


dimStateSelect <- function(env,states = c('enabled','disabled','hidden')) {
    v <- c()

    for(d in names(env$dims)) {
        if(env$dims[[d]]$state %in% states) {
            v <- c(v,d)
        }
    }
    v

}

dimTypeSelect <- function(env,types = c('bidir','input','output')) {
    v <- c()

    for(d in names(env$dims)) {
        if(env$dims[[d]]$type %in% types) {
            v <- c(v,d)
        }
    }
    v
}

dimSelectableSelect <- function(env,sModes = c('single','multi','none')) {
    v <- c()

    for(d in names(env$dims)) {
        if(env$dims[[d]]$selectMode %in% sModes) {
            v <- c(v,d)
        }
    }
    v
}

dimProxySelect <- function(env) {
    unlist(lapply(names(env$dims),function(x)  { if (!is.null(env$dims[[x]]$parentDim)) x else NULL }))
}

visibleDims <- function(env) {
    dimStateSelect(env,c('enabled'))
}

inVisibleDims <- function(env) {
    dimStateSelect(env,c('disabled','hidden'))
}

filteringDims <- function(env) {
    setdiff(intersect(dimTypeSelect(env,c('bidir','input')),dimStateSelect(env,c('enabled','hidden'))),dimProxySelect(env))
}

outputDims <- function(env) {
    intersect(dimTypeSelect(env,c('bidir','output')),dimStateSelect(env,c('enabled')))
}

inputDims <- function(env) {
    intersect(dimTypeSelect(env,c('bidir','input')),dimStateSelect(env,c('enabled')))
}

selectableDims <- function(env) {
    dimSelectableSelect(env,c('single','multi'))
}

isSingleLevel <- function(env,dim) {

    length(env$dims[[dim]]$useLevels) == 2
}

dimSetHasSubselect <- function(env,dim) {

    dd <- env$dims[[dim]]
    
    getAncestors <- function(level,label,parent) {
        ancestors <- c()

        pc <- dd$pc
  
        if (level >= 1) {
            for (i in level:1) {
                par <- pc$parentLabel[pc$level == i & pc$label == label & pc$parentLabel == parent][1]
                gpar <- pc$gparentLabel[pc$level == i & pc$label == label  & pc$parentLabel == parent][1]
                
                label <- par
                parent <- gpar
                ancestors <- c(label,ancestors)
            }
        }
        ancestors
    }
    
    hss <- NULL
    selected <- dd$selected

    for (n in 1:nrow(selected)) {
        
        ancestors <- getAncestors(selected$level[n],selected$label[n],selected$parent[n])

        if (!is.null(ancestors)) {
            hss <- rbind(
                hss,
                data.frame(
                    level = 0:(length(ancestors) - 1),
                    label = ancestors,
                    stringsAsFactors = FALSE))
        }
            
    }
    
    hss <- unique(hss)
    
    a <- dd$hasSubselect
    a <- isNull(a$label[a$level == dd$level],character(0))
    b <- isNull(hss$label[hss$level == dd$level],character(0))

    if (!(length(a) == 0 && length(b) == 0) &&
        dd$parent %in% dd$hasSubselect$label &&
        !identical(a[order(a,method = 'radix')], b[order(b,method = 'radix')])) {
        
        dd$hasSubselect <- hss
        return(TRUE)
        
    } else {
    
        dd$hasSubselect <- hss
        return(FALSE)
    }
}

dimCorrectSelectionInfo <- function(input,env,dim) {

    dd <- env$dims[[dim]]
    
    l <- dd$selected
    gdim <- dd$gdim
    
    if ((nrow(l) > 1 && dd$selectMode == 'single') ||
        (nrow(l) > 1 && dd$selectMode == 'multi' && !(input[[paste0(gdim,'DimMs')]]))) {
        
        rwv <- isNa(dd$rowLastAccessed$value[dd$rowLastAccessed$level == dd$level],'')
        rw <- which(dd$seleced$label == rwv)
        
        if(length(rw) == 1) {
            l <- l[rw,]
        } else {
            l <- l[1,]
        }

        dd$selected <- l
     
        return (TRUE)
    }

    return (FALSE)
}


isColor <- function(x)
{
    res <- try(col2rgb(x),silent=TRUE)
    return(!"try-error"%in%class(res))
}

getColors <- function(dt,pal,trans) {

    colors <- NULL

    if (!is.null(pal)) {

        palInfo <- RColorBrewer::brewer.pal.info
        reverse <- FALSE

        if (length(pal) == 1 && substr(pal,1,1) == '-') {
            pal <- substr(pal,2,1000)
            reverse <- TRUE
        }

        if (length(pal) == 1 && pal %in% rownames(palInfo)) {

            palVec <- suppressWarnings(RColorBrewer::brewer.pal(12,pal))
            palFun <- colorRampPalette(palVec)
            colors <- palFun(length(dt))

            if (reverse)
                colors <- rev(colors)

        } else {
            colors <- rep(unlist(pal),length.out = length(dt))
        }

        # Bij een sequentieel palette de waarde van de meetwaarde gebruiken

        if (length(pal) == 1 && pal %in% rownames(palInfo[palInfo$category == 'seq',])) {

            if(!is.null(trans) && trans == 'log2') {
                dt[dt <= 0] <- NA
                colors <- scales::col_numeric(pal, domain = NULL)(log2(dt))
            } else {
                colors <- scales::col_numeric(pal, domain = NULL)(dt)
            }

            if (reverse)
                colors <- rev(colors)
        }

    }

    return(colors)
}


getFirstRow <- function(env,dim,tab) {
    firstRow <- 1

    dd <- env$dims[[dim]]

    lvl <- dd$level
    orderViewColumn <- dd$orderViewColumn
    orderColumnDir <- dd$orderColumnDir
    orderViewColumn2 <- dd$orderViewColumn2
    meas <- getMeasList(env,dim)
    sortColumn <- meas$viewColumn[grepl('*_sort',meas$viewColumn)]

    nm <- ''
    nm <- dd$rowLastAccessed$value[dd$rowLastAccessed$level == lvl]

    if (nm == '') {
        firstRow <- 1
    } else {
        if (length(sortColumn) > 0) {
            firstRow <- which(tab$member == nm)
        } else {
            if (!is.null(orderViewColumn2)) {
                firstRow <- which(
                    tab[order(
                        tab[,orderViewColumn],
                        tab[,orderViewColumn2],
                        tab$member,
                        method = 'radix',
                        decreasing = (orderColumnDir == 'desc')),]$member == nm)
            } else {
                firstRow <- which(
                    tab[order(
                        tab[,orderViewColumn],
                        tab$member,
                        method = 'radix',
                        decreasing = (orderColumnDir == 'desc')),]$member == nm)
            }
        }
    }

    if(is.null(firstRow) || is.na(firstRow) || length(firstRow) == 0) {
        firstRow <- 1
    }

    return(firstRow[1])

}


getMeasList <- function(env,dim) {
    dd <- env$dims[[dim]]
    lvl <- dd$levelMap$from[dd$levelMap$to == dd$level]
    meas <- dd$measList
    meas[which(bitwAnd(2**lvl,meas$applyToLevels) %in% 2**lvl),]
}


vec2Bit <- function(x) {

    ret <- 0
    for (i in x) {
        ret = ret + 2**i
    }
    return(ret)
}

#'
#' @export
#' 
isNull <- function(x,y) {
    if(is.null(x)) y else x
}

#'
#' @export
#' 
isNa <- function(x,y) {
    if(is.na(x)) y else x
}

getCacheFile <- function(id) {
    paste0(getwd(), '/tmp/', glob.env$dashboardName, '_', id, '_cache.rds')
}

getCache <- function(env) {

    cacheFile <- getCacheFile(env$id)

    if (glob.env$sessionCount == 1) {

        if (is.na(file.info(cacheFile)$mtime)) {

            dwhrStop(paste0('cacheFile: ',cacheFile,' not Found'))

        } else {

            glob.env$globalCache[[env$id]] <- readRDS(cacheFile)
        }
    }

    invisible()
}

getGlobalId <- function(starId,dim) {
    paste0(starId,toupper(substr(dim,1,1)),substr(dim,2,100))   
}

getZoom <- function(env,dim) {
    
    dd <- env$dims[[dim]]
    members <- dd$membersFiltered$member
    if (dd$level == dd$maxLevel) 
        return (' ')
    
}

#'
#' @export
#' 
dwhrMerge <- function(cumDT,incDT,keyCols,noDeletes = TRUE) {

    mutCols <- setdiff(names(cumDT),keyCols)

    if (!'data.table' %in% class(cumDT)) {
        dwhrStop('dwhMerge: cumDT must be of class data.table.')
    } 
    
    if (!'data.table' %in% class(incDT)) {
        dwhrStop('dwhMerge: incDT must be of class data.table.')
    } 
    
    if (!identical(sort(names(cumDT)), sort(names(incDT)))) {
        dwhrStop('dwhMerge: column names differ')
    }
  
    incDT <- copy(incDT[,names(cumDT), with = FALSE])

    if (!identical(sapply(cumDT,class), sapply(incDT,class))) {
        dwhrStop('dwhMerge: types differ')
    }

    # update
    

    eval(parse(text = paste0(
        "cumDT[incDT,(mutCols) := list(",
        paste0("i.",mutCols, collapse = ","),
        "), on = c(",
        paste0("'",keyCols,"'",collapse = ","),
        ")]")))
    
    # insert

    cumDT <- rbindlist(list(cumDT,eval(parse(text = paste0(
        "incDT[!cumDT,on = c(",
        paste0("'",keyCols,"'",collapse = ","),
        ")]")))))
    
}

#'
#' @export
#' 
latexEscape <- function(paragraph) {
    
    # Replace a \ with $\backslash$
    # This is made more complicated because the dollars will be escaped
    # by the subsequent replacement. Easiest to add \backslash
    # now and then add the dollars
    
    paragraph <- gsub('\\\\','\\\\backslash',paragraph)
    #$paragraph =~ s/\\/\\backslash/g;
    
    # Must be done after escape of \ since this command adds latex escapes
    # Replace characters that can be escaped
    
    paragraph <- gsub('([$#&%_{}])',"\\\\\\1",paragraph)
    #$paragraph =~ s/([\$\#&%_{}])/\\$1/g;
    
    # Replace ^ characters with \^{} so that $^F works okay
    paragraph <- gsub('(\\^)','\\\\\\1\\{\\}',paragraph)
    #$paragraph =~ s/(\^)/\\$1\{\}/g;
    
    # Replace tilde (~) with \texttt{\~{}}
    paragraph <- gsub('~','\\\\texttt\\{\\\\~\\{\\}\\}',paragraph)
    #$paragraph =~ s/~/\\texttt\{\\~\{\}\}/g;
    
    # Now add the dollars around each \backslash
    paragraph <- gsub('(\\\\backslash)','$\\1$',paragraph)
    #$paragraph =~ s/(\\backslash)/\$$1\$/g;
    
    # replace > < and |
    
    paragraph <- gsub('<','\\\\textless ',paragraph)
    paragraph <- gsub('>','\\\\textgreater ',paragraph)
    paragraph <- gsub('\\|','\\\\textbar ',paragraph)
    
    # replace \n 
    
    paragraph <- gsub('\\n','\\\\newline ',paragraph)
    paragraph;
}


#'
#' @export
#'
checkVersion = function(pkg_name, min_version) {
    cur_version = packageVersion(pkg_name)
    if(cur_version < min_version) stop(sprintf("Package %s needs a newer version, found %s, need at least %s", pkg_name, cur_version, min_version))
}



#'
#' @export
#'
getReportName <- function(title) {
    
    if (glob.env$omgeving != 'PRD') 
        return(paste0(title,' <font color="red">TEST</font>'))
    
    return(title)
}

makeRangeSelection <- function(env,dim,from,to) {
    
    dd <- env$dims[[dim]]
    root <- dd$rootLabel
    
    dt <- dd$data[['level1Label']]
    dt <- dt[dt >= from & dt <= to]
    if (length(dt) > 0)
        data.frame(level = 1L, parent = root, label = dt, stringsAsFactors = FALSE)
    else 
        NULL
    
}

#'
#' @export
#'
sparkRelativeChange <- function(spark) {
    
    unlist(lapply(paste0('c(',spark,')'),function(q) {
        y <- eval(parse(text = q))
        x <- 1:length(y)
        co <- coef(lm(y~x))
        fn <- function(x,a,b) {(a * x) + b}
        ref <- fn(1,co[2],co[1])
        
        #zz <- sign(co[2]) * abs((fn(1,co[2],co[1]) - fn(length(y),co[2],co[1]))) / max(abs(fn(1,co[2],co[1])),abs(fn(length(y),co[2],co[1])))
         
        zz <- (fn(length(y),co[2],co[1]) - ref) / abs(ref)
        zz[is.nan(zz)] <- 0
        zz
    }))
}