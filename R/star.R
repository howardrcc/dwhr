
dwhrStop <- function(mes) {
    shinyjs::js$dumpToConsole(trace = c(
        mes,
        'Stack trace (innermost first):',
        shiny::formatStackTrace(calls = sys.calls(), offset = TRUE)[-1:-6]))
    stop(mes)
}

printDebug <- function(env, dim, dumpReactive = NULL, eventIn, eventOut = NULL, info = NULL){

    if (pkg.env$debug && (is.null(pkg.env$debugDims) || dim %in% pkg.env$debugDims))  {

        txt <- paste0(dim, '|eventIn:', eventIn)

        if (!is.null(eventOut)) {
            txt <- paste0(txt,'|eventOut:', eventOut)
        }

        if (!is.null(info)) {
            txt <- paste0(txt,'|', info)
        }

        if (is.null(dumpReactive)) dumpReactive <- pkg.env$debugDumpReactive

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
    aggregateFun = c('sum','dcount','median','mean','min','max'),
    format = c('standard','integer','euro','euro2','keuro','perc','perc1','perc2','decimal1','decimal2','decimal3','hidden'),
    ordering = c('HL','LH','asc','desc'),
    presType = c('dataTable','highCharts','radioButton','selectInput'),
    selectMode = c('single','multi','none'),
    dataTableOpts =  c('measures', 'pageLength', 'pageLengthList','serverSideTable'),
    dataTableMeasures = c('colorBarColor1','colorBarColor2','viewColumn','format', 'orderable',
                          'bgStyle','fgStyle','width','fontWeight','align','cursor','visible'),
    dataTableStyle = c('cuts','levels','values','valueColumn'),
    fontWeight = c('bold','normal'),
    highChartsOpts = c('chart','tooltip','xAxis', 'yAxis', 'legend', 'series', 'plotOptions', 'title','dashboard','pane'),
    simpleOpts = c('inline'),
    navOpts = c('syncNav', 'hideNoFilter', 'hideAll', 'hideBreadCrumb', 'links'),
    orderBy = c('key','name')
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
        print(paste0(dim, ': cacheHit!'))
    }

    dfl

}

makeSsHash <- function() {

    for (d in sort(filteringDims(env))) {
        dd <- env$dims[dd]

        for (lvl in 0:dd$maxLevel) {


        }

        ss <- rbind(ss,env$dims[[d]]$selected)
    }



}

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

            colsX <- c()
            colsY <- c()

            if (lvl == 1) {
                colsX <- c('level1Label')
                colsY <- c('label')
            } else {
                colsX <- c(paste0('level',lvl - 1,'Label'),paste0('level',lvl,'Label'))
                colsY <- c('parent','label')
            }

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
getMembers <- function(env, dim, addSummary = FALSE, level = NULL, parent = NULL, selected = NULL) {
    dd <- env$dims[[dim]]

    f <- NULL

    data <- dd$data
    keyColumn <- names(data)[1]

    tmp <- NULL

    s <- NA
    footer <- NA
    summary <- NULL
    adhoc <- FALSE

    if (!is.null(level) || !is.null(parent) || !is.null(selected) || addSummary) {
        adhoc <- TRUE
        gcache <- NULL
    } else {
        gcache <- cacheFind(env, dim)
    }

    if (dim %in% selectableDims(env)) {
        s <- dd$selected
    }

    lvl <- isNull(level,dd$level)
    parent <- isNull(parent,dd$parent)

    maxLvl <- dd$maxLevel

    ignoreDims <- union(env$proxyDims,dd$ignoreDims)

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
        
        if(!addSummary) {
            if (dd$fixedMembers) {
                if (lvl == 3) {
                    tmp <- tmp[data.table::data.table(data)[level2Label == parent], on = keyColumn]
                } else {
                    if (lvl == 2) {
                        tmp <- tmp[data.table::data.table(data)[level1Label == parent], on = keyColumn]
                    } else {
                        tmp <- tmp[data.table::data.table(data), on = keyColumn]
                    }
                }
            } else {
                if (lvl == 3) {
                    tmp <- tmp[data.table::data.table(data)[level2Label == parent], on = keyColumn, nomatch=0]
                } else {
                    if (lvl == 2) {
                        tmp <- tmp[data.table::data.table(data)[level1Label == parent], on = keyColumn, nomatch=0]
                    } else {
                        tmp <- tmp[data.table::data.table(data), on = keyColumn, nomatch=0]
                    }
                }
            }
        } else {
            tmp <- tmp[data.table::data.table(data), on = keyColumn, nomatch=0]
        }

        cnt2 <- nrow(tmp)

        if(cnt1 != 0 && cnt2 == 0 && !(adhoc)) {

            parent = dd$rootLabel
            lvl = 1

            dd$level <- lvl
            dd$parent <- parent
            dd$ancestors <- c('',parent)
            dd$reactive$levelChange <- dd$reactive$levelChange + 1

            printDebug(env = env, dim, eventIn = 'getMembers', eventOut = 'levelChange', info = 'parent not found')

            return(getMembers(env, dim, addSummary))
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
                    if (dd$na.rm)
                        measFun <- paste0(measFun,"sum(",factColumn,", na.rm = TRUE)")
                    else
                        measFun <- paste0(measFun,"sum(",factColumn,")")
                }

                if (fun == 'median') {
                    measFun <- paste0(measFun,"median(",factColumn,")")
                }

                if (fun == 'mean') {
                    measFun <- paste0(measFun,"mean(",factColumn,")")
                }

                if (fun == 'min') {
                    measFun <- paste0(measFun,"min(",factColumn,")")
                }

                if (fun == 'max') {
                    measFun <- paste0(measFun,"max(",factColumn,")")
                }
            }

        }

        measFun <- paste0(measFun,')')

        if(addSummary) {

            colsBy <- c()

            for (q in 0:maxLvl) {
                colsBy <- c(colsBy,paste0('level',q,'Label'))
            }

            summary <- tmp[,eval(expr = parse(text = measFun)),by = colsBy]
            summary <- as.data.frame(summary)
            names(summary) <- c(colsBy,measCols)

        } else {

            if (lvl == 0) {

                body <- tmp[,eval(expr = parse(text = measFun)),by = level0Label]

                if(lvl %in% dd$footerLevels) {
                    footer <- body
                }

            } else {

                if (lvl == 1) {

                    body <- tmp[,eval(expr = parse(text = measFun)),by = level1Label]

                    if(lvl %in% dd$footerLevels) {
                        footer <- tmp[,eval(expr = parse(text = measFun)),by = level0Label]
                    }

                } else {
                    
                    if (lvl == 2) {
                        
                        body <- tmp[level1Label == parent,eval(expr = parse(text = measFun)),by = level2Label]
                        
                        if(lvl %in% dd$footerLevels) {
                            footer <- tmp[level1Label == parent,eval(expr = parse(text = measFun)),by = level1Label]
                        }
                    } else {
                        
                        body <- tmp[level2Label == parent,eval(expr = parse(text = measFun)),by = level3Label]
                        
                        if(lvl %in% dd$footerLevels) {
                            footer <- tmp[level2Label == parent,eval(expr = parse(text = measFun)),by = level2Label]
                        }
                    }
                }
            }
            
            names(body) <- c('member',measCols)
            body$member <- as.character(body$member)
            
            lookup <- dd$pc[dd$pc$level == lvl,][,c('label','code')]
            names(lookup) <- c('member','memberKey')
            
            body <- as.data.frame(body[lookup,on = 'member',nomatch = 0])
            
            if(lvl %in% dd$footerLevels) {
                footer <- as.data.frame(footer)
            }
            
            if (dim %in% selectableDims(env) && !(dd$type == 'output')) {
                
                diff <- setdiff(s$label[s$level == lvl &
                                            s$parent == parent],body$member)
                
                if (length(diff) > 0) {
                    for (s in diff) {
                        body <- appendZeroRow(s,dim,body)
                    }
                }
                
            }
            
            if (nrow(body) == 0) {
                body <- appendZeroRow('Onbekend',dim,body)
            }
            
            if(lvl %in% dd$footerLevels && nrow(footer) == 0) {
                footer <- appendZeroRow('Onbekend',dim,footer)
            }
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
            
            if(addSummary) {
                
                e$type <- 'summary'
                e$df <- summary
                smry <- do.call(what = fun, args = list(), envir = e)
                if (is.vector(smry) && length(smry) == nrow(summary)) {
                    summary[[vc]] <- smry
                } else {
                    warning(paste0('userFunc: ', fun, ' has invalid summary length'))
                    summary[[vc]] <- 0
                }
                e$summary <- summary
                
            } else {
                
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

        }

        if(!addSummary) {
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
        }

        res = list(
            body = body,
            footer = footer,
            summary = summary)

        cacheDim(env,dim,res)

    } else {
        res <- gcache
    }
# browser()
    res
}

wrapUp <- function() {
    pkg.env$sessionCount <- pkg.env$sessionCount - 1

    if (pkg.env$sessionCount == 0) {

        if (!is.null(pkg.env$dbhandle))
            RODBC::odbcCloseAll()

        if (exists('globalCache', env = pkg.env)) {

            for (id in names(pkg.env$globalCache)) {
                saveRDS(pkg.env$globalCache[[id]],getCacheFile(id))
            }

        }

        pkg.env$dimUiIds <- c()
        pkg.env$globalCache <- list()

    }

    print(paste0('exit: ',pkg.env$sessionCount))
    stopApp()
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

visibleDims <- function(env) {
    dimStateSelect(env,c('enabled'))
}

inVisibleDims <- function(env) {
    dimStateSelect(env,c('disabled','hidden'))
}

filteringDims <- function(env) {
    intersect(dimTypeSelect(env,c('bidir','input')),dimStateSelect(env,c('enabled','hidden')))
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
    
    level <- dd$level
    l <- dd$selected

    x <- unique(l[,c('level','parent')])
    x$level <- x$level - 1
    x <- x[x$level >= 0,]

    names(x) <- c('level','label')

    #todo functie gebruiken als dimensie dieper is dan 2 levels

    if (nrow(x[x$level > 0,]) > 0 && nrow(x[x$level == 0,]) == 0) {
        x[nrow(x)+1,] = list(level = 0, label = dd$rootLabel)
    }
    
    if (nrow(x[x$level > 0,]) > 0 && nrow(x[x$level == 1,]) == 0) {
        x <- rbind(x,data.frame(
            level = 1,
            label = unique(dd$data$level1Label[dd$data$level2Label %in% x$label[x$level == 2]])
        ))
    }
    
    a <- env$dims[[dim]]$hasSubselect
    a <- a$label[a$level == level]
    b <- x$label[x$level == level]

    if (!identical(a[order(a,method = 'radix')], b[order(b,method = 'radix')])) {
        env$dims[[dim]]$reactive$dimRefresh <- env$dims[[dim]]$reactive$dimRefresh + 1
    }

    env$dims[[dim]]$hasSubselect <- x[order(x$level, x$label ,method = 'radix'),]

}

dimCorrectSelectionInfo <- function(input,env,dim) {

    l <- env$dims[[dim]]$selected

    if (nrow(l) > 1 && !(input[[paste0(dim,'DimMs')]])) {
        l <- l[nrow(l),]

        env$dims[[dim]]$selected <- l
        env$dims[[dim]]$reactive$selectChange <- env$dims[[dim]]$reactive$selectChange + 1

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

    return(firstRow)

}


getMeasList <- function(env,dim) {
    lvl <- env$dims[[dim]]$level
    meas <- env$dims[[dim]]$measList
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
    paste0(getwd(), '/tmp/', pkg.env$dashboardName, '_', id, '_cache.rds')
}

getCache <- function(env) {

    cacheFile <- getCacheFile(env$id)

    if (pkg.env$sessionCount == 1) {

        if (is.na(file.info(cacheFile)$mtime)) {

            dwhrStop(paste0('cacheFile: ',cacheFile,' not Found'))

        } else {

            pkg.env$globalCache[[env$id]] <- readRDS(cacheFile)
        }
    }

    invisible()
}

getZoom <- function(env,dim) {
    
    dd <- env$dims[[dim]]
    members <- dd$membersFiltered$member
    if (dd$level == dd$maxLevel) 
        return (' ')
    
    
    
    
    
}

