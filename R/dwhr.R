#' Creeer nieuw sterschema-object.
#' 
#' Maak een sterschema-object gebaseerd op een feiten-tabel. Aan het resulterende object kunnen dimensie views worden toegevoegd met
#' \code{\link{addDimView}}. \code{new.star()} moet uitgevoerd worden binnen een shiny-server sessie.
#'
#' @param starId string, Id voor dit sterschema-object.
#' @param session een shiny sessie object.
#' @param facts dataframe met aggregeerbare meetwaarden en foreign-keys naar dimView-dataframes
#' @param caching boolean, controleert de caching van geaggreerde meetwaarden. Te gebruiken bij grote feiten-tabellen. default FALSE
#' @param foreignKeyCheck boolean, bepaalt of er een foreignkey-check wordt uitgevoerd. default TRUE
#'
#' @return resultaat een sterschema-object van klasse 'star'. Resultaat is een environment met velden:
#' \itemize{
#'     \item{\code{factsFiltered()}}: reactive dataframe met gefilterde feiten uitgaande van de selecties in de dimensie views
#'     \item{dims}: een lijst met dimView objecten zoals die nu met de feiten verbonden zijn
#'     }  .
#'
#' @export
new.star <- function(starId, session, facts, caching = FALSE, foreignKeyCheck = TRUE) {

    withCallingHandlers({
        assert_is_a_string(starId)
        assert_is_data.frame(facts)
        assert_is_a_bool(caching)
        assert_is_a_bool(foreignKeyCheck)

        'ShinySession' %in% class(session) || stop('session is not of class shinySession')

        ses <- session$userData

        if (!exists('authenticated', envir = ses) || !ses$authenticated) {
            stop('Not authenticated')
        }

        if (!exists('starList',envir = ses)) {
            ses$starList <- list()
        } else {
            starId %in% ses$starList && stop(paste0('star-object with id: ', starId, 'already exists'))
        }

    },

    error = function(c) {
        dwhrStop(conditionMessage(c))
    })

    ses$starList <- c(ses$starList,starId)

    env <- new.env(parent = emptyenv())
    class(env) <- 'star'

    env$ce <- parent.frame()    # calling environment

    env$id <- starId
    env$call <- match.call()
    env$facts <- facts
    env$dims <- list()
    env$caching <- caching
    env$foreignKeyCheck <- foreignKeyCheck
    env$session <- session

    if (env$caching)
        getCache(env)

    # vars for datatable renderer

    env$dtRenderers <- list()
    env$dtPrep <- list()
    env$dtUiId <- list()

    # vars for highcharts renderer

    env$hcRenderers <- list()
    env$customPatterns <- list()
    env$hcPrev <- list()
    env$hcPrep <- list()
    
    env$reactive <- shiny::reactiveValues(factsChange = 0)

    env$factsFiltered <- shiny::reactive({

        f <- env$facts
        
        env$reactive$factsChange

        for (d in dimTypeSelect(env,c('bidir','input'))) {
            env$dims[[d]]$reactive$selectedIdsChange
        }

        for (d in filteringDims(env)) {
            if (any(env$dims[[d]]$selected$level > 0)) {
                dkey <- env$dims[[d]]$keyColumn
                f <- f[f[[dkey]] %in% env$dims[[d]]$selectedIds,]
            }
        }

        f
    })

    env
}

#' Voeg dimensie view (dimView) toe aan sterschema-object
#'
#' Deze functie koppelt dimensie-data aan de feiten-tabel gegeven een sterschema-object \code{env}. Een dimView-object
#' wordt toegevoegd aan de \code{env$dims} list. Een dimView is een combinatie van een dimensie-kolom (dimView-member) met geaggregeerde 
#' meetwaarden uit de feiten-tabel. Het dimView object kan meerdere nivo's hebben. Het maximum aantal nivo's is op dit moment
#' 5. Na het toevoegen van een dimView-object aan het sterschema kunnen er meetwaarden (via \code{\link{addMeasure}}) en 
#' afgeleide meetwaarden (via \code{\link{addMeasureDerrived}}) toegevoegd worden.
#' 
#' @section reactive values:
#' De volgende reactive values zijn beschikbaar per dimView
#' \itemize{
#'    \item levelChange integer, wordt opgehoogd als gebruiker van level veranderd in de UI (door op + te klikken of door 
#'    via breadcrumb naar ander nivo te navigeren). Dit triggered een dimRefresh. 
#'    \item dimRefresh integer, het verhogen van deze waarde triggered een verversing van de presentaties die aan deze dimView 
#'    gekoppeld zijn.
#'    \item selectChange integer, wordt opgehoogd als gebruiker een item selecteerd in de presentatie, of als er programatisch een 
#'    selectie-wijziging doorgevoerd wordt via \code{\link{setSelection}}. Dit triggered een selectedIdsChange, en isFiltered wordt 
#'    bijgewerkt.
#'    \item selectedIdsChange integer, wordt opgehoogd als selectie-wijziging daadwerkelijk resulteert in andere geselecteerde id's. 
#'    Dit triggered een dimRefresh.
#'    \item visChange = 0,
#'    \item isFiltered = !(any(selectLevel == 0)),
#'    \item orderChange = 0,
#'    \item pageChange = 0,
#'    \item pageLengthChange = 0,
#'    \item presChange = 0,
#'    \item nameChange = 0,
#'    \item linksChange = 0,
#'    \item clickMeasureEvent 
#'}
#'    
#'
#' @param env sterschema-object, gemaakt met \code{\link{new.star}}
#' @param dim string, uniek id van deze dimView. Dit id moet corresponderen met de parameter \code{dim} in
#' \code{\link{getDimUI}}
#' @param name string, getoonde naam in UI als titel van de dimView
#' @param data data.frame, tabel met dimensie-data met de volgende regels:
#' \itemize{
#'   \item De eerste kolom is de sleutel van de dimensie. Deze sleutel wordt gebruikt om een join te leggen met een 
#'   kolom met \strong{dezelfde} naam in de feiten-tabel. Als voor het sterschema-object \code{env}  geldt dat \code{env$foreignKeyCheck == TRUE} 
#'   dan wordt een foreignkey-check uitgevoerd en als dit fouten oplevert worden de eerste 10 verschillende foute id's getoond.
#'   \item er *moet* een kolom zijn met de naam level1Label, Dit is de kolom met het laagste nivo aan details (meest geaggregeerd). Minder gegaggregeerde
#'   (hogere) nivo's krijgen corresponderende namen: level2Label, level3Label etc. Top level0Label wordt automatisch toegevoegd en hoort niet 
#'   in het data.frame te zitten.}
#'@param levelNames character, vector van level-namen beginnend met de naam van het top-level. Deze level-namen worden getoond in de UI (in het kruimel-pad
#'en als getoonde kolomnaam van de dimView).
#'@param initLevel integer, initieele nivo wat getoond wordt. initlevel moet aanwezig zijn in de dimensie-data in de vorm van een kolom met de naam: 
#'level<initLevel>Label' (behalve voor level 0). default 0.
#'@param initParent string, specifieke ouder van het initieele level wat getoont wordt. Ouder moet in data zitten. Als dit niet opgegeven wordt, wordt de eerste
#'ouder van het betreffende initLevel gebruikt. default ""
#'@param selectLevel integer, initieele selectie-nivo. Als leafOnly == TRUE worden selectLevel en selectableLevels op maxLevel gezet, anders is dit default gelijk 
#'aan 0 (geen selectie, of selectie van top level).
#'@param selectLabel string, intitieel geselecteerd label voor het level volgens selectLevel. Als selectLevel == 0 dan wordt deze waarde gezet op het label van het top-level
#'(levelNames[1]). Anders: selectLabel moet een geldige waarde zijn in de data voor het betreffende selectLevel.
#'@param state string, deze parameter met default 'enabled' bepaalt of de dimView een filterend effect op de feiten heeft of niet en of de dimView zichtbaar is of niet.
#'Mogelijk waarden zijn:
#'\itemize{
#'  \item enabled: dimView heeft filterend effect op de feiten en is zichtbaar.
#'  \item hidden: dimView niet zichtbaar, maar heeft nog steeds een filterend effect op de feiten.
#'  \item disabled: dimView heeft geen filterend effect en is niet zichtbaar.
#'}
#'verandering van state kan plaats vinden via \code{\link{dimChangeState}}.
#'@param type string, deze parameter met default 'bidir' bepaalt of dimView meetwaarden of afgeleide meetwaarden heeft of niet en of de dimView selecteerbaar is of niet. 
#'Mogeljke waarden zijn:
#'\itemize{
#'  \item bidir: dimView heeft meetwaarden en is selecteerbaar.
#'  \item input: dimView heeft geen meetwaarden maar is wel selecteerbaar.
#'  \item output: dimView heeft meetwaarden maar is niet selecteerbaar.
#'}
#'@param selectMode string, mogelijke waarden:
#'\itemize{
#'  \item none: dimView niet selecteerbaar.
#'  \item single: single select.
#'  \item multi: multiple select.
#'}
#'@param useLevels integer, bepaalt welke nivo's uit levelNames daadwerkelijk gebruikt worden in de dimView.
#'@param cntName string, getoonde column-header voor de standaard count meetwaarde.
#'@param itemName string, getoonde column-header voor dimView-member.
#'@param ignoreDims character, vector met dimView-namen die geen filterend of berekend effect mogen hebben op de geaggregeerde waarden van deze dimView.
#'@param leafOnly boolean, als TRUE heeft deze dimView alleen geaggregeerde waarden op het hoogste (meest gedetaileerde) nivo. Te gebruiken wanneer dimensie-members 
#'niet aggregeerbaar zijn. De footer wordt dan in de UI niet getoond.
#'@param fixedMembers boolean, als TRUE worden alle dimensie-members getoont in de UI ongeachtof feiten bestaan voor de members of niet. Correspondeert met een left-
#'outer join tussen dimensie en feiten-tabel.
#'@param keepUnused boolean, als TRUE: niet gebruikte records in data (geen facts) worden niet opgeschoond, default FALSE. 
#'@param na.rm boolean, als TRUE worden NA values verwijderd voor het uitvoeren van de aggregatie-functie.
#'@param selectableLevels integer, bepaalt welke nivo's van de dimView selecteerbaar zijn, staat standaard op alle nivo's.
#'@param footerLevels integer, bepaalt welke nivo's een footer krijgen, staat standaard op alle nivo's.
#'@param presListType string, bepaalt de manier waarop de presentatielijst getoond wordt. Mogelijke waarden:
#'\itemize{
#'  \item dropdown: lijst wordt getoond via een dropdown box
#'  \item links: lijst wordt getoond via meerdere links 
#'}
#'
#'@return gewijzigd sterschema-object.
#'
#' @export
addDimView <- function(
    env, dim, name, data, levelNames, initLevel = 0, initParent = "", selectLevel = 0,
    selectLabel = levelNames[1], selectParent = NULL, state = 'enabled', type = 'bidir', selectMode = 'single', useLevels = NULL,
    cntName = 'cnt', itemName = 'Naam', ignoreDims = NULL, leafOnly = FALSE, fixedMembers = FALSE, keepUnused = FALSE,
    na.rm = TRUE, orderBy = 'name', selectableLevels = NULL, footerLevels = NA_integer_ , presListType = 'dropdown',
    returnPrepData = FALSE, selectedIds = NULL) {

    withCallingHandlers({
        class(env) == 'star' || stop('env is not of class star')

        assert_is_a_string(dim)
        assert_is_a_string(name)
        assert_is_data.frame(data)
        data <- as.data.frame(data)
        assert_is_character(levelNames)
        assert_is_a_number(initLevel)
        initLevel <- as.integer(initLevel)
        assert_is_a_string(initParent)
        assert_is_a_number(selectLevel)
        selectLevel <- as.integer(selectLevel)
        assert_is_character(selectLabel)
        assert_is_a_string(isNull(selectParent,''))
        assert_is_a_string(state)
        state %in% c('enabled','disabled','hidden') || stop('invalid state parameter')
        assert_is_a_string(type)
        type %in% c('bidir','input','output') || stop('invalid type parameter')
        assert_is_a_string(selectMode)
        selectMode %in% c('none','single','multi') || stop('invalid selectMode parameter')
        assert_is_numeric(isNull(useLevels,0))
        useLevels <- as.integer(useLevels)
        assert_is_numeric(isNull(selectableLevels,0))
        selectableLevels <- as.integer(selectableLevels)
        assert_is_numeric(isNull(footerLevels,0))
        assert_is_a_string(cntName)
        assert_is_a_string(itemName)
        assert_is_character(isNull(ignoreDims,''))
        assert_is_a_bool(leafOnly)
        assert_is_a_bool(fixedMembers)
        assert_is_a_bool(keepUnused)
        assert_is_a_bool(na.rm)
        assert_is_a_string(orderBy)
        assert_is_subset(orderBy,domains[['orderBy']])
        assert_is_a_string(presListType)
        assert_is_subset(presListType,domains[['presListType']])
        assert_is_subset(isNull(selectedIds,data[1,1]),data[[1]])
        
        if (!is.null(selectedIds) || selectMode == 'none') {
            selectLevel <- 0
            selectLabel <- levelNames[1]
            selectParent <- NULL
        }

        maxLevel <- length(levelNames) - 1
        rootLabel <- levelNames[1]

        # correct type

        if (type %in% c('bidir','input') && selectMode == 'none' ) {
            type = 'output'
        }

        # add level0Label
        data$level0Label <- rootLabel

        # check column names

        lapply(c(1:maxLevel),function(x) {
            label <- paste0('level',x,'Label')
            code <- paste0('level',x,'Code')
            label %in% names(data) || stop(paste0('level',x,'Label missing in data'))
            if (!code %in% names(data)) {
                #data[[code]] <<- apply(data[,c(label),drop = FALSE],1,digest::digest)
                data[[code]] <<- data[[label]]
            }

            length(unique(data[[code]])) >= length(unique(data[[label]])) || stop(paste0('level',x,'Code is no key for level',x,'Label'))
        })

        # filter dimension data 
        
        keyColumn <- names(data)[1]

        if(!fixedMembers) {
            keyColumn %in% names(env$facts) || stop(paste0('keyColumn ', dim, ' is not a foreign key in fact-table'))
            if (!keepUnused) {
                data <- data[data[[keyColumn]] %in% unique(env$facts[[keyColumn]]),]
                nrow(data) > 0 || stop('dimension and facts are completely disjoint')
            }
        }

        # foreignkey check
        if (env$foreignKeyCheck) {
            missing <- setdiff(unique(env$facts[[keyColumn]]),data[[keyColumn]])
            if (length(missing) > 0) {
                stop(c(paste0(dim, ': Foreign key error. first 10:'), paste0('    ',head(missing,10))))
            }
        } else {
            nr1 <- nrow(env$facts)
            env$facts <- env$facts[env$facts[[keyColumn]] %in% data[[keyColumn]],] 
            nr2 <- nrow(env$facts)
            if (nr2 < nr1) {
                warning(paste0(dim,': removing ',nr1 - nr2, ' records from facts'))
            }
        }

        # setting/checking selectLevel selectLabel, selectMode, initLevel and useLevels

        if (leafOnly) {
            
            selectLevel <- maxLevel
            selectableLevels <- maxLevel
            footerLevels <- c()
            
            if (!selectMode == 'single') {
                warning(paste0(dim,': Invalid selectMode for leafOnly dim, selectMode set to single'))
                selectMode = 'single'
            }

        } 

        if(length(useLevels) > 0) {
            if (leafOnly) {
                if (!maxLevel %in% useLevels) {
                    warning(paste0(dim,': for leafOnly useLevels must contain maxLevel, useLevels set to c(0..maxLevel).'))
                    useLevels <- c(0:maxLevel)
                }
            }
            useLevels <- unique(c(0,useLevels))
            if (!all(useLevels %in% c(0:maxLevel))) {
                warning(paste0(dim,': useLevels out of range, useLevels set to c(0..maxLevel)'))
                useLevels <- c(0:maxLevel)
            }
        } else {
            useLevels <- c(0:maxLevel)
        }
        
        if(length(selectableLevels) > 0) {
            if (!all(selectableLevels %in% useLevels)) {
                warning(paste0(dim,': selectableLevels out of range, selectableLevels set to useLevels'))
                selectableLevels <- useLevels
            }
        } else {
            selectableLevels <- useLevels
        }
        
        if (!is.null(footerLevels)) {
            if(!any(is.na(footerLevels))) {
                if (!all(footerLevels %in% useLevels)) {
                    warning(paste0(dim,': footerLevels out of range, footerLevels set to useLevels'))
                    footerLevels <- useLevels
                }
            } else {
                footerLevels <- useLevels
            }
        }
        
        if (!initLevel %in% useLevels) {
            warning(paste0(dim,': initLevel out of range. initLevel set to 0'))
            initLevel <- 0
        }
        
        if (!selectLevel %in% selectableLevels) {
            warning(paste0(dim,': selectLevel out of range. selectLevel set to selectableLevels[1]'))
            selectLevel <- selectableLevels[1]
        }

        if (!selectLevel %in% useLevels) {
            warning(paste0(dim,': selectLevel out of range. selectLevel set to 0'))
            selectLevel <- 0
        }
        
        if (!any(selectLabel %in% data[[paste0('level',selectLevel,'Label')]])) {
            warning(paste0(dim,': selectLabel not in data, label set to first label in selectLevel'))
            selectLabel <- data[[paste0('level',selectLevel,'Label')]][1]
        }

        # map columns based on useLevels
        
        org <- list(
            data = data,
            levelNames = levelNames,
            maxLevel = maxLevel,
            initLevel = initLevel,
            selectLevel = selectLevel)

        if (length(setdiff(c(0:maxLevel),useLevels)) != 0)  {

            nw <- c()
            old <- c()
            a <- 1

            for (i in c(1:maxLevel)) {
                if (!(i %in% useLevels)) {
                    data[[paste0('level',i,'Label')]] <- NULL
                    data[[paste0('level',i,'Code')]] <- NULL
                } else {
                    old <- c(old,i)
                    nw <- c(nw,a)
                    a <- a + 1
                }
            }

            names(data)[names(data) %in% paste0('level',old,'Label')] <- paste0('level',nw,'Label')
            names(data)[names(data) %in% paste0('level',old,'Code')] <- paste0('level',nw,'Code')
            levelNames <- levelNames[c(useLevels + 1)]

            if (initLevel %in% old) {
                initLevel <- nw[which(initLevel %in% old)]
            }

            if (selectLevel %in% old) {
                selectLevel <- nw[which(selectLevel %in% old)]
            }

            maxLevel <- length(levelNames) - 1

            levelMap <- data.frame(
                from = c(0,old),
                to = c(0,nw)
            )

        } else {
            levelMap <- data.frame(
                from = 0:maxLevel,
                to = 0:maxLevel
            )
        }

        # parent-child relatie opbouwen

        pc <- data.frame(
            level = 0,
            gparentLabel = '',
            gparentCode = '',
            parentLabel = '',
            parentCode = '',
            label = rootLabel,
            code = 'root',
            stringsAsFactors = FALSE)

        pc <- rbind(
            pc,
            data.frame(
                level = 1,
                gparentLabel = '',
                gparentCode = '',
                parentLabel = rootLabel,
                parentCode = 'root',
                label = unique(data$level1Label),
                code = unique(data$level1Code),
                stringsAsFactors = FALSE))

        if (maxLevel >= 2) {
            for (lvl in 2:maxLevel) {
                c1 <- paste0('level',lvl - 2,'Label')
                c2 <- paste0('level',lvl - 2,'Code')
                c3 <- paste0('level',lvl - 1,'Label')
                c4 <- paste0('level',lvl - 1,'Code')
                c5 <- paste0('level',lvl,'Label')
                c6 <- paste0('level',lvl,'Code')
                
                if (lvl == 2) {
                    l2 <- unique(data[c(c3,c4,c5,c6)])
                    l2$gparent <- rootLabel
                    l2$gparentCode <- 'root'
                    l2$level <- lvl
                    names(l2) = c('parentLabel','parentCode','label','code','gparentLabel','gparentCode','level')
                } else {
                    l2 <- unique(data[c(c1,c2,c3,c4,c5,c6)])
                    l2$level <- lvl
                    names(l2) = c('gparentLabel','gparentCode','parentLabel','parentCode','label','code','level')
                }

                pc <- rbind(pc,l2)
            }
        }

        selectCode <- pc$code[pc$label %in% selectLabel & pc$level == selectLevel]

        if (initParent == '' || initLevel <= 1) {
            initParent <- pc$parentLabel[pc$level == initLevel ][1]
        } else {
            if (!(initParent %in% pc$parentLabel[pc$level == initLevel])) {
                warning(paste0('initParent: ', initParent, ' not found for initLevel: ',initLevel,'. initParent set to first parent.'))
                initParent <- pc$parentLabel[pc$level == initLevel][1]
            }
        }

        parentCode <- pc$code[pc$label == initParent & pc$level == initLevel]

        ancestors <- c(initParent)

        if (initLevel >= 1) {
            for (i in initLevel:1) {
                p <- ancestors[1]
                ancestors <- c(unique(pc$parentLabel[pc$level == i - 1 & pc$label == p]),ancestors)
            }
        }
        
        if (is.null(selectParent)) {
            selectParent <- pc$parentLabel[pc$level == selectLevel & pc$label %in% selectLabel]
        } else {
            if (!(selectParent %in% pc$parentLabel[pc$level == selectLevel & pc$label %in% selectLabel])) {
                warning(paste0('selectParent: ', selectParent, ' not found for selectLevel: ',selectLevel,' and selectLabel: ',selectLabel, '. selectParent set to first parent.'))
                selectParent <- pc$parentLabel[pc$level == selectLevel & pc$label %in% selectLabel]
            }
        }

        if(is.null(selectParent)) {
            selectParent = ''
            selectLevel = 0
            selectLabel = rootLabel
            selectCode = 'root'
        }
    },

    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    
    if (returnPrepData) {
        return(list(
            data = data,
            pc = pc
        ))
    }
    
    selectableLevels <- levelMap$to[levelMap$from %in% selectableLevels]
    
    if (!is.null(selectedIds)) {
        sel <- getSelected(data,maxLevel,selectableLevels,selectedIds)
        selectLevel <- sel$level
        selectLabel <- sel$label
        selectParent <- sel$parent
    }

    l <- new.env(parent = emptyenv())
    class(l) <- 'dimView'

    l$org <- org
    l$presListType <- presListType
    l$call <- match.call()
    l$measureCalls <- list()
    l$derrivedMeasureCalls <- list()
    l$presentationCalls <- list()
    l$name <- name
    l$gdim <- getGlobalId(env$id,dim)
    l$master <- TRUE
    l$views <- list()
    l$data <- data
    l$type <- type
    l$selectMode <- selectMode
    l$useLevels <- useLevels
    l$levelMap <- levelMap
    l$pc <- pc
    l$parent <- initParent
    l$ancestors <- ancestors
    l$level <- initLevel
    l$maxLevel <- maxLevel
    l$levelNames <- levelNames
    l$rootLabel <- rootLabel
    l$keyColumn <- names(data)[1]
    l$hasSubselect <- data.frame(
        level = numeric(0),
        label = character(0),
        stringsAsFactors = FALSE)
    l$defaultSelected <- data.frame(
        level = selectLevel,
        parent = selectParent,
        label = selectLabel,
        stringsAsFactors = FALSE)
    l$rootSelected <- data.frame(
        level = 0,
        parent = '',
        label = rootLabel,
        stringsAsFactors = FALSE)
    l$selected <- l$defaultSelected
    l$itemName <- itemName
    l$orderColumn <- itemName
    l$orderViewColumn <- ifelse(orderBy == 'name','member','memberKey')
    l$orderColumnDir = 'asc'
    l$orderBy = orderBy
    l$na.rm = na.rm
    l$rowLastAccessed <- data.frame(
        row = rep(NA,l$maxLevel + 1),
        value = '',
        level = seq(from = 0, to = l$maxLevel),
        stringsAsFactors = FALSE)
    l$rowLastAccessed$value[l$rowLastAccessed$level == selectLevel[1]] <- selectLabel[1]
    l$reactive <- shiny::reactiveValues(
        levelChange = 0,
        dimRefresh = 0,
        selectChange = 0,
        selectedIdsChange = 0,
        visChange = 0,
        isFiltered = !(any(selectLevel == 0)),
        orderChange = 0,
        pageChange = 0,
        pageLengthChange = 0,
        presChange = 0,
        nameChange = 0,
        linksChange = 0,
        clickMeasureEvent = list(
            clickCount = 0,
            clickViewColumn = '',
            clickMember = '',
            clickMemberKey = '',
            value = NA
        )
    )
    l$membersFiltered <- NA
    l$footer <- NA
    l$highchartsClickEvent <- -1
    l$searchTxt <- ""
    l$prevSearchTxt <- ""
    l$cntName <- cntName
    l$selectSource <- ''
    l$measList <- data.frame(
        factColumn = '*',
        viewColumn = 'cnt',
        fun = 'count',
        as = cntName,
        sort = 0,
        type = 'direct',
        category = 'fact',
        processingOrder = 0,
        format = 'standard',
        formatRef = NA,
        applyToLevels = vec2Bit(c(0:org$maxLevel)),
        stringsAsFactors = FALSE)
    l$presList <- list()
    l$presVec <- NULL
    l$defPres <- NULL
    l$observers <- NULL
    l$msState <- FALSE
    l$wait <- FALSE
    l$ignoreDims <- ignoreDims
    l$currentPage <- 1
    l$leafOnly <- leafOnly
    l$selectableLevels <- selectableLevels
    l$footerLevels <- levelMap$to[levelMap$from %in% footerLevels]
    l$hasFormatColumn  <- FALSE
    l$fixedMembers <- fixedMembers
    l$memberChangeLevel <- shiny::reactive({
        if (env$dims[[dim]]$reactive$levelChange > 0)
            printDebug(env = env, dim, dumpReactive = FALSE, eventIn = 'levelChange', eventOut = 'memberChange', info = paste0('target: ',dim))
        env$dims[[dim]]$reactive$levelChange
    })
    l$memberChangeVis <- shiny::reactive({
        if (env$dims[[dim]]$reactive$visChange > 0)
            printDebug(env = env, dim, dumpReactive = FALSE, eventIn = 'visChange', eventOut = 'memberChange', info = paste0('target: ',dim))
        env$dims[[dim]]$reactive$visChange
    })
    l$memberChangeOther <- shiny::reactive({
        v <- 0
        for (d in setdiff(setdiff(dimTypeSelect(env,c('bidir','input')),dim),ignoreDims)) {
            if (env$dims[[d]]$reactive$selectedIdsChange > 0) {
                printDebug(env = env, d, dumpReactive = FALSE, eventIn = 'selectedIdsChange', eventOut = 'memberChange', info = paste0('target: ',dim))
                v <- v + env$dims[[d]]$reactive$selectedIdsChange
            }
        }
        v
    })
    l$pres <- 'stub'
    l$state <- state
    l$debounce <- TRUE
    
    l$factsFilteredDim <- shiny::reactive({
        
        f <- env$facts
        
        env$reactive$factsChange
        env$dims[[dim]]$memberChangeOther()
        
        for (d in setdiff(setdiff(filteringDims(env),dim),ignoreDims)) {
            if (any(env$dims[[d]]$selected$level > 0)) {
                dkey <- env$dims[[d]]$keyColumn
                f <- f[f[[dkey]] %in% env$dims[[d]]$selectedIds,]
            }
        }
        
        dkey <- env$dims[[dim]]$keyColumn
        f[env$dims[[dim]]$data, on = dkey, nomatch = 0]
    })

    if (state == 'enabled') {
        l$visible <- TRUE
    } else {
        l$visible <- FALSE
        shinyjs::js$hideDim(dim = l$gdim)
    }

    env$dims[[dim]] <- l
    env$dims[[dim]]$selectedIds <- getSelectedIds(env,dim)

    startObserversData(env,dim)
    env
}


.setLevels <- function(dd,levels) {
    assert_is_numeric(isNull(levels,0))
    levels <- as.integer(levels)
    if (length(levels) == 0) {
        levels <- c(0:dd$org$maxLevel)
    }
    levels
}

.setAs <- function(dd,levels,as) {
    assert_is_character(as)
    assert_has_no_duplicates(as)

    ml <- dd$measList
    meas <- ml[ml$as %in% as,]
    if (nrow(meas) > 0)
        any(sapply(levels,function(x) { bitwAnd(2**x,meas$applyToLevels) %in% 2**x })) &&
            stop('as already exists for these levels')
    as
}

.setSort <- function(dd,len,sort) {
    assert_is_numeric(isNull(sort,0))
    sort <- as.integer(sort)
    ml <- dd$measList

    if (length(sort) == 0){
        mx <- max(ml$sort[ml$sort < 999])
        sort <- c((mx + 1):(mx + len))
    } else {
        length(sort) %in% c(1,len) || stop('Invalid length sort')
    }

    sort
}

.setViewColumn <- function(dd,levels,as,viewColumn) {
    assert_is_character(viewColumn)
    assert_are_same_length(viewColumn,as)
    assert_has_no_duplicates(viewColumn)

    ml <- dd$measList
    meas <- ml[ml$viewColumn %in% viewColumn,]
    if (nrow(meas) > 0)
        any(sapply(levels,function(x) { bitwAnd(2**x,meas$applyToLevels) %in% 2**x })) &&
            stop('viewColumn already exists for these levels')
    any(sapply(glob.env$reservedColumnPatterns,function(x) {length(grep(x,viewColumn)) > 0})) &&
        stop('viewColumn contains reserved names')

    viewColumn
}

.setFormat <- function(as,format) {
    assert_is_character(format)
    assert_is_subset(format,domains[['format']])
    length(format) %in% c(1,length(as)) || stop('Invalid length format')
    format
}

.setFormatColumn <- function(dd,as,formatColumn) {

    if (!is.null(formatColumn)) {
        assert_is_character(formatColumn)
        formatColumn %in% names(dd$data) || stop('Invalid formatColumn')
        length(formatColumn) %in% c(1,length(as)) || stop('Invalid length formatColumn')
        lapply(unique(formatColumn),function (x) assert_is_subset(dd$data[[x]],domains[['format']]))
    }

    formatColumn
}

.setStyle <- function(style) {
    assert_is_list(style)
    assert_is_subset(names(style), domains[['dataTableStyle']])
    length(style) %in% c(2,3) || stop('dataTableStyle must have 2 or 3 elements')
    is.null(style$values) && stop('missing values element in dataTableStyle')
    assert_is_character(style$values)
    
    if (!is.null(style$cuts)) {
        assert_is_numeric(style$cuts)
        length(style$cuts) == length(style$values) - 1 || stop('length cuts must be one less then length values')
        ret <- list(
            values = deparse(style$values),
            cuts = deparse(style$cuts))
    }
    if (!is.null(style$levels)) {
        assert_is_numeric(style$levels)
        length(style$levels) == length(style$values) || stop('length levels must be equal to length values')
        ret <- list(
            values = deparse(style$values),
            levels = deparse(style$levels))
    }
    
    if (!is.null(style$valueColumn)) {
        assert_is_a_string(style$valueColumn)
        ret$valueColumn <- style$valueColumn
    }
    
    ret
}

#' Toevoegen van meetwaarden of ander kolom-type aan dimView-object.
#'
#' Deze functies voegen meetwaarden, afgeleide meetwaarden of een ander type kolom zoals 
#' sort-column, tooltip-column text-column of grouping-column toe aan een bestaand dimView object
#'
#' @param env sterschema object, gecreeerd met \code{\link{new.star}}.
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' @param factColumn character, naam van numeric kolom in feiten tabel.
#' @param fun character, te gebruiken aggregatie functie voor deze meetwaarde. mogelijk waarden:
#' \itemize{
#'   \item sum: sommeer de kolomwaarde uit de feiten tabel.
#'   \item min: minimum kolomwaarde uit de feiten.
#'   \item max: maximum.
#'   \item dcount: distinct count van de factColumn
#'   \item count: aantal feitenregels.
#'   \item median: mediaan van factColumn.
#'   \item mean: gemiddelde van factColumn.
#'   \item userFunc: naam van custom aggregate functie. Functie moet gedefineerd zijn in de calling environment en moet een scalar retourneren.
#'}
#' @param as character, getoonde naam van de meetwaarde in de UI.
#' @param viewColumn character, naam van de technische kolom zoals gebruikt in de dimView. Als dit niet door gebruiker opgegeven wordt, wordt er een naam gegenereerd. De generatie van 
#' de naam verloopt zo:
#' \itemize{
#'     \item addMeasure: viewColumn = fun + '_' + factColumn
#'     \item addMeasureDerrived: viewColumn = d[1..n] met n het aantal afgeleide meetwaarden in de volgorde zoals die aangemaakt zijn.
#' } 
#' @param sort numeric, volgorde waarin de kolommen getoond wroden in de UI. (naar presentatie?)
#' @param format character, getoonde format in UI. Mogelijk waarden zijn:
#' \itemize{
#'   \item standard: waarde getoond as is.
#'   \item integer: afronding naar dichtsbijzijnde integer waarde.
#'   \item euro: afronding naar geheel getal met euro teken ervoor.
#'   \item euro2: idem maar dan afgerond op 2 decimalen.
#'   \item keuro: waarde wordt gedeeld door duizend en afgrond naar geheel getal. Euro teken wordt voor het resultaat geplaatst.
#'   \item perc: afronding naar geheel getal + %
#'   \item perc1: afronding op 1 decimaal + %
#'   \item perc2: afronding op 2 decimalen + %
#'   \item decimal1: waarde afgerond op 1 decimaal
#'   \item decimal2: waarde afgerond op 2 decimalen
#'   \item decimal3: waarde afgerond op 3 decimalen
#'}
#'@param levels numeric. bepaalt voor welke levels de meetwaarden getoond worden in de UI (addMeasure, addDerrivedMeasure and addText) of voor welke levels de kolommen actief 
#'zijn (addSortColumn, addTooltipColumn).
#'De nummers refereren aan de originele levels van de dimensie-data (voordat parameter \code{useLevels} in \code{addDimView} is toegepast)
#'
#'@return gewijzigd sterschema object.
#'
#'@export
addMeasure <- function(env, dim, factColumn, fun, as = factColumn, viewColumn = NULL,
                       sort = NULL, format = 'standard', formatColumn = NULL, levels = NULL) {

    withCallingHandlers({

        class(env) == 'star' || stop('env is not of class star')

        assert_is_a_string(dim)
        dim %in% names(env$dims) || stop('Unknown dim')

        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        ml <- dd$measList

        assert_is_character(factColumn)
        for (c in factColumn) {
            c %in% names(env$facts) || stop(paste0(c, ' is not a valid factColumn'))
            # if(!is.numeric(env$facts[[c]]))
            #     stop(paste0('factColumn ', c, ' is not numeric'))
        }

        if (dd$leafOnly) {
            levels <- max(dd$useLevels)
        }
        
        levels <- .setLevels(dd,levels)

        as <- .setAs(dd,levels,as)
        assert_are_same_length(as,factColumn)

        sort <- .setSort(dd,length(as),sort)

        assert_is_character(fun)
        
        for (x in fun) {
            x %in% domains[['aggregateFun']] || class(get(x, envir = env$ce)) == 'function' ||  stop('Invalid function')   
        }
        
        length(fun) %in% c(1,length(factColumn)) || stop('Invalid length fun')

        viewColumn <- .setViewColumn(dd,levels,as,isNull(viewColumn,paste0(fun,'_',factColumn)))
        format <- .setFormat(as,format)
        formatColumn <- .setFormatColumn(dd,as,formatColumn)
        formatRef <- isNull(formatColumn,NA)

        length(dd$presList) + length(dd$childDims) == 0 || stop('Dimension already has presentation')

    },

    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    dd$measureCalls[[length(dd$measureCalls) + 1]] <- match.call()

    ml <- rbind(
        ml,
        data.frame(
            factColumn = factColumn,
            viewColumn = viewColumn,
            fun = fun,
            as = as,
            sort = sort,
            type = 'direct',
            category = 'fact',
            processingOrder = 0,
            format = format,
            formatRef = NA,
            applyToLevels = vec2Bit(levels),
            stringsAsFactors = FALSE))

    if (!is.null(formatColumn)) {

        # only add formatColumn to measList if it not already exists

        formatColumn <- unique(setdiff(formatColumn,ml$viewColumn[ml$category == 'format']))

        if (length(formatColumn) > 0)
            ml <- rbind(
                ml,
                data.frame(
                    factColumn = formatColumn,
                    viewColumn = formatColumn,
                    fun = 'max',
                    as = formatColumn,
                    sort = 999,
                    type = 'direct',
                    category = 'format',
                    processingOrder = 999,
                    format = format,
                    formatRef = NA,
                    applyToLevels = vec2Bit(0:dd$org$maxLevel),   # formatColumns are for all levels
                    stringsAsFactors = FALSE))

        dd$hasFormatColumn  <- TRUE

    }

    dd$measList <- ml
    env
}

#'
#' @rdname addMeasure
#'
#' @param userFunc character, naam van gebruikers functie voor het implementeren van de een afgeleide meetwaarde. De functie moet de volgende eigenschappen hebben:
#' \itemize{
#'   \item userFunc moet gedefinieerd zijn in de calling environment van \code{\link{new.star}}.
#'   \item the functie heeft geen parameters.
#'   \item the parent environment bevat de volgende variabelen:
#'       \itemize{
#'         \item star: het sterchema object
#'         \item columnName: de naam van de dimView kolom die de functie moet opleveren.
#'         \item dim: id van de huidige dimView.
#'         \item df: de tot op dat moment berekende data.frame voor deze dimView. Normaal gesproken is dit de basis voor de berekening van de afgeleide meetwaarde.
#'         \item type: type van data.frame df: `footer` of `body`
#'         }
#'   \item de functie moet een vector retourneren met de lengte gelijk aan: \code{nrow(df)}.
#' }
#' @param processingOrder numeric, volgorde waarin de afgeleide meetwaarden berekend worden.
#'
#' @export
addMeasureDerrived <- function(env, dim, userFunc, as, viewColumn = NULL, sort = NULL, processingOrder = NULL,
                               format = 'standard', formatColumn = NULL, levels = NULL) {
    withCallingHandlers({

        class(env) == 'star' || stop('env is not of class star')

        assert_is_a_string(dim)
        dim %in% names(env$dims) || stop('Unknown dim')

        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        ml <- dd$measList
        
        if (dd$leafOnly) {
            levels <- max(dd$useLevels)
        }
        
        levels <- .setLevels(dd,levels)
        as <- .setAs(dd,levels,as)

        assert_is_character(userFunc)
        all(unique(sapply(userFunc,function(x) class(get(x, envir = env$ce)))) == 'function') ||
            stop('Invalid function')
        assert_are_same_length(userFunc,as)

        sort <- .setSort(dd,length(as),sort)

        assert_is_numeric(isNull(processingOrder,0))
        processingOrder <- as.integer(processingOrder)
        if (length(processingOrder) == 0) {
            mx <- max(ml$processingOrder[ml$processingOrder < 999])
            processingOrder <- c((mx + 1):(mx + length(as)))
        } else {
            assert_are_same_length(processingOrder,as)
            assert_are_disjoint_sets(ml$processingOrder, processingOrder)
        }

        viewColumn <- .setViewColumn(dd,levels,as,isNull(viewColumn,paste0('d',processingOrder)))
        format <- .setFormat(as,format)

        formatColumn <- .setFormatColumn(dd,as,formatColumn)
        formatRef <- isNull(formatColumn,NA)

        length(dd$presList) + length(dd$childDims) == 0 || stop('Dimension already has presentation')
    },

    error = function(c) {
        dwhrStop(conditionMessage(c))
    })

    dd$derrivedMeasureCalls[[length(dd$derrivedMeasureCalls) + 1]] <- match.call()
    
    ml <- rbind(
        ml,
        data.frame(
            factColumn = '?',
            viewColumn = viewColumn,
            fun = userFunc,
            as = as,
            sort = sort,
            type = 'indirect',
            category = 'derrived',
            processingOrder = processingOrder,
            format = format,
            formatRef = formatRef,
            applyToLevels = vec2Bit(levels),
            stringsAsFactors = FALSE))

    if (!is.null(formatColumn)) {

        # only add formatColumn to measList if it not already exists

        formatColumn <- unique(setdiff(formatColumn,ml$viewColumn[ml$category == 'format']))

        if (length(formatColumn) > 0)
            ml <- rbind(
                ml,
                data.frame(
                    factColumn = formatColumn,
                    viewColumn = formatColumn,
                    fun = 'max',
                    as = formatColumn,
                    sort = 999,
                    type = 'direct',
                    category = 'format',
                    processingOrder = 999,
                    format = format,
                    formatRef = NA,
                    applyToLevels = vec2Bit(0:dd$org$maxLevel),   # formatColumns are for all levels
                    stringsAsFactors = FALSE))

        dd$hasFormatColumn  <- TRUE

    }

    dd$measList <- ml
    env
}


#' @rdname addMeasure
#'
#' @param sortColumn string, naam van de kolom in de dimensie-data die gebruikt moet worden om de dimView-member te sorteren.
#' De sortColumn zelf is verborgen in de UI. Als een sortColumn gebruikt wordt, is de ordering via de UI uitgeschakeld.
#' Er kan een andere sortColumn gebruikt worden voor ieder level. Gebruik hiervoor de levels parameter. Als er meer dan 1 sortColumn gespecificeerd wordt
#' voor 1 level, wordt er een foutmelding gegenereerd.
#'
#' @export
addSortColumn <- function(env, dim, sortColumn, levels = NULL) {

    withCallingHandlers({

        class(env) == 'star' || stop('env is not of class star')

        assert_is_a_string(dim)
        assert_is_a_string(sortColumn)

        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        sortColumn %in% names(dd$data) || stop('Invalid sortColumn')
        levels <- .setLevels(dd,levels)

        ml <- dd$measList

        sort <- max(ml$sort) + 1
        as <- .setAs(dd,levels,'sort_sort')
        viewColumn <- as

    },

    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    dd$measureCalls[[length(dd$measureCalls) + 1]] <- match.call()

    ml <- rbind(
        ml,
        data.frame(
            factColumn = sortColumn,
            viewColumn = viewColumn,
            fun = 'max',
            as = as,
            sort = sort,
            type = 'direct',
            category = 'sort',
            processingOrder = 0,
            format = NA,
            formatRef = NA,
            applyToLevels = vec2Bit(levels),
            stringsAsFactors = FALSE))

    dd$measList <- ml
    env
}


#' @rdname addMeasure
#'
#' @param tooltipColumn string, te gebruiken kolom uit de dimensie-data die als tooltip gehanteerd wordt voor de dimview-member.
#'
#' @export
addTooltipColumn <- function(env, dim, tooltipColumn, levels = NULL) {

    withCallingHandlers({

        class(env) == 'star' || stop('env is not of class star')

        assert_is_a_string(dim)
        assert_is_a_string(tooltipColumn)

        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        tooltipColumn %in% names(dd$data) || stop('Invalid tooltipColumn')
        levels <- .setLevels(dd,levels)

        ml <- dd$measList

        as <- .setAs(dd,levels,'member_tooltip')
        viewColumn <- as

    },

    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    dd$measureCalls[[length(dd$measureCalls) + 1]] <- match.call()
    
    sort <- max(ml$sort) + 1

    ml <- rbind(
        ml,
        data.frame(
            factColumn = tooltipColumn,
            viewColumn = 'member_tooltip',
            fun = 'max',
            as = 'member_tooltip',
            sort = sort,
            type = 'direct',
            category = 'tooltip',
            processingOrder = 0,
            format = 'standard',
            formatRef = NA,
            applyToLevels = vec2Bit(levels),
            stringsAsFactors = FALSE))

    dd$measList <- ml
    env
}

#' @rdname addMeasure
#'
#' @param textColumn string, extra textuele kolom voor dimview.
#'
#' @export
addTextColumn <- function(env, dim, textColumn, as, viewColumn, sort = NULL, levels = NULL) {


    withCallingHandlers({

        class(env) == 'star' || stop('env is not of class star')

        assert_is_a_string(dim)
        assert_is_a_string(textColumn)
        assert_is_a_string(as)
        assert_is_a_string(viewColumn)

        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        textColumn %in% names(dd$data) || stop('Invalid textColumn')
        levels <- .setLevels(dd,levels)

        ml <- dd$measList

        as <- .setAs(dd,levels,as)
        viewColumn <- .setViewColumn(dd,levels,as,viewColumn)
        sort <- .setSort(dd,length(as),sort)

    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    dd$measureCalls[[length(dd$measureCalls) + 1]] <- match.call()

    ml <- rbind(
        ml,
        data.frame(
            factColumn = textColumn,
            viewColumn = viewColumn,
            fun = 'max',
            as = as,
            sort = sort,
            type = 'direct',
            category = 'text',
            processingOrder = 0,
            format = NA,
            formatRef = NA,
            applyToLevels = vec2Bit(levels),
            stringsAsFactors = FALSE))

    env$dims[[dim]]$measList <- ml
    env

}

#' @rdname addMeasure
#'
#' @param rowGroupColumn string, extra groeperings kolom voor dimView
#'
#' @export
addRowGroupColumn <- function(env, dim, rowGroupColumn, levels = NULL) {
    
    
    withCallingHandlers({
        
        class(env) == 'star' || stop('env is not of class star')
        
        assert_is_a_string(dim)
        assert_is_a_string(rowGroupColumn)
        
        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        rowGroupColumn %in% names(dd$data) || stop('Invalid rowGroupColumn')
        levels <- .setLevels(dd,levels)
        
        ml <- dd$measList
        
        as <- .setAs(dd,levels,'rowGroupColumn')
        viewColumn <- as
    
        
    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    dd$measureCalls[[length(dd$measureCalls) + 1]] <- match.call()
    sort <- max(ml$sort) + 1
    
    ml <- rbind(
        ml,
        data.frame(
            factColumn = rowGroupColumn,
            viewColumn = viewColumn,
            fun = 'max',
            as = as,
            sort = sort,
            type = 'direct',
            category = 'group',
            processingOrder = 0,
            format = 'standard',
            formatRef = NA,
            applyToLevels = vec2Bit(levels),
            stringsAsFactors = FALSE))
    
    dd$measList <- ml
    env
    
}

#'
#' Toevoegen van een presentatie aan een dimView object.
#'
#' presentaties zijn nodig om gegevens te tonen en om interactie mogelijk te maken met eindgebruiker.
#'
#' @param env sterschema object, gecreeerd met \code{\link{new.star}}.
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' @param uiId string, id in de UI dat gebruikt wordt om deze presentatie in af te beelden. Als leeg geld: uiId = dim. Bij meerdere presentaties
#' bij 1 uiId kan de presentatie gekozen worden via een dropdown lijst. Door een aparte uiId te gebruiken die afwijkt van dim kan dezelfde data via 2 presentaties tegelijkertijd
#' getoond worden op het scherm. Hoe de navigatie in dat geval moet verlopen kan via de parameter navOpts geregeld worden. Het uiId moet voorkomen in UI.R
#' @param type string, bepaalt het type presentatie. Geldige waarden zijn:
#' \itemize{
#'    \item dataTable: de presentatie is een datatable, precieze opties worden via parameter dataTableOpts vastgelegd
#'    \item highCharts: de presentatie is een highcharts chart, precieze opties worden via parameter highChartOpts vastgelegd
#'    \item radioButton: detail opties in simpleOpts
#'    \item selectInput: detail opties in simpleOpts
#'}
#' @param as string, naam van deze presentatie zoals getoond in de UI
#' @param name string, titel van presentatie in het geval deze op aparte positie op het scherm getoond wordt, dus als uiId <> dim 
#' @param isDefault boolean, relevant als er op 1 positie op het scherm meerdere presentaties zijn. Als TRUE wordt deze presentatie dan als eerste getoond. 
#' @param height integer, hoogte in pixels van presentatie
#' @param width integer, breedte in pixels van presentatie
#' @param useLevels, alleen relevant als uiId <> dim, bepaalt welke nivo's uit levelNames daadwerkelijk gebruikt worden in deze presentatie. 
#' @param navOpts list, met de volgende mogelijke items:
#' \itemize{
#'     \item syncNav: boolean, als TRUE, synchroniseer navigatie tussen presentaties. Alleen relevant als uiId <> dim, dus als presentaties tegelijk op het scherm staan. default TRUE
#'     \item hideNoFilter: boolean, als TRUE, verberg de 'Verwijder Filter' link nadat gebruiker selectie heeft gemaakt. default FALSE
#'     \item hideAll: boolean, als TRUE, verberg het top nivo in het kruimelpad. default FALSE
#'     \item hideBreadCrumb: boolean, als TRUE: verberg het kruimelpad. default FALSE
#'     \item links: list, lijst met te tonen url's in de header van de presentatie.
#' }
#' @param simpleOpts list, relevant voor het type radioButton. De lijst moet in dat geval de volgende items hebben:
#' \itemize{
#'     \item inline: boolean, als TRUE worden radiobuttons op 1 regel getoond ipv onderelkaar.
#' }
#' @param dataTableOpts list, verplicht in het geval van een presentatie-type dataTable. De lijst moet in dat geval de volgende items hebben:
#' \itemize{
#'     \item measures: list, verlpicht, lijst met meetwaarden die in tabel opgenomen moeten worden. De list heeft het volgende format:
#'     \itemize{
#'         \item viewColumn: string, verplicht, identificatie van de kolom. Dit is de naam zoals meegegeven aan \code{\link{addMeasure}} of aan \code{\link{addMeasureDerrived}} of zoals die 
#'         in deze functies wordt afgeleid.
#'         \item colorBarColor1: string, optioneel, te gebruiken achtergrond kleur colorbar voor betreffende kolom. 
#'         \item colorBarColor2: string, optioneel, te gebruiken achtergrond kleur colorbar voor negatieve waarden. colorbarColor1 moet in dit geval ook gegeven zijn. colorbarColor1 wordt 
#'         dan gebruikt voor de positieve waarden van de kolom.
#'         \item format: string, te gebruiken format voor deze kolom. Zie \code{\link{addMeasure}} voor toegestane waarden.
#'         \item orderable: boolean, geeft aan of kolom te ordenen moet zijn via de UI. Is nog niet geimplementeerd.
#'         \item bgStyle: list, specificatie van de background style van deze viewColumn. List kent volgende waarden:
#'         \itemize{
#'             \item cuts: numeric, vector van waarden waar de kolomwaarde over verdeeld wordt.
#'             \item levels: numeric, vector van waarden waar de kolomwaarde mee overeen moet stemmen.  Of cuts wordt opgegven of levels, 1 van beiden. 
#'             \item values: character, vector met kleuren waar de kolomwaarde op afgebeeld wordt via ofwel cuts ofwel levels. In het geval van cuts moet de vector van kleuren 
#'             1 langer zijn dan de vector cuts. Bij gebruik van levels moet de vector van kleuren even lang zijn als de levels-vector.
#'             \item valueColumn: Normaal wordt de waarde uit de viewColumn zelf gehaald, de waarde kan echter ook uit een andere viewColumn komen.
#'         }
#'         \item fgStyle: list, specificatie van de foreground style van deze viewColumn. List heeft zelfde structuur als bij het bgStyle item.
#'         \item width: integer, breedte in pixels van deze kolom.
#'         \item fontWeight: string, mag zijn: normal of bold. default normal.
#'         \item align: string, bepaalt uitlijning in kolom: left, center of right. 
#'         \item cursor: string, te hanteren cursor voor deze kolom als muis over de kolom gaat.
#'         \item visible: boolean, als FALSE is kolom niet zichtbaar in UI, default TRUE. 
#'         \item print: boolean, als FALSE wordt kolom niet in printbare documenten (pdf etc) getoond, default TRUE.
#'     }
#'     \item pageLength: integer, aantal regels per pagina in het geval van een langere lijst van dimView-members.
#'     \item pageLengthList: numeric, vector met pagina lengtes waaruit gebruiker kan kiezen.
#'     \item serverSideTable: boolean, Als TRUE wordt de data slechts gedeeltelijk geladen in client. Toe te passen voor (zeer) grote dimViews.
#' }
#' @param highChartOpts list, ....
#' @param checkUiId boolean, als TRUE: controleer of uiId in de client voorkomt, default TRUE.
#'
#'@return gewijzigd sterschema object.
#'
#'@export
#'
addPresentation <- function(env, dim, uiId = dim, type, as, name = '', isDefault = FALSE, height = NULL, width = NULL,
    useLevels = NULL, navOpts = NULL, simpleOpts = NULL, dataTableOpts = NULL, highChartsOpts = NULL, rangeOpts = NULL, checkUiId = TRUE) {

    withCallingHandlers({

        class(env) == 'star' || stop('env is not of class star')

        assert_is_a_string(dim)
        assert_is_a_string(uiId)
        assert_is_a_string(type)
        assert_is_a_string(as)
        assert_is_a_string(name)
        assert_is_a_bool(isDefault)
        assert_is_a_number(isNull(height,0))
        height <- as.integer(height)
        assert_is_a_number(isNull(width,0))
        width <- as.integer(width)
        assert_is_numeric(isNull(useLevels,0))
        useLevels <- as.integer(useLevels)

        assert_is_list(isNull(navOpts,list()))
        assert_is_list(isNull(simpleOpts,list()))
        assert_is_list(isNull(dataTableOpts,list()))
        assert_is_list(isNull(highChartsOpts,list()))
        assert_is_list(isNull(rangeOpts,list()))

        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')

        gdim <- getGlobalId(env$id,uiId)
        
        !checkUiId || gdim %in% glob.env$dimUiIds || stop('uiId not in UI')
        length(useLevels) == 0 || dim != uiId || stop('useLevels not valid for dim == uiId')

        assert_is_subset(type,domains[['presType']])

        # navOpts checks

        if (!is.null(navOpts))
            assert_is_subset(names(navOpts),domains[['navOpts']])
        else
            navOpts <- list()

        navOpts$syncNav <- isNull(navOpts$syncNav,TRUE)
        navOpts$hideNoFilter <- isNull(navOpts$hideNoFilter,FALSE)
        if (!0 %in% dd$selectableLevels) navOpts$hideNoFilter <- TRUE
        navOpts$hideAll <- isNull(navOpts$hideAll,FALSE)
        navOpts$hideBreadCrumb <- isNull(navOpts$hideBreadCrumb,FALSE)
        navOpts$links <- isNull(navOpts$links,list())
        navOpts$minBreadCrumbLevel <- isNull(navOpts$minBreadCrumbLevel,0)

        assert_is_a_bool(navOpts$syncNav)
        assert_is_a_bool(navOpts$hideNoFilter)
        assert_is_a_bool(navOpts$hideAll)
        assert_is_a_bool(navOpts$hideBreadCrumb)
        assert_is_list(navOpts$links)
        assert_is_a_number(navOpts$minBreadCrumbLevel)

        if (!navOpts$syncNav && dim == uiId) {
            navOpts$syncNav <- TRUE
        } 
        
        if (is.null(simpleOpts)  + is.null(dataTableOpts) + is.null(highChartsOpts) + is.null(rangeOpts) != 3) {
            stop('Invalid options')
        }

        # simpleOpts checks

        if (!is.null(simpleOpts)) {
            assert_is_subset(names(simpleOpts),domains[['simpleOpts']])
            simpleOpts$inline <- isNull(simpleOpts$inline,FALSE)
            assert_is_a_bool(simpleOpts$inline)
        }

        # dataTableOpts checks

        if (!is.null(dataTableOpts)) {

            assert_is_subset(names(dataTableOpts),domains[['dataTableOpts']])
            assert_is_list(dataTableOpts$measures)
            assert_is_non_empty(dataTableOpts$measures,'elements')

            i <- 1
            for (x in dataTableOpts$measures) {
                is.null(x$viewColumn) && stop('Missing viewColumn field')
                assert_is_subset(names(x), domains[['dataTableMeasures']])

                if ('colorBarColor2' %in% names(x) && !('colorBarColor1' %in% names(x))) {
                    stop('Missing colorBarColor1')
                }
                if ('colorBarColor1' %in% names(x) && ('bgStyle' %in% names(x))) {
                    stop('Incorrect combination of measure opts')
                }
                
                if ('format' %in% names(x)) {
                    assert_is_a_string(x$format)
                    assert_is_subset(x$format,domains[['dataTableFormats']])
                }

                if ('bgStyle' %in% names(x)) {
                    dataTableOpts$measures[[i]]$bgStyle <- .setStyle(x$bgStyle)
                }

                if ('fgStyle' %in% names(x)) {
                    dataTableOpts$measures[[i]]$fgStyle <- .setStyle(x$fgStyle)
                }

                if ('fontWeight' %in% names(x)) {
                    assert_is_a_string(x$fontWeight)
                    assert_is_subset(x$fontWeight, domains[['fontWeight']])
                }

                if ('width' %in% names(x)) {
                    assert_is_a_number(x$width)
                    assert_all_are_positive(x$width)
                    dataTableOpts$measures[[i]]$width <- as.integer(x$width)
                }

                if ('align' %in% names(x)) {
                    assert_is_a_string(x$align)
                    assert_is_subset(x$align,c('left','center','right'))
                }

                if ('cursor' %in% names(x)) {
                    assert_is_a_string(x$cursor)
                }
                
                if ('visible' %in% names(x)) {
                    assert_is_a_bool(x$visible)
                    dataTableOpts$measures[[i]]$visible <- x$visible
                } else {
                    dataTableOpts$measures[[i]]$visible <- TRUE
                }
                
                if ('print' %in% names(x)) {
                    assert_is_a_bool(x$print)
                    dataTableOpts$measures[[i]]$print <- x$print
                } else {
                    dataTableOpts$measures[[i]]$print <- TRUE
                }
                
                
                if ('orderable' %in% names(x)) {
                    assert_is_a_bool(x$orderable)
                    dataTableOpts$measures[[i]]$orderable <- x$orderable
                } else {
                    dataTableOpts$measures[[i]]$orderable <- TRUE
                }
                
                
                if ('tooltip' %in% names(x)) {
                    assert_is_a_string(x$tooltip)
                }
                
                if ('sparkOpts' %in% names(x)) {
                    assert_is_list(x$sparkOpts)
                    dataTableOpts$measures[[i]]$sparkOpts <- as.character(jsonlite::toJSON(x$sparkOpts))
                }

                dataTableOpts$measures[[i]] <- rlist::list.flatten(dataTableOpts$measures[[i]])
                dataTableOpts$measures[[i]]$colOrder <- i

                i <- i + 1

            }

            if ('pageLengthList' %in% names(dataTableOpts)) {
                assert_is_numeric(dataTableOpts$pageLengthList)
                assert_all_are_positive(dataTableOpts$pageLengthList)
                pll <- dataTableOpts$pageLengthList
                pll <- sort(unique(as.integer(pll)))
            } else {
                pll <- c()
            }

            if ('pageLength' %in% names(dataTableOpts)) {
                assert_is_a_number(dataTableOpts$pageLength)
                assert_all_are_positive(dataTableOpts$pageLength)
                pl <- dataTableOpts$pageLength
                pl <- as.integer(pl)
            } else {
                pl <- pll[1]
            }

            pl <- isNull(pl,10)
            pll <- isNull(pll,pl)

            pl %in% pll || stop('pageLength not in pageLengthList')

            is.null(dd$pageLength) || dd$pageLength == pl ||  stop(paste0('pageLength already set to', dd$pageLength))

            dataTableOpts$pageLength <- pl
            dataTableOpts$pageLengthList <- pll
            dd$pageLength <- pl

            if ('serverSideTable' %in% names(dataTableOpts)) {
                assert_is_a_bool(dataTableOpts$serverSideTable)
                is.null(dd$serverSideTable) || dd$serverSideTable == dataTableOpts$serverSideTable ||
                    stop(paste0('serverSideYable already set to', dd$serverSideTable))
                dd$serverSideTable <- dataTableOpts$serverSideTable
            } else {
                dd$serverSideTable <- isNull(dd$serverSideTable,FALSE)
                dataTableOpts$serverSideTable <- dd$serverSideTable
            }
        }
        
        # highChartsOpts checks
        
        if (!is.null(highChartsOpts)) {

            (dd$selectMode %in% c('none','single')) || (uiId != dim) || stop('HighCharts presentation not available for multi-select')
            
        }
        
        # check links

        if (length(navOpts$links) > 0) {
            types <- sapply(navOpts$links,function(x) {x$type})
            assert_is_subset(types,domains[['navOptsLinkTypes']])
        }
    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    dd$presentationCalls[[length(dd$presentationCalls) + 1]] <- match.call()

    if (!is.null(highChartsOpts)) domainCheck(names(highChartsOpts),'highChartsOpts')

    if (type %in% c('radioButton','selectInput'))
        (dd$type == 'input' && isSingleLevel(env,dim) && dd$selectMode == 'single') || dwhrStop('Presentation type not valid for this dim')

    if (uiId != dim) {

        if (!uiId %in% names(env$dims)) {
            
            call <- dd$call

            call$dim <- uiId
            call$ignoreDims <- c(eval(call$ignoreDims),dim,dd$childDims)
            call$name <- name
            call$env <- env
            
            if (length(useLevels) != 0) {
                navOpts$syncNav <- FALSE
                max(dd$useLevels) == max(useLevels) || dwhrStop('invalid useLevels parameter')
                call$useLevels <- useLevels
                
                if (any(dd$selected$level > 0)) {
                    call$selectedIds <- dd$selectedIds
                }
            }
            
            is.null(dd$syncNav) || dd$syncNav == navOpts$syncNav || dwhrStop('Incompatible syncNav')
            
            if (!is.null(highChartsOpts) && dd$selectMode %in% c('multi')) {
                warning('multi-select not implemented for highCharts: dimView set to single-select')
                call$selectMode <- 'single' 
            }

            if (!is.null(rangeOpts)) {
                call$initLevel <- max(dd$useLevels)
                call$initParent <- ""
                call$selectMode <- 'multi'
                call$type <- 'input'
            }
            
            
            eval(call, envir = env$ce)

            env$dims[[uiId]]$measList <- dd$measList
            env$dims[[uiId]]$parentDim <- dim

            for (x in dd$childDims) {
                env$dims[[x]]$ignoreDims <- unique(c(env$dims[[x]]$ignoreDims,uiId))
            }

            dd$childDims <-  c(dd$childDims,uiId)
            dd$ignoreDims <- c(dd$ignoreDims,uiId)

            dd$syncNav <- navOpts$syncNav
            env$dims[[uiId]]$syncNav <- navOpts$syncNav

            if (navOpts$syncNav) {

                length(dd$presList) > 0 || dwhrStop(paste0(dim, ': no presentation to sync with'))
                #  !is.null(dd$pageLength) || dwhrStop('No paging source')
                setOrdering(env = env, dim = uiId, as = dd$orderColumn, sort = dd$orderColumnDir, as2 = dd$orderColumn2)
                env$dims[[uiId]]$pageLength <- dd$pageLength
            }

            for (z in names(env$dims)) {
                ign <- env$dims[[z]]$ignoreDims
                if (dim %in% ign && !(uiId %in% ign)) {
                    env$dims[[z]]$ignoreDims <- c(ign,uiId)
                }
            }

            # env$proxyDims <- c(env$proxyDims,uiId)

        }

        dim <- uiId
        dd <- env$dims[[dim]]

    }
    
    withCallingHandlers({
        
        # rangeOpts checks
        
        if (!is.null(rangeOpts)) {
            dd$maxLevel == 1 || stop('dim has too many levels for dateRange or rangeSlider presentation')
            dd$level == 1 || stop('dim level must be 1 for dateRange or rangeSlider presentation')
            assert_is_subset(names(rangeOpts),domains[['rangeOpts']])
            
            if (type == 'dateRangeInput') {
                dates <- unique(dd$data[['level1Label']])
                
                assert_is_character(dates)
                assert_all_are_date_strings(dates,format = '%Y-%m-%d')
            }
            
            if (!is.null(rangeOpts$label)) {
                assert_is_a_string(rangeOpts$label)
            }
            
            if (!is.null(rangeOpts$throttle)) {
                assert_is_a_number(rangeOpts$throttle)
            }
            
            if (!is.null(rangeOpts$debounce)) {
                assert_is_a_number(rangeOpts$debounce)
            }
            
            minVal <- min(dd$data[['level1Label']])
            maxVal <- max(dd$data[['level1Label']])
            rangeOpts[['min']] <- minVal
            rangeOpts[['max']] <- maxVal
            
            navOpts$hideBreadCrumb <- TRUE
            
        }
        
    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    

    pl <- dd$presList
    as %in% sapply(pl,function(x) x$as) && dwhrStop('Prestentation already exists')

    # eerst vector bijwerken

    pv <- dd$presVec

    newKey <- paste0(type,length(pv) + 1)

    nms <- c(names(pv),as)
    pv <- c(pv,newKey)

    names(pv) <- nms

    dd$presVec <- pv

    pl[[newKey]] <- list(
        as = as,
        uiId = uiId,
        type = type,
        height = height,
        width = width,
        navOpts = navOpts,
        simpleOpts = simpleOpts,
        dataTableOpts = dataTableOpts,
        highChartsOpts = highChartsOpts,
        rangeOpts = rangeOpts)

    if (isDefault || length(pl) == 1) {
        dd$defPres <- newKey
        dd$pres <- newKey
    }

    dd$presList <- pl

    startObserversPres(env,dim,newKey)
    env
}

#'
#' Wijzig format van bestaande meetwaarde
#'
#' Pas het format aan van een meetwaarde uit een dimView. Er wordt gerefereerd naar de meetwaarde via de 
#' gepresenteerde naam (as).
#'
#' @param env sterschema object, gecreeerd met \code{\link{new.star}}.
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' @param viewColumn character, naam van de technische meetwaarde.
#' @param format character, nieuw te gebruiken format.
#' \itemize{
#'   \item standard: waarde getoond as is.
#'   \item integer: afronding naar dichtsbijzijnde integer waarde.
#'   \item euro: afronding naar geheel getal met euro teken ervoor.
#'   \item euro2: idem maar dan afgerond op 2 decimalen.
#'   \item keuro: waarde wordt gedeeld door duizend en afgrond naar geheel getal. Euro teken wordt voor het resultaat geplaatst.
#'   \item perc: afronding naar geheel getal + %
#'   \item perc1: afronding op 1 decimaal + %
#'   \item perc2: afronding op 2 decimalen + %
#'   \item decimal1: waarde afgerond op 1 decimaal
#'   \item decimal2: waarde afgerond op 2 decimalen
#'   \item decimal3: waarde afgerond op 3 decimalen
#'}
#'
#' @export
#'
changeFormatMeasure <- function(env, dim, viewColumn, format) {
    dd <- env$dims[[dim]]
    domainCheck(format,'format')
    ml <- dd$measList
    ml$format[ml$viewColumn %in% viewColumn] <- format
    dd$measList <- ml

    if (isNull(dd$syncNav,FALSE) && (as.character(sys.call(-3)) != 'changeFormatMeasure')) {
        lapply(union(dd$parentDim,dd$childDims), function(x) changeFormatMeasure(env = env, dim = x, viewColumn = viewColumn, format = format))
    }
    env

}

#'
#' Wijzig kolom(men) waarop dimView in de UI gesorteerd wordt.
#'
#' Pas de sorteer kolommen van dimView aan. Er wordt gerefereerd naar de kolom via de 
#' gepresenteerde naam (as).
#'
#' @param env sterschema object, gecreeerd met \code{\link{new.star}}.
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' @param as string, naam van de eerste kolom waarop gesorteerd moet worden
#' @param sort string, richting van sorteren. Mogelijk waarden: 
#' \itemize{
#'   \item asc: oplopend sorteren
#'   \item desc: aflopend sorteren
#'   \item LH: oplopend sorteren
#'   \item HL: aflopend sorteren
#'}
#' @param as2 string, naam van de tweede kolom waarop gesorteerd moet worden
#' 
#'@export
#'
setOrdering <- function(env, dim, as,sort, as2 = NULL) {

    dd <- env$dims[[dim]]
    domainCheck(sort,'ordering')
    ord <- switch(sort, LH = 'asc', HL = 'desc', asc = 'asc', desc = 'desc')

    ml <- dd$measList
    itemName <- dd$itemName

    if (as == itemName) {
        if (dd$orderBy =='key') {
            vc <- 'memberKey'
        } else {
            vc <- 'member'
        } 
    } else {
        vc <- ml$viewColumn[ml$as == as]
    }

    length(vc) > 0 || dwhrStop('invalid sortcolumn')

    if (!is.null(as2)) {

        if (as2 == itemName) {
            if (dd$orderBy =='key') {
                vc2 <- 'memberKey'
            } else {
                vc2 <- 'member'
            }
        } else {
            vc2 <- ml$viewColumn[ml$as == as2]
        }

        length(vc2) > 0 || dwhrStop('invalid sortcolumn2')

    }

    change <- FALSE

    if (dd$orderColumn != as || dd$orderViewColumn != vc || dd$orderColumnDir != ord)
        change <- TRUE

    dd$orderColumn <- as
    dd$orderViewColumn <- vc
    dd$orderColumnDir <- ord

    if (!is.null(as2)) {
        if (is.null(dd$orderColumn2) ||
            is.null(dd$orderViewColumn2) ||
            dd$orderColumn2 != as2 ||
            dd$orderViewColumn2 != vc2)
            change <- TRUE

        dd$orderColumn2 <- as2
        dd$orderViewColumn2 <- vc2
    }

    if (change && isNull(dd$syncNav,FALSE) && (as.character(sys.call(-3)) != 'setOrdering')) {
        lapply(union(dd$parentDim,dd$childDims), function(x) setOrdering(env = env, dim = x, as = as, sort = ord, as2 = as2))
    }
    change
}

#'
#' Zet programmatisch de selectie van een dimView.
#' 
#' De selectie van een dimView kan enerzijds via de UI ingesteld worden, anderszijds is het vaak nodig 
#' de selectie op programmatische wijze aan te passen, dit kan via setSelection.
#' 
#' @param env sterschema object, gecreeerd met \code{\link{new.star}}.
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' @param sel data.frame, data.frame met te selecteren items. Format:
#' \itemize{
#'     \item level, level van te selecteren items.
#'     \item label, label van te selecteren items.
#'     \item parent, parent-label van de te selecteren items.
#' }
#' @param source string, extra info over de bron van de selectieverandering.
#' @param dimRefresh boolean, indicatie of UI-output bijgewerkt moet worden of niet.
#' 
#'@export
#'
setSelection <- function(env,dim,sel,source = 'setSelection',dimRefresh = TRUE) {
    
    withCallingHandlers({
        
        class(env) == 'star' || stop('env is not of class star')
        assert_is_a_string(dim)
        
        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        
        assert_is_data.frame(sel)
        length(intersect(names(sel),c('label','parent','level'))) == 3 || stop('Invalid format selection data.frame')
    
        assert_is_a_string(source)
        assert_is_a_bool(dimRefresh)
    
    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    dd$selected$level <- as.numeric(dd$selected$level)
    sel$level <- as.numeric(sel$level)

    if (!identical(dd$selected, sel)) {
        dd$debounce <- FALSE
        dd$selected <- sel
        dd$selectSource <- source
        dd$rowLastAccessed$value[dd$rowLastAccessed$level == sel$level[1]] <- sel$label[1]
        
        dd$reactive$selectChange <- dd$reactive$selectChange + 1
        printDebug(env = env, dim, eventIn = 'setSelection', eventOut = 'selectChange', info = paste0('selected: (',sel$level,',',sel$label,')'))
        if (dimRefresh) {
            dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
            printDebug(env = env, dim, eventIn = 'setSelection', eventOut = 'dimRefresh')
        }
    }

}

setSelection2 <- function(env,dim,dimOrg,source = 'setSelection',dimRefresh = TRUE) {
    
    ddOrg <- env$dims[[dimOrg]]
    sel <- ddOrg$selected
    selIds <- ddOrg$selectedIds
    
    dd <- env$dims[[dim]]
    
    if (!identical(dd$selectedIds, selIds)) {
        
        dd$debounce <- FALSE
 
        dd$selected <- getSelected(dd$data,dd$maxLevel,dd$selectableLevels,selIds)
        dd$selectSource <- source
        
        if (isNull(dd$syncNav,FALSE)) {
            dd$rowLastAccessed <- ddOrg$rowLastAccessed
        }
        
        dd$reactive$selectChange <- dd$reactive$selectChange + 1
        dimSetHasSubselect(env,dim)
        
        printDebug(env = env, dim, eventIn = 'setSelection', eventOut = 'selectChange')
        if (dimRefresh) {
            dd$reactive$dimRefresh <- dd$reactive$dimRefresh + 1
            printDebug(env = env, dim, eventIn = 'setSelection', eventOut = 'dimRefresh')
        }
    }
    
}

getSelected <- function(data,maxLevel,selectableLevels,selectedIds) {
    
    if (length(selectedIds) >= nrow(data)) {
        return(data.frame(
            level = 0,
            parent = "",
            label = unique(data$level0Label)[1],
            stringsAsFactors = FALSE))
    }
    
    keyCol <- names(data)[1]
    
    if (maxLevel == 1) {
        return(data.frame(
            level = 1,
            parent = unique(data$level0Label)[1],
            label =  unique(data$level1Label[data[,keyCol] %in% selectedIds]),
            stringsAsFactors = FALSE))
    }
    
    dt <- data.table(data)
    sel <- NULL
    
    for (lvl in 1:(maxLevel - 1)) {
        
        cols <- c(paste0('level',lvl - 1,'Label'),paste0('level',lvl,'Label'))
        zz <- dt[dt[[keyCol]] %in% selectedIds,list(aantal = eval(parse(text = paste0('length(',keyCol,')')))),by = cols]
        names(zz) <- c('parent','label','aantal')
        
        xx <- dt[,list(aantal = eval(parse(text = paste0('length(',keyCol,')')))),by = cols]
        names(xx) <- c('parent','label','aantal')
        
        yy <- xx[zz,on = c('parent','label','aantal'),nomatch = 0]
        
        if (nrow(yy) > 0) {
            sel <- rbind(
                sel,
                data.frame(
                    level = as.integer(lvl),
                    parent = yy$parent,
                    label = yy$label,
                    stringsAsFactors = FALSE))
            
            names(yy) <- c(cols,'aantal')
            selectedIds <- setdiff(selectedIds,dt[yy,on = cols,nomatch = 0][[keyCol]])
        }
    }
    
    if (length(selectedIds) > 0) {

        cols <- c(paste0('level',maxLevel - 1,'Label'),paste0('level',maxLevel,'Label'))
        zz <- dt[dt[[keyCol]] %in% selectedIds,list(aantal = eval(parse(text = paste0('length(',keyCol,')')))),by = cols]
        names(zz) <- c('parent','label','aantal')
        
        sel <- rbind(
            sel,
            data.frame(
                level = as.integer(maxLevel),
                parent = zz$parent,
                label = zz$label,
                stringsAsFactors = FALSE))
    }
    
    sel
}
    

#'
#' Aanpassen state van dimView
#' 
#' met deze functie kan de toestand van een dimView aangepast worden. Toegestane toestanden (state) zijn:
#'\itemize{
#'  \item enabled: dimView heeft filterend effect op de feiten en is zichtbaar.
#'  \item hidden: dimView niet zichtbaar, maar heeft nog steeds een filterend effect op de feiten.
#'  \item disabled: dimView heeft geen filterend effect en is niet zichtbaar.
#'}
#'
#' @param env sterschema-object, gemaakt met \code{\link{new.star}}
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' @param newState string, nieuwe toestand voor dimView
#'
#'@export
#'
dimChangeState <- function(env, dim, newState) {
    
    withCallingHandlers({
        
        class(env) == 'star' || stop('env is not of class star')
        assert_is_a_string(dim)
        assert_is_a_string(newState)
        
        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
        
    state <- dd$state
    gdim <- dd$gdim

    if(newState != state ) {

        oldVis <- switch(as.character(dim %in% visibleDims(env)),'TRUE' = 'Y', 'FALSE' = 'N')
        oldFilt <- switch(as.character(dim %in% filteringDims(env)),'TRUE' = 'Y', 'FALSE' = 'N')

        dd$state <- newState

        newVis <- switch(as.character(dim %in% visibleDims(env)),'TRUE' = 'Y', 'FALSE' = 'N')
        newFilt <- switch(as.character(dim %in% filteringDims(env)),'TRUE' = 'Y', 'FALSE' = 'N')

        if (oldVis != newVis) {
            if (newVis != 'Y') {
                shinyjs::js$hideDim(dim = gdim)
                env$dims[[dim]]$visible <- FALSE
            }
            
            # showDim uitstellen tot na render. Zie ready events van highCharts en dataTable
            # als het niet tot een render komt, dan wordt dit via de observer op memberchange opgelost
            # door het uitstellen krijg je bij het tonen altijd alleen de nieuwe output te zien

            dd$reactive$visChange <- dd$reactive$visChange + 1
            printDebug(env = env, dim, eventIn = 'dimChangeState', eventOut = 'visChange', info = paste0('visible: ', newVis))
        }

        if (oldFilt != newFilt) {
            if (any(dd$selected$level > 0)) {
                dd$selected <- dd$rootSelected
                dimSetHasSubselect(env,dim)
                dd$reactive$selectChange <- dd$reactive$selectChange + 1
                dd$level <- 0
                dd$parent <- ''
                dd$ancestors <- c('')
                dd$reactive$levelChange <- dd$reactive$levelChange + 1

            }
        }

    }
    env

}

#'
#' Inlezen csv bestand in een data.table of data.frame. 
#' 
#' Bestand kan na initieele load opgeslagen worden als rds.
#' Dit rds bestand wordt dan de volgende keer als bron gebruikt (veel sneller als inlezen csv)
#' 
#' Als csv bestand een recentere datum heeft dan bestaande rds, dan wordt het csv bestand weer als bron 
#' genomen. \code{getCSV} is een alias voor \code{getFacts}.
#'
#' @param file string, pad van bron bestand.
#' @param key character, optionele vector met keys om toe te passen op het resultaat. Is voor performance redenen 
#' @param useRDS boolean, als TRUE dan wordt rds aangemaakt en gelezen, anders altijd lezen vanuit csv. Het rds bestand wordt 
#' opgeslagen in de tmp directory van de shiny-app. De tmp directory moet dus wel bestaan.
#' @param sep string, te gebruiken separator
#' @param fileEncoding string, te gebruiken encoding, default UTF-8-BOM ivm Byte Order Mark in windows bestanden 
#' @param as.df boolean, als TRUE maak dan een data.frame aan ipv een data.table
#' @param ... overige parameters zoals geaccepteerd door read.csv
#'
#'@export
#'
getFacts <- function(file, key = NULL, useRDS = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', as.df = FALSE, header = FALSE, ...) {
    
    withCallingHandlers({
        assert_is_a_string(file)
        assert_is_character(isNull(key,''))
        assert_is_a_bool(useRDS)
        assert_is_a_string(sep)
        assert_is_a_string(fileEncoding)
        assert_is_a_bool(as.df)
    },
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    if (exists('glob.env',envir = globalenv())) {
        tmpFactsFile <- paste0(getwd(),'/tmp/',glob.env$dashboardName,'_',unlist(strsplit(basename(file),'[.]'))[1],'.rds')
    } else {
        tmpFactsFile <- paste0(getwd(),'/tmp/',basename(getwd()),'_',unlist(strsplit(basename(file),'[.]'))[1],'.rds')
    }
    
    mtime1 <- file.info(file)$mtime
    mtime2 <- file.info(tmpFactsFile)$mtime

    facts <- NA

    if (!useRDS || is.na(mtime2) || mtime1 > mtime2) {

        facts <- read.csv(
            file = file,
            sep = sep,
            header = header,
            fileEncoding = fileEncoding,
            stringsAsFactors = FALSE,
            ...)

        if (!as.df) {
            facts <- data.table::data.table(facts)
            
            if(!is.null(key))
                data.table::setkeyv(facts,key)
        }

        if (useRDS)
            saveRDS(facts,tmpFactsFile)

    } else {
        facts <- readRDS(tmpFactsFile)
    }

    return (facts)
}

#' @rdname getFacts
#'
#' @export
#'
getCSV <- function(...) {
    getFacts(...)
}

#'
#' Pas de gepresenteerde kolomnaam aan van een meetwaarde uit een dimView. 
#' 
#' Er wordt gerefereerd naar de meetwaarde via de gepresenteerde naam (colFrom, colTo). Alternatieve selectie van 
#' de te wijzigen kolom kan ook via de viewColumn (viewColFrom)
#' 
#' @param env sterschema object, gecreeerd met \code{\link{new.star}}.
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' 
#' @return gewijzigd sterschema-object.
#'
#' @export
#'
setColumnName <- function(env,dim,colFrom, viewColFrom = NULL,colTo) {
    ml <- env$dims[[dim]]$measList
    
    if (!is.null(viewColFrom)) {
        viewColFrom %in% ml$viewColumn || dwhrStop('viewColFrom invalid')
        colFrom <- ml$as[ml$viewColumn == viewColFrom]
    } else {
        colFrom %in% ml$as || dwhrStop('colFrom invalid')
    }
    
    if (colTo %in% ml$as)
        return()
    
    ml$as[ml$as == colFrom] <- colTo
    
    if (isNull(env$dims[[dim]]$orderColumn,'') == colFrom) {
        env$dims[[dim]]$orderColumn <- colTo
    }
    
    if (isNull(env$dims[[dim]]$orderColumn2,'') == colFrom) {
        env$dims[[dim]]$orderColumn2 <- colTo
    }
    
    env$dims[[dim]]$measList <- ml
    
    if (isNull(env$dims[[dim]]$syncNav,FALSE) && (as.character(sys.call(-3)) != 'setColumName')) {
        lapply(union(env$dims[[dim]]$parentDim,env$dims[[dim]]$childDims), function(x) setColumnName(env = env, dim = x, colFrom = colFrom, viewColFrom = viewColFrom, colTo = colTo))
    }
    env
    
}

#'
#' @export
#'
setDebug <- function(debug,debugDims = NULL) {
    glob.env$debug <- debug
    glob.env$debugDims <- debugDims
}


#'
#' @export
#'
clone.star <- function(from, toId, facts = NULL, dimViews = NULL, checkUiId = FALSE, print = FALSE) {
    
    call <- from$call
    call$starId <- toId
    
    if (!is.null(facts)) {
        call$facts <- facts
    }
    
    to <- eval(call, envir = from$ce)
    
    for (dv in names(dimViews)) {
        
        call <- from$dims[[dv]]$call
        call$env <- to
        
        if (!is.null(dimViews[[dv]]$state)) {
            call$state <- dimViews[[dv]]$state
        }
        
        eval(call, envir = from$ce)
        
        to$dims[[dv]]$print <- print
        
        if(isNull(dimViews[[dv]]$measures,TRUE)) {
            
            for(mCall in from$dims[[dv]]$measureCalls) {
                mCall$env <- to
                eval(mCall, envir = from$ce)
            }
         
            
            if(isNull(dimViews[[dv]]$derrivedMeasures,TRUE)) {
                
                for(dmCall in from$dims[[dv]]$derrivedMeasureCalls) {
                    dmCall$env <- to
                    eval(dmCall, envir = from$ce)
                }
              
                pres <- sapply(from$dims[[dv]]$presentationCalls,function(x) {x$as})
                
                if (length(pres) > 0) {
                    if (!is.null(dimViews[[dv]]$presentations)) {
                        
                        if (is.character(dimViews[[dv]]$presentations)) {
                            pres <- dimViews[[dv]]$presentations
                        } else {
                            if (is.logical(dimViews[[dv]]$presentations) && !dimViews[[dv]]$presentations) 
                                pres <- c()
                            
                        }
                    }
                    
                    for(pCall in from$dims[[dv]]$presentationCalls) {
                        if (pCall$as %in% pres) {
                            pCall$env <- to
                            pCall$checkUiId <- checkUiId
                            eval(pCall, envir = from$ce)
                        }
                    }
                }
                
            }
            
        }
    }
    
    to

}

#'
#' @export
#'
getDimViewPrepData <- function(env,dv) {
    
    call <- env$dims[[dv]]$call
    
    call$returnPrepData <- TRUE
    call$env <- env
    return(eval(call, envir = env$ce))
}

#'
#' @export
#'
navigate <- function(env, dim, level, parent, gparent = NULL) {
    
    dd <- env$dims[[dim]]
    
    if (dd$level != level || dd$parent != parent) {

        ancestors <- c(parent)

        if (level >= 1) {
            for (i in level:1) {
                p <- ancestors[1]
                p0 <- unique(dd$pc$parentLabel[dd$pc$level == i - 1 & dd$pc$label == p])
                
                if (length(p0) == 0) 
                    return(FALSE)
                
                if (length(p0) > 1) {
                    if (i == level && !is.null(gparent)) {
                        p0 <- p0[p0 == gparent]
                    }
                    p0 <- p0[1]
                }
                
                ancestors <- c(p0,ancestors)
            }
        }
        
        dd$level <- level
        dd$parent <- parent
        dd$ancestors <- ancestors
        dd$reactive$levelChange <- dd$reactive$levelChange + 1
        
    }
    
    TRUE
}

#'
#' @export
#'
runExampleDwhr <- function (example = NA, port = NULL, launch.browser = getOption("shiny.launch.browser", interactive()),
                        host = getOption("shiny.host", "127.0.0.1"), 
                        display.mode = c("auto", "normal", "showcase")) {
    
    examplesDir <- system.file("examples", package = 'dwhr')
    dir <- shiny:::resolve(examplesDir, example)

    if (is.null(dir)) {
        if (is.na(example)) {
            errFun <- message
            errMsg <- ""
        }
        else {
            errFun <- stop
            errMsg <- paste("Example", example, "does not exist. ")
        }
        errFun(errMsg, "Valid examples are '", paste(list.files(examplesDir), collapse = "', '"), "'")
    }
    else {
        shiny::runApp(dir, port = port, host = host, launch.browser = launch.browser, 
               display.mode = display.mode)
    }
}

#'
#' Wijzig stijl of zichtbaarheid van een kolom in een dataTable presentatie.
#'
#' @param env sterschema object, gecreeerd met \code{\link{new.star}}.
#' @param dim string, dimView id gecreeerd met \code{\link{addDimView}}.
#' @param pres string, naam van de dataTable presentatie zoals gecreerd met \code{\link{addPresentation}} en
#' waarin te wijzigen kolom voorkomt. 
#' @param viewColumn string, identificatie van de kolom. Dit is de naam zoals in eerste instantie meegegeven aan \code{\link{addMeasure}} 
#' of aan \code{\link{addMeasureDerrived}} of zoals die in deze functies wordt afgeleid en die bovendien voorkomt in eerder opgegeven presentatie. 
#' @param fgStyle list, specificatie van de nieuwe foreground style van deze viewColumn. List heeft zelfde structuur als bij de bgStyle parameter. 
#'
#' @export
#' 
changeDtFgStyle <- function(env,dim,pres,viewColumn,fgStyle = NULL) {
    
    withCallingHandlers({
        class(env) == 'star' || stop('env is not of class star')
        
        assert_is_a_string(dim)
        dim %in% names(env$dims) || stop('Unknown dim')
        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        
        assert_is_a_string(pres)
        
        pl <- dd$presList
        presAs <- sapply(pl,function(x) x$as)
        pres %in% presAs || dwhrStop('Prestentation does not exist')
        presNum <- which(presAs == pres)
        
        pl[[presNum]]$type == 'dataTable' || dwhrStop('Presentation is not of type dataTable')
        
        assert_is_a_string(viewColumn)
        presVc <- sapply(pl[[presNum]]$dataTableOpts$measures,function(x) x$viewColumn)
        viewColumn %in% presVc || dwhrStop('viewColumn invalid')
        measNum <- which(presVc == viewColumn)

        if (!is.null(fgStyle)) {
            fgStyle  <- .setStyle(fgStyle)
        }
    },
    
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$fgStyle.cuts <- NULL
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$fgStyle.values <- NULL
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$fgStyle.levels <- NULL
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$fgStyle.valueColumn <- NULL
    
    if (!is.null(fgStyle)) {
        pl[[presNum]]$dataTableOpts$measures[[measNum]]$fgStyle <- fgStyle
        pl[[presNum]]$dataTableOpts$measures[[measNum]] <- rlist::list.flatten(pl[[presNum]]$dataTableOpts$measures[[measNum]])
    }   
    
    dd$presList <- pl
    
}

#' @rdname changeDtFgStyle
#' 
#' @param bgStyle list, specificatie van de nieuwe background style van deze viewColumn. List kent volgende waarden:
#' \itemize{
#'    \item cuts: numeric, vector van waarden waar de kolomwaarde over verdeeld wordt.
#'    \item levels: numeric, vector van waarden waar de kolomwaarde mee overeen moet stemmen.  Of cuts wordt opgegven of levels, 1 van beiden. 
#'    \item values: character, vector met kleuren waar de kolomwaarde op afgebeeld wordt via ofwel cuts ofwel levels. In het geval van cuts moet de vector van kleuren 
#' 1 langer zijn dan de vector cuts. Bij gebruik van levels moet de vector van kleuren even lang zijn als de levels-vector.
#'    \item valueColumn: Normaal wordt de waarde uit de viewColumn zelf gehaald, de waarde kan echter ook uit een andere viewColumn komen.
#'}
#'
#' @export
#' 
changeDtBgStyle <- function(env,dim,pres,viewColumn,bgStyle = NULL) {
    
    withCallingHandlers({
        class(env) == 'star' || stop('env is not of class star')
        
        assert_is_a_string(dim)
        dim %in% names(env$dims) || stop('Unknown dim')
        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        
        assert_is_a_string(pres)
        
        pl <- dd$presList
        presAs <- sapply(pl,function(x) x$as)
        pres %in% presAs || dwhrStop('Prestentation does not exist')
        presNum <- which(presAs == pres)
        
        pl[[presNum]]$type == 'dataTable' || dwhrStop('Presentation is not of type dataTable')
        
        assert_is_a_string(viewColumn)
        presVc <- sapply(pl[[presNum]]$dataTableOpts$measures,function(x) x$viewColumn)
        viewColumn %in% presVc || dwhrStop('viewColumn invalid')
        measNum <- which(presVc == viewColumn)
        
        if (!is.null(bgStyle)) {
            bgStyle  <- .setStyle(bgStyle)
        }
    },
    
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$bgStyle.cuts <- NULL
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$bgStyle.values <- NULL
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$bgStyle.levels <- NULL
    pl[[presNum]]$dataTableOpts$measures[[measNum]]$bgStyle.valueColumn <- NULL
    
    if (!is.null(bgStyle)) {
        pl[[presNum]]$dataTableOpts$measures[[measNum]]$bgStyle <- bgStyle
        pl[[presNum]]$dataTableOpts$measures[[measNum]] <- rlist::list.flatten(pl[[presNum]]$dataTableOpts$measures[[measNum]])
    }   
    
    dd$presList <- pl
    
}

#' @rdname changeDtFgStyle
#' 
#' @param visible boolean, controleert de zichtbaarheid van een datatable kolom
#' 
#' @export
#'
setDtVisible <- function(env,dim,pres,viewColumn,visible) {
    
    withCallingHandlers({
        class(env) == 'star' || stop('env is not of class star')
        
        assert_is_a_string(dim)
        dim %in% names(env$dims) || stop('Unknown dim')
        dd <- env$dims[[dim]]
        class(dd) == 'dimView' || stop('dim is not of class dimView')
        
        assert_is_a_string(pres)
        
        pl <- dd$presList
        presAs <- sapply(pl,function(x) x$as)
        pres %in% presAs || dwhrStop('Prestentation does not exist')
        presNum <- which(presAs == pres)
        
        pl[[presNum]]$type == 'dataTable' || dwhrStop('Presentation is not of type dataTable')
        
        assert_is_character(viewColumn)
        presVc <- sapply(pl[[presNum]]$dataTableOpts$measures,function(x) x$viewColumn)
        viewColumn %in% presVc || dwhrStop('viewColumn invalid')
        measNum <- which(presVc %in% viewColumn)
        
        assert_is_a_bool(visible)
    },
    
    error = function(c) {
        dwhrStop(conditionMessage(c))
    })
    
    for (x in measNum)
        pl[[presNum]]$dataTableOpts$measures[[x]]$visible <- visible
    
    dd$presList <- pl
    
}

topx <- function(env,dim,x,orderBy,restCatName) {
    
    dd <- env$dims[[dim]]
    
    tab <- dd$membersFiltered
    lvl <- dd$level
    col <- paste0('level',lvl,'Label')
    code <- paste0('level',lvl,'Code')
    dt <- dd$data
    
    tab <- tab[order(tab[[orderBy]],decreasing = TRUE, method = 'radix'),]
    
    if (nrow(tab) > x) {
        
        z <- tab[(x + 1):nrow(tab),]$member
    
        dt[dt[[col]] %in% z,][[code]] <- 'xxx'
        dt[dt[[col]] %in% z,][[col]] <- restCatName
        altData <- getMembers(env, dim, altData = dt)
        
        altData$body <- altData$body[c(
            setdiff(
                order(altData$body[[orderBy]],decreasing = TRUE, method = 'radix'),
                which(altData$body$member == restCatName)),
            which(altData$body$member == restCatName)),]
        
        prep <- prepDt(env = env, dim = dim, pres = dd$pres, altData = altData)
      
    } else {
        
        prep <- prepDt(env = env, dim = dim, pres = dd$pres, altData = list(body = tab, footer = dd$footer))
    }
    
    prep
    
}

