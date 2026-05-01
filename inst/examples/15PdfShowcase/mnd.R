s1 <- s1  %>% 
    addDimView(
        dim = 'mnd',
        name = '',
        data = inMaand,
        type = 'input',
        levelNames = c('All','Kies'),
        useLevels = 1,
        selectLabel = userData$dashOpts$mndSel$label,
        initLevel = 1,
        ignoreDims = c('perAfd'),
        leafOnly = TRUE) %>%
    addPresentation(
        dim = 'mnd', 
        type = 'radioButton', 
        as = 'whatever', 
        isDefault = TRUE, 
        checkUiId = FALSE,
        navOpts = list(
            hideBreadCrumb = TRUE
        ),
        simpleOpts = list(
            inline = TRUE))

mndSelectChangeHook <- function(env) {
    
    if (env$id != 's1')
        return()
    
    mndSel <- env$dims[['mnd']]$selected$label
    
    for (d in c('perInst','perAfd')) {
        
        presList <- env$dims[[d]]$presList
        
        if (mndSel == 'Within the Moon') {
            presList[[1]]$highChartsOpts$series[['waardeTrend']]$showInLegend <- TRUE
        } else {
            presList[[1]]$highChartsOpts$series[['waardeTrend']]$showInLegend <- FALSE
        }
        
        env$dims[[d]]$presList <- presList
        
    }
    
    if (mndSel == 'Within the Moon' && env$dims[['kpi']]$selected$label != 'Unique Smallfolk') {
        env$dims[['per']]$selectMode <- 'multi'
        env$dims[['per']]$reactive$presChange <- env$dims[['per']]$reactive$presChange + 1
    } else {
        env$dims[['per']]$selectMode <- 'single'
        env$dims[['per']]$reactive$presChange <- env$dims[['per']]$reactive$presChange + 1
    }
}


s1 <- s1  %>% 
    addDimView(
        dim = 'mnd4',
        name = 'mnd4',
        data = mnd4,
        type = 'input',
        levelNames = c('All','Kies'),
        useLevels = 1,
        selectLabel = 'mnd4',
        selectLevel = 1,
        state = ifelse(userData$dashOpts$perState == 'tertiaal','enabled','disabled'))

