library(shiny)
library(dwhr)
library(data.table)
library(magrittr)

checkVersion('dwhr','1.2')

# inlezen data

rDataFile <- paste0(getwd(),"/tmp/kpiRvb.RData") 

load(rDataFile)
source('func.R', local = TRUE)

comments <- initComments()
score <- initScore()

source('columnChart.R', local = TRUE)

shinyServer(function(input, output,session) {
    
    shinyjs::runjs("Shiny.onInputChange('userAgent',{agent: navigator.userAgent});")
    
    agent <- ''
    
    observeEvent(input$userAgent,{
        agent <<- input$userAgent[[1]]
        print(agent)
    })
    
    shinyjs::runjs("Shiny.onInputChange('windowHeight',{height: window.innerHeight});")
    shinyjs::runjs("Shiny.onInputChange('windowWidth',{width: window.innerWidth});")
    
    observeEvent(input$windowHeight,{
        headerHeight <- 90
        height <- input$windowHeight[[1]] - headerHeight
        
        shinyjs::runjs(paste0('$("#leftpane").css("max-height",',height,');'))
        shinyjs::runjs(paste0('$("#leftpane").css("min-height",',height,');'))
        shinyjs::runjs(paste0('$("#leftpane").css("height",',height,');'))
        
        
        for (g in c('s1PerInst','s1Kpl2','s1PerAfd')) {
            js$hcSetHeight(gdim = g,height = (height / 3),source = '')
        }
        
        for (d in c('perInst','kpl2','perAfd')) {
            s1$dims[[d]]$presList$highCharts1$height <- (height / 3)
            s1$dims[[d]]$presList$highCharts2$height <- (height / 3)    
        }
        
    })
    
    observeEvent(input$windowWidth,{
        shinyjs::runjs(paste0('$("#leftpane0").css("width","41.6%");'))
        shinyjs::runjs(paste0('$("#rightpane").css("width","58.3%");'))
    })
    
    authenticate(session) 
    
    userData <- session$userData
    dashUser <- userData$dashUser
    dashUserName <- userData$dashUserName
    dashUserFunc <- userData$dashUserFunc
    
    comments <<- updateComments()
    score <<- initScore()
    
    source('pref.R',local = TRUE)
    initDashOpts(userData)

    s1 <- new.star(
        starId = 's1',
        session = session,
        facts = facts,
        foreignKeyCheck = FALSE) 
    
    getKpiProps(env = s1, init = TRUE) 

    initKpi <- s1$kpiProps$kpiLabel
    initLvl <- s1$kpiProps$kpiLvl
    initParent <- s1$kpiProps$kpiParent
    
    initFormat <- s1$kpiProps$format
    initSort <- s1$kpiProps$sort
    initNormNaam <-  s1$kpiProps$normNaam
    initVerschilNaam <- s1$kpiProps$verschilNaam
    initRealNaam <- s1$kpiProps$realNaam

    source('gs.R',local = TRUE)
    source('mnd.R',local = TRUE)
    source('kpi.R',local = TRUE)
    source('perInst.R',local = TRUE)
    source('kpl.R',local = TRUE)
    source('perAfd.R',local = TRUE)
    
    renderDims(s1,input,output)

    source('print.R', local = TRUE)
    source('maatregel.R', local = TRUE)
    source('voortgang.R', local = TRUE)
    source('conc.R', local = TRUE)
    source('opmerking.R', local = TRUE)
    source('maandrapportage.R',local = TRUE)
    source('samenvatting.R',local = TRUE)
    source('meetplan.R',local = TRUE)

    #
    # batchMode gelateerde variabelen
    #
    
    if (batchMode) {
        
        if (batchArgs[1] == 'MP')
            batchMP(batchArgs)
        
        if (batchArgs[1] == 'MR')
            batchMR(batchArgs)
        
    } 
    
    sessionEndHook <- function(session) {
         if(!is.null(session$userData$lock)) {
             print('releasing lock')
             releaseLock(s1,session$userData$lock)
         }
    }
    
    observeEvent(input$afdgs,{
        if(input$afdgs == 'Geldstroom') {
            lapply(c('kpl','per'),function(x) { dimChangeState(s1,x,'hidden') })
            dimChangeState(s1,'gs','enabled')
        } 
        
        if(input$afdgs == 'Afdeling') {
            lapply(c('gs','per'),function(x) { dimChangeState(s1,x,'hidden') })
            dimChangeState(s1,'kpl','enabled')
            s1$dims[['kpl']]$reactive$dimRefresh <- s1$dims[['kpl']]$reactive$dimRefresh + 1 
        }
        
        if(input$afdgs == 'Periode') {
            lapply(c('gs','kpl'),function(x) { dimChangeState(s1,x,'hidden') })
            dimChangeState(s1,'per','enabled')
            s1$dims[['per']]$reactive$dimRefresh <- s1$dims[['per']]$reactive$dimRefresh + 1 
        }
    })
    
    observeEvent({
        input$leftpane0_size
        input$leftpane0_is_resizing
    },{

        width <- input$leftpane0_size$width

        if (is.null(input$windowWidth[[1]]))
            return()

        rwidth <- input$windowWidth[[1]] - width -50

        shinyjs::runjs(paste0('$("#rightpane").css("width","',rwidth,'px")'))

    })
    
    blockTab <- function(dd) {
        if (!dd == 'kpl')
            shinyjs::hide(selector = "li a[data-value=Afdeling]")
        if (!dd == 'per')
            shinyjs::hide(selector = "li a[data-value=Periode]")
        if (!dd == 'gs')
            shinyjs::hide(selector = "li a[data-value=Geldstroom]")
    }
    
    unBlockTab <- function(dd) {
        if (!dd == 'kpl')
            shinyjs::show(selector = "li a[data-value=Afdeling]")
        if (!dd == 'per')
            shinyjs::show(selector = "li a[data-value=Periode]")
        if (!dd == 'gs' && unique(kpi[kpi$kpiLabel == s1$dims[['kpi']]$selected$label,]$gs))
            shinyjs::show(selector = "li a[data-value=Geldstroom]")
    }   
    
    observeEvent( input[['s1PerDimWait']],{
        
        if (input[['s1PerDimWait']]) 
            blockTab('per')
        else 
            unBlockTab('per')
            
    })
    
    
})

