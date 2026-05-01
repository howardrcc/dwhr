library(shiny)
library(shinyjs)
library(dwhr)
library(shinyjqui)

wikiUrl <- "https://example.com/westeros-realm-indicators"

title <- 'Westeros Realm Indicators'

fluidPage(
    dwhrInit(),
    inlineCSS('
#maatr + div, #meetp + div {
    margin-bottom: 0;
}
#s1KpiDimLinks .form-group {
    margin-bottom: 0;
    margin-top: 3px;
}
#s1MndDimSimple {
    margin-bottom:23px;
}
table.dataTable tr.dtrg-group td {
    background-color: white !important;
}
.modal-lg { 
    width: 70% !important; 
}
body {
    overflow: hidden;
}
.container-fluid {
    padding-right: 0px;
}
.tabRed { 
    color: red; 
}
.selectize-dropdown [data-value=\"Groen\"], .selectize-dropdown [data-value=\" Groen\"] {
    background: Lightgreen !important; 
}
.selectize-dropdown [data-value=\"Oranje\"], .selectize-dropdown [data-value=\" Oranje\"] {
    background: #FECE00 !important; 
}
.selectize-dropdown [data-value=\"Rood\"], .selectize-dropdown [data-value=\" Rood\"] {
    background: red !important; 
}
.selectize-dropdown-content {
    max-height: 700px !important;
}
.glyphicon.glyphicon-menu-hamburger {
    font-size: 30px;
    margin-right: 0px;
}
'),
    if (!batchMode) 
        div(id = "app-content",
            fluidRow(
                column( 
                    width = 5,
                    div(id = 'header-left',
                        HTML(paste0(
                            '<table class = "db-header" width = "100%">'
                            , '<tbody>'
                            , '<tr>'
                            , '<td class="db-header"><h3 class="db-header">', title, '</h3></td>'
                            , '<td class="db-header"><a href = "', portalUrl(), '" target="_blank">BI startpunt</a></td>'
                            , '<td class="db-header"><a href="', wikiUrl, '" target="_blank"><img src="dwhRs/info-sign.png" height="16"></a>'
                            , '&nbsp&nbsp<a href="', wikiUrl, '" target="_blank">Wiki</a></td>'
                            , '<td class="db-header" style="padding-top: 60px"></td>'
                            , '</tr></tbody></table>'))
                        , style = "font-size: 100%; width: 100%;")),
                column( 
                    width = 7
                    , div( id = 'header-right',
                           HTML(paste0(
                               '<table class = "db-header" width = "100%">'
                               , '<tbody>'
                               , '<tr>'
                               , '<td class="db-header" style="width:400px">', getDimUI(starId = 's1', dim = 'mnd', skipTopRow = TRUE), '</td>'
                               , '<td class="db-header" style="padding-top: 0px"><a href="javascript:void(0);" onclick="startIntro();">intro</a></td>'
                               , '<td class="db-header" style="width:120px;text-align:right"><div class="logo-container"><img src = "got-logo.svg" width = "160px" height = "60px"><div class="logo-bottom-center">Citadel Records</div></div></td>'
                               , '<td class="db-header" style="width:70px;padding-top: 0px;text-align:right;padding-right:15px">', actionButton('pref',NULL,icon = icon("menu-hamburger", lib = "glyphicon"), style='border: none;',
                                                                                                                                                'data-toggle' = "tooltip",
                                                                                                                                                'data-container' = "body",
                                                                                                                                                'data-placement' = "bottom",
                                                                                                                                                title = "Persoonlijke voorkeuren",),'</td>'
                               , '</tr></tbody></table>'))
                           , style = "font-size: 100%; width: 100%;")
                )),
            jqui_resizable(
                options = list(handles = 'e', minWidth = 400, maxWidth = 1200),
                div(id = 'leftpane0',
                    style = 'width:41.6%;display: inline-block;border-right-style:solid; border-right-width:1px',
                    div(id = 'leftpane',
                        # Debug-only: dwhr exposes a `browser()` button when
                        # output$browser is truthy. Commented out for the
                        # public demo so contributors aren't shown a debug
                        # entry point. Uncomment to re-enable for dev.
                        # conditionalPanel(
                        #     condition = "output.browser",
                        #     actionButton(inputId = 'browser','browser()')),
                        getDimUI(starId = 's1', dim = 'kpi',accordion = TRUE),
                        h5('Afdeling / Periode / Geldstroom', style = 'background:#f9fafc;padding-top: 8px;padding-bottom: 8px'),
                        tabsetPanel(
                            id= 'afdgs',
                            tabPanel(
                                title = 'Afdeling',
                                getDimUI(starId = 's1', dim = 'kpl',skipTopRow = TRUE)),
                            tabPanel(
                                title = 'Periode',
                                uiOutput('perLink',style = 'text-align:right'),
                                getDimUI(starId = 's1', dim = 'per',skipTopRow = TRUE)),
                            tabPanel(
                                title = 'Geldstroom',
                                getDimUI(starId = 's1', dim = 'gs', skipTopRow = TRUE))),
                        div(id = 'filler', style = "max-height = 500px; min-height: 500px; height: 500px"),
                        style = "overflow-y:auto; max-height = 850px; min-height: 850px; height: 850px;"))),
            div(id = 'rightpane',
                style = "width:58.3%;padding-right:20px;display: inline-block;float:right;",
                getDimUI(starId = 's1', dim = 'perInst', skipTopRow = TRUE),
                getDimUI(starId = 's1', dim = 'kpl2', skipTopRow = TRUE),
                getDimUI(starId = 's1', dim = 'perAfd', skipTopRow = TRUE)
            ),
            style = "font-size: 85%; width: 100%")
    else '',   
    tags$head(tags$script(src= 'intro.js')),
    title = title
)

