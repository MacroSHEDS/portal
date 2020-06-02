
source("helpers.R")
#source('ui/oneSiteNVar_ui.R')
source('ui/nSiteNVar_ui.R')
# source('ui/site_comparison_ui.R')
source('ui/about_ui.R')
source('ui/participants_ui.R')
# source('ui/summary_biplot_ui.R')
source('ui/map_ui.R')
source('ui/now_hiring_ui.R')

ui = fluidPage(
# shinyUI(fluidPage(

    #screen shouldn't go gray when plots are updating.
    # tags$style(type="text/css", ".recalculating { opacity: 1.0; }" ),
    tags$head(tags$style(HTML(
        "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css")),
    useShinyjs(),
    extendShinyjs(script='js/general.js'),

    dashboardPage(
        dashboardHeader(disable=TRUE),
        dashboardSidebar(width='40%',
            div(class='sidebar-sub',
                # HTML('<input type="text" id="MAPDATA" style="display: none">'),
                tabsetPanel(id='left_tabs',
                    # now_hiring_tab,
                    about_tab,
                    participants_tab,
                    map_tab
                )
            ),
            div(style='width: 36px; display: inline-block; float: right',
                actionLink('COLLAPSE_SIDEBAR', label='', icon=icon('arrows-h'),
                    class='sidebar-toggle', `data-toggle`='offcanvas',
                    style='margin: 6px')
            )
        ),
        dashboardBody(
            tags$head(
                tags$link(rel='stylesheet', type='text/css', href='style.css')
            ),
            tabsetPanel(id='right_tabs',
                # summary_biplot_tab,
                # oneSiteNVar_tab,
                nSiteNVar_tab
                # site_comparison_tab
            )
        )
    )
)
# ))
