
source("helpers.R")
source('ui/oneSiteNVar_ui.R')
source('ui/site_comparison_ui.R')
source('ui/about_ui.R')
source('ui/summary_biplot_ui.R')
source('ui/map_ui.R')

shinyUI(fluidPage(

    #screen shouldn't go gray when plots are updating.
    # tags$style(type="text/css", ".recalculating { opacity: 1.0; }" ),
    tags$head(tags$style(HTML(
        "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css")),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script='js/general.js'),
    # navbarPage(title=p(strong(a('MacroSHEDS',
    #     href='https://macrosheds.org/'))), inverse=TRUE,
    #     windowTitle='MacroSHEDS',

    sidebarLayout(
        sidebarPanel(width=4,
            tabsetPanel(id='left_tabs',
                about_tab,
                map_tab
            )
        ),
        mainPanel(width=8,
            tabsetPanel(id='right_tabs',
                summary_biplot_tab,
                oneSiteNVar_tab,
                site_comparison_tab
            )
        )
    )
))
