
#load plot functions
source("helpers.R")

get_plotheight = "
shinyjs.init = function() {
  $(window).resize(shinyjs.getHeight50);
}

//shinyjs.calcHeight = function(propHeight) {
//  var h = $(window).height() * propHeight;
//  Shiny.onInputChange('plotHeight', Number(h.toFixed(0)));

shinyjs.getHeight50 = function() {
  Shiny.onInputChange('height50', $(window).height() * .5);
}
"

source('ui/timeseries_ui.R')
source('ui/site_comparison_ui.R')
source('ui/about_ui.R')
source('ui/summary_biplot_ui.R')
source('ui/map_ui.R')

shinyUI(fluidPage(

    #screen shouldn't go gray when plots are updating.
    tags$style(type="text/css", ".recalculating { opacity: 1.0; }" ),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text=get_plotheight,
        functions=c('getHeight50', 'init')),
    # navbarPage(title=p(strong(a('MacroSHEDS',
    #     href='https://macrosheds.org/'))), inverse=TRUE,
    #     windowTitle='MacroSHEDS',

    sidebarLayout(
        sidebarPanel(width=4,
            tabsetPanel(
                about_tab,
                map_tab
            )
        ),
        mainPanel(width=8,
            tabsetPanel(id='tabs',
                summary_biplot_tab,
                timeseries_tab,
                site_comparison_tab
            )
        )
    )
))
