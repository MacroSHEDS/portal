
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

shinyUI(
    fluidPage(

        #screen shouldn't go gray when plots are updating.
        tags$style(type="text/css", ".recalculating { opacity: 1.0; }" ),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text=get_plotheight,
            functions=c('getHeight50', 'init')),
        navbarPage(title=p(strong(a('MacroSHEDS',
            href='https://macrosheds.org/'))), inverse=TRUE,
            windowTitle='MacroSHEDS',
            tabPanel('About',
                p("Looking for some river data? Here's all of it.")
            ),
            tabPanel(HTML('Biplot Supreme'),
                fluidRow(
                    column(12, align='left',
                        div(align='center', style=paste0(
                                'display: inline-block;',
                                'vertical-align:middle;'),
                            p('Info and stuff; maybe some options')
                        )
                    )
                ),
                fluidRow(
                    plotOutput('MAIN_BIPLOT', brush='biplot_brush')
                        # height='auto', width='auto')
                )
            )
        )
    )
)
