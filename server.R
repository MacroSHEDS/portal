# library(shiny)
# library(readr)
# library(shinyjs)

options(shiny.usecairo=TRUE)

hyetograph_file = 'js/hyetograph.js'
hyetograph_js = readChar(hyetograph_file, file.info(hyetograph_file)$size)

shinyServer(function(input, output, session){

    # #hacky way to specify div height by % with js
    # height50 = reactive({
    #     ifelse(is.null(input$height50), 0, input$height50)
    # })
    # js$getHeight50()

    landing_page_trigger = TRUE

    observeEvent(input$COLLAPSE_SIDEBAR, {
        shinyjs::toggleClass(selector='.sidebar-sub',
            class='sidebar-sub-gone')
        shinyjs::toggleClass(selector='.content-wrapper',
            class='content-wrapper-wide')
    })

    source('server/site_comparison_server.R', local=TRUE)
    source('server/oneSiteNVar_server.R', local=TRUE)
    source('server/nSiteNVar_server.R', local=TRUE)
    source('server/summary_biplot_server.R', local=TRUE)
    source('server/map_server.R', local=TRUE)

    #register clicking of map popup links
    observeEvent(input$SITE_EXPLORE, {
        updateTabsetPanel(session, "right_tabs", selected="site_exploration")
    })

    output$NSTREAMS = renderText({
        15
    })
    output$NSITES = renderText({
        30
    })
    output$NOBS = renderText({
        'a zillion'
    })

    observeEvent(once=TRUE,ignoreNULL=FALSE, ignoreInit=FALSE,
            eventExpr=landing_page_trigger, {
        showModal(
            modalDialog(title=NULL, footer=NULL,
                fluidRow(class='text-center',
                    h1('macrosheds'),
                    # img(src='macrosheds_logo.svg',
                    #     style='height: 70px; width: 200px'),
                    br(),
                    br(),
                    column(4,
                        img(src='water-solid-396060.svg',
                            style='height: 56px; width: 56px'),
                        h1(textOutput('NSTREAMS')),
                        p('STREAMS')
                    ),
                    column(4,
                        icon('map-marker', class='fa-4x'),
                        h1(textOutput('NSITES')),
                        p('SITES')
                    ),
                    column(4,
                        icon('chart-bar', class='fa-4x'),
                        h1(textOutput('NOBS')),
                        p('OBSERVATIONS')
                    ),
                    br(),
                    br(),
                    p('hear iz data foar rivars'),
                    actionButton('DISMISS_LANDING', label='Continue')
                )
            )
        )
    })

    observeEvent(input$DISMISS_LANDING, {
        removeModal(session)
    })

})
