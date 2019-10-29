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

    init_vals = reactiveValues()
    init_vals$enable_facets = FALSE
    init_vals$enable_unitconvert = FALSE

    observeEvent(input$COLLAPSE_SIDEBAR, {
        shinyjs::toggleClass(selector='.sidebar-sub',
            class='sidebar-sub-gone')
        shinyjs::toggleClass(selector='.content-wrapper',
            class='content-wrapper-wide')
    })

    map_buttons = read_file('ui/map_buttons.html')
    source('ui/landing_page_ui.R', local=TRUE)
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
        length(unique(site_data$stream))
    })
    output$NSITES = renderText({
        nrow(site_data)
    })
    output$NOBS = renderText({
        nrow(grab) + nrow(Q) + nrow(flux) + nrow(P)
    })

    observeEvent(once=TRUE, ignoreNULL=FALSE, ignoreInit=FALSE,
            eventExpr=TRUE, {
        landing_page
        init_vals$enable_unitconvert = TRUE
    })

    observeEvent(once=TRUE, ignoreNULL=FALSE, ignoreInit=TRUE,
            eventExpr=input$SOLUTES4, {
        init_vals$enable_facets = TRUE
    })

    observeEvent(input$MAPDATA, {

        map_selection = str_match(input$MAPDATA, '__(.+?)_goto$')[,2]
        updateSelectizeInput(session, 'SITES4',
            label=NULL, selected=map_selection, choices=default_sites) #temporary

        session$sendCustomMessage('flash_plot', jsonlite::toJSON('placeholder'))
        # input_vals$flash_plot = input_vals$flash_plot + 1
    })

    # observeEvent(once=TRUE, ignoreNULL=FALSE, ignoreInit=FALSE,
    #         eventExpr=TRUE, {
    #
    # })

    observeEvent(input$DISMISS_LANDING, {
        removeModal(session)
    })

})
