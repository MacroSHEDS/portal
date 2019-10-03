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

    # landing_page = read_file('ui/landing_page_ui.R')
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
        nrow(grab) + nrow(sensor)
    })

    observeEvent(once=TRUE,ignoreNULL=FALSE, ignoreInit=FALSE,
            eventExpr=landing_page_trigger, {
        # eval(parse(text=landing_page))
        landing_page
    })

    observeEvent(input$DISMISS_LANDING, {
        removeModal(session)
    })

})
