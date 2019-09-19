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

    # output$COLLAPSIBLE_SECTION=renderMenu({
    #     sidebarMenu(
    #         # menuItem('Dashboard', tabName='dashboard', icon=icon('dashboard')),
    #         # menuItem('Widgets', icon=icon('th'), tabName='widgets',
    #         #     badgeLabel='new',
    #         #     badgeColor='green'
    #         # ),
    #         # menuItem('Charts', icon=icon('bar-chart-o'),
    #         #     menuSubItem('Sub-item 1', tabName='subitem1'),
    #         #     menuSubItem('Sub-item 2', tabName='subitem2')
    #         # )
    #     )
    # })

    # collapse_bool = reactiveValues()
    # collapse_bool$val = FALSE

    observeEvent(input$COLLAPSE_SIDEBAR, {
        # shinyjs::hide(selector = ".main-sidebar");
        # shinyjs::addClass(selector = "body", class = "sidebar-collapse")

        # collapse_bool$val = ! collapse_bool$val
        # if(collapse_bool$val){
        shinyjs::toggleClass(selector='.sidebar-sub',
            class='sidebar-sub-gone')
        shinyjs::toggleClass(selector='.content-wrapper',
            class='content-wrapper-wide')
        # }
        # } else {
        #     shinyjs::addClass(selector='.sidebar-sub', class='sidebar-sub-gone')
        # }
    })

    source('server/site_comparison_server.R', local=TRUE)
    source('server/oneSiteNVar_server.R', local=TRUE)
    source('server/summary_biplot_server.R', local=TRUE)
    source('server/map_server.R', local=TRUE)

    #register clicking of map popup links
    observeEvent(input$SITE_EXPLORE, {
        updateTabsetPanel(session, "right_tabs", selected="site_exploration")
    })

})
