# library(shiny)
# library(readr)
# library(shinyjs)

options(shiny.usecairo=TRUE)

hyetograph_file <- 'www/js/hyetograph.js'
hyetograph_js <- readChar(con = hyetograph_file,
                         nchars = file.info(hyetograph_file)$size)

server <- function(input, output, session){

    guide1$init()
    guide2$init()
    guide3$init()

    # #hacky way to specify div height by % with js
    # height50 = reactive({
    #     ifelse(is.null(input$height50), 0, input$height50)
    # })
    # js$getHeight50()

    init_vals <- reactiveValues()
    # init_vals$enable_facets = FALSE
    init_vals$enable_unitconvert <- FALSE
    init_vals$recent_domain <- 'hbef'

    observeEvent(input$COLLAPSE_SIDEBAR, {

        shinyjs::toggleClass(selector = '.sidebar-sub',
                             class = 'sidebar-sub-gone')

        shinyjs::toggleClass(selector = '.content-wrapper',
                             class = 'content-wrapper-wide')
    })

    stream_gauge_buttons <- read_file('ui/stream_gauge_buttons.html')
    rain_gauge_buttons <- read_file('ui/rain_gauge_buttons.html')
    source('ui/landing_page_ui.R', local = TRUE)
    source('server/summary_biplot_server.R', local = TRUE)
    # source('server/oneSiteNVar_server.R', local=TRUE)
    source('server/nSiteNVar_server.R', local = TRUE)
    source('server/map_server.R', local = TRUE)
    source('ui/data_ui.R', local = TRUE)
    source('server/data_server.R', local = TRUE)

    #register clicking of map popup links
    observeEvent(input$SITE_EXPLORE, {
        #updateTabsetPanel(session, "right_tabs", selected="site_exploration")
        updateTabsetPanel(session,
                          "right_tabs",
                          selected = "multisite_exploration")
    })

    output$NSTREAMS <- renderText({

        nrow(unique(site_data[, c('stream', 'domain')]))
    })

    output$NSITES <- renderText({

        sum(site_data$site_type %in% c('stream_gauge', 'stream_sampling_point'),
            na.rm = TRUE)
    })

    output$NOBS <- renderText({

        readr::read_file('data/general/total_nonspatial_observations.txt') %>%
            as.numeric() %>%
            round(-4) %>%
            format(scientific = FALSE,
                   big.mark = ',')
    })

    observeEvent(once = TRUE,
                 ignoreNULL = FALSE,
                 ignoreInit = FALSE,
                 eventExpr = TRUE,
                 handler.quoted = TRUE,
                 handlerExpr = landing_page
                    # init_vals$enable_unitconvert = TRUE
                # }
        )

    # observeEvent(once=TRUE, ignoreNULL=FALSE, ignoreInit=TRUE,
    #         eventExpr=input$VARS3, handlerExpr = {
    #     init_vals$enable_facets = TRUE
    # })

    observeEvent(input$MAPDATA, {

        map_selection <- str_match(input$MAPDATA, '(.+?)__(.+?)_goto.*$')[,2:3]

        domain_sel <- map_selection[1]
        site_sel <- map_selection[2]

        dmn_sitelist <- get_sitelist(domain = domain_sel,
                                     type = c('stream_gauge',
                                              'stream_sampling_point'))

        updateSelectizeInput(session = session,
                             inputId = 'DOMAINS3',
                             label = NULL,
                             selected = domain_sel,
                             choices = domains_pretty)

        updateSelectizeInput(session = session,
                             inputId = 'SITES3',
                             label = NULL,
                             selected = site_sel,
                             choices = dmn_sitelist)
        
        click('GEN_PLOTS3')

        session$sendCustomMessage('flash_plot',
                                  jsonlite::toJSON('placeholder'))
        # input_vals$flash_plot = input_vals$flash_plot + 1
    })

    observeEvent(
        eventExpr = {
            input$DISMISS_MODAL
            # input$DISMISS_VARIABLE_CATALOG
            # input$DISMISS_VARIABLE_SUBCATALOG
            # input$DISMISS_SITE_CATALOG
            # input$DISMISS_SITE_SUBCATALOG
            # input$DISMISS_LANDING
        },
        handlerExpr = {
            removeModal(session)
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )

     observeEvent(
         eventExpr = input$TAKE_TOUR,
         handlerExpr = {
             guide1$start()
         },
         autoDestroy = FALSE,
         ignoreInit = TRUE
     )
     observeEvent(
         eventExpr = input$CONTINUE_TOUR,
         handlerExpr = {
             guide2$start()
         },
         autoDestroy = FALSE,
         ignoreInit = TRUE
     )
     observeEvent(
         eventExpr = input$START_DATA_TOUR,
         handlerExpr = {
             guide3$start()
         },
         autoDestroy = FALSE,
         ignoreInit = TRUE
     )

    observeEvent(
        eventExpr = input$BACK_TO_VARIABLE_CATALOG,
        handlerExpr = {
            removeModal(session)
            shinyjs::click('VARIABLE_CATALOG_BUTTON')
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )

    observeEvent(
        eventExpr = input$BACK_TO_SITE_CATALOG,
        handlerExpr = {
            removeModal(session)
            shinyjs::click('SITE_CATALOG_BUTTON')
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )
}
