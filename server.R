# library(shiny)
# library(readr)
# library(shinyjs)

options(shiny.usecairo = TRUE)

hyetograph_file <- "www/js/hyetograph.js"
hyetograph_js <- readChar(
    con = hyetograph_file,
    nchars = file.info(hyetograph_file)$size
)

server <- function(input, output, session) {
    guide1a$init()
    guide1b$init()
    guide2a$init()
    guide2b$init()

    # #hacky way to specify div height by % with js
    # height50 = reactive({
    #     ifelse(is.null(input$height50), 0, input$height50)
    # })
    # js$getHeight50()

    init_vals <- reactiveValues()
    init_vals$enable_unitconvert <- FALSE
    init_vals$recent_domain <- "hbef"
    init_vals$basedata_change_reloads_plots <- FALSE
    init_vals$initial_plots_loaded <- FALSE
    init_vals$ts_tab_is_pristine <- TRUE

    observeEvent(input$COLLAPSE_SIDEBAR, {
        shinyjs::toggleClass(
            selector = ".sidebar-sub",
            class = "sidebar-sub-gone"
        )
        shinyjs::toggleClass(
            selector = ".content-wrapper",
            class = "content-wrapper-wide"
        )
    })

    observeEvent(input$COLLAPSE_DATA, {
        shinyjs::toggleClass(
            selector = ".data-sub",
            class = "data-sub-gone"
        )
        shinyjs::toggleClass(
            selector = ".content-wrapper",
            class = "data-wrapper-wide"
        )
        shinyjs::toggleClass(
            selector = ".main-sidebar",
            class = "main-sidebar-wide"
        )
        shinyjs::toggleClass(
            selector = "#data-toggler",
            class = "data-toggled"
        )
    })

    observeEvent(input$COLLAPSE_ATTRIBUTES, {
        # shinyjs::toggleClass(
        #     selector = "#attribute-content",
        #     class = "hidden"
        # )
        shinyjs::toggleClass(
            selector = "#MAP",
            class = "fullmap"
        )
    })


    stream_gauge_buttons <- read_file("ui/stream_gauge_buttons.html")
    rain_gauge_buttons <- read_file("ui/rain_gauge_buttons.html")
    loading_dots <- HTML(read_file("ui/loading_dots.html"))

    show_loading_dots <- function(id, duration = NULL, message = NULL) {
        msg <- if (is.null(message)) "" else message

        showNotification(loading_dots,
            ui = message,
            id = id,
            duration = duration,
            closeButton = FALSE,
            type = "message"
        )
    }

    # source("ui/loading_dots_ui.R", local = TRUE)
    source("server/summary_biplot_server.R", local = TRUE)
    # source('server/oneSiteNVar_server.R', local=TRUE)
    source("server/nSiteNVar_server.R", local = TRUE)
    source("server/map_server.R", local = TRUE)
    source("ui/data_ui.R", local = TRUE)
    source("server/data_server.R", local = TRUE)

    # register clicking of map popup links
    observeEvent(input$SITE_EXPLORE, {
        updateTabsetPanel(session,
            "right_tabs",
            selected = "multisite_exploration"
        )
    })

    output$NSTREAMS <- renderText({
        nrow(unique(site_data[, c("stream", "domain")]))
    })

    output$NSITES <- renderText({
        sum(site_data$site_type %in% c("stream_gauge", "stream_sampling_point"),
            na.rm = TRUE
        )
    })

    output$NOBS <- renderText({
        readr::read_file("data/general/total_nonspatial_observations.txt") %>%
            as.numeric() %>%
            round(-4) %>%
            format(
                scientific = FALSE,
                big.mark = ","
            )
    })

    # observeEvent(
    #     once = TRUE,
    #     ignoreNULL = FALSE,
    #     ignoreInit = FALSE,
    #     eventExpr = TRUE,
    #     handler.quoted = TRUE,
    #     handlerExpr = {
    #         show_loading_dots("LOADING_POPUP",
    #             message = "loading"
    #         )
    #     }
    # )

    observeEvent(
        eventExpr = input$MAPDATA,
        priority = 110,
        handlerExpr = {
            init_vals$basedata_change_reloads_plots <- TRUE

            print("site one")
            print(str(input$MAPDATA$value))
            rank_one <- str_match(input$MAPDATA$value, "(.+?)__(.+?)_goto.*$")[, 2:3]

            print("site two")
            print(str(input$MAPDATA$rest$value))
            rank_two <- str_match(input$MAPDATA$rest$value, "(.+?)__(.+?)_goto.*$")[, 2:3]

            print("site three")
            print(str(input$MAPDATA$rest$rest$value))
            rank_three <- str_match(input$MAPDATA$rest$rest$value, "(.+?)__(.+?)_goto.*$")[, 2:3]

            # map_selection <- str_match(input$MAPDATA, "(.+?)__(.+?)_goto.*$")[, 2:3]
            # map_selection <- rbind(map_selection, c(NA, NA))

            # first pair
            domain_sel <- rank_one[1]
            site_sel <- rank_one[2]

            # second pair
            domain_sel_two <- rank_two[1]
            site_sel_two <- rank_two[2]

            # third pair
            domain_sel_three <- rank_three[1]
            site_sel_three <- rank_three[2]

            # all sites
            site_all <- c(site_sel, site_sel_two, site_sel_three)
            # all domains names
            dmns <- c(domain_sel, domain_sel_two, domain_sel_three)

            # get sitelist for first site pair
            dmn_sitelist <- get_sitelist(
                domain = domain_sel,
                type = c(
                    "stream_gauge",
                    "stream_sampling_point"
                )
            )

            dmn_all <- dmn_sitelist

            # if there is a second pair
            if (is.na(domain_sel_two)) {
                print("no rank two station available")
            } else {
                # get sitelist
                dmn_sitelist_two <- get_sitelist(
                    domain = toString(domain_sel_two),
                    type = c(
                        "stream_gauge",
                        "stream_sampling_point"
                    )
                )
                dmn_all <- c(dmn_sitelist, dmn_sitelist_two)
            }

            # if there is a third pair
            if (is.na(domain_sel_three)) {
                print("no rank three station available (NA)")
            } else if (exists(domain_sel_three)) {
                print("no rank three station available (not exists)")
            } else {
                # get sitelist
                dmn_sitelist_three <- get_sitelist(
                    domain = toString(domain_sel_three),
                    type = c(
                        "stream_gauge",
                        "stream_sampling_point"
                    )
                )
                dmn_all <- c(dmn_sitelist, dmn_sitelist_two, dmn_sitelist_three)
            }
            print("domains")
            print(dmn_all)
            print("sites")
            print(site_all)

            updateSelectizeInput(
                session = session,
                inputId = "DOMAINS3",
                label = NULL,
                selected = dmns,
                choices = domains_pretty,
                options = list(maxItems = 3)
            )

            updateSelectizeInput(
                session = session,
                inputId = "SITES3",
                label = NULL,
                selected = site_all,
                choices = dmn_all,
                options = list(maxItems = 3)
            )

            print("MAPDATA")
        }
    )

    observeEvent(
        eventExpr = {
            input$DISMISS_MODAL
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
            guide1a$start()
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )
    observeEvent(
        eventExpr = input$CONTINUE_TOUR,
        handlerExpr = {
            guide1b$start()
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )

    observeEvent(
        eventExpr = init_vals$loading_modal,
        handlerExpr = {
            # showModal(p('oi'))
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )

    observeEvent(
        eventExpr = input$START_DATA_TOUR,
        handlerExpr = {
            if (init_vals$ts_tab_is_pristine) {
                guide2a$start()
            } else {
                click("GEN_PLOTS3")
                show_loading_dots("LOADING_POPUP",
                    message = "loading"
                )
            }
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )

    observeEvent(
        eventExpr = input$TRIGGER_LOADING_DOTS,
        handlerExpr = show_loading_dots("LOADING_POPUP",
            message = input$TRIGGER_LOADING_DOTS
        )
    )

    observeEvent(
        eventExpr = input$CONTINUE_DATA_TOUR,
        handlerExpr = {
            removeNotification("LOADING_POPUP")
            if (input$CONTINUE_DATA_TOUR == "a") {
                guide2a$start()
            } else if (input$CONTINUE_DATA_TOUR == "b") {
                guide2b$start()
            }
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )

    # observeEvent(
    #     eventExpr = input$CONTINUE_DATA_TOUR,
    #     handlerExpr = {
    #         guide2b$start()
    #     },
    #     autoDestroy = FALSE,
    #     ignoreInit = TRUE
    # )

    observeEvent(
        eventExpr = input$BACK_TO_VARIABLE_CATALOG,
        handlerExpr = {
            removeModal(session)
            shinyjs::click("VARIABLE_CATALOG_BUTTON")
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )

    observeEvent(
        eventExpr = input$BACK_TO_SITE_CATALOG,
        handlerExpr = {
            removeModal(session)
            shinyjs::click("SITE_CATALOG_BUTTON")
        },
        autoDestroy = FALSE,
        ignoreInit = TRUE
    )
}
