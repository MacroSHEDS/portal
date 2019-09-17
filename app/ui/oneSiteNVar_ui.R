oneSiteNVar_tab = tabPanel("Single Site", value='site_exploration',
    sidebarLayout(
        # Sidebar with tabs for Solute, Sites, Options
        sidebarPanel(
            div('Choose a site', class='widget-title text-center'),
            selectizeInput('SITES4', label=NULL,
                choices=c(sites_streams, sites_precip)),
            br(),
            div('Choose variables', class='widget-title text-center'),
            selectizeInput('SOLUTES4', label=NULL,
                selected=grabvars_display[[1]][[1]],
                multiple=TRUE, choices=grabvars_display),
            div('(Up to 9)', class='widget-subtitle text-center'),
            # helpText(textOutput("LIMITS4"),
            #     style = "color:#fc9272; font-size:85%;"
            # ),
            hr(),

            # Options for "Precipitation" Graph
            #**********************************
            p("Precipitation", style = "font-weight:bold; font-size:1.1em;"),
            checkboxInput("PRECIP4_OPTION",
                label = p("Show graph", style = "font-size:0.9em; color:#919191;"),
                value = TRUE
            ),
            # below only appears if "Precipitation" is selected
            conditionalPanel(
                condition = "input.PRECIP4_OPTION == true",
                radioButtons(
                    "PRECIP_SOURCE4",
                    label = "Precip data source:",
                    choices = c("Collector Catch (mm)" = "precipCatch",
                        "ETI" = "precipETI"
                    ),
                    selected = "precipCatch"
                ),
                style = "color:#919191; font-size:0.9em;"
            ), #end of conditional panel

            hr(),

            # Options for "Solutes" Graph
            #****************************
            p("Grab Variables",
                style="font-weight:bold; font-size:1.1em;"),
            checkboxInput("SOLUTE4_OPTION",
                label = p("Show graph(s)", style = "font-size:0.9em; color:#919191;"),
                value = TRUE
            ),
            conditionalPanel(
                condition = "input.SOLUTE4_OPTION == true",
                checkboxInput("FIELDCODE4",
                    label = "Show field codes",
                    value = FALSE
                ),
                selectInput("SOLUTES4_COLOR",
                    label = "Colors apply to:",
                    choices = c("Solutes", "Sites"),
                    width = "80%"),
                style = "color:#919191; font-size:0.9em;"
            ), # end

            hr(),

            # Options for "Discharge" Graph
            #******************************
            p("Discharge", style = "font-weight:bold; font-size:1.1em;"),
            checkboxInput("DISCHARGE4_OPTION",
                label = p("Show graph", style = "font-size:0.9em; color:#919191;"),
                value = TRUE
            ),
            # below only appears if "Discharge" is selected
            conditionalPanel(
                condition = "input.DISCHARGE4_OPTION == true",
                checkboxInput("HYDROLIMB4",
                    label = "Add hydrograph limb",
                    value = FALSE
                ),
                p("Discharge data sources:", style = "font-weight:bold; text-decoration:underline;"),
                p(selectInput("FLOW_SITE4",
                    label = p("Data from site:", style = "font-weight:bold"),
                    choices = c(sites_streams),
                    selected = "W1",
                    width = "80%"),
                    style = "margin-bottom:0px; font-size:0.9em;"
                ),
                radioButtons(
                    "FLOW_SOURCE4",
                    label = p("Data type:", style = "font-weight:bold"),
                    choices = c("Gage Height (ft)" = "gageHt",
                        "Q from Gage Height (L/s)" = "flowGageHt",
                        "Q from Sensor (L/s)" = "flowSens"
                    ),
                    selected = "gageHt"
                ),
                style = "color:#919191; margin-top:0px; font-size:0.9em;"
            ),#end of conditional panel
        width=3), # closes sidebarPanel

        # Plot
        mainPanel(
            wellPanel(
                sliderInput(
                    "DATE4",
                    label = "Date Range",
                    min =as.Date("1963-06-01"),
                    max = as.Date(maxDate),
                    value = as.Date(c(maxDate-365, maxDate)),
                    width = "100%",
                    timeFormat = "%b %Y",
                    step = 30,
                    dragRange = TRUE)
            ),
            #tags$h4(textOutput("TITLE4")),
            conditionalPanel(
                condition = "input.PRECIP4_OPTION == true",
                fluidRow(
                    dygraphOutput("GRAPH_PRECIP4", height='100px'),
                    br()
                )
            ),
            # conditionalPanel(
            #     condition = "input.SOLUTE4_OPTION == true",
            fluidRow(
                conditionalPanel('input.SOLUTES4 !== null',
                    dygraphOutput("GRAPH_MAIN4a", height='150px'),
                    br()
                ),
                conditionalPanel('input.SOLUTES4.length > 3',
                    dygraphOutput("GRAPH_MAIN4b", height='150px'),
                    br()
                ),
                conditionalPanel('input.SOLUTES4.length > 6',
                    dygraphOutput("GRAPH_MAIN4c", height='150px'),
                    br()
                )
            ),
            # ),
            conditionalPanel(
                condition = "input.DISCHARGE4_OPTION == true",
                fluidRow(
                    dygraphOutput("GRAPH_FLOW4", height='100px'))
            )
            # use for when testing data selection
            # hr(),
            # h4("Table of Selected Data"),
            # HTML("<p>Search bar finds specific values within selected
            #    data (e.g. '2014-06', '5.'). <br> Arrows (to the right
            #    of column names) sort data in ascending or descending
            #    order.</p>"
            # ),
            # used when testing data sorting
            # dataTableOutput("TABLE4")
        ) # closes mainPanel
    ) # closes sidebarLayout
) # Closes Panel 4 tabPanel
