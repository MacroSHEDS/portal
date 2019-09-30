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
            conditionalPanel('false', #hiding this until it's necessary
                radioButtons('PRECIP_SOURCE4', label='Precip data source',
                    choices=c('Collector Catch (mm)'='precipCatch',
                        'ETI'='precipETI'), selected='precipCatch'),
                checkboxInput("FIELDCODE4",
                    label = "Show field codes",
                    value = FALSE
                ),
                radioButtons("FLOW_SOURCE4", label='Discharge data source',
                    choices=c('Rating curve (L/s)'='flowGageHt',
                        'Sensor (L/s)'='flowSens'))
            ),
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
            fluidRow(
                dygraphOutput("GRAPH_PRECIP4", height='100px'),
                br()
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
            fluidRow(
                dygraphOutput("GRAPH_FLOW4", height='100px')
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
