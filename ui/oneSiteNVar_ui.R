oneSiteNVar_tab = tabPanel("Single Site", value='site_exploration',
    sidebarLayout(
        # Sidebar with tabs for Solute, Sites, Options
        sidebarPanel(
            div('Choose a site', class='widget-title text-center'),
            selectizeInput('SITES4', label=NULL, selected=default_site,
                choices=sites),
            br(),
            div('Choose variables', class='widget-title text-center'),
            selectizeInput('SOLUTES4', label=NULL,
                selected=grabvars_display_subset[[1]][[1]],
                multiple=TRUE, choices=grabvars_display_subset),
            div('(Up to 9)', class='widget-subtitle text-center'),
            radioButtons('CONC_FLUX4', label=NULL,
                choices=c('concentration', 'flux'), selected='concentration'),
            conditionalPanel('false', #hiding this until it's necessary
                radioButtons('PRECIP_SOURCE4', label='Precip data source',
                    choices=c('Collector Catch (mm)'='precipCatch',
                        'ETI'='precipETI'), selected='precipCatch'),
                checkboxInput("FIELDCODE4",
                    label = "Show field codes",
                    value = FALSE
                ),
                radioButtons("FLOW_SOURCE4", label='Discharge data source',
                    choices=c('Sensor (L/s)'='flowSens',
                        'Rating curve (L/s)'='flowGageHt'))
            ),
        width=3), # closes sidebarPanel

        # Plot
        mainPanel(
            wellPanel(
                sliderInput("DATE4", label=NULL,
                    min=dtrng[1], max=dtrng[2],
                    value=c(max(dtrng[2] - lubridate::days(365),
                        initial_dtrng[1], na.rm=TRUE),
                        dtrng[2]),
                    width="100%", timeFormat="%b %Y", step=30,
                    dragRange=TRUE)
            ),
            #tags$h4(textOutput("TITLE4")),
            fluidRow(
                dygraphOutput("GRAPH_PRECIP4", height='75px'),
                br()
            ),
            # conditionalPanel(
            #     condition = "input.SOLUTE4_OPTION == true",
            fluidRow(
                conditionalPanel('input.SOLUTES4 !== null',
                    dygraphOutput("GRAPH_MAIN4a", height='125px'),
                    br()
                ),
                conditionalPanel('input.SOLUTES4.length > 3',
                    dygraphOutput("GRAPH_MAIN4b", height='125px'),
                    br()
                ),
                conditionalPanel('input.SOLUTES4.length > 6',
                    dygraphOutput("GRAPH_MAIN4c", height='125px'),
                    br()
                )
            ),
            # ),
            fluidRow(
                dygraphOutput("GRAPH_FLOW4", height='75px')
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
