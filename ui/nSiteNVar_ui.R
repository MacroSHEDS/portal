nSiteNVar_tab = tabPanel("Multisite", value='multisite_exploration',
    sidebarLayout(
        # Sidebar with tabs for Solute, Sites, Options
        sidebarPanel(
            div('Choose sites', class='widget-title text-center'),
            selectizeInput('SITES3', label=NULL, selected=default_site,
                choices=sites, multiple=TRUE),
            div('(Up to 3)', class='widget-subtitle text-center'),
            br(),
            div('Choose variables', class='widget-title text-center'),
            selectizeInput('SOLUTES3', label=NULL,
                selected=grabvars_display_subset[[1]][[1]],
                multiple=TRUE, choices=grabvars_display_subset),
            div('(Up to 3)', class='widget-subtitle text-center'),
        width=3), # closes sidebarPanel

        # Plot
        mainPanel(
            wellPanel(
                sliderInput("DATE3", label=NULL,
                    min=dtrng[1], max=dtrng[2],
                    value=c(max(dtrng[2] - lubridate::days(365),
                        initial_dtrng[1], na.rm=TRUE),
                        dtrng[2]),
                    width="100%", timeFormat="%b %Y", step=30,
                    dragRange=TRUE)
            ),
            #tags$h4(textOutput("TITLE3")),
            fluidRow(
                dygraphOutput("GRAPH_PRECIP3", height='100px'),
                br()
            ),
            # conditionalPanel(
            #     condition = "input.SOLUTE3_OPTION == true",
            fluidRow(
                conditionalPanel(paste('input.SOLUTES3 !== null &&',
                        'input.SITES3 !== null'),
                    dygraphOutput("GRAPH_MAIN3a", height='150px'),
                    br()
                ),
                conditionalPanel('input.SITES3.length > 1',
                    dygraphOutput("GRAPH_MAIN3b", height='150px'),
                    br()
                ),
                conditionalPanel('input.SITES3.length > 2',
                    dygraphOutput("GRAPH_MAIN3c", height='150px'),
                    br()
                )
            ),
            # ),
            fluidRow(
                dygraphOutput("GRAPH_FLOW3", height='100px')
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
            # dataTableOutput("TABLE3")
        ) # closes mainPanel
    ) # closes sidebarLayout
) # Closes Panel 3 tabPanel
