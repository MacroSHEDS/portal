oneSiteNVar_tab = tabPanel("Single Site", value='site_exploration',
    sidebarLayout(
        # Sidebar with tabs for Solute, Sites, Options
        sidebarPanel(
            div('Site', class='widget-title text-center'),
            selectizeInput('SITES4', label=NULL, selected=default_site,
                choices=default_sites),
            br(),
            div('Variables', class='widget-title text-center'),
            selectizeInput('SOLUTES4', label=NULL,
                selected=grabvars_display_subset[[1]][[1]],
                multiple=TRUE, choices=grabvars_display_subset),
            div('(Up to 9)', class='widget-subtitle text-center'),
            br(),
            div('Unit', class='widget-title text-center'),
            div('(Applies to solutes only)',
                class='widget-subtitle text-center'),
            radioButtons('CONC_FLUX4', label=NULL,
                choices=c('Concentration'='Concentration',
                    'Flux (interpolated)'='Flux'), selected='Concentration'),
            conditionalPanel('input.CONC_FLUX4 == "Concentration"',
                selectizeInput('CONC_UNIT4', label=NULL, choices=conc_units,
                    selected='mg/L')
            ),
            conditionalPanel('input.CONC_FLUX4 == "Flux"',
                selectizeInput('FLUX_UNIT4', label=NULL, choices=flux_units,
                    selected='kg/ha/d')
            ),
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
        width=3),

        mainPanel(
            fluidRow(class='text-center',
                wellPanel(
                    sliderInput("DATE4", label=NULL,
                        min=dtrng[1], max=dtrng[2],
                        value=c(max(dtrng[2] - lubridate::days(365),
                            initial_dtrng[1], na.rm=TRUE),
                            dtrng[2]),
                        width="100%", timeFormat="%b %Y", step=30,
                        dragRange=TRUE)
                ),
                dygraphOutput("GRAPH_PRECIP4", height='75px'),
                br(),
                div(id='main4a'),
                conditionalPanel('input.SOLUTES4 !== null',
                    dygraphOutput("GRAPH_MAIN4a", height='125px'),
                    br()
                ),
                div(id='main4b'),
                conditionalPanel('input.SOLUTES4.length > 3',
                    dygraphOutput("GRAPH_MAIN4b", height='125px'),
                    br()
                ),
                div(id='main4c'),
                conditionalPanel('input.SOLUTES4.length > 6',
                    dygraphOutput("GRAPH_MAIN4c", height='125px'),
                    br()
                ),
                dygraphOutput("GRAPH_FLOW4", height='75px')
            )
        )
    )
)
