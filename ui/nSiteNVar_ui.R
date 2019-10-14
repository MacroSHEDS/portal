nSiteNVar_tab = tabPanel("Multisite", value='multisite_exploration',
    sidebarLayout(
        sidebarPanel(
            div('Select sites', class='widget-title text-center'),
            selectizeInput('SITES3', label=NULL, selected=default_site,
                choices=sites, multiple=TRUE),
            div('(Up to 3)', class='widget-subtitle text-center'),
            br(),
            div('Select variables', class='widget-title text-center'),
            selectizeInput('SOLUTES3', label=NULL,
                selected=grabvars_display_subset[[1]][[1]],
                multiple=TRUE, choices=grabvars_display_subset),
            div('(Up to 3)', class='widget-subtitle text-center'),
            br(),
            div('Select units', class='widget-title text-center'),
            radioButtons('CONC_FLUX3', label=NULL,
                choices=c('Concentration'='Concentration',
                    'Flux (interpolated)'='Flux'), selected='Concentration'),
            conditionalPanel('input.CONC_FLUX3 == "Concentration"',
                selectizeInput('CONC_UNIT3', label=NULL, choices=conc_units,
                    selected='mg/L')
            ),
            conditionalPanel('input.CONC_FLUX3 == "Flux"',
                selectizeInput('FLUX_UNIT3', label=NULL, choices=flux_units,
                    selected='kg/ha/d')
            ),
        width=3),

        mainPanel(
            fluidRow(class='text-center',
                wellPanel(
                    sliderInput("DATE3", label=NULL, min=dtrng[1], max=dtrng[2],
                        value=c(max(dtrng[2] - lubridate::days(365),
                            initial_dtrng[1], na.rm=TRUE),
                            dtrng[2]),
                        width="100%", timeFormat="%b %Y", step=30,
                        dragRange=TRUE),
                    dygraphOutput("GRAPH_PRECIP3", height='75px'),
                    br(),
                    div(id='main3a'),
                    conditionalPanel(paste('input.SOLUTES3 !== null &&',
                            'input.SITES3 !== null'),
                        dygraphOutput("GRAPH_MAIN3a", height='125px'),
                        br()
                    ),
                    div(id='main3b'),
                    conditionalPanel('input.SOLUTES3.length > 1',
                        dygraphOutput("GRAPH_MAIN3b", height='125px'),
                        br()
                    ),
                    div(id='main3c'),
                    conditionalPanel('input.SOLUTES3.length > 2',
                        dygraphOutput("GRAPH_MAIN3c", height='125px'),
                        br()
                    ),
                    div(id='flow3'),
                    dygraphOutput("GRAPH_FLOW3", height='75px')
                )
            )
        )
    )
)
