landing_page = quote({
    showModal(
        modalDialog(title=NULL, footer=NULL, easyClose=TRUE, id='landing',
            fluidRow(class='text-center',
                column(12,
                    img(src='new_logo_full.png',
                        style='height: 100px; width: 300px'),
                    br(),
                    br(),
                    br()
                )
            ),
            fluidRow(class='text-center',
                column(4,
                    img(src='water-solid-193d85.svg',
                        style='height: 56px; width: 56px'),
                    h1(textOutput('NSTREAMS')),
                    p('STREAMS')
                ),
                column(4,
                    icon('map-marker', class='fa-4x'),
                    h1(textOutput('NSITES')),
                    p('SITES')
                ),
                column(4,
                    icon('chart-bar', class='fa-4x'),
                    h1(textOutput('NOBS')),
                    p('OBSERVATIONS')
                )
            ),
            fluidRow(class='text-center',
                column(8, offset=2,
                    br(),
                    br(),
                    p(HTML(paste('This portal is under heavy development, and',
                            'we\'re still working out <strong>plenty of bugs</strong>.',
                            'Features and data may change.'))),
                )
            ),
            fluidRow(class='text-center',
                column(12,
                    br(),
                    br(),
                    br(),
                    br(),
                    actionButton('DISMISS_LANDING', label='Continue')
                )
            )
        )
    )

    init_vals$enable_unitconvert = TRUE
})
