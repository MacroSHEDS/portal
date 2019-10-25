landing_page =
    showModal(
        modalDialog(title=NULL, footer=NULL, easyClose=TRUE,
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
                    h1(textOutput('NSTREAMS'), style='color: gray'),
                    p('STREAMS')
                ),
                column(4,
                    icon('map-marker', class='fa-4x'),
                    h1(textOutput('NSITES'), style='color: gray'),
                    p('SITES')
                ),
                column(4,
                    icon('chart-bar', class='fa-4x'),
                    h1(textOutput('NOBS'), style='color: gray'),
                    p('OBSERVATIONS')
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
