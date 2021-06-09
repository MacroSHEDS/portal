data_tab <- tabPanel('Data',

    br(),
    fluidRow(class = 'text-center',

        column(6, offset = 3,

            h2('Explore'),
            actionButton(inputId = 'SITE_CATALOG_BUTTON',
                         label = 'Site Catalog',
                         width = '100%'),
                # icon = icon("th"))
            actionButton(inputId = 'VARIABLE_CATALOG_BUTTON',
                         label = 'Variable Catalog',
                         width = '100%'),

            h2('Download Data'),
            actionButton(inputId = 'TIMESERIES_DL_BUTTON',
                         label = 'Time-series',
                         width = '100%')
        )
    )
)
