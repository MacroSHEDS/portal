catalog_tab <- tabPanel('Data Catalog',

    br(),
    fluidRow(class = 'text-center',
        column(8, offset = 2,
            actionButton(inputId = 'SITE_CATALOG_BUTTON',
                         label = 'Site Catalog',
                         width = '200px'),
                # icon = icon("th"))

     # onclick = "window.open('/site_catalog', '_blank')")

    # HTML("<a href='?site_catalog' target='_blank'>Site Catalog</a>")

    # a(href = route_link('/site_catalog'), 'Site Catalog')
    # router$ui

    # rHandsontableOutput('SITE_CATALOG')

            actionButton(inputId = 'VARIABLE_CATALOG_BUTTON',
                         label = 'Variable Catalog',
                         width = '200px')
        )
    )

    # textInput('VARIABLE_SUBCATALOG_BUTTON_LISTENER',
    #           label = '',
    #           value = NULL,

)

# site_catalog <-
#     showModal(
#         modalDialog(title = NULL,
#             footer = NULL,
#             easyClose = TRUE,
#             id = 'site_catalog',
#             fluidRow(class = 'text-left',
#                 column(12,
#                     p('monkey')
#                 )
#             )
#         )
#     )
