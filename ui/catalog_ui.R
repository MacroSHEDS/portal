catalog_tab <- tabPanel('Data Catalog',

    actionButton(inputId = 'SITE_CATALOG_BUTTON',
                 label = 'Site Catalog')
                 # icon = icon("th"))

                 # onclick = "window.open('/site_catalog', '_blank')")

    # HTML("<a href='?site_catalog' target='_blank'>Site Catalog</a>")

    # a(href = route_link('/site_catalog'), 'Site Catalog')
    # router$ui

    # rHandsontableOutput('SITE_CATALOG')
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
