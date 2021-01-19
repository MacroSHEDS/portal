catalog_tab <- tabPanel('Data Catalog',

    actionButton(inputId = 'SITE_CATALOG_BUTTON',
                 label = 'Site Catalog',
                 icon = icon("th"),
                 onclick = "window.open('https://www.google.com', '_blank')")
                 # onclick = "window.open('/site_catalog_page', '_blank')")

    # rHandsontableOutput('SITE_CATALOG')
)
