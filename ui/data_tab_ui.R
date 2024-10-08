data_tab <- tabPanel(
    "Data",
    br(),
    fluidRow(
        class = "text-center",
        column(6,
            offset = 3,
            h2("Explore",
                style = "color: #7b96b6"
            ),
            actionButton(
                inputId = "SITE_CATALOG_BUTTON",
                label = "Site catalog",
                width = "100%",
                style = "color: #333"
            ),
            actionButton(
                inputId = "VARIABLE_CATALOG_BUTTON",
                label = "Variable catalog",
                width = "100%",
                style = "color: #333"
            ),
            # br(),
            # h2("Download",
            #     style = "color: #7b96b6"
            # ),
            # downloadButton(
            #     outputId = "DL_SUBMIT_SITE",
            #     label = "Site summary table",
            #     style = "color: #333 !important; display: block; margin-left: 15px; margin-right: 5px; margin-top: 6px; margin-bottom: 6px; width: 100%"
            # ),
            # downloadButton(
            #     outputId = "DL_SUBMIT_VAR",
            #     label = "Variable table",
            #     style = "color: #333 !important; display: block; margin-left: 15px; margin-right: 5px; margin-top: 6px; margin-bottom: 6px; width: 100%"
            # ),
            br(),
            h2("Get Data",
                style = "color: #7b96b6"
            ),
            actionButton(
                inputId = "EDI_LINK",
                label = "MacroSheds EDI",
                icon = icon("external-link",
                    lib = "font-awesome"
                ),
                width = "100%",
                style = "color: #333",
                onclick = "window.open('https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262', '_blank')"
            ),
            div(
                class = "well-sm",
                tags$footer("for R users,",
                          tags$a(href="https://github.com/MacroSHEDS/macrosheds", "the macrosheds R package"),
                          "is the most convenient way to download, explore, and manipulate",
                          "MacroSheds data", style = "color: white")
            )
        )
    )
)
