map_tab <- tabPanel(
    # "Map",
    # mapdeckOutput("MAP")
    "Map",
    div(
        id = "mapcontainer",
        class = "well",
        leafletOutput("MAP", height = 350),
    ),
    br(),

    # shopping cart
    fluidRow(
        column(
            width = 12,
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Selected Sites",
                    rank_list_basic,
                ),
                tabPanel(
                    "Watershed Attributes",
                    # textOutput("MAP_SITE_INFO_TITLE"),
                    tableOutput("MAP_SITE_INFO") %>%
                        tagAppendAttributes(class = "table") %>%
                        tagAppendAttributes(class = "horizontal-scroll"),
                )
            )
        )
    ),
    # this button opens the site exploration tab. the button is hidden and unclickable.
    # it's triggered by links in the popups on the map tab.
    actionButton("SITE_EXPLORE", "", style = "display: none"),

    # DT::dataTableOutput('MAP_SITE_INFO')
    # div('Site Information', class = 'widget-title text-center'),
    div(
        class = "label",
        tags$footer(
            p("go to 'Notes/Caveats' tab for more information"),
            # class = "leftpanel-text"
        )
    )
)
