map_tab <- tabPanel(
  # "Map",
  # mapdeckOutput("MAP")
  "Map",
  leafletOutput("MAP", height = 350),
  br(),

  # shopping cart
  fluidRow(
    column(
      width = 12,
      tags$h2("Default, multi-drag and swapping behaviour"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Default",
          tags$b("Exercise"),
          rank_list_basic,
          tags$b("Result"),
          verbatimTextOutput("results_basic")
        )
      )
    )
  ),
  # this button opens the site exploration tab. the button is hidden and unclickable.
  # it's triggered by links in the popups on the map tab.
  actionButton("SITE_EXPLORE", "", style = "display: none"),

  # DT::dataTableOutput('MAP_SITE_INFO')
  textOutput("MAP_SITE_INFO_TITLE"),
  # div('Site Information', class = 'widget-title text-center'),
  tableOutput("MAP_SITE_INFO"),
  p('See "Map" section of the Notes/Caveats tab',
    class = "leftpanel-text"
  )
)
