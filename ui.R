source("helpers.R")
# source('ui/oneSiteNVar_ui.R')
source("ui/nSiteNVar_ui.R")
# source('ui/site_comparison_ui.R')
source("ui/about_ui.R")
source("ui/participants_ui.R")
source("ui/summary_biplot_ui.R")
source("ui/map_ui.R")
source("ui/now_hiring_ui.R")
source("ui/data_tab_ui.R")
source("ui/notes_ui.R")

ui <- fluidPage(
  use_cicerone(),
  tags$head(includeHTML(("ui/google_analytics.html"))),
  tags$meta(name="description",
            content="Welcome to the MacroSheds dataset web portal! MacroSheds unites stream and watershed data from all these sources on one platform, making it easy for anyone to explore the hydrology, water quality, and biogeochemistry of rivers across North America and beyond."),
  # screen shouldn't go gray when plots are updating.
  # tags$style(type="text/css", ".recalculating { opacity: 1.0; }" ),
  tags$head(tags$style(HTML(
    "@import url('https://fonts.googleapis.com/css?family=Montserrat');"
  ))),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
  # useShinyjs(),
  # extendShinyjs(script = 'js/general.js',
  #               functions = c()),

  div(
    id = "loading-start",
    includeHTML("ui/loading_gif.html"),
  ),
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      width = "40%",
      div(
        class = "sidebar-sub",
        # HTML('<input type="text" id="MAPDATA" style="display: none">'),
        tabsetPanel(
          id = "left_tabs",
          # now_hiring_tab,
          about_tab,
          map_tab,
          data_tab,
          participants_tab,
          notes_tab
        )
      ),
      div(
        style = "width: 36px; display: inline-block; float: right",
        actionLink("COLLAPSE_SIDEBAR",
          label = "", icon = icon("circle-arrow-left", lib = "glyphicon", class = "map-mode gi-semi-x"),
          class = "sidebar-toggle", `data-toggle` = "offcanvas",
          style = "margin: 6px"
        )
      )
    ),
    dashboardBody(
      useShinyjs(),
      extendShinyjs(
        script = "js/general.js",
        functions = c()
      ),
      # tags$head(
      #     tags$link(rel='stylesheet', type='text/css', href='style.css')
      # ),
      div(
        id = "data-toggler",
        style = "width: 36px; display: inline-block; float: left",
        actionLink("COLLAPSE_DATA",
          label = "", icon = icon("circle-arrow-right", lib = "glyphicon", class = "data-mode gi-semi-x"),
          class = "data-hider", # `data-toggle` = "offcanvas",
          # style = "margin: -6px"
        )
      ),
      div(
        class = "data-sub",
        tabsetPanel(
          id = "right_tabs",
          nSiteNVar_tab,
          summary_biplot_tab
          # oneSiteNVar_tab,
          # site_comparison_tab
        )
      )
    )
  )
)
# ))
