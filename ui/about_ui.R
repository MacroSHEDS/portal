about_tab <- tabPanel("About",
  value = "about",
  div(
    style = "padding: 50px;",
    class = "jumbotron",
    fluidRow(
      class = "text-center",
      column(
        12,
        img(
          src = "new_logo_full.png",
          style = "height: 100px; width: 300px"
        ),
        br(),
        br(),
        br()
      )
    ),
    fluidRow(
      class = "text-center",
      column(
        4,
        img(
          src = "water-solid-193d85.svg",
          style = "height: 56px; width: 56px"
        ),
        h2(textOutput("NSTREAMS")),
        p("STREAMS")
      ),
      column(
        4,
        icon("map-marker", class = "fa-4x"),
        h2(textOutput("NSITES")),
        p("SITES")
      ),
      column(
        4,
        icon("chart-bar", class = "fa-4x"),
        h2(textOutput("NOBS")),
        p("OBSERVATIONS")
      )
    ),
    fluidRow(
      div(
        includeHTML("ui/landing_slides.html"),
        style = "padding: 0px 20px; color: #7b96b6; font-size: 1.2em;",
        class = "text-left"
      )
    )
  ),
  div(includeHTML("ui/landing_blurb.html"),
    style = "padding: 0px 0px; color: #7b96b6; font-size: 1.2em;",
    class = "text-left"
  )
)
