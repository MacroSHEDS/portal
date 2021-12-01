about_tab <- tabPanel("About",
    value = "about",
    fluidRow(
        class = "text-center",
        column(
            12,
            br(),
            # img(
            #     src = "new_logo_full.png",
            #     style = "height: 100px; width: 300px"
            # ),
            div(includeHTML("ui/landing_blurb.html"),
                style = "padding: 0px 20px; color: #7b96b6; font-size: 1.2em;",
                class = "text-left"
            )
        )
    )
)
