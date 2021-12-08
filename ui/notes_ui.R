notes_tab <- tabPanel("Notes",
    value = "notes",
    br(),
    div(includeHTML("ui/notes_tab.html"),
        style = "padding: 20px"
    )
)
