site_catalog <- quote({
    showModal(
        modalDialog(title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'site_catalog',
            size = 'l',
            fluidRow(class = 'text-left',
                column(11,
                    h3('Site Catalog', style = 'margin-top: 0px'),
                ),
                column(1, class = 'text-right',
                       actionButton(inputId = 'DISMISS_SITE_SUBCATALOG',
                                    label = 'Close'),
                ),
            ),
            fluidRow(class = 'text-left',
                column(12,
                    br(),
                    DT::dataTableOutput('SITE_CATALOG')
                    # rHandsontableOutput('SITE_CATALOG')
                )
            )
        )
    )
    # tags$head(tags$style(".modal-dialog{ width:100%}"))
})

variable_catalog <- quote({
    showModal(
        modalDialog(title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'variable_catalog',
            size = 'l',
            fluidRow(class = 'text-left',
                column(11,
                    h3('Variable Catalog', style = 'margin-top: 0px')
                ),
                column(1, class = 'text-right',
                       actionButton(inputId = 'DISMISS_VARIABLE_CATALOG',
                                    label = 'Close'),
                ),
            ),
            fluidRow(class = 'text-left',
                column(12,
                    br(),
                    DT::dataTableOutput('VARIABLE_CATALOG')
                )
            )
        )
    )
})

variable_subcatalog <- quote({

    output$VARIABLE_SUBCATALOG_TITLE <- renderUI({
        HTML(glue('Availability for VariableCode: <strong>{v}</strong>',
              v = isolate(input$VARIABLE_SUBCATALOG_BUTTON_LISTENER)))
    })

    showModal(
        modalDialog(title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'variable_subcatalog',
            size = 'l',
            fluidRow(class = 'text-left',
                column(10,
                    h3(htmlOutput('VARIABLE_SUBCATALOG_TITLE'),
                       style = 'margin-top: 0px')
                ),
                column(1, class = 'text-right',
                    actionButton(inputId = 'BACK_TO_VARIABLE_CATALOG',
                                 label = 'Back'),
                ),
                column(1, class = 'text-right',
                    actionButton(inputId = 'DISMISS_VARIABLE_SUBCATALOG',
                                 label = 'Close'),
                )
            ),
            fluidRow(class = 'text-left',
                column(12,
                    br(),
                    DT::dataTableOutput('VARIABLE_SUBCATALOG')
                )
            )
        )
    )

    # shiny::updateTextInput(session = session,
    #                        inputId = 'VARIABLE_SUBCATALOG_BUTTON_LISTENER',
    #                        value = NULL)
})

site_subcatalog <- quote({

    output$SITE_SUBCATALOG_TITLE <- renderUI({

        ntw_dmn_sit <- isolate(input$SITE_SUBCATALOG_BUTTON_LISTENER)
        ntw_dmn_sit <- stringr::str_match(
            string = ntw_dmn_sit,
            pattern = '^([a-zA-Z0-9]+)_([a-zA-Z0-9]+)_(.+)$'
        )[2:4]

        HTML(glue('Network: <strong>{n}</strong>, domain: <strong>{d}</strong>, SiteCode: <strong>{s}</strong>',
              n = ntw_dmn_sit[1],
              d = ntw_dmn_sit[2],
              s = ntw_dmn_sit[3]))
    })

    showModal(
        modalDialog(title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'site_subcatalog',
            size = 'l',
            fluidRow(class = 'text-left',
                column(10,
                    h3(htmlOutput('SITE_SUBCATALOG_TITLE'),
                       style = 'margin-top: 0px')
                ),
                column(1, class = 'text-right',
                    actionButton(inputId = 'BACK_TO_SITE_CATALOG',
                                 label = 'Back'),
                ),
                column(1, class = 'text-right',
                    actionButton(inputId = 'DISMISS_SITE_SUBCATALOG',
                                 label = 'Close'),
                )
            ),
            fluidRow(class = 'text-left',
                column(12,
                    br(),
                    DT::dataTableOutput('SITE_SUBCATALOG')
                )
            )
        )
    )

    # shiny::updateTextInput(session = session,
    #                        inputId = 'VARIABLE_SUBCATALOG_BUTTON_LISTENER',
    #                        value = NULL)
})
