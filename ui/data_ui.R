#modal popup contents. for the data tab itself, see data_tab_ui.R

site_catalog <- quote({
    print('SITE CATALOG')
    showModal(
        modalDialog(title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'site_catalog',
            size = 'l',
            fluidRow(class = 'text-left',
                column(10,
                    h3('Site Catalog', style = 'margin-top: 0px'),
                ),
                column(2, class = 'text-right',
                       actionButton(inputId = 'DISMISS_MODAL',
                                    label = 'Close (Esc)'),
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
    print('VARIABLE CATALOG')
    showModal(
        modalDialog(title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'variable_catalog',
            size = 'l',
            fluidRow(class = 'text-left',
                column(10,
                    h3('Variable Catalog', style = 'margin-top: 0px')
                ),
                column(2, class = 'text-right',
                       actionButton(inputId = 'DISMISS_MODAL',
                                    label = 'Close (Esc)'),
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
    print('VARIABLE SUB-CATALOG')

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
                column(9,
                    h3(htmlOutput('VARIABLE_SUBCATALOG_TITLE'),
                       style = 'margin-top: 0px')
                ),
                column(1, class = 'text-right',
                    actionButton(inputId = 'BACK_TO_VARIABLE_CATALOG',
                                 label = 'Back'),
                ),
                column(2, class = 'text-right',
                    actionButton(inputId = 'DISMISS_MODAL',
                                 label = 'Close (Esc)'),
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
})

site_subcatalog <- quote({

    print('SITE SUB-CATALOG')

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
        modalDialog(
            title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'site_subcatalog',
            size = 'l',
            fluidRow(class = 'text-left',
                column(9,
                    h3(htmlOutput('SITE_SUBCATALOG_TITLE'),
                       style = 'margin-top: 0px')
                ),
                column(1, class = 'text-right',
                    actionButton(inputId = 'BACK_TO_SITE_CATALOG',
                                 label = 'Back'),
                ),
                column(2, class = 'text-right',
                    actionButton(inputId = 'DISMISS_MODAL',
                                 label = 'Close (Esc)'),
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
})

timeseries_dl <- quote({
    showModal(
        modalDialog(
            title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'timeseries_dl',
            size = 'l',
            fluidRow(class = 'text-left',
                column(10,
                    h2('Time-series Data Download', style = 'margin-top: 0px'),
                ),
                column(2, class = 'text-right',
                    actionButton(inputId = 'DISMISS_MODAL',
                                 label = 'Close (Esc)'),
                ),
            ),
            fluidRow(class = 'text-left',
                column(12,
                    br(),
                    # checkboxInput(inputId = 'DL_ALLSITES',
                    #               label = 'Include all sites from all data domains',
                    #               value = TRUE),
                    # hr(),
                    # conditionalPanel('input.DL_ALLSITES == false',
                    # div(id = 'DL_CHECKBOX_TREE_DIV',
                    #     style = 'display: none',
                    div(style = 'position: relative; height: 4em',
                        h4('Network', style = 'position: absolute; font-color: gray; bottom: 0; margin-top: 0'),
                        h4('Domain', style = 'font-color: gray; margin-left: 150px; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Watersheds', style = 'font-color: gray; margin-left: 31em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Areas (ha)', style = 'font-color: gray; margin-left: 39em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Centroid (WGS84)', style = 'font-color: gray; margin-left: 45em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Zip size (MB)', style = 'font-color: gray; margin-left: 53em; position: absolute; margin-top: 0; bottom: 0')
                    ),
                    uiOutput('DL_CHECKBOX_TREE')
                    # )
                    downloadButton('DL_SUBMIT_TS')
                )
            )
        )
    )
})
