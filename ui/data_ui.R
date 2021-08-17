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
                    div('Choose datasets:',
                        style = "padding-bottom: 6px",
                        class = 'widget-title text-left'),
                    checkboxGroupInput(
                        inputId = 'DL_SETS_TS',
                        label = NULL,
                        choiceNames = c('Stream discharge', 'Stream chemistry',
                                        'Stream flux', 'Precipitation',
                                        'Precipitation chemistry', 'Precipitation flux'),
                        choiceValues = c('discharge', 'stream_chemistry', 'stream_flux_inst_scaled',
                                         'precipitation', 'precip_chemistry', 'precip_flux_inst_scaled')
                    ),
                    br(),
                    div(style = "white-space: nowrap",
                        div('Choose filetype:',
                            style = "display: inline-block; vertical-align: middle; white-space: normal; padding-bottom: 6px",
                            class = 'widget-title text-left'
                        ),
                        div(class = 'ms-tooltip',
                            style = "display: inline-block; vertical-align: top",
                            title = paste('Choose "Feather" if you will be working',
                                          'with these data in R, Python, Julia, etc.',
                                          'Otherwise choose "CSV", but note that your',
                                          'download may be significantly delayed while',
                                          'we reformat the data.'),
                            enc2native('\U2753')
                        ),
                        radioButtons('DL_FORMAT_TS',
                                     label = NULL,
                                     choices = c('Feather', 'CSV'),
                                     selected = 'Feather',
                                     inline = TRUE),
                    ),
                    br(),

                    div(style = "white-space: nowrap",
                        div('Choose data domains:',
                            style = "display: inline-block; vertical-align: middle; white-space: normal;",
                            class = 'widget-title text-left'),
                        div(class = 'ms-tooltip',
                            style = "display: inline-block; vertical-align: top",
                            title = paste('MacroSheds data are organized by',
                                          'Network (top-level managing institution)',
                                          'Domain (collecting/reporting institution)',
                                          'and site (a stream gauge and its watershed).',
                                          'For more information, download our site',
                                          'data table.'),
                            enc2native('\U2753')
                        )
                    ),
                    p("(No more than 4 domains at a time, please. If download immediately fails, try again.\nIf it contains files that won't open, please contact us. Note that precip datasets are not available for all domains.)",
                      style = 'color: blue; font-size: 0.75em'),

                    div(style = 'position: relative; height: 3em',
                        h4('Network', style = 'position: absolute; font-color: gray; bottom: 0; margin-top: 0'),
                        h4('Domain', style = 'font-color: gray; margin-left: 150px; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Watersheds', style = 'font-color: gray; margin-left: 31em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Areas (ha)', style = 'font-color: gray; margin-left: 39em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Centroid (WGS84)', style = 'font-color: gray; margin-left: 45em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Zip size (~MB)', style = 'font-color: gray; margin-left: 53em; position: absolute; margin-top: 0; bottom: 0')
                    ),
                    uiOutput('DL_CHECKBOX_TREE'),
                    hr(),
                    downloadButton('DL_SUBMIT_TS')
                )
            )
        )
    )
})

spatial_dl <- quote({
    showModal(
        modalDialog(
            title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = 'spatial_dl',
            size = 'l',
            fluidRow(class = 'text-left',
                column(10,
                    h2('Spatial Data Download', style = 'margin-top: 0px'),
                ),
                column(2, class = 'text-right',
                    actionButton(inputId = 'DISMISS_MODAL',
                                 label = 'Close (Esc)'),
                ),
            ),
            fluidRow(class = 'text-left',
                column(12,
                    br(),
                    h3('Watershed summary statistics',
                       style = 'margin-bottom: 0px'),
                    div('Spatiotemporal summaries of gridded data. One row for each site.',
                        class = 'widget-caption text-left',
                        style = 'margin-bottom: 6px'),
                    downloadButton('DL_SUBMIT_SPATIALSUMM'),

                    h3('Watershed attribute data, complete',
                       style = 'margin-bottom: 0px'),
                    div('Spatial summaries of gridded data in long (indexed) format, as time series where applicable.',
                        class = 'widget-caption text-left',
                        style = "padding-bottom: 6px"),
                        # style = 'margin-bottom: 6px'),
                    # div('NOTICE: each may take several minutes. Please wait.',
                    #     class = 'widget-caption text-left',
                    #     style = 'margin-bottom: 6px; color: red'),
                    HTML('<p><span>First, </span><span>'),
                    downloadButton('DL_SPATIALTS_METADATA'),
                    HTML(paste('</span><span> related metadata. Then, download data one category',
                               'at a time (to take it easy on our server).</span></p>')),
                    div('Choose category:',
                        style = "padding-bottom: 6px; padding-top: 6px",
                        class = 'widget-title text-left'),
                    # checkboxGroupInput(
                    radioButtons(
                        inputId = 'DL_SPATIALTS_SELECTIONS',
                        label = NULL,
                        choiceNames = c('Climate (precip, temp, deposition, ET ref)',
                                        'Hydrology (just BFI for now)',
                                        'Parent Material (soil, geochem)',
                                        'Terrain (slope, elev, aspect)',
                                        'Landcover (NLCD classes)',
                                        'Vegetation (phenology, metabolism, cover, etc.)'),
                        choiceValues = c('c', 'h', 'p', 't', 'l', 'v'),
                        selected = character(0),
                        width = '100%'
                    ),
                    downloadButton('DL_SUBMIT_SPATIALTS'),
                    hr(),
                    h3('GIS files',
                       style = 'margin-bottom: 0px'),
                    div('Includes watershed boundaries, stream site locations, and precip gauge locations.',
                        class = 'widget-caption text-left'),
                    br(),
                        # style = 'padding-bottom: 8px'),
                    div(style = "white-space: nowrap",
                        div('Choose filetype:',
                            style = "display: inline-block; vertical-align: middle; white-space: normal; padding-bottom: 6px",
                            class = 'widget-title text-left'
                        ),
                        div(class = 'ms-tooltip',
                            style = "display: inline-block; vertical-align: top",
                            title = paste('Shapefiles are lame, but we still use',
                                          'them. If you choose geoJSON, allow up',
                                          'to several minutes for conversion.'),
                            enc2native('\U2753')
                        ),
                        radioButtons('DL_FORMAT_GIS',
                                     label = NULL,
                                     choices = c('Shapefile', 'geoJSON'),
                                     selected = 'Shapefile',
                                     inline = TRUE),
                    ),
                    br(),

                    div(style = "white-space: nowrap",
                        div('Choose data domains:',
                            style = "display: inline-block; vertical-align: middle; white-space: normal;",
                            class = 'widget-title text-left'),
                        div(class = 'ms-tooltip',
                            style = "display: inline-block; vertical-align: top",
                            title = paste('MacroSheds data are organized by',
                                          'Network (top-level managing institution)',
                                          'Domain (collecting/reporting institution)',
                                          'and site (a stream gauge and its watershed).',
                                          'For more information, download our site',
                                          'data table.'),
                            enc2native('\U2753')
                        )
                    ),

                    div(style = 'position: relative; height: 3em',
                        h4('Network', style = 'position: absolute; font-color: gray; bottom: 0; margin-top: 0'),
                        h4('Domain', style = 'font-color: gray; margin-left: 150px; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Watersheds', style = 'font-color: gray; margin-left: 31em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Areas (ha)', style = 'font-color: gray; margin-left: 39em; position: absolute; margin-top: 0; bottom: 0'),
                        h4('Centroid (WGS84)', style = 'font-color: gray; margin-left: 45em; position: absolute; margin-top: 0; bottom: 0')
                        # h4('Zip size (~MB)', style = 'font-color: gray; margin-left: 53em; position: absolute; margin-top: 0; bottom: 0')
                    ),
                    uiOutput('DL_CHECKBOX_TREE2'),
                    br(),
                    downloadButton('DL_SUBMIT_GIS')
                )
            )
        )
    )
})
