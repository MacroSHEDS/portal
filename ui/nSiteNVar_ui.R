nSiteNVar_tab <- tabPanel("Inspection",
                          value = 'multisite_exploration',

    sidebarLayout(
        sidebarPanel(width = 3,

            #domain selector
            div('Domains',
                class = 'widget-title text-center'),

            selectizeInput('DOMAINS3',
                           label = NULL,
                           selected = default_domain,
                           choices = domains_pretty,
                           multiple = TRUE,
                           options = list(maxItems = 3,
                                          allowEmptyOption = FALSE)),

            div('(Up to 3; populates sites)',
                class = 'widget-caption text-center'),
            br(),

            #site selector
            div('Sites',
                class = 'widget-title text-center'),

            selectizeInput('SITES3',
                           label = NULL,
                           selected = default_site,
                           choices = default_sitelist,
                           multiple = TRUE,
                           options = list(maxItems = 3)),

            div('(Up to 3; populates variables)',
                class = 'widget-caption text-center'),
            br(),

            #variable selector
            div('Variables',
                class = 'widget-title text-center'),

            selectizeInput('VARS3',
                           label = NULL,
                           selected = chemvars_display_subset[[1]][[1]],
                           multiple = TRUE,
                           choices = chemvars_display_subset,
                           options = list(maxItems = 3)),

            div('(Up to 3)',
                class = 'widget-caption text-center'),
            br(),

            #display options (set 1)
            checkboxInput('SHOW_PCHEM3',
                          value = FALSE,
                          label = paste('Show precip chemistry')),
                                        # enc2native('\U2753'))),

            div(style = "white-space: nowrap",
                div(style = "display: inline-block; vertical-align: middle; white-space: normal",
                    checkboxInput('SHOW_QC3',
                                  value = FALSE,
                                  label = paste('Show Q-C (Q-F) plots')),
                ),
                div(class = 'ms-tooltip',
                    style = "display: inline-block; vertical-align: middle",
                    title = paste('Discharge vs. concentration or flux, (precip',
                                  'and precip chem excluded); the y-axis',
                                  'reflects all selections made in',
                                  'the Unit section below.'),
                    enc2native('\U2753')
                )
            ),
            br(),

            # conditionalPanel('input.SHOW_PCHEM3 == true',
            #     fluidRow(
            #         column(8, offset=2,
            #             div(style=paste0('border: 1px solid; border-color:',
            #                 raincolor, '; padding: 6px; background-color: ',
            #                 raincolorpale),
            #                 div('Rain gauge', class='widget-title-sm text-center'),
            #                 selectizeInput('RAINSITES3', label=NULL,
            #                     selected=sites_with_P$hbef[1],
            #                     choices=sites_with_P$hbef),
            #                 div('(populates rain variables)',
            #                     class='widget-caption text-center',
            #                     style='color: #333000'),
            #                 br(),
            #                 div('Rain chem variables',
            #                     class='widget-title-sm text-center'),
            #                 selectizeInput('RAINVARS3', label=NULL,
            #                     selected=pchemvars_display_subset[[1]][[1]],
            #                     multiple=TRUE, choices=pchemvars_display_subset,
            #                     options=list(maxItems=3))
            #             ),
            #             br()
            #         )
            #     )
            # ),

            div('Unit', class='widget-title text-center'),
            div('(Applies to solutes only)',
                class='widget-caption text-center'),
            radioButtons('CONC_FLUX3', label=NULL,
                choices=conc_flux_names, selected='Concentration'),
            conditionalPanel('["Concentration", "VWC"].includes(input.CONC_FLUX3)',
                selectizeInput('CONC_UNIT3', label=NULL, choices=conc_units,
                    selected='mg/L')
            ),
            conditionalPanel('input.CONC_FLUX3 == "Flux"',
                selectizeInput('FLUX_UNIT3', label=NULL, choices=flux_units,
                    selected='kg/ha/d')
            ),
            br(),
            div(style = "white-space: nowrap",
                class = 'widget-title text-center',
                div(style = "display: inline-block; vertical-align: middle; white-space: normal",
                    div('Aggregation')
                ),
                div(class = 'ms-tooltip',
                    style = "display: inline-block; vertical-align: middle",
                    title = paste0('Daily aggregation requires that ',
                    # title=paste('Instantaneous and Daily aggregation require that',
                                 'Unit is set to "Concentration" or "Flux". ',
                                 # 'and "Show rain chemistry" is off."',
                                 'Precipitation aggregates by sum',
                                 enc2native('\U02014'),
                                 'everything else by mean. See Notes/Caveats tab for more'),
                    enc2native('\U2753')
                )
            ),
            # div(paste('Aggregation', enc2native('\U2753')),
            #     class='widget-title text-center',
            #     title=paste0('Daily aggregation requires that ',
            #     # title=paste('Instantaneous and Daily aggregation require that',
            #         'Unit is set to "Concentration" or "Flux". ',
            #         # 'and "Show rain chemistry" is off."',
            #         'Precipitation aggregates by sum',
            #         enc2native('\U02014'),
            #         'everything else by mean.')),
            radioButtons('AGG3', label=NULL, selected='Monthly',
                # choices=c('Instantaneous', 'Daily', 'Monthly', 'Yearly')),
                choices=c('Daily', 'Monthly', 'Yearly')),
            hr(),

            div(class = 'text-center',
                actionButton('ADDTL_OPTIONS',
                             label = 'Additional options',
                             class = 'text-center')
            ),

            conditionalPanel('input.ADDTL_OPTIONS % 2 == 1',

                br(),
                div(style = 'display: none', #hiding this until we reactivate high res mode
                div('Time system',
                    class = 'widget-title text-center'),

                selectizeInput('TIME_SCHEME3',
                               label = NULL,
                               choices = c('UTC', 'Local', 'Solar'),
                               selected = 'UTC'),
                br()
                ),

                div('Include data collected by:',
                    class = 'widget-title text-center'),
                div('(must check at least one)',
                    class = 'widget-caption text-center'),

                checkboxGroupInput('INSTALLED_V_GRAB3',
                                    label = NULL,
                                    inline = FALSE,
                                    choiceNames = c('Grab sample', 'Installed unit'),
                                    choiceValues = c('G', 'I'),
                                    selected = c('G', 'I')),

                # checkboxInput('SHOW_GRAB3',
                #               label = 'Grab sample',
                #               value = TRUE),

                # checkboxInput('SHOW_INSTALLED3',
                #               label = 'Installed unit',
                #               value = TRUE),
                br(),

                div('Include data measured with:',
                    class = 'widget-title text-center'),
                div('(must check at least one)',
                    class = 'widget-caption text-center'),

                checkboxGroupInput('SENSOR_V_NONSENSOR3',
                                   label = NULL,
                                   inline = FALSE,
                                   choiceNames = c('Sensor', 'Lab-analysis/Other'),
                                   choiceValues = c('S', 'N'),
                                   selected = c('S', 'N')),
                br(),

                div('Uncertainty',
                    class = 'widget-title text-center'),
                div('(See Notes/Caveats tab for details; not shown for Q or P.)',
                    class = 'widget-caption text-center'),

                div(class = 'text-center',
                    checkboxInput('SHOW_UNCERT3',
                                  label = 'Show bounds',
                                  value = FALSE)
                ),
                br(),

                div(paste('Hide/show points'),
                    class = 'widget-title text-center'),

                div(style = "white-space: nowrap",
                    div(style = "display: inline-block; vertical-align: middle; white-space: normal",
                        checkboxInput('FLAGS3',
                              label = 'With minor QA/QC issue',
                              # label = paste('With minor QA/QC issue',
                                            # enc2native('\U2753')),
                              value = TRUE)
                    ),
                    div(class = 'ms-tooltip',
                        style = "display: inline-block; vertical-align: middle",
                        title = paste('Points flagged as erroneous (bad data)',
                                      'have been removed from the dataset.',
                                      'Points flagged as questionable can be',
                                      'toggled here.'),
                        enc2native('\U2753')
                    )
                ),

                div(style = "white-space: nowrap",
                    div(style = "display: inline-block; vertical-align: middle; white-space: normal",
                        checkboxInput('INTERP3',
                                      label = 'Imputed by MacroSheds',
                                      value = TRUE)
                    ),
                    div(class = 'ms-tooltip',
                        style = "display: inline-block; vertical-align: middle",
                        title = paste('We linearly interpolate data gaps of up',
                                      'to 3 days for discharge and precip, and',
                                      'up to 15 days for chemistry.'),
                        enc2native('\U2753')
                    )
                )
            )
        ),

        mainPanel(
            fluidRow(class='text-center',
                wellPanel(
                    sliderInput("DATES3", label=NULL, min=dtrng[1], max=dtrng[2],
                        value=most_recent_year(dtrng),
                        width='100%', timeFormat='%b %Y', step=30,
                        dragRange=TRUE),
                    # fluidRow(class='text-right',
                    #     actionButton('EXPAND_PRECIP3', icon('external-link-alt'))
                    # ),

                    #REFRESH can be clicked by js to trigger R events
                    actionButton('REFRESH', '', style='display: none'),

                    #precip facets (just one now)
                    div(id='P3'),
                    dygraphOutput("GRAPH_PRECIP3", height='75px'),
                    br(),
                    # conditionalPanel('input.SITES3.length > 1',
                    #     div(id='main3bP'),
                    #     dygraphOutput("GRAPH_PRECIP3b", height='75px'),
                    #     br()
                    # ),
                    # conditionalPanel('input.SITES3.length > 2',
                    #     div(id='main3cP'),
                    #     dygraphOutput("GRAPH_PRECIP3c", height='75px'),
                    #     br()
                    # ),

                    #facet A
                    conditionalPanel(paste('input.VARS3 !== null &&',
                            'input.SITES3 !== null'),

                        # #ONE WAY TO GET PLOTS SIDE-BY-SIDE (requires
                        # #two output objects with two legend divs)
                        # div(id='main3a'),
                        # conditionalPanel('input.SHOW_QC3 == true',
                        #     fluidRow(
                        #         column(9,
                        #             dygraphOutput("GRAPH_MAIN3a", height='125px')
                        #         ),
                        #         column(3,
                        #             plotOutput('GRAPH_QC3a', height='125px')
                        #         )
                        #     )
                        # ),
                        # conditionalPanel('input.SHOW_QC3 == false',
                        #     dygraphOutput("GRAPH_MAIN3aFULL", height='125px'),
                        # ),

                        #ANOTHER WAY (requires somewhat hacky css and js)
                        div(id = 'main3a'), #plot key: communicates with dygraphs
                        div(id = 'inlineContainerA',
                            style = 'font-size: 0px',
                            div(id = 'inlineMAIN3a',
                                style = 'width: 100%; display: inline-block; vertical-align: top',
                                dygraphOutput("GRAPH_MAIN3a", height='125px')
                            ),
                            div(id = 'inlineQC3a',
                                style = 'width: 0%; display: inline-block; vertical-align: top',
                                # conditionalPanel('input.SHOW_QC3 == true',
                                    plotOutput('GRAPH_QC3a', height='125px')
                                # )
                            )
                        ),

                        # #FOR TESTING
                        # div(id='main3a'), #plot key: communicates with dygraphs
                        # div(id = 'inlineContainerA',
                        #     style = 'font-size: 0px',
                        #     div(id = 'inlineMAIN3a',
                        #         style = 'width: 75%; display: inline-block; vertical-align: top',
                        #         dygraphOutput("GRAPH_MAIN3a", height='125px')
                        #     ),
                        #     div(id = 'inlineQC3a',
                        #         style = 'width: 25%; display: inline-block; vertical-align: top',
                        #             plotOutput('GRAPH_QC3a', height='125px')
                        #     )
                        # ),

                        # #A THIRD WAY (nope)
                        # div(id='main3a'), #plot key: communicates with dygraphs
                        # fluidRow(
                        #     dygraphOutput("GRAPH_MAIN3a", height='125px', width='auto'),
                        #     plotOutput('GRAPH_QC3a', height='125px', width='25%')
                        # ),

                        # actionButton('EXPAND_MAIN3a', icon('external-link-alt')),
                        br()
                    ),

                    ##old attempt to build facets B and C to be dynamic
                    # conditionalPanel('input.VARS3.length > 1',
                    #     conditionalPanel('input.SHOW_QC3 == true',
                    #         div(id='main3b'),
                    #         fluidRow(
                    #             column(9,
                    #                 dygraphOutput("GRAPH_MAIN3b", height='125px')
                    #             ),
                    #             column(3,
                    #                 plotOutput('GRAPH_QC3b', height='125px')
                    #             )
                    #         )
                    #     ),
                    #     conditionalPanel('input.SHOW_QC3 == false',
                    #         dygraphOutput("GRAPH_MAIN3bFULL", height='125px')
                    #     ),
                    #     br()
                    # ),
                    # conditionalPanel('input.VARS3.length > 2',
                    #     conditionalPanel('input.SHOW_QC3 == true',
                    #         div(id='main3b'),
                    #         fluidRow(
                    #             column(9,
                    #                 dygraphOutput("GRAPH_MAIN3c", height='125px')
                    #             ),
                    #             column(3,
                    #                 plotOutput('GRAPH_QC3c', height='125px')
                    #             )
                    #         )
                    #     ),
                    #     conditionalPanel('input.SHOW_QC3 == false',
                    #         dygraphOutput("GRAPH_MAIN3cFULL", height='125px')
                    #     ),
                    #     br()
                    # ),
                    #
                    # conditionalPanel('input.VARS3.length > 1',
                    #     div(id='main3b'),
                    #     dygraphOutput('GRAPH_MAIN3b', height='125px'),
                    #     br()
                    # ),
                    # conditionalPanel('input.VARS3.length > 2',
                    #     div(id='main3c'),
                    #     dygraphOutput("GRAPH_MAIN3c", height='125px'),
                    #     br()
                    # ),
                    # fluidRow(
                     #    column(10, offset=1,
                      #      div(id='graph_q3'),
                       #  ),
                        # column(1,
                         #    actionButton('EXPAND_Q3', icon('external-link-alt'))
                        # )
                     #),

                    #facet B
                    conditionalPanel('input.VARS3.length > 1',
                        div(id='main3b'),
                        div(id = 'inlineContainerB',
                            style = 'font-size: 0px',
                            div(id = 'inlineMAIN3b',
                                style = 'width: 100%; display: inline-block; vertical-align: top',
                                dygraphOutput("GRAPH_MAIN3b", height='125px')
                            ),
                            div(id = 'inlineQC3b',
                                style = 'width: 0%; display: inline-block; vertical-align: top',
                                conditionalPanel('input.SHOW_QC3 == true',
                                    plotOutput('GRAPH_QC3b', height='125px')
                                )
                            )
                        )
                    ),

                    #facet C
                    conditionalPanel('input.VARS3.length > 2',
                        div(id='main3c'),
                        div(id = 'inlineContainerC',
                            style = 'font-size: 0px',
                            div(id = 'inlineMAIN3c',
                                style = 'width: 100%; display: inline-block; vertical-align: top',
                                dygraphOutput("GRAPH_MAIN3c", height='125px')
                            ),
                            div(id = 'inlineQC3c',
                                style = 'width: 0%; display: inline-block; vertical-align: top',
                                conditionalPanel('input.SHOW_QC3 == true',
                                    plotOutput('GRAPH_QC3c', height='125px')
                                )
                            )
                        )
                    ),

                    #discharge plot
                    div(id='Q3'),
                    dygraphOutput("GRAPH_Q3", height='75px'),
                    br(),
                    HTML(paste0('<p style="color: blue; font-size: 0.75em">',
                                'Click and drag to zoom. Double-click to reset view. ',
                                'You may have to double-click after changing ',
                                '<strong><span style="color: var(--darkblue)">Sites</span></strong> ',
                                'selection.</p>'))
                      # style = 'color: blue; font-size: 0.75em')
                )
            )
        )
    )
)
