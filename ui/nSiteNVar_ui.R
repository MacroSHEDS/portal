nSiteNVar_tab <- tabPanel("Time Series",
                          value = 'multisite_exploration',

    sidebarLayout(
        sidebarPanel(width = 3,

            div(class = 'text-center',
                actionButton('GEN_PLOTS3',
                             label = 'Update Plots',
                             class = 'text-center, btn btn-block btn-primary')
            ),
            br(),

            div(style = 'display: none',

                #these are ineligant alternatives to an "invalidateInput" function
                #that must exist to accompany updateSelectizeInput, etc., but
                #seemingly doesn't
                sliderInput('DATES_INVISIBLE3',
                            label = NULL,
                            min = dtrng[1],
                            max = dtrng[2],
                            value = dtrng),
                selectizeInput('VARS_INVISIBLE3',
                               label = NULL,
                               selected = 'a',
                               multiple = TRUE,
                               choices = c('a', 'b'),
                               options = list(maxItems = 3))
            ),

            #domain selector
            div('Domains',
                class = 'widget-title text-center'),

            div(id = 'domains_div',
                selectizeInput('DOMAINS3',
                               label = NULL,
                               selected = default_domain,
                               choices = domains_pretty,
                               multiple = TRUE,
                               options = list(maxItems = 3,
                                              allowEmptyOption = FALSE))
            ),

            div('(Up to 3; populates sites)',
                class = 'widget-caption text-center'),
            br(),

            #site selector
            div('Sites',
                class = 'widget-title text-center'),

            div(id = 'sites_div',
                selectizeInput('SITES3',
                               label = NULL,
                               selected = default_site,
                               choices = default_sitelist,
                               multiple = TRUE,
                               options = list(maxItems = 3))
            ),

            div('(Up to 3; populates variables)',
                class = 'widget-caption text-center'),
            br(),

            #variable selector
            div('Variables',
                class = 'widget-title text-center'),

            selectizeInput('VARS3',
                           label = NULL,
                           selected = 'SO4_S',
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

            div(id = "showqc",
                style = "white-space: nowrap",
                div(style = "display: inline-block; vertical-align: middle; white-space: normal",
                    checkboxInput('SHOW_QC3',
                                  value = FALSE,
                                  label = paste('Show Q-C (Q-F) plots')),
                ),
            ),

            br(),

            div('Unit', class='widget-title text-center'),
            div('(Applies to solutes only)',
                class='widget-caption text-center'),
            div(id = 'cf_div',
                radioButtons('CONC_FLUX3', label=NULL,
                    choices=conc_flux_names, selected='Concentration')
            ),
            conditionalPanel('["Concentration", "VWC"].includes(input.CONC_FLUX3)',
                selectizeInput('CONC_UNIT3', label=NULL, choices=conc_units,
                    selected='mg/L')
            ),
            conditionalPanel('input.CONC_FLUX3 == "Flux"',
                selectizeInput('FLUX_UNIT3', label=NULL, choices=flux_units,
                    selected='kg/ha/d')
            ),
            br(),

            div(id="aggregation"),

            radioButtons('AGG3', label=NULL, selected='Monthly',
                # choices=c('Instantaneous', 'Daily', 'Monthly', 'Yearly')),
                choices=c('Daily', 'Monthly', 'Yearly')),
            hr(),

            div(id = 'addtl_div',
                class = 'text-center',
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
                        value=dtrng,
                        width='100%', timeFormat='%b %Y', step=30,
                        dragRange=TRUE),

                    #REFRESH can be clicked by js to trigger R events
                    actionButton('REFRESH', '', style='display: none'),

                    #precip facets (just one now)
                    div(id='P3'),
                    dygraphOutput("GRAPH_PRECIP3", height='75px'),
                    br(),

                    #facet A
                    conditionalPanel(paste('input.VARS3 !== null &&',
                            'input.SITES3 !== null'),

                        div(id = 'main3a'), #plot key: communicates with dygraphs
                        div(id = 'inlineContainerA',
                            style = 'font-size: 0px',
                            div(id = 'inlineMAIN3a',
                                style = 'width: 100%; display: inline-block; vertical-align: top',
                                dygraphOutput("GRAPH_MAIN3a", height='125px')
                            ),
                            div(id = 'inlineQC3a',
                                style = 'width: 0%; display: inline-block; vertical-align: top',
                                conditionalPanel('output.SHOW_QC_GEN3 == true',
                                    plotOutput('GRAPH_QC3a', height='125px')
                                 )
                            )
                        ),
                        br()
                    ),

                    #facet B
                    conditionalPanel('output.n_plots3 > 1',
                        div(id='main3b'),
                        div(id = 'inlineContainerB',
                            style = 'font-size: 0px',
                            div(id = 'inlineMAIN3b',
                                style = 'width: 100%; display: inline-block; vertical-align: top',
                                dygraphOutput("GRAPH_MAIN3b", height='125px')
                            ),
                            div(id = 'inlineQC3b',
                                style = 'width: 0%; display: inline-block; vertical-align: top',
                                conditionalPanel('output.SHOW_QC_GEN3 == true',
                                    plotOutput('GRAPH_QC3b', height='125px')
                                )
                            )
                        )
                    ),

                    #facet C
                    conditionalPanel('output.n_plots3 > 2',
                        div(id='main3c'),
                        div(id = 'inlineContainerC',
                            style = 'font-size: 0px',
                            div(id = 'inlineMAIN3c',
                                style = 'width: 100%; display: inline-block; vertical-align: top',
                                dygraphOutput("GRAPH_MAIN3c", height='125px')
                            ),
                            div(id = 'inlineQC3c',
                                style = 'width: 0%; display: inline-block; vertical-align: top',
                                conditionalPanel('output.SHOW_QC_GEN3 == true',
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
                                'Click and drag to zoom. Double-click to reset view.</p>'))
                )
            )
        )
    )
)
