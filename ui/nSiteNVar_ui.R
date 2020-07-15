nSiteNVar_tab = tabPanel("Multisite", value='multisite_exploration',
    sidebarLayout(
        sidebarPanel(
            div('Domains', class='widget-title text-center'),
            selectizeInput('DOMAINS3', label=NULL, selected=default_domain,
                choices=domains_pretty, multiple=TRUE,
                options=list(maxItems=3, allowEmptyOption=FALSE)),
            div('(Up to 3; populates sites)',
                class='widget-caption text-center'),
                # choices=domains_pretty, multiple=TRUE, options=list(maxItems=2)),
            # div('(Choose 1 or 2; populates sites)',
            #     class='widget-caption text-center'),
            br(),
            div('Sites', class='widget-title text-center'),
            selectizeInput('SITES3', label=NULL, selected=default_site,
                choices=default_sitelist, multiple=TRUE, options=list(maxItems=3)),
            div('(Up to 3; populates variables)',
                class='widget-caption text-center'),
            br(),
            div('Variables', class='widget-title text-center'),
            selectizeInput('VARS3', label=NULL,
                selected=chemvars_display_subset[[1]][[1]],
                multiple=TRUE, choices=chemvars_display_subset,
                options=list(maxItems=3)),
            div('(Up to 3)', class='widget-caption text-center'),
            br(),
            checkboxInput('SHOW_PCHEM3', value=FALSE,
                label=paste('Show precip chemistry', enc2native('\U2753'))),
            checkboxInput('SHOW_QC3', value=FALSE,
                label=paste('Show Q-C plots')),
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
            # radioButtons('CONC_FLUX3', label=NULL,
            #     choices=c('Concentration'='Concentration',
            #         'Flux (interpolated)'='Flux', 'VWC'='VWC'),
            #     selected='Concentration'),
            conditionalPanel('["Concentration", "VWC"].includes(input.CONC_FLUX3)',
                selectizeInput('CONC_UNIT3', label=NULL, choices=conc_units,
                    selected='mg/L')
            ),
            conditionalPanel('input.CONC_FLUX3 == "Flux"',
                selectizeInput('FLUX_UNIT3', label=NULL, choices=flux_units,
                    selected='kg/ha/d')
            ),
            br(),
            div(paste('Aggregation', enc2native('\U2753')),
                class='widget-title text-center',
                title=paste('Instantaneous and Daily aggregation require that',
                    'Unit is set to "Concentration" or "Flux (Interpolated)"',
                    'and "Show rain chemistry" is off."',
                    'Precipitation and concentration aggregate by mean,',
                    'flux by sum, and discharge by max')),
            radioButtons('AGG3', label=NULL, selected='Monthly',
                choices=c('Instantaneous', 'Daily', 'Monthly', 'Yearly')),
            # actionButton('DEBUG', 'debug'),
        width=3),

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
                    div(id='main3aP'),
                    dygraphOutput("GRAPH_PRECIP3a", height='75px'),
                    br(),
                    conditionalPanel('input.DOMAINS3.length > 1',
                        div(id='main3bP'),
                        dygraphOutput("GRAPH_PRECIP3b", height='75px'),
                        br()
                    ),
                    conditionalPanel('input.DOMAINS3.length > 2',
                        div(id='main3cP'),
                        dygraphOutput("GRAPH_PRECIP3c", height='75px'),
                        br()
                    ),
                    conditionalPanel(paste('input.VARS3 !== null &&',
                            'input.SITES3 !== null'),
                        #ONE WAY TO GET PLOTS SIDE-BY-SIDE
                        conditionalPanel('input.SHOW_QC3 == true',
                            div(id='main3a'),
                            fluidRow(
                                column(9,
                                    dygraphOutput("GRAPH_MAIN3a", height='125px')
                                ),
                                column(3,
                                    plotOutput('GRAPH_QC3a', height='125px')
                                )
                            )
                        ),
                        conditionalPanel('input.SHOW_QC3 == false',
                                    dygraphOutput("GRAPH_MAIN3aFULL", height='125px')
                        ),

                        # #ANOTHER WAY (troublesome)
                        # div(id='main3a'), #plot key: communicates with dygraphs
                        # div(id = 'inlineContainerA',# class = 'container', #style='width: 500px',
                        #     div(id = 'inlineQC3a', style = 'width: 0px',
                        #     # div(id = 'inlineQC3a', #style = 'display: none',
                        #             # style = 'display: inline-block; vertical-align: top',
                        #             # style = 'width: 200px; float: right',#; display: none',
                        #         plotOutput('GRAPH_QC3a', height='125px')
                        #     ),
                        #     div(id = 'inlineMAIN3a',
                        #             # style = 'display: inline-block; vertical-align: top',
                        #             # style = 'overflow: hidden',
                        #         dygraphOutput("GRAPH_MAIN3a", height='125px')
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
                    conditionalPanel('input.VARS3.length > 1',
                        div(id='main3b'),
                        dygraphOutput('GRAPH_MAIN3b', height='125px'),
                        br()
                    ),
                    conditionalPanel('input.VARS3.length > 2',
                        div(id='main3c'),
                        dygraphOutput("GRAPH_MAIN3c", height='125px'),
                        br()
                    ),
                    # fluidRow(
                    #     column(10, offset=1,
                            div(id='Q3'),
                    #     ),
                    #     column(1,
                    #         actionButton('EXPAND_Q3', icon('external-link-alt'))
                    #     )
                    # ),
                    dygraphOutput("GRAPH_Q3", height='75px')
                )
            )
        )
    )
)
