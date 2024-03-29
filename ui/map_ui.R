map_tab <- tabPanel(
    # "Map",
    # mapdeckOutput("MAP")
    "Map",
    div(
        id = "mapcontainer",
        class = "well",
        leafletOutput("MAP", height = 350),
    ),
    br(),

    # shopping cart
    fluidRow(
        id = "attribute-table",
        column(
            width = 12,
            ## div(
            ##     id = "legend-container",
            ##     style = "position: absolute; right: 4em",
            ##     bsTooltip(
            ##         id = "legend-container",
            ##         title = "this is a BETA legend, improved legend coming soon",
            ##         placement = "bottom",
            ##         trigger = "hover"
            ##     ),
            ##     HTML('<div class="dropup" id="legend-button"><button class="btn btn-primary dropdown-toggle" type="button"
            ##     id="about-us" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">Legend
            ##     <span class="caret"></span></button><ul class="dropdown-menu" aria-labelledby="legend-drop">
            ##     <li><a id="No-Leg" href="#">No Legend</a></li>
            ##     <li><a id="3DEP-Leg" href="#">3DEP Elevation</a></li>
            ##     <li><a id="Tree-Leg" href="#">Tree Canopy</a></li>
            ##     <li><a id="Impervious-Leg" href="#">Impervious Surfaces</a></li>
            ##     <div class = "ms-tooltip" style = "margin-inline: 11%" title = "landcover imagery tiles do not visualize at lowest 8 zoom levels"><li><a id="Landcover-Leg" href="#">Landcover</a></li> </div>
            ##     <li><a id="LCChange-Leg" href="#">Landcover Change</a></li>
            ##     <li><a id="Geology-Leg" href="#">Geology</a></li></ul></div>')
            ##     # <li><a id="Ecoregions" href="#">Ecoregions</a></li> <li><a id="TreeCanopyChange-Leg" href="#">Tree Canopy Change</a></li>
            ## ),
            div(
                style = "width: 36px; position: absolute; right: 1em",
                actionLink("COLLAPSE_ATTRIBUTES",
                    label = "", icon = icon("menu-down", lib = "glyphicon", class = "full-map-mode gi-semi-x"),
                    class = "full-map-toggle",
                    # `data-toggle` = "offcanvas",
                    style = "margin: 6px"
                )
            ),
            tabsetPanel(
                id = "attribute-content",
                type = "tabs",
                selected = HTML('<span class="glyphicon glyphicon-shopping-cart"></span>'),
                tabPanel(
                    id = "bucket-info",
                    HTML('<span class="glyphicon glyphicon-question-sign"></span>'),
                    div(
                        includeHTML("ui/info_tab.html")
                        # style = "padding: 20px"
                    ),
                    div(
                        class = "label",
                        tags$footer(
                            p(
                                id = "notes-footer",
                                "go to 'Notes' tab for more information"
                            )
                            # class = "leftpanel-text"
                        )
                    )
                ),
                tabPanel(
                    id = "site-bucket",
                    HTML('<span class="glyphicon glyphicon-shopping-cart"></span>'),
                    style = "overflow-y:scroll; height: 300px;",
                    rank_list_basic,
                    div(
                        class = "label",
                        # put into html for "plot" button under lsit
                        # <button id="#GEN_PLOTS3" class="btn btn-sm btn-primary shiny-bound-input" style="">
                        # <a target="_blank" data-toggle="tooltip" data-placement="bottom" title="click to clear all the Map Selections">
                        # <span  class="glyphicon glyphicon-stats gi-semi-x"></span>
                        # </a>
                        # </button>
                        HTML('
                            <div class="text-center">
                            <button id="map-site-clear" class="btn btn-sm btn-primary" style="">
                            <a target="_blank" data-toggle="tooltip" data-placement="bottom" title="click to clear all the Map Selections">
                            <span class="glyphicon glyphicon-trash gi-semi-x"></span>
                            </a>
                            </button></div>')
                    )
                ),


                tabPanel(
                    "Watershed",
                    id = "watershed",
                    # textOutput("MAP_SITE_INFO_TITLE"),
                    tableOutput("MAP_SITE_INFO") %>%
                        tagAppendAttributes(class = "table") %>%
                        tagAppendAttributes(class = "horizontal-scroll"),
                    div(
                        class = "label",
                        tags$footer(
                            p(
                                id = "notes-footer",
                                "go to 'Notes' tab for more information"
                            )
                            # class = "leftpanel-text"
                        )
                    )
                ),

                tabPanel(
                    "Meta",
                    id = "meta-info",
                    ## tableOutput("MAP_SITE_INFO") %>%
                    ##     tagAppendAttributes(class = "table") %>%
                    ##     tagAppendAttributes(class = "horizontal-scroll"),
                    div(
                      class = "well",
                      ## div(
                      ##   id = "meta-menu",
                      ##   includeHTML("ui/meta/meta_list.html")
                      ## ),
                      selectInput(
                        "button",
                        "meta information",
                        choices = c(
                          "none",
                          "history",
                          "citation"
                        )
                      ),
                      hidden(
                        div(
                          id = "meta-disturbance",
                          class = "meta-table",
                          tableOutput("MAP_DISTURBANCE_INFO") %>%
                            tagAppendAttributes(class = "table") %>%
                            tagAppendAttributes(class = "horizontal-scroll")
                      )
                    ),
                    hidden(
                        div(
                          id = "meta-citation",
                          class = "meta-table",
                          tableOutput("MAP_CITATION_INFO") %>%
                            tagAppendAttributes(class = "table") %>%
                            tagAppendAttributes(class = "horizontal-scroll")

                      )
                    )
                    )
                ),

                tabPanel( # at some pont, make list element float right
                    id = "Legend",
                    HTML('<span class="glyphicon glyphicon-list-alt"></span>'),
                    style = "overflow-y:scroll; height: 300px;",
                  div(class='well',
                      includeHTML("ui/legend/legend_list.html"),
                      includeHTML("ui/legend/nlcd.html"),
                      includeHTML("ui/legend/3DEP.html"),
                      includeHTML("ui/legend/geology.html"),
                      includeHTML("ui/legend/impervious.html"),
                      includeHTML("ui/legend/ecoregion.html"),
                      includeHTML("ui/legend/nlcd_change.html"),
                      includeHTML("ui/legend/temp.html")
                      ),
                    div(
                        class = "label",
                        tags$footer(
                            p(
                                id = "notes-footer",
                                "go to 'Notes' tab for more information"
                            ),
                            # class = "leftpanel-text"
                        )
                    )
                )
            )
        )
    ),
    # this button opens the site exploration tab. the button is hidden and unclickable.
    # it's triggered by links in the popups on the map tab.
    actionButton("SITE_EXPLORE", "", style = "display: none"),

    # DT::dataTableOutput('MAP_SITE_INFO')
    # div('Site Information', class = 'widget-title text-center'),
    # div(
    #     class = "label",
    #     HTML('<span class="glyphicon glyphicon-trash"> </span> <p> click to empty all site selections </p>'),
    #     tags$footer(
    #         p(
    #             id = "notes-footer",
    #             "go to 'Notes' tab for more information"
    #         ),
    #         # class = "leftpanel-text"
    #     )
    # )
)
