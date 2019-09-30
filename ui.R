
source("helpers.R")
source('ui/oneSiteNVar_ui.R')
source('ui/nSiteNVar_ui.R')
source('ui/site_comparison_ui.R')
source('ui/about_ui.R')
source('ui/summary_biplot_ui.R')
source('ui/map_ui.R')

shinyUI(fluidPage(

    #screen shouldn't go gray when plots are updating.
    # tags$style(type="text/css", ".recalculating { opacity: 1.0; }" ),
    tags$head(tags$style(HTML(
        "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css")),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script='js/general.js'),
    # navbarPage(title=p(strong(a('MacroSHEDS',
    #     href='https://macrosheds.org/'))), inverse=TRUE,
    #     windowTitle='MacroSHEDS',

#     # sidebarLayout(
#     # fluidRow(
#         # column(width=4, class='collapse width',
#     bsCollapse(id='MAINCOLLAPSE', open='Hide top panel',
#         # sidebarPanel(width=4,
#         bsCollapsePanel('left_collapse', value='Hide top panel',
#             tabsetPanel(id='left_tabs',
#                 about_tab,
#                 map_tab
#            )
#         )
#     ),
# # ),
# # column(width=8,
# # mainPanel(width=8,
#     tabsetPanel(id='right_tabs',
#         summary_biplot_tab,
#         oneSiteNVar_tab,
#         site_comparison_tab
#     )
#         # )
#     # )

    dashboardPage(
        dashboardHeader(disable=TRUE),
        # dashboardHeader(),
        dashboardSidebar(width='50%',
            # sidebarMenuOutput("COLLAPSIBLE_SECTION"),
            # div(style='display: inline-block; float: left',
            div(class='sidebar-sub',
            # div(style='display: inline-block; float:left',
            # column(width=11,
                tabsetPanel(id='left_tabs',
                    about_tab,
                    map_tab
                )
            ),
            # div(style='width: 36px; display: inline-block; position: absolute; float: right',
            div(style='width: 36px; display: inline-block; float: right',
            # div(style='display: inline-block; float:right',
            # column(width=1,
                actionLink('COLLAPSE_SIDEBAR', label='', icon=icon('arrows-h'),
                    class='sidebar-toggle', `data-toggle`='offcanvas',
                    style='margin: 6px')
            )
        ),
        dashboardBody(
            tags$head(
                tags$link(rel='stylesheet', type='text/css', href='style.css')
            ),
            tabsetPanel(id='right_tabs',
                summary_biplot_tab,
                oneSiteNVar_tab,
                nSiteNVar_tab,
                site_comparison_tab
            )
        )
    )
))
