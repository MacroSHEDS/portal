site_comparison_tab = tabPanel(HTML('Biplot Supreme'),
    fluidRow(
        column(12, align='left',
            div(align='center', style=paste0(
                'display: inline-block;',
                'vertical-align:middle;'),
                p('Info and stuff; maybe some options')
            )
        )
    ),
    fluidRow(
        column(1,
            selectizeInput('Y_VAR', '', choices=grabcols,
                selected=grabcols[2])
            # div(id='Y_VAR', grabcols[2],
            #     style=paste0('-webkit-transform: rotate(270deg);',
            #     'transform: rotate(270deg);',
            #     'color: DodgerBlue'))
        ),
        column(11,
            plotOutput('MAIN_BIPLOT', brush='biplot_brush')
            # height='auto', width='auto')
        )
    ),
    fluidRow(
        column(12, align='left',
            selectizeInput('X_VAR', '', choices=grabcols,
                selected=grabcols[1])
        )
    )
)

