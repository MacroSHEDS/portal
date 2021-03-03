#
# output$SITE_CATALOG <- renderRHandsontable({
#
#     site_table <- readr::read_csv('data/general/catalog_files/all_sites.csv')
#
#     rhandsontable::rhandsontable(data = site_table,
#                                  rowHeaders = NULL,
#                                  # search = TRUE,
#                                  height = 700,
#                                  readOnly = TRUE) %>%
#         rhandsontable::hot_cols(colWidths = 100) %>%
#         rhandsontable::hot_rows(rowHeights = 50) %>%
#         rhandsontable::hot_table(overflow = 'hidden')
# })

output$SITE_CATALOG <- DT::renderDataTable({

    d <- sm(readr::read_csv('data/general/catalog_files/all_sites.csv'))
    DT::datatable(d,
                  options = list(scrollX = 'true'),
                  escape = FALSE)
                                    # autoWidth = 'true'
                                    # columnDefs = list(list(width = '50px',
                                    #                        targets = 1))
        # '[{ targets: [0, 1], width: 250},{ targets: "_all", width: 50}]'
})

output$VARIABLE_CATALOG <- DT::renderDataTable(
    expr = {
        d <- sm(readr::read_csv('data/general/catalog_files/all_variables.csv'))
        DT::datatable(d,
                      options = list(scrollX = 'true'),
                      escape = FALSE)
    }
    # server = FALSE
)

output$VARIABLE_SUBCATALOG <- DT::renderDataTable({

    d <- sm(readr::read_csv(glue('data/general/catalog_files/indiv_variables/{v}.csv',
                         v = isolate(input$VARIABLE_SUBCATALOG_BUTTON_LISTENER))))
    DT::datatable(d, options = list(scrollX = 'true'))
})

output$SITE_SUBCATALOG <- DT::renderDataTable({
    d <- sm(readr::read_csv(glue('data/general/catalog_files/indiv_sites/{s}.csv',
                         s = isolate(input$SITE_SUBCATALOG_BUTTON_LISTENER))))
    DT::datatable(d, options = list(scrollX = 'true'))
})

# output$SITE_CATALOG <- renderUI(tagList(
#     fluidPage(
#         fluidRow(
#             column(12,
#                    # HTML("<h3><a href='?home'>Home</a> | ",
#                    #      "<a href='?old-faithful'>Old Faithful</a> |",
#                    #      "<a href='?page3'>Nothing</a>",
#                    #      "</h3>")
#
#             )
#         ),
#         uiOutput('TEST')
#     )
# ))

# fname <- isolate(session$clientData$url_search)
# print(fname)
#
# output$TEST <- renderUI(taglist(
#     fluidRow(
#         column(5,
#                HTML("<h2>404 Not Found Error:</h2><p>Donkey</p>")
#         )
#     )
# ))

# if(nchar(fname) == 0) fname <- '?home'
# fname <- paste0(substr(fname, 2, nchar(fname)), '.R')
# source(fname, local = TRUE)

# router$server(input, output, session)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    autoDestroy = FALSE,
    handler.quoted = TRUE,
    # suspended = TRUE,
    eventExpr = input$SITE_CATALOG_BUTTON,
    handlerExpr = site_catalog
)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$VARIABLE_CATALOG_BUTTON,
    handlerExpr = variable_catalog
)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$VARIABLE_SUBCATALOG_BUTTON_LISTENER,
    handlerExpr = variable_subcatalog
)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$SITE_SUBCATALOG_BUTTON_LISTENER,
    handlerExpr = site_subcatalog
)
