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

    d = readr::read_csv('data/general/catalog_files/all_sites.csv')
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
    # autoDestroy = FALSE,
    handler.quoted = TRUE,
    # suspended = TRUE,
    eventExpr = input$SITE_CATALOG_BUTTON,
    handlerExpr = site_catalog
)
