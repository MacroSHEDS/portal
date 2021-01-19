


output$SITE_CATALOG <- renderRHandsontable({

    site_table <- readr::read_csv('data/general/catalog_files/all_sites.csv')
    rhandsontable::rhandsontable(data = site_table,
                                 rowHeaders = NULL,
                                 readOnly = TRUE)
                                 # search = TRUE,
})
