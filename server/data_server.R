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

output$DL_CHECKBOX_TREE <- renderUI({

    dlcheck_template_network <- read_file('ui/dlcheck_template_network.html')
    dlcheck_template_domain <- read_file('ui/dlcheck_template_domain.html')
    dlcheck_template_site <- read_file('ui/dlcheck_template_site.html')

    networks <- network_domain_default_sites %>%
        select(network, pretty_network) %>%
        distinct()

    # firstRecord <- sm(readr::read_csv('data/general/catalog_files/all_sites.csv')) %>%
    #     select(Network, Domain, SiteCode, FirstRecordUTC)

    dom_summ <- site_data %>%
        filter(site_type == 'stream_gauge') %>%
        group_by(network, domain) %>%
        summarize(pretty_domain = first(pretty_domain),
                  nsites = length(site_code),
                  areamin = round(min(ws_area_ha, na.rm = TRUE), 0),
                  areamax = round(max(ws_area_ha, na.rm = TRUE), 0),
                  latmean = round(mean(latitude, na.rm = TRUE), 1),
                  lonmean = round(mean(longitude, na.rm = TRUE), 1),
                  .groups = 'drop') %>%
        left_join(read_csv('data/general/download_sizes/timeseries.csv'),
                  by = 'domain') %>%
        mutate(dl_size_MB = ifelse(is.na(dl_size_MB), 'pending', dl_size_MB)) %>%
        # mutate(summstring = glue('[watersheds: {ns}; area range (ha): {amin}-{amax};',
        #                          ' centroid (WGS84): {latm}, {lonm}]',
        mutate(summstring = glue('{d} {ns}&nbsp;&nbsp;&nbsp;{amin} - {amax}',
                                 '&nbsp;&nbsp;&nbsp;({latm}, {lonm})&nbsp;&nbsp;&nbsp;{dl}',
                                 d = str_trunc(pretty_domain, 50, side = 'right',
                                               ellipsis = '...^') %>%
                                     str_pad(50, side = 'right', pad = '^'),
                                 ns = str_pad(nsites, 4, side = 'left', pad = '^'),
                                 amin = str_pad(areamin, 6, side = 'left', pad = '^'),
                                 amax = str_pad(areamax, 6, side = 'right', pad = '^'),
                                 latm = str_pad(latmean, 5, side = 'left', pad = '^'),
                                 lonm = str_pad(lonmean, 6, side = 'right', pad = '^'),
                                 dl = str_pad(dl_size_MB, 10, side = 'left', pad = '^')),
               summstring = str_replace_all(summstring, '\\^', '&nbsp;')) %>%
               # summstring = str_replace_all(summstring, '\\^', '&#x2800;')) %>%
        select(network, domain, summstring)

    checkboxlist <- list()
    checkboxlist_counter <- 0
    for(i in 1:nrow(networks)){

        checkboxlist_counter <- checkboxlist_counter + 1
        checkboxlist[[checkboxlist_counter]] <- HTML(glue(
            dlcheck_template_network,
            nn = networks$network[i],
            N = networks$pretty_network[i]))

        # domains <- network_domain_default_sites %>%
        #     filter(network == networks$network[i]) %>%
        #     select(domain, pretty_domain) %>%

        domains <- dom_summ %>%
            filter(network == networks$network[i])

        for(j in 1:nrow(domains)){

            checkboxlist_counter <- checkboxlist_counter + 1
            checkboxlist[[checkboxlist_counter]] <- HTML(glue(
                dlcheck_template_domain,
                nn = networks$network[i],
                dd = domains$domain[j],
                D = domains$summstring[j]))

            # sites <- site_data %>%
            #     filter(network == networks$network[i],
            #            domain == domains$domain[j]) %>%
            #     select(site_code, full_name)
            #
            # for(k in 1:nrow(sites)){
            #
            #     checkboxlist_counter <- checkboxlist_counter + 1
            #     checkboxlist[[checkboxlist_counter]] <- HTML(glue(
            #         dlcheck_template_site,
            #         nn = networks$network[i],
            #         dd = domains$domain[j],
            #         ss = sites$site_code[k],
            #         S = paste0(sites$full_name[k], ' (', sites$site_code[k], ')')))
            # }
        }
    }

    return(checkboxlist)
})

output$DL_CHECKBOX_TREE2 <- renderUI({

    dlcheck_template_network <- read_file('ui/dlcheck_template_network.html')
    dlcheck_template_domain <- read_file('ui/dlcheck_template_domain.html')
    dlcheck_template_site <- read_file('ui/dlcheck_template_site.html')

    networks <- network_domain_default_sites %>%
        select(network, pretty_network) %>%
        distinct()

    # firstRecord <- sm(readr::read_csv('data/general/catalog_files/all_sites.csv')) %>%
    #     select(Network, Domain, SiteCode, FirstRecordUTC)

    dom_summ <- site_data %>%
        filter(site_type == 'stream_gauge') %>%
        group_by(network, domain) %>%
        summarize(pretty_domain = first(pretty_domain),
                  nsites = length(site_code),
                  areamin = round(min(ws_area_ha, na.rm = TRUE), 0),
                  areamax = round(max(ws_area_ha, na.rm = TRUE), 0),
                  latmean = round(mean(latitude, na.rm = TRUE), 1),
                  lonmean = round(mean(longitude, na.rm = TRUE), 1),
                  .groups = 'drop') %>%
        left_join(read_csv('data/general/download_sizes/timeseries.csv'),
                  by = 'domain') %>%
        mutate(dl_size_MB = ifelse(is.na(dl_size_MB), 'pending', dl_size_MB)) %>%
        # mutate(summstring = glue('[watersheds: {ns}; area range (ha): {amin}-{amax};',
        #                          ' centroid (WGS84): {latm}, {lonm}]',
        mutate(summstring = glue('{d} {ns}&nbsp;&nbsp;&nbsp;{amin} - {amax}',
                                 '&nbsp;&nbsp;&nbsp;({latm}, {lonm})',
                                 d = str_trunc(pretty_domain, 50, side = 'right',
                                               ellipsis = '...^') %>%
                                     str_pad(50, side = 'right', pad = '^'),
                                 ns = str_pad(nsites, 4, side = 'left', pad = '^'),
                                 amin = str_pad(areamin, 6, side = 'left', pad = '^'),
                                 amax = str_pad(areamax, 6, side = 'right', pad = '^'),
                                 latm = str_pad(latmean, 5, side = 'left', pad = '^'),
                                 lonm = str_pad(lonmean, 6, side = 'right', pad = '^')),
                                 # dl = str_pad(dl_size_MB, 10, side = 'left', pad = '^')),
               summstring = str_replace_all(summstring, '\\^', '&nbsp;')) %>%
               # summstring = str_replace_all(summstring, '\\^', '&#x2800;')) %>%
        select(network, domain, summstring)

    checkboxlist <- list()
    checkboxlist_counter <- 0
    for(i in 1:nrow(networks)){

        checkboxlist_counter <- checkboxlist_counter + 1
        checkboxlist[[checkboxlist_counter]] <- HTML(glue(
            dlcheck_template_network,
            nn = networks$network[i],
            N = networks$pretty_network[i]))

        # domains <- network_domain_default_sites %>%
        #     filter(network == networks$network[i]) %>%
        #     select(domain, pretty_domain) %>%

        domains <- dom_summ %>%
            filter(network == networks$network[i])

        for(j in 1:nrow(domains)){

            checkboxlist_counter <- checkboxlist_counter + 1
            checkboxlist[[checkboxlist_counter]] <- HTML(glue(
                dlcheck_template_domain,
                nn = networks$network[i],
                dd = domains$domain[j],
                D = domains$summstring[j]))

            # sites <- site_data %>%
            #     filter(network == networks$network[i],
            #            domain == domains$domain[j]) %>%
            #     select(site_code, full_name)
            #
            # for(k in 1:nrow(sites)){
            #
            #     checkboxlist_counter <- checkboxlist_counter + 1
            #     checkboxlist[[checkboxlist_counter]] <- HTML(glue(
            #         dlcheck_template_site,
            #         nn = networks$network[i],
            #         dd = domains$domain[j],
            #         ss = sites$site_code[k],
            #         S = paste0(sites$full_name[k], ' (', sites$site_code[k], ')')))
            # }
        }
    }

    return(checkboxlist)
})

# download_clicks <- reactiveValues(ts_download = 0)

# observeEvent(input$NO_SELECTIONS_WARNING, {
# # observeEvent(input$DL_TS_SELECTIONS, {
#
#     # if(length(input$DL_TS_SELECTIONS) == 1 && is.numeric(input$DL_TS_SELECTIONS)){
#     showNotification(input$NO_SELECTIONS_WARNING[2],
#                      duration = 0.75,
#                      closeButton = FALSE,
#                      type = 'warning')
#     # } else {
#     #     # download_clicks$ts_download <- download_clicks$ts_download + 1
#     #     print(download_clicks$ts_download)
#     # }
# })

output$DL_SUBMIT_TS <- downloadHandler(
    filename = 'macrosheds_timeseries.zip',
    content = function(file){

        datasets <- isolate(input$DL_SETS_TS)
        file_format <- isolate(input$DL_FORMAT_TS)
        selected_domains <- input$DL_TS_SELECTIONS

        if(is.null(datasets)){
            showNotification('No datasets selected',
                             duration = 0.75,
                             closeButton = FALSE,
                             type = 'warning')

            #this still initiates an http request when it returns,
            #but it doesn't seem to cause
            #any trouble. i couldn't get downloadHandler to fire without
            #directly clicking a corresponding downloadButton, which seems to leave
            #no choice but to fire warnings here.
            return()
        }

        if(length(selected_domains) == 1 && is.numeric(selected_domains)){
            showNotification('No domains selected',
                             duration = 0.75,
                             closeButton = FALSE,
                             type = 'warning')

            return()
        }

        showNotification(HTML(paste0('<div>Please wait while we prepare your data</div>',
                                     '<div class="lds-ellipsis">',
                                     '<div></div><div></div><div></div><div></div>',
                                     '</div>')),
                         id = 'TS_LOADING_POPUP',
                         duration = NULL,
                         closeButton = FALSE,
                         type = 'message')

        unlink('macrosheds_timeseries',
               recursive = TRUE)

        newpaths <- list()
        for(i in seq_along(selected_domains)){

            dmndata <- list.files(paste0('data/', selected_domains[i]),
                                  full.names = TRUE,
                                  recursive = TRUE,
                                  include.dirs = FALSE,
                                  pattern = '(?:\\.feather$|documentation_)')

            if(any(c('precipitation', 'precip_chemistry',
                     'precip_flux_inst_scaled') %in% datasets)){

                #grab some documentation files whose names might differ
                datasets <- c(datasets, 'documentation_precip_pchem_pflux',
                              'documentation_stream_flux_inst')
            }

            dmndata <- grep(pattern = paste(datasets,
                                            collapse = '|'),
                            x = dmndata,
                            value = TRUE)

            dmndata_newpaths <- sub(pattern = '^data/',
                                    replacement = 'macrosheds_timeseries/',
                                    x = dmndata)

            for(j in seq_along(dmndata)){

                dir.create(dirname(dmndata_newpaths[j]),
                           recursive = TRUE,
                           showWarnings = FALSE)

                sw(file.copy(from = dmndata[j],
                             to = dmndata_newpaths[j],
                             recursive = TRUE))
            }

            newpaths[[i]] <- dmndata_newpaths
        }

        newpaths <- unlist(newpaths)

        if(file_format == 'CSV'){

            for(i in seq_along(newpaths)){

                if(! grepl('*.feather$', newpaths[i])) next
                d <- feather::read_feather(newpaths[i])
                unlink(newpaths[i])
                newpaths[i] <- sub('\\.feather', '.csv', newpaths[i])
                readr::write_csv(d, newpaths[i])
            }
        }

        file.copy(from = 'static/documentation/timeseries/columns.txt',
                  to = 'macrosheds_timeseries/columns.txt')
        file.copy(from = 'static/documentation/timeseries/README.txt',
                  to = 'macrosheds_timeseries/README.txt')
        warning('Are we serving data files from our server yet? if so, remove the above readme')

        zip(zipfile = file,
            flags = '-r6Xq',
            files = c(newpaths,
                      'macrosheds_timeseries/columns.txt',
                      'macrosheds_timeseries/README.txt'))

        unlink('macrosheds_timeseries',
               recursive = TRUE)

        removeNotification('TS_LOADING_POPUP')
    },
    contentType = 'application/zip')

output$DL_SUBMIT_SITE <- downloadHandler(
    filename = 'macrosheds_sitedata.csv',
    content = function(file){

        dlset <- select(site_data_copy,
                        network, pretty_network, domain, pretty_domain, site_code,
                        epsg_code = CRS,
                        timezone_olson = local_time_zone) %>%
            right_join(read_csv('data/general/catalog_files/all_sites.csv',
                       col_types = cols()),
                      by = c(pretty_network = 'Network',
                             pretty_domain = 'Domain',
                             site_code = 'SiteCode')) %>%
            select(network,
                   network_fullname = pretty_network,
                   domain,
                   domain_fullname = pretty_domain,
                   site_code,
                   site_fullname = SiteName,
                   stream_name = StreamName,
                   site_type = SiteType,
                   latitude = Latitude,
                   longitude = Longitude,
                   epsg_code,
                   ws_area_ha = AreaHectares,
                   n_observations = Observations,
                   n_variables = Variables,
                   first_record_utc = FirstRecordUTC,
                   last_record_utc = LastRecordUTC,
                   timezone_olson)
            # mutate(Q_data_available = paste(domain, site_code, sep = '_') %in%
            #            sites_with_Q)

        write_csv(dlset, file)
    },
    contentType = 'text/csv')

output$DL_SUBMIT_VAR <- downloadHandler(
    filename = 'macrosheds_vardata.csv',
    content = function(file){

        dlset <- read_csv('data/general/catalog_files/all_variables.csv',
                          col_types = cols()) %>%
            # mutate(method = 'pending') %>% #TODO: include method info?
            select(variable_code = VariableCode,
                   variable_name = VariableName,
                   chem_category = ChemCategory,
                   unit = Unit,
                   # method
                   observations = Observations,
                   n_sites = Sites,
                   # mean_obs_per_site = MeanObsPerSite,
                   first_record_utc = FirstRecordUTC,
                   last_record_utc = LastRecordUTC)
                   # type = variable_type,
                   # subtype = variable_subtype)

        write_csv(dlset, file)
    },
    contentType = 'text/csv')

output$DL_SUBMIT_SPATIALSUMM <- downloadHandler(
    filename = 'macrosheds_watershed_summaries.zip',
    content = function(file){

        zip(zipfile = file,
            flags = '-r6Xqj',
            files = c('data/general/spatial_downloadables/watershed_summaries.csv',
                      'static/documentation/variable_category_codes.csv',
                      'static/documentation/data_source_codes.csv',
                      'static/documentation/watershed_summary/columns.csv',
                      'static/documentation/watershed_summary/README.txt'))
    },
    contentType = 'application/zip')

output$DL_SUBMIT_SPATIALTS <- downloadHandler(
    filename = 'macrosheds_watershed_summary_timeseries.zip',
    content = function(file){

        require(fst)

        selected_components <- input$DL_SPATIALTS_SELECTIONS

        unlink('macrosheds_spatial_ts')

        if(is.null(selected_components)){
            showNotification('No components selected',
                             duration = 0.75,
                             closeButton = FALSE,
                             type = 'warning')

            return()
        }

        showNotification(HTML(paste0('<div>Please wait while we prepare your data</div>',
                                     '<div class="lds-ellipsis">',
                                     '<div></div><div></div><div></div><div></div>',
                                     '</div>')),
                         id = 'SPATIALTS_LOADING_POPUP',
                         duration = NULL,
                         closeButton = FALSE,
                         type = 'message')

        dir.create('macrosheds_spatial_ts')

        fst::read_fst(paste0('data/general/spatial_downloadables/',
                                  'watershed_raw_spatial_timeseries.fst')) %>%
            filter(substr(var, 1, 1) %in% selected_components) %>%
            write_csv('macrosheds_spatial_ts/macrosheds_watershed_summary_timeseries.csv')

        zip(zipfile = file,
            flags = '-r9Xqj',
            files = c('static/documentation/variable_category_codes.csv',
                      'static/documentation/data_source_codes.csv',
                      'static/documentation/watershed_trait_timeseries/README.txt',
                      'static/documentation/watershed_trait_timeseries/columns.csv',
                      'macrosheds_spatial_ts/macrosheds_watershed_summary_timeseries.csv'))

        unlink('macrosheds_spatial_ts',
               recursive = TRUE)
        removeNotification('SPATIALTS_LOADING_POPUP')
    },
    contentType = 'application/zip')

output$DL_SUBMIT_GIS <- downloadHandler(
    filename = 'macrosheds_GIS_files.zip',
    content = function(file){

        # datasets <- isolate(input$DL_SETS_GIS)
        file_format <- isolate(input$DL_FORMAT_GIS)
        selected_domains <- input$DL_GIS_SELECTIONS

        if(length(selected_domains) == 1 && is.numeric(selected_domains)){
            showNotification('No domains selected',
                             duration = 0.75,
                             closeButton = FALSE,
                             type = 'warning')

            return()
        }

        showNotification(HTML(paste0('<div>Please wait while we prepare your data</div>',
                                     '<div class="lds-ellipsis">',
                                     '<div></div><div></div><div></div><div></div>',
                                     '</div>')),
                         id = 'GIS_LOADING_POPUP',
                         duration = NULL,
                         closeButton = FALSE,
                         type = 'message')

        unlink('macrosheds_GIS_files',
               recursive = TRUE)

        newpaths <- list()
        for(i in seq_along(selected_domains)){

            dmndata <- list.files(paste0('data/', selected_domains[i]),
                                  full.names = TRUE,
                                  recursive = TRUE,
                                  include.dirs = FALSE)

            dmndata <- grep(pattern = '_locations|_boundary',
                            x = dmndata,
                            value = TRUE)

            dmndata_newpaths <- sub(pattern = '^data/',
                                    replacement = 'macrosheds_GIS_files/',
                                    x = dmndata)

            for(j in seq_along(dmndata)){

                dir.create(dirname(dmndata_newpaths[j]),
                           recursive = TRUE,
                           showWarnings = FALSE)

                sw(file.copy(from = dmndata[j],
                             to = dmndata_newpaths[j],
                             recursive = TRUE))
            }

            newpaths[[i]] <- dmndata_newpaths
        }

        newpaths <- unlist(newpaths)

        if(file_format == 'geoJSON'){

            require('geojsonio')
            require('geojsonlint')
            for(i in seq_along(newpaths)){

                if(! grepl('*.shp$', newpaths[i])) next

                shapefile_base <- str_match(
                    string = newpaths[i],
                    pattern = '(.*?)\\.(?:prj|shx|shp|dbf)$')[, 2]

                shapefile_components <- grep(pattern = shapefile_base,
                                             x = newpaths,
                                             value = TRUE)

                d <- sf::st_read(newpaths[i])
                d <- geojsonio::geojson_json(d)

                lapply(shapefile_components, unlink)

                newpaths[i] <- sub('\\.shp', '.geojson', newpaths[i])
                geojson_write(input = d,
                              file = newpaths[i])
            }

            newpaths <- grep(pattern = '\\.(?:prj|shx|dbf)$',
                             x = newpaths,
                             value = TRUE,
                             invert = TRUE)
        }

        zip(zipfile = file,
            flags = '-r6Xq',
            files = newpaths)

        unlink('macrosheds_GIS_files',
               recursive = TRUE)

        removeNotification('GIS_LOADING_POPUP')
    },
    contentType = 'application/zip')

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

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$TIMESERIES_DL_BUTTON,
    handlerExpr = timeseries_dl
)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$SPATIAL_DL_BUTTON,
    handlerExpr = spatial_dl
)
