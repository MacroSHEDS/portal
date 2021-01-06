# Load in and filter summary files ####

# load either year or month summary file, currently only the year file is up
# to date
summary <- reactive({

    agg <- input$AGG2

    if(agg %in% c('YEARLY2', 'WHOLE2')) {
        sum <- read_feather('data/biplot/year.feather')
    }

    if(agg == 'MONTHLY2') {
        sum <- read_feather('data/biplot/month.feather')
    }

    return(sum)
})

#filter for sites and years. This file is used to update options for axis
#in if only a few domains are selected and not all variables are available
#at that site
pre_filtered_bi <- reactive({

    # date1 <<- input$DATES2_INTER[1]
    # date2 <<- input$DATES2_INTER[2]
    # domains <<- input$DOMAINS2
    # domains_s <<- input$DOMAINS2_S
    # sites <<- input$SITES2
    # type <<- switch(input$SITE_SELECTION2,
    #                 ALL_SITES2 = 'all',
    #                 DOMINE_NETWORK2 = 'dom',
    #                 BY_SITE2 = 'site')
    # raw <- summary()

    date1 <- input$DATES2_INTER[1]
    date2 <- input$DATES2_INTER[2]
    domains <- input$DOMAINS2
    sites <- input$SITES2
    type <- switch(input$SITE_SELECTION2,
                    ALL_SITES2 = 'all',
                    DOMINE_NETWORK2 = 'dom',
                    BY_SITE2 = 'site')
    raw <- summary()



    if(type == 'dom') {
        fill <- raw %>%
            filter(domain %in% domains)
    }

    if(type == 'site') {
        fill <- raw %>%
            filter(domain %in% domains_s) %>%
            filter(site_name %in% sites)
    }

    if(type == 'all') {
        fill <- raw
    }

    final <- fill %>%
        filter(Date >= !!date1,
               Date <= !!date2)

    return(final)
})

#Final filtering for variables and configures table for plotly graph
filtered_bi <- reactive({

    # x_var <<- input$X_VAR2
    # y_var <<- input$Y_VAR2
    # include_size <<- input$ADD_SIZE2
    # size_var <<- input$SIZE_VAR2
    # x_unit <<- input$X_UNIT2
    # y_unit <<- input$Y_UNIT2
    # size_unit <<- input$SIZE_UNIT2
    # chem_x <<- input$X_TYPE2
    # chem_y <<- input$Y_TYPE2
    # chem_size <<- input$SIZE_TYPE2
    # agg <<- input$AGG2
    # fill <- pre_filtered_bi()
    # raw <- isolate(summary())

    x_var <- input$X_VAR2
    y_var <- input$Y_VAR2
    include_size <- input$ADD_SIZE2
    size_var <- input$SIZE_VAR2
    x_unit <- input$X_UNIT2
    y_unit <- input$Y_UNIT2
    size_unit <- input$SIZE_UNIT2
    chem_x <- input$X_TYPE2
    chem_y <- input$Y_TYPE2
    chem_size <- input$SIZE_TYPE2
    agg <- input$AGG2
    fill <- pre_filtered_bi()
    raw <- isolate(summary())



    #Create column names, add conc or flux
    x_var_ <- case_when(chem_x == 'Discharge' ~ 'discharge',
                        chem_x == 'Concentration' ~ paste0(x_var, '_conc'),
                        chem_x == 'Flux' ~ paste0(x_var, '_flux'),
                        chem_x == 'Watershed Characteristics' ~ x_var,
                        chem_x == 'Year' ~ 'Year')

    y_var_ <- case_when(chem_y == 'Discharge' ~ 'discharge',
                        chem_y == 'Concentration' ~ paste0(y_var, '_conc'),
                        chem_y == 'Flux' ~ paste0(y_var, '_flux'),
                        chem_y == 'Watershed Characteristics' ~ y_var)

    size_var_ <- case_when(chem_size == 'Discharge' ~ 'discharge',
                           chem_size == 'Concentration' ~ paste0(size_var, '_conc'),
                           chem_size == 'Flux' ~ paste0(size_var, '_flux'),
                           chem_size == 'Watershed Characteristics' ~ size_var)

    if(include_size == 'SIZE_YES2') {
        filter_vars <- c(x_var_, y_var_, size_var_)
    } else {
        filter_vars <- c(x_var_, y_var_)
    }

    #Filter summary table for needed vars and spread to wide format
    final <- fill %>%
        filter(var %in% !!filter_vars) %>%
        # filter(Date >= !!date1,
        #        Date <= !!date2) %>%
        group_by(site_name, Date, Year, var, domain) %>%
        summarise(val = mean(val, na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = 'var', values_from = 'val')

    # For varibles that are not associated with a year (constant through time),
    # they need to be added this way becuase their year column is NA
    if(x_var_ %in% c('area', 'slope_mean')){
        terrain <- raw %>%
            filter(var == !!x_var_) %>%
            rename(!!x_var_ := val) %>%
            select(-Year, -var, -Date)

        final <- final %>%
            left_join(., terrain, by = c('site_name', 'domain'))
    }

    if(y_var_ %in% c('area', 'slope_mean')){
        terrain <- raw %>%
            filter(var == !!y_var_) %>%
            rename(!!y_var_ := val) %>%
            select(-Year, -var, -Date)

        final <- final %>%
            left_join(., terrain, by = c('site_name', 'domain'))
    }

    if(size_var_ %in% c('area', 'slope_mean')){
        terrain <- raw %>%
            filter(var == !!size_var_) %>%
            rename(!!size_var_ := val) %>%
            select(-Year, -var, -Date)

        final <- final %>%
            left_join(., terrain, by = c('site_name', 'domain'))
    }

    # Filter to include only sites with all variables available
    final <- final[complete.cases(final),]

    x_unit_start <- variables %>%
        filter(variable_code == x_var)


    #Unit conversions. Could be improved for sure
    if(chem_x == 'Concentration') {
        x_unit_start <- pull(variables %>%
                                 filter(variable_code == x_var) %>%
                                 select(unit))

        final <- convert_conc_units_bi(final, x_var_, x_unit_start, x_unit)
    }
    if(chem_x == 'Flux') {
        final <- convert_flux_units_bi(final, x_var_, 'kg', x_unit, summary_file = raw)
    }
    if(chem_x == 'Discharge' && x_unit == 'Area normalized (meters/year)') {
        final <- convert_area_nor_q_bi(final, summary_file = raw)
    }

    if(chem_y == 'Concentration') {
        y_unit_start <- pull(variables %>%
                                 filter(variable_code == y_var) %>%
                                 select(unit))

        final <- convert_conc_units_bi(final, y_var_, y_unit_start, y_unit)
    }
    if(chem_y == 'Flux') {
        final <- convert_flux_units_bi(final, y_var_, 'kg', y_unit, summary_file = raw)
    }
    if(chem_y == 'Discharge' && y_unit == 'Area normalized (meters/year)') {
        final <- convert_area_nor_q_bi(final, summary_file = raw)
    }


    if(include_size == 'SIZE_YES2'){

        if(chem_size == 'Concentration') {
            size_unit_start <- pull(variables %>%
                                        filter(variable_code == size_var) %>%
                                        select(unit))

            final <- convert_conc_units_bi(final, size_var_, size_unit_start, size_unit)
        }
        if(chem_size == 'Flux') {
            final <- convert_flux_units_bi(final, size_var_, 'kg', size_unit, summary_file = raw)
        }
        if(chem_size == 'Discharge' && size_unit == 'Area normalized (meters/year)') {
            final <- convert_area_nor_q_bi(final, summary_file = raw)
        }
    }

    #If the whole record summary is selected, so that here
    if(agg == 'WHOLE2') {
        final <- final %>%
            select(-Year) %>%
            group_by(site_name, domain) %>%
            summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))
    }

    return(final)
})

# Update axis options ####
observe({
    data <- pre_filtered_bi()
    data_type <- input$X_TYPE2

    if(data_type == 'Concentration') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'conc')
        units <- conc_units_bi
        choose <- 'mg/L'}

    if(data_type == 'Flux') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'flux')
        units <- flux_units_bi
        choose <- 'kg'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- c('m^3', 'Area normalized (meters/year)')
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    if(data_type == 'Year') {
        select <- 'Year'
        units <- ''
        choose <- 'Year'}

    updateSelectInput(session, 'X_VAR2', choices = select)
    updateSelectInput(session, 'X_UNIT2', choices = units, selected = choose)
})

observe({
    data <- pre_filtered_bi()
    data_type <- input$Y_TYPE2

    if(data_type == 'Concentration') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'conc')
        units <- conc_units_bi
        choose <- 'mg/L'}

    if(data_type == 'Flux') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'flux')
        units <- flux_units_bi
        choose <- 'kg'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- c('m^3', 'Area normalized (meters/year)')
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    updateSelectInput(session, 'Y_VAR2', choices = select)
    updateSelectInput(session, 'Y_UNIT2', choices = units, selected = choose)
})

observe({
    data <- pre_filtered_bi()
    data_type <- input$SIZE_TYPE2

    if(data_type == 'Concentration') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'conc')
        units <- conc_units_bi
        choose <- 'mg/L'}

    if(data_type == 'Flux') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'flux')
        units <- flux_units_bi
        choose <- 'kg'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- c('m^3', 'Area normalized (meters/year)')
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    updateSelectInput(session, 'SIZE_VAR2', choices = select)
    updateSelectInput(session, 'SIZE_UNIT2', choices = units, selected = choose)
})

observe({
    data_type <- input$DOMAINS2_S

    sites_choices <- site_data %>%
        filter(site_type == 'stream_gauge',
               domain %in% !!data_type) %>%
        pull(site_name)

    updateSelectInput(session, 'SITES2', choices = sites_choices)
})

# Plot data ####

#color by sites or domain
n_sites <- reactive({
    sites <- filtered_bi()

    sites_num <- sites %>%
        distinct(site_name) %>%
        pull(site_name)

    num <- length(sites_num)

    return(num)
})

output$SUMMARY_BIPLOT = renderPlotly({

    # bi_table <<- filtered_bi()
    # domains <<- isolate(input$DOMAINS2)
    # sites <<- isolate(input$SITES2)
    # x_var <<- isolate(input$X_VAR2)
    # y_var <<- isolate(input$Y_VAR2)
    # include_size <<- isolate(input$ADD_SIZE2)
    # size_var <<- isolate(input$SIZE_VAR2)
    # x_unit <<- isolate(input$X_UNIT2)
    # y_unit <<- isolate(input$Y_UNIT2)
    # size_unit <<- isolate(input$SIZE_UNIT2)
    # agg <<- switch(isolate(input$AGG2),
    #               'YEARLY2' = 'year',
    #               'MONTHLY2' = 'm',
    #               'WHOLE2' = 'year ')
    # chem_x <<- isolate(input$X_TYPE2)
    # chem_y <<- isolate(input$Y_TYPE2)
    # chem_size <<- isolate(input$SIZE_TYPE2)
    # num_sites <<- n_sites()

    bi_table <- filtered_bi()
    domains <- isolate(input$DOMAINS2)
    sites <- isolate(input$SITES2)
    x_var <- isolate(input$X_VAR2)
    y_var <- isolate(input$Y_VAR2)
    include_size <- isolate(input$ADD_SIZE2)
    size_var <- isolate(input$SIZE_VAR2)
    x_unit <- isolate(input$X_UNIT2)
    y_unit <- isolate(input$Y_UNIT2)
    size_unit <- isolate(input$SIZE_UNIT2)
    agg <- switch(isolate(input$AGG2),
                   'YEARLY2' = 'year',
                   'MONTHLY2' = 'm',
                   'WHOLE2' = 'year ')
    chem_x <- isolate(input$X_TYPE2)
    chem_y <- isolate(input$Y_TYPE2)
    chem_size <- isolate(input$SIZE_TYPE2)
    num_sites <- n_sites()

    # Color blind safe palette
    safe_cols <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                   "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

    x_test <- convertible(x_var)
    y_test <- convertible(y_var)
    size_test <- convertible(size_var)

    if(!x_test) { x_unit <- '' }
    if(!y_test) { y_unit <- '' }
    if(!size_test) { size_unit <- '' }

    x_tvar <- case_when(chem_x == 'Discharge' ~ 'discharge',
                        chem_x == 'Concentration' ~ paste0(x_var, '_conc'),
                        chem_x == 'Flux' ~ paste0(x_var, '_flux'),
                        chem_x == 'Watershed Characteristics' ~ x_var,
                        chem_x == 'Year' ~ 'Year')

    y_tvar <- case_when(chem_y == 'Discharge' ~ 'discharge',
                        chem_y == 'Concentration' ~ paste0(y_var, '_conc'),
                        chem_y == 'Flux' ~ paste0(y_var, '_flux'),
                        chem_y == 'Watershed Characteristics' ~ y_var)

    size_tvar <- case_when(chem_size == 'Discharge' ~ 'discharge',
                           chem_size == 'Concentration' ~ paste0(size_var, '_conc'),
                           chem_size == 'Flux' ~ paste0(size_var, '_flux'),
                           chem_size == 'Watershed Characteristics' ~ size_var)

    if(chem_x == 'Flux') {
        x_unit <- paste0(x_unit, '/', agg)
    }

    if(chem_y == 'Flux') {
        y_unit <- paste0(y_unit, '/', agg)
    }

    if(chem_size == 'Flux') {
        size_unit <- paste0(size_unit, '/', agg)
    }

    if(chem_x == 'Watershed Characteristics') {
        x_var <- str_split_fixed(names(ws_traits_names[ws_traits_names==x_var]), '[.]', n = Inf)[1,1]
    }

    if(chem_y == 'Watershed Characteristics') {
        y_var <- str_split_fixed(names(ws_traits_names[ws_traits_names==y_var]), '[.]', n = Inf)[1,1]
    }

    if(chem_size == 'Watershed Characteristics') {
        size_var <- str_split_fixed(names(ws_traits_names[ws_traits_names==size_var]), '[.]', n = Inf)[1,1]
    }

    if(num_sites > 12){
        col_by <- 'domain'
    } else {
        col_by <- 'site_name'
    }

    #Currently disabled
    if(agg == 'm') {

        plot <- bi_table %>%
            plotly::plot_ly(x = ~get(x_tvar),
                y = ~get(y_tvar),
                size = ~get(size_tvar),
                color = ~get(col_by),
                colors = safe_cols,
                frame = ~Date,
                type = 'scatter',
                mode = 'markers',
                text = ~paste0(size_var, ' ', size_unit, ':', get(size_var))) %>%
        plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit)),
                       yaxis = list(title = paste0(y_var, ' ', y_unit)),
                       paper_bgcolor='rgba(0,0,0,0)',
                       plot_bgcolor='rgba(0,0,0,0)')
            #animation_opts(frame = 1000, easing = 'linear', transition = 1000, redraw = TRUE)
    }
    if(agg == 'year') {

        if(include_size == 'SIZE_YES2') {

            plot <- bi_table %>%
                plotly::plot_ly(x = ~get(x_tvar),
                                y = ~get(y_tvar),
                                size = ~get(size_tvar),
                                color = ~get(col_by),
                                colors = safe_cols,
                                frame = ~Year,
                                fill = '',
                                type = 'scatter',
                                mode = 'markers',
                                text = ~paste0(size_var, ' ', size_unit, ':', round(get(size_tvar)), ', \nsite:', site_name, ', \ndomain:', domain)) %>%
                plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit)),
                               yaxis = list(title = paste0(y_var, ' ', y_unit)),
                               paper_bgcolor = 'rgba(0,0,0,0)',
                               plot_bgcolor = 'rgba(0,0,0,0)')
        } else {

            plot <- bi_table %>%
                #accumulate_by(~Year) %>%
                plotly::plot_ly(x = ~get(x_tvar),
                                y = ~get(y_tvar),
                                size = 2,
                                color = ~get(col_by),
                                colors = safe_cols,
                                frame = ~Year,
                                fill = '',
                                type = 'scatter',
                                mode = 'markers',
                                #line = list(width = 2, color = ~get(col_by)),
                                text = ~paste0('site:', site_name, ', \ndomain:', domain)) %>%
                plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit)),
                               yaxis = list(title = paste0(y_var, ' ', y_unit)),
                               paper_bgcolor = 'rgba(0,0,0,0)',
                               plot_bgcolor = 'rgba(0,0,0,0)')
        }
    }

    #above case if(agg %in% c('year', 'm')) catches this?
    if(agg == 'year ') {

        if(include_size == 'SIZE_YES2') {
            plot <- bi_table %>%
                plotly::plot_ly(x = ~get(x_tvar),
                                y = ~get(y_tvar),
                                size = ~get(size_tvar),
                                color = ~get(col_by),
                                colors = safe_cols,
                                fill = '',
                                type = 'scatter',
                                mode = 'markers',
                                text = ~paste0(size_var, ' ', size_unit, ':', round(get(size_tvar), digits = 2), '\nsite:', site_name, ', \ndomain:', domain)) %>%
                plotly::layout(xaxis = list(title = paste0('Mean', ' ', x_var, ' ', x_unit)),
                               yaxis = list(title = paste0('Mean', ' ', y_var, ' ', y_unit)),
                               paper_bgcolor='rgba(0,0,0,0)',
                               plot_bgcolor='rgba(0,0,0,0)')
        } else{
            plot <- bi_table %>%
                plotly::plot_ly(x = ~get(x_tvar),
                                y = ~get(y_tvar),
                                size = 2,
                                color = ~get(col_by),
                                colors = safe_cols,
                                fill = '',
                                type = 'scatter',
                                mode = 'markers',
                                text = ~paste0('\nsite:', site_name, ', \ndomain:', domain)) %>%
                plotly::layout(xaxis = list(title = paste0('Mean', ' ', x_var, ' ', x_unit)),
                               yaxis = list(title = paste0('Mean', ' ', y_var, ' ', y_unit)),
                               paper_bgcolor='rgba(0,0,0,0)',
                               plot_bgcolor='rgba(0,0,0,0)')
        }
    }

    if(input$LOG_X2 == 'XAXIS_log2'){
        plot <- plot %>%
            layout(xaxis = list(type = "log"))
    }

    if(input$LOG_Y2 == 'YAXIS_log2'){
        plot <- plot %>%
            layout(yaxis = list(type = "log"))
    }

    return(plot)
})


# Old code, will be usful for map selections ####

# accumulate_by <- function(dat, var) {
#     var <- lazyeval::f_eval(var, dat)
#     lvls <- plotly:::getLevels(var)
#     dats <- lapply(seq_along(lvls), function(x) {
#         cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
#     })
#     dplyr::bind_rows(dats)
# }

#
# observe({
#     selected <- sites$sites
#
#     map_dom <- site_data %>%
#         filter(site_name %in% selected) %>%
#         pull(domain)
#
#     updateSelectInput(session, 'DOMAINS2_S', selected=map_dom)
#     updateSelectInput(session, 'SITES2', selected=selected)
#
#     updateSelectInput(session, )
# })

# observe({
#     data_type <- input$X_TYPE2
#     raw <- summary()
#
#     if(data_type == 'Concentration') {
#         select <- filter_dropdown_varlist_bi(raw, vartype = 'conc')
#         units <- conc_units_bi
#         choose <- 'mg/L'}
#
#     if(data_type == 'Flux') {
#         select <- filter_dropdown_varlist_bi(raw, vartype = 'flux')
#         units <- flux_units_bi
#         choose <- 'kg/ha'}
#
#     if(data_type == 'Discharge') {
#         select <- 'Q'
#         units <- 'm^3'
#         choose <- 'm^3'}
#
#     if(data_type == 'Watershed Characteristics') {
#         select <- ws_traits
#         units <- ''
#         choose <- ''}
#
#
#     if(data_type == 'Year') {
#         select <- 'Year'
#         units <- ''
#         choose <- 'Year'}
#
#     updateSelectInput(session, 'X_VAR2', choices = select)
#     updateSelectInput(session, 'X_UNIT2', choices = units, selected = choose)
#
#     })
#
# observe({
#     data_type <- input$Y_TYPE2
#     raw <- summary()
#
#     if(data_type == 'Concentration') {
#         select <-  filter_dropdown_varlist_bi(raw, vartype = 'conc')
#         units <- conc_units_bi
#         choose <- 'mg/L'}
#
#     if(data_type == 'Flux') {
#         select <-  filter_dropdown_varlist_bi(raw, vartype = 'flux')
#         units <- flux_units_bi
#         choose <- 'kg/ha'}
#
#     if(data_type == 'Discharge') {
#         select <- 'Q'
#         units <- 'm^3'
#         choose <- 'm^3'}
#
#     if(data_type == 'Watershed Characteristics') {
#         select <- ws_traits
#         units <- ''
#         choose <- ''}
#
#     updateSelectInput(session, 'Y_VAR2', choices = select)
#     updateSelectInput(session, 'Y_UNIT2', choices = units, selected = choose)
#
# })
#
# observe({
#     data_type <- input$SIZE_TYPE2
#     raw <- summary()
#
#     if(data_type == 'Concentration') {
#         select <- filter_dropdown_varlist_bi(raw, vartype = 'conc')
#         units <- conc_units_bi
#         choose <- 'mg/L'}
#
#     if(data_type == 'Flux') {
#         select <- filter_dropdown_varlist_bi(raw, vartype = 'flux')
#         units <- flux_units_bi
#         choose <- 'kg/ha'}
#
#     if(data_type == 'Discharge') {
#         select <- 'Q'
#         units <- 'm^3'
#         choose <- 'm^3'}
#
#     if(data_type == 'Watershed Characteristics') {
#         select <- ws_traits
#         units <- ''
#         choose <- ''}
#
#     updateSelectInput(session, 'SIZE_VAR2', choices = select)
#     updateSelectInput(session, 'SIZE_UNIT2', choices = units, selected = choose)
#
# })