# Load in and filter summary files ####

# load either year or month summary file, currently only the year file is up
# to date
# summary <- reactive({
#
#     agg <- input$AGG2
#
#     if(agg %in% c('YEARLY2', 'WHOLE2')) {
#         sum <- read_feather('data/general/biplot/year.feather')
#     }
#
#     if(agg == 'MONTHLY2') {
#         sum <- read_feather('data/general/biplot/month.feather')
#     }
#
#     return(sum)
# })

sum <- read_feather('data/general/biplot/year.feather')

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
    #raw <- sum

    date1 <- year(input$DATES2_INTER[1])
    date2 <- year(input$DATES2_INTER[2])
    domains <- input$DOMAINS2
    domains_s <- input$DOMAINS2_S
    sites <- input$SITES2
    type <- switch(input$SITE_SELECTION2,
                    ALL_SITES2 = 'all',
                    DOMINE_NETWORK2 = 'dom',
                    BY_SITE2 = 'site')
    #raw <- summary()
    raw <- sum



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
        filter(Year >= !!date1,
               Year <= !!date2)

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
    # agg <<- isolate(input$AGG2)
    # domains <<- isolate(input$DOMAINS2)
    # sites <<- isolate(input$SITES2)
    # fill <- pre_filtered_bi()
    # #raw <- isolate(summary())
    # raw <- sum

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
    agg <- isolate(input$AGG2)
    domains <- isolate(input$DOMAINS2)
    sites <- isolate(input$SITES2)
    fill <- pre_filtered_bi()
    #raw <- isolate(summary())
    raw <- sum

    if(nrow(fill) == 0){

        final <- tibble()
        return(final)
    }

    x_var_ <- biplot_selection_to_name(chem = chem_x,
                                       unit = x_unit,
                                       var = x_var)

    y_var_ <- biplot_selection_to_name(chem = chem_y,
                                       unit = y_unit,
                                       var = y_var)

    size_var_ <- biplot_selection_to_name(chem = chem_size,
                                          unit = size_unit,
                                          var = size_var)

    if(include_size == 'SIZE_YES2') {
        filter_vars <- c(x_var_, y_var_, size_var_)
    } else {
        filter_vars <- c(x_var_, y_var_)
    }

    if('missing' %in% filter_vars){
        #Filter summary table for needed vars and spread to wide format
        final <- fill %>%
            filter(!is.na(Year)) %>%
            filter(var %in% !!filter_vars) %>%
            group_by(site_name, Date, Year, var, domain) %>%
            summarise(val = mean(val, na.rm = TRUE),
                      missing = mean(missing, na.rm = TRUE)) %>%
            ungroup() %>%
            pivot_wider(names_from = 'var', values_from = 'val')
    } else{

        #Filter summary table for needed vars and spread to wide format
        final <- fill %>%
            filter(!is.na(Year)) %>%
            select(-missing) %>%
            filter(var %in% !!filter_vars) %>%
            group_by(site_name, Date, Year, var, domain) %>%
            summarise(val = mean(val, na.rm = TRUE)) %>%
            ungroup() %>%
            pivot_wider(names_from = 'var', values_from = 'val')
    }



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

    if(any(!filter_vars %in% names(final))) {
        return(tibble())
    }

    x_unit_start <- variables %>%
        filter(variable_code == x_var)

    #Unit conversions. Could be improved for sure
    if(chem_x %in% c('Stream Concentration', 'Precipitation Chemistry')) {
        x_unit_start <- pull(variables %>%
                                 filter(variable_code == x_var) %>%
                                 select(unit))

        final <- convert_conc_units_bi(final, x_var_, x_unit_start, x_unit)
    }
    if(chem_x %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
        final <- convert_flux_units_bi(final, x_var_, 'kg/year', x_unit, summary_file = raw)
    }
    if(chem_x == 'Discharge' && x_unit == 'mm/d'){
        final <- final %>%
            mutate(discharge_a = discharge_a/365)
    }

    if(chem_y %in% c('Stream Concentration', 'Precipitation Chemistry')) {
        y_unit_start <- pull(variables %>%
                                 filter(variable_code == y_var) %>%
                                 select(unit))

        final <- convert_conc_units_bi(final, y_var_, y_unit_start, y_unit)
    }
    if(chem_y %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
        final <- convert_flux_units_bi(final, y_var_, 'kg/year', y_unit, summary_file = raw)
    }
    if(chem_y == 'Discharge' && y_unit == 'mm/d'){
        final <- final %>%
            mutate(discharge_a = discharge_a/365)
    }


    if(include_size == 'SIZE_YES2'){

        if(chem_size %in% c('Stream Concentration', 'Precipitation Chemistry')) {
            size_unit_start <- pull(variables %>%
                                        filter(variable_code == size_var) %>%
                                        select(unit))

            final <- convert_conc_units_bi(final, size_var_, size_unit_start, size_unit)
        }
        if(chem_size %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
            final <- convert_flux_units_bi(final, size_var_, 'kg/year', size_unit, summary_file = raw)
        }
        if(chem_size == 'Discharge' && size_unit == 'mm/d'){
            final <- final %>%
                mutate(discharge_a = discharge_a/365)
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

filtered_bi_d <- filtered_bi %>%
    debounce(100)

# Update axis options ####
#remove year as an axis option when aggrigation the whole records
observe({
    agg <- input$AGG2
    yearly <- 'Year'

    if(agg == 'YEARLY2'){
        updateSelectInput(session, 'X_TYPE2', choices = yearly)

        biplot_data_types_size <- append(biplot_data_types, 'Proportion of Record Missing', after = 0)
        updateSelectInput(session, 'SIZE_TYPE2', choices = biplot_data_types_size)
        updateRadioButtons(session, inputId = 'LOG_X2', selected = 'XAXIS_sta2')
    } else{
        updateSelectInput(session, 'X_TYPE2', choices = biplot_data_types)
        updateSelectInput(session, 'SIZE_TYPE2', choices = biplot_data_types)
    }
})

# reactivre value saves varibles so when conc is changed to flux, it will not change the
# var unless it is not flux convertable
current_selection <- reactiveValues(old_x = 'old',
                                    old_y = 'old',
                                    old_size = 'old')
observeEvent(input$X_VAR2,{
    current_selection$old_x <- input$X_VAR2})


#update individual options for varibles based on varible type
observe({
    data <- isolate(pre_filtered_bi())
    data_type <- input$X_TYPE2
    doms <- input$DOMAINS2_S
    input$DOMAINS2
    sites <- input$SITES2
    site_select <- input$SITE_SELECTION2
    old_selection <- isolate(current_selection$old_x)

    if(data_type == 'Stream Concentration') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'conc')
        units <- conc_units_bi
        choose <- 'mg/L'}

    if(data_type == 'Precipitation Chemistry') {
        select <- filter_dropdown_varlist_bi(data, vartype = 'precip_conc')
        units <- conc_units_bi
        choose <- 'mg/L'}

    if(data_type %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
        select <- filter_dropdown_varlist_bi(data, vartype = 'flux')
        units <- flux_units_bi
        choose <- 'kg/year'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- discharge_units_bi
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    if(data_type == 'Year') {
        select <- 'Year'
        units <- ''
        choose <- 'Year'}

    if(data_type == 'Precipitation') {
        select <- 'P'
        units <- 'mm'
        choose <- 'mm'}

    if(old_selection %in% unlist(select)){
        updateSelectInput(session, 'X_VAR2', choices = select, selected = old_selection)
    } else{
        updateSelectInput(session, 'X_VAR2', choices = select)
    }

    updateSelectInput(session, 'X_UNIT2', choices = units, selected = choose)
})

observeEvent(input$Y_VAR2,{
    current_selection$old_y <- input$Y_VAR2})

observe({
    data <- isolate(pre_filtered_bi())
    data_type <- input$Y_TYPE2
    doms <- input$DOMAINS2_S
    input$DOMAINS2
    sites <- input$SITES2
    site_select <- input$SITE_SELECTION2
    old_selection <- isolate(current_selection$old_y)

    if(data_type %in% c('Stream Concentration', 'Precipitation Chemistry')) {
        select <- filter_dropdown_varlist_bi(data, vartype = 'conc')
        units <- conc_units_bi
        choose <- 'mg/L'
        }

    if(data_type %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
        select <- filter_dropdown_varlist_bi(data, vartype = 'flux')
        units <- flux_units_bi
        choose <- 'kg/year'
        }

    if(data_type == 'Discharge') {
        select <- 'Q'
        var <- 'Q'
        units <- discharge_units_bi
        choose <- 'm^3'
        }

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        var <- ws_traits[1]
        units <- ''
        choose <- ''
    }

    if(data_type == 'Precipitation') {
        select <- 'P'
        units <- 'mm'
        choose <- 'mm'}

    if(old_selection %in% unlist(select)){
        updateSelectInput(session, 'Y_VAR2', choices = select, selected = old_selection)
    } else{
        updateSelectInput(session, 'Y_VAR2', choices = select)
    }

    updateSelectInput(session, 'Y_UNIT2', choices = units, selected = choose)

})

observeEvent(input$SIZE_VAR2,{
    current_selection$old_size <- input$SIZE_VAR2})

observe({
    data <- isolate(pre_filtered_bi())
    data_type <- input$SIZE_TYPE2
    doms <- input$DOMAINS2_S
    input$DOMAINS2
    sites <- input$SITES2
    site_select <- input$SITE_SELECTION2
    old_selection <- isolate(current_selection$old_size)
    x_var <- isolate(input$X_VAR2)
    y_var <- isolate(input$Y_VAR2)

    if(data_type %in% c('Stream Concentration', 'Precipitation Chemistry')) {
        select <- filter_dropdown_varlist_bi(data, vartype = 'conc')
        units <- conc_units_bi
        choose <- 'mg/L'}

    if(data_type %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
        select <- filter_dropdown_varlist_bi(data, vartype = 'flux')
        units <- flux_units_bi
        choose <- 'kg/year'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- discharge_units_bi
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    if(data_type == 'Precipitation') {
        select <- 'P'
        units <- 'mm'
        choose <- 'mm'}

    if(data_type == 'Proportion of Record Missing'){
        select <- '% of record missing'
        units <- ''
        choose <- '% of record missing'
    }

    if(old_selection %in% unlist(select)){
        updateSelectInput(session, 'SIZE_VAR2', choices = select, selected = old_selection)
    } else{
        updateSelectInput(session, 'SIZE_VAR2', choices = select)
    }

    updateSelectInput(session, 'SIZE_UNIT2', choices = units, selected = choose)
})

#update sites based on domains selected
observe({
    data_type <- input$DOMAINS2_S

    sites_choices <- site_data %>%
        filter(site_type == 'stream_gauge',
               domain %in% !!data_type) %>%
        pull(site_name)

    updateSelectInput(session, 'SITES2', choices = sites_choices)
})

#update unit options if they are not convertable
observe({
    x_var <- input$X_VAR2
    y_var <- input$Y_VAR2
    size_var <- input$SIZE_VAR2

    chem_x <- isolate(input$X_TYPE2)
    chem_y <- isolate(input$Y_TYPE2)
    chem_size <- isolate(input$SIZE_TYPE2)

    if(!convertible(x_var) && chem_x %in% c('Stream Concentration', 'Precipitation Chemistry')){
        updateSelectInput(session, 'X_UNIT2', choices = '')
    }

    if(convertible(x_var) && chem_x %in% c('Stream Concentration', 'Precipitation Chemistry')){
        updateSelectInput(session, 'X_UNIT2', choices = conc_units_bi, selected = conc_units_bi[3])
    }

    if(!convertible(y_var) && chem_y %in% c('Stream Concentration', 'Precipitation Chemistry')){
        updateSelectInput(session, 'Y_UNIT2', choices = '')
    }

    if(convertible(y_var) && chem_y %in% c('Stream Concentration', 'Precipitation Chemistry')){
        updateSelectInput(session, 'Y_UNIT2', choices = conc_units_bi, selected = conc_units_bi[3])
    }

    if(!convertible(size_var) && chem_size %in% c('Stream Concentration', 'Precipitation Chemistry')){
        updateSelectInput(session, 'SIZE_UNIT2', choices = '')
    }

    if(convertible(size_var) && chem_size %in% c('Stream Concentration', 'Precipitation Chemistry')){
        updateSelectInput(session, 'SIZE_UNIT2', choices = conc_units_bi, selected = conc_units_bi[3])
    }

})

# Plot data ####

#color by sites or domain
n_sites <- reactive({
    sites <- filtered_bi()

    if(nrow(sites) == 0){
        num <- 1
    } else{

        sites_num <- sites %>%
            distinct(site_name) %>%
            pull(site_name)

        num <- length(sites_num)

    }

    return(num)
})

output$SUMMARY_BIPLOT <- renderPlotly({

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
    # num_sites <<- isolate(n_sites())

    bi_table <- filtered_bi_d()
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
                  'MONTHLY2' = 'm',
                  'YEARLY2' = 'year',
                  'WHOLE2' = 'year ')
    chem_x <- isolate(input$X_TYPE2)
    chem_y <- isolate(input$Y_TYPE2)
    chem_size <- isolate(input$SIZE_TYPE2)
    num_sites <- isolate(n_sites())

    empty_plot <- plotly::plot_ly() %>%
        plotly::layout(annotations = list(text="No data available for \nthe selected variables",
                                          xref = "paper",
                                          yref = "paper",
                                          opacity = 0.4,
                                          "showarrow" = F,
                                          font=list(size = 30)))

    if(nrow(bi_table) == 0 || x_var == '' || y_var == '' || size_var == '') {

        return(empty_plot)
    }

    # Color blind safe palette
    safe_cols <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                   "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

    x_tvar <- biplot_selection_to_name(chem = chem_x,
                                       unit = x_unit,
                                       var = x_var)

    y_tvar <- biplot_selection_to_name(chem = chem_y,
                                       unit = y_unit,
                                       var = y_var)

    size_tvar <- biplot_selection_to_name(chem = chem_size,
                                       unit = size_unit,
                                       var = size_var)

    emplty_blank <- plotly::plot_ly() %>%
        plotly::layout()

    if(include_size == 'SIZE_NO2') {
        if(any(!c(x_tvar, y_tvar) %in% names(bi_table))){
            return(emplty_blank)
        }
    } else {
        if(any(!c(x_tvar, y_tvar, size_tvar) %in% names(bi_table))){
            return(emplty_blank)
        }
    }

    # if(chem_x %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
    #     x_unit <- paste0(x_unit, '/', agg)
    # }
    #
    # if(chem_y %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
    #     y_unit <- paste0(y_unit, '/', agg)
    # }
    #
    # if(chem_size %in% c('Stream Flux', 'Precipitation Chemistry Flux')) {
    #     size_unit <- paste0(size_unit, '/', agg)
    # }

    if(chem_x == 'Watershed Characteristics') {
        if(x_var %in% c('area', 'slope_mean')){
            x_var <- case_when(x_var == 'area' ~ 'Area',
                               x_var == 'slope_mean' ~ 'Slope')
        } else{
            x_var <- str_split_fixed(names(ws_traits_names[ws_traits_names==x_var]), '[.]', n = Inf)[1,1]
        }
    }

    if(chem_y == 'Watershed Characteristics') {
        if(y_var %in% c('area', 'slope_mean')){
            y_var <- case_when(y_var == 'area' ~ 'Area',
                               y_var == 'slope_mean' ~ 'Slope')
        } else{
        y_var <- str_split_fixed(names(ws_traits_names[ws_traits_names==y_var]), '[.]', n = Inf)[1,1]
        }
    }

    if(chem_size == 'Watershed Characteristics') {
        if(size_var %in% c('area', 'slope_mean')){
            size_var <- case_when(size_var == 'area' ~ 'Area',
                                  size_var == 'slope_mean' ~ 'Slope')
        } else{
        size_var <- str_split_fixed(names(ws_traits_names[ws_traits_names==size_var]), '[.]', n = Inf)[1,1]
        }
    }

    if(num_sites > 12){
        col_by <- 'pretty_domain'

        networks_cite <- network_domain_default_sites$pretty_network[network_domain_default_sites$domain %in% unique(bi_table$domain)]

        networks_cite <- unique(networks_cite)

        networks_cite <- paste(networks_cite, collapse = ', ')

    } else {
        col_by <- 'site_name'

        domains_cite <- network_domain_default_sites$pretty_domain[network_domain_default_sites$domain %in% unique(bi_table$domain)]
        domains_n_cite <- network_domain_default_sites$pretty_network[network_domain_default_sites$domain %in% unique(bi_table$domain)]

        networks_cite <- paste(paste(domains_cite, domains_n_cite, sep = ' '), collapse = ', ')
    }

    bi_table <- network_domain_default_sites %>%
        select(domain, pretty_domain) %>%
        right_join(.,bi_table, by = 'domain')

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

    }

    if(agg == 'year') {

        #define ticks if gtreater than 15 years
        min_year <- min(as.numeric(pull(bi_table[x_tvar])))
        max_year <- max(as.numeric(pull(bi_table[x_tvar])))

        year_length <- max_year-min_year
        all_years <-  unique(as.numeric(pull(bi_table[x_tvar])))

        if(year_length >= 15){
            tick_vals <- all_years[nchar(all_years/5) == 3]
        } else {
            tick_vals <- all_years
        }

        if(include_size == 'SIZE_YES2') {

            plot <- bi_table %>%
                plotly::plot_ly(x = ~get(x_tvar),
                                y = ~get(y_tvar),
                                type = 'scatter',
                                mode = 'lines+markers',
                                alpha = 0.8,
                                size = ~get(size_tvar),
                                color = ~get(col_by),
                                colors = safe_cols,
                                split = ~site_name,
                                line = list(width = 2, color = ~get(col_by)),
                                text = ~paste0(size_var, ' ', size_unit, ':', round(get(size_tvar), digits = 2), '\nsite:', site_name, ', \ndomain:', domain),
                                legendgroup = ~get(col_by)

                ) %>%
                plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit),
                                            range = c(min_year, max_year),
                                            tickvals = tick_vals),
                               yaxis = list(title = paste0(y_var, ' ', y_unit)),
                               paper_bgcolor = 'rgba(0,0,0,0)',
                               plot_bgcolor = 'rgba(0,0,0,0)',
                               legend= list(itemsizing='constant'))

#             plot <- bi_table %>%
#                # accumulate_by(~Year) %>%
#                 plotly::plot_ly(x = ~get(x_tvar),
#                                 y = ~get(y_tvar),
#                                 size = ~get(size_tvar),
#                                 color = ~get(col_by),
#                                 colors = safe_cols,
#                                 frame = ~Year,
#                                 fill = '',
#                                 type = 'scatter',
#                                 mode = 'markers',
#                                 text = ~paste0(size_var, ' ', size_unit, ':', round(get(size_tvar)), ', \nsite:', site_name, ', \ndomain:', domain)) %>%
#                 plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit),
#                                             range = c(min(as.numeric(pull(bi_table[x_tvar]))), max(as.numeric(pull(bi_table[x_tvar]))))),
#                                yaxis = list(title = paste0(y_var, ' ', y_unit)),
#                                paper_bgcolor = 'rgba(0,0,0,0)',
#                                plot_bgcolor = 'rgba(0,0,0,0)')
        } else {

            plot <- bi_table %>%
                plotly::plot_ly(x = ~get(x_tvar),
                                y = ~get(y_tvar),
                                type = 'scatter',
                                mode = 'lines',
                                alpha = 0.8,
                                color = ~get(col_by),
                                colors = safe_cols,
                                split = ~site_name,
                                line = list(width = 2, color = ~get(col_by)),
                                text = ~paste0('site:', site_name, ', \ndomain:', pretty_domain),
                                legendgroup = ~get(col_by)
                ) %>%
                plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit),
                                            range = c(min_year, max_year),
                                            tickvals = tick_vals),
                               yaxis = list(title = paste0(y_var, ' ', y_unit)),
                               paper_bgcolor = 'rgba(0,0,0,0)',
                               plot_bgcolor = 'rgba(0,0,0,0)'
                            )

            #'https://github.com/MacroSHEDS/portal/blob/master/www/new_logo_full.png?raw=true',
            # plot <- bi_table %>%
            #   #  accumulate_by(~Year) %>%
            #     plotly::plot_ly(x = ~get(x_tvar),
            #                     y = ~get(y_tvar),
            #                     size = 2,
            #                     type = 'scatter',
            #                     mode = 'markers',
            #                     frame = ~Year,
            #                     color = ~get(col_by),
            #                     colors = safe_cols,
            #                     fill = '',
            #                     #line = list(width = 2, color = ~get(col_by)),
            #                     text = ~paste0('site:', site_name, ', \ndomain:', domain)) %>%
            #     plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit),
            #                                 range = c(min(as.numeric(pull(bi_table[x_tvar]))), max(as.numeric(pull(bi_table[x_tvar]))))),
            #                    yaxis = list(title = paste0(y_var, ' ', y_unit)),
            #                    paper_bgcolor = 'rgba(0,0,0,0)',
            #                    plot_bgcolor = 'rgba(0,0,0,0)')
        }
    }

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

    plot <- plot %>%
        plotly::layout(annotations = list(
            text = paste0('MacroSheds data provided by: ', networks_cite),
            xref = "paper",
            yref = "paper",
            opacity = 0.10,
            showarrow = FALSE,
            textangle = -45,
            font = list(size = 28)))

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
