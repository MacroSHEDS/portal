# Update inputs ####

observe({
    doms <- input$DOMAINS2_S

    dom_sites <- site_data %>%
        filter(site_type == 'stream_gauge') %>%
        filter(domain %in% doms) %>%
        pull(site_name)

    updateSelectInput(session, 'SITES2', choices = dom_sites)
})


observe({
    data_type <- input$X_TYPE2

    if(data_type == 'Concentration') {
        select <- chemvars_display_subset
        units <- conc_units
        choose <- 'mg/L'}

    if(data_type == 'Flux') {
        select <- chemvars_display_subset
        units <- flux_units_bi
        choose <- 'kg/ha'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- 'm^3'
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    updateSelectInput(session, 'X_VAR2', choices = select)
    updateSelectInput(session, 'X_UNIT2', choices = units, selected = choose)

    })

observe({
    data_type <- input$Y_TYPE2

    if(data_type == 'Concentration') {
        select <- chemvars_display_subset
        units <- conc_units
        choose <- 'mg/L'}

    if(data_type == 'Flux') {
        select <- chemvars_display_subset
        units <- flux_units_bi
        choose <- 'kg/ha'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- 'm^3'
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    updateSelectInput(session, 'Y_VAR2', choices = select)
    updateSelectInput(session, 'Y_UNIT2', choices = units, selected = choose)

})

observe({
    data_type <- input$SIZE_TYPE2

    if(data_type == 'Concentration') {
        select <- chemvars_display_subset
        units <- conc_units
        choose <- 'mg/L'}

    if(data_type == 'Flux') {
        select <- chemvars_display_subset
        units <- flux_units_bi
        choose <- 'kg/ha'}

    if(data_type == 'Discharge') {
        select <- 'Q'
        units <- 'm^3'
        choose <- 'm^3'}

    if(data_type == 'Watershed Characteristics') {
        select <- ws_traits
        units <- ''
        choose <- ''}

    updateSelectInput(session, 'SIZE_VAR2', choices = select)
    updateSelectInput(session, 'SIZE_UNIT2', choices = units, selected = choose)

})

# Load in and filter summary files ####
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

 filtered_bi <- reactive({
#     x_var <- input$X_VAR2
#     y_var <- input$Y_VAR2
#     size_var <- input$SIZE_VAR2
#     x_unit <- input$X_UNIT2
#     y_unit <- input$Y_UNIT2
#     size_unit <- input$SIZE_UNIT2
#     date1 <- input$DATES2_INTER[1]
#     date2 <- input$DATES2_INTER[2]
#     chem_x <- input$X_TYPE2
#     chem_y <- input$Y_TYPE2
#     chem_size <- input$SIZE_TYPE2
#     domains <- input$DOMAINS2
#     sites <- input$SITES2
#     type <- switch(input$SITE_SELECTION2,
#                     ALL_SITES2 = 'all',
#                     DOMINE_NETWORK2 = 'dom',
#                     BY_SITE2 = 'site')
#     agg <- input$AGG2
#     raw <- summary()

    x_var <<- input$X_VAR2
    y_var <<- input$Y_VAR2
    size_var <<- input$SIZE_VAR2
    x_unit <<- input$X_UNIT2
    y_unit <<- input$Y_UNIT2
    size_unit <<- input$SIZE_UNIT2
    date1 <<- input$DATES2_INTER[1]
    date2 <<- input$DATES2_INTER[2]
    chem_x <<- input$X_TYPE2
    chem_y <<- input$Y_TYPE2
    chem_size <<- input$SIZE_TYPE2
    domains <<- input$DOMAINS2
    sites <<- input$SITES2
    type <<- switch(input$SITE_SELECTION2,
                   ALL_SITES2 = 'all',
                   DOMINE_NETWORK2 = 'dom',
                   BY_SITE2 = 'site')
    agg <<- input$AGG2
    raw <- summary()

    if(type == 'dom') {
        fill <- raw %>%
            filter(domain %in% domains)
    }

    if(type == 'site') {
        fill <- raw %>%
            filter(domain %in% domains) %>%
            filter(site_name %in% sites)
    }

    if(type == 'all') {
        fill <- raw
    }

    x_var_ <- case_when(chem_x == 'Discharge' ~ 'discharge',
                       chem_x == 'Concentration' ~ paste0(x_var, '_conc'),
                       chem_x == 'Flux' ~ paste0(x_var, '_flux'),
                       chem_x == 'Watershed Characteristics' ~ x_var)

    y_var_ <- case_when(chem_y == 'Discharge' ~ 'discharge',
                       chem_y == 'Concentration' ~ paste0(y_var, '_conc'),
                       chem_y == 'Flux' ~ paste0(y_var, '_flux'),
                       chem_y == 'Watershed Characteristics' ~ y_var)

    size_var_ <- case_when(chem_size == 'Discharge' ~ 'discharge',
                       chem_size == 'Concentration' ~ paste0(size_var, '_conc'),
                       chem_size == 'Flux' ~ paste0(size_var, '_flux'),
                       chem_size == 'Watershed Characteristics' ~ size_var)

    final <- fill %>%
        #select(site_name, domain, Date, Year, var,) %>%
        filter(var %in% c(!!x_var_, !!y_var_, !!size_var_)) %>%
         filter(Date >= !!date1,
                Date <= !!date2) %>%
        group_by(site_name, Date, Year, var, domain) %>%
        summarise(val = mean(val, na.rm = TRUE)) %>%
        ungroup() %>%
            pivot_wider(names_from = 'var', values_from = 'val')

    x_unit_start <- variables %>%
        filter(variable_code == x_var)


    if(chem_x == 'Concentration') {
        x_unit_start <- pull(variables %>%
            filter(variable_code == x_var) %>%
                select(unit))

        final <- convert_conc_units_bi(final, x_var_, x_unit_start, x_unit)
    }
    if(chem_x == 'Flux') {
        final <- convert_flux_units_bi(final, x_var_, 'kg/ha', x_unit)
    }

    if(chem_y == 'Concentration') {
        y_unit_start <- pull(variables %>%
                                 filter(variable_code == y_var) %>%
                                 select(unit))

        final <- convert_conc_units_bi(final, y_var_, y_unit_start, y_unit)
    }
    if(chem_y == 'Flux') {
        final <- convert_flux_units_bi(final, y_var_, 'kg/ha', y_unit)
    }

    if(chem_size == 'Concentration') {
        size_unit_start <- pull(variables %>%
                                 filter(variable_code == size_var) %>%
                                 select(unit))

        final <- convert_conc_units_bi(final, size_var_, size_unit_start, size_unit)
    }
    if(chem_size == 'Flux') {
        final <- convert_flux_units_bi(final, size_var_, 'kg/ha', size_unit)
    }

    if(agg == 'WHOLE2') {
    final <- final %>%
        select(-Year) %>%
        group_by(site_name, domain) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) }

    return(final)
})

# Plot data ####
n_sites <- reactive({
    sites <- filtered_bi()

    sites_num <- sites %>%
        distinct(site_name) %>%
        pull(site_name)

    num <- length(sites_num)

    return(num)
})

output$SUMMARY_BIPLOT = renderPlotly({

    bi_table <<- filtered_bi()
    domains <<- isolate(input$DOMAINS2)
    sites <<- isolate(input$SITES2)
    x_var <<- isolate(input$X_VAR2)
    y_var <<- isolate(input$Y_VAR2)
    size_var <<- isolate(input$SIZE_VAR2)
    x_unit <<- isolate(input$X_UNIT2)
    y_unit <<- isolate(input$Y_UNIT2)
    size_unit <<- isolate(input$SIZE_UNIT2)
    agg <<- switch(isolate(input$AGG2),
                  'YEARLY2' = 'year',
                  'MONTHLY2' = 'm',
                  'WHOLE2' = 'year ')
    chem_x <<- isolate(input$X_TYPE2)
    chem_y <<- isolate(input$Y_TYPE2)
    chem_size <<- isolate(input$SIZE_TYPE2)
    num_sites <<- n_sites()

    # bi_table <- filtered_bi()
    # domains <- isolate(input$DOMAINS2)
    # sites <- isolate(input$SITES2)
    # x_var <- isolate(input$X_VAR2)
    # y_var <- isolate(input$Y_VAR2)
    # size_var <- isolate(input$SIZE_VAR2)
    # x_unit <- isolate(input$X_UNIT2)
    # y_unit <- isolate(input$Y_UNIT2)
    # size_unit <- isolate(input$SIZE_UNIT2)
    # agg <- switch(isolate(input$AGG2),
    #               'YEARLY2' = 'year',
    #               'MONTHLY2' = 'm',
    #               'WHOLE2' = 'year ')
    # chem_x <- isolate(input$X_TYPE2)
    # chem_y <- isolate(input$Y_TYPE2)
    # chem_size <- isolate(input$SIZE_TYPE2)
    # num_sites <- n_sites()

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
                        chem_x == 'Watershed Characteristics' ~ x_var)

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


    # if(chem_x == 'Watershed Characteristics') {
    #     x_tvar <- x_var
    #     x_var <- str_split_fixed(x_var, pattern = '_', n = Inf)[1]
    #     x_unit <- ''
    # } else {x_tvar <- x_var}
    #
    # if(chem_y == 'Watershed Characteristics') {
    #     y_tvar <- y_var
    #     y_var <- str_split_fixed(y_var, pattern = '_', n = Inf)[1]
    #     y_unit <- ''
    # } else {y_tvar <- y_var}
    #
    # if(chem_size == 'Watershed Characteristics') {
    #     size_tvar <- size_var
    #     size_var <- str_split_fixed(size_var, pattern = '_', n = Inf)[1]
    #     size_unit <- ''
    # } else {
    #     size_tvar <- size_var
    # }

    if(num_sites > 12){
        col_by <- 'domain'
    } else {
        col_by <- 'site_name'
    }

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
                            text = ~paste0(size_var, ' ', size_unit, ':', get(size_tvar))) %>%
            plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit)),
                           yaxis = list(title = paste0(y_var, ' ', y_unit)),
                           paper_bgcolor = 'rgba(0,0,0,0)',
                           plot_bgcolor = 'rgba(0,0,0,0)')
            #animation_opts(frame = 1000, easing = 'linear', transition = 1000, redraw = TRUE)
    }

    #above case if(agg %in% c('year', 'm')) catches this?
    if(agg == 'year ') {
        plot <- bi_table %>%
            plotly::plot_ly(x = ~get(x_tvar),
                            y = ~get(y_tvar),
                            size = ~get(size_tvar),
                            color = ~get(col_by),
                            colors = safe_cols,
                            fill = '',
                            type = 'scatter',
                            mode = 'markers',
                            text = ~paste0(size_var, ' ', size_unit, ':', get(size_tvar))) %>%
            plotly::layout(xaxis = list(title = paste0('Mean', ' ', x_var, ' ', x_unit)),
                           yaxis = list(title = paste0('Mean', ' ', y_var, ' ', y_unit)),
                           paper_bgcolor='rgba(0,0,0,0)',
                           plot_bgcolor='rgba(0,0,0,0)')
        #animation_opts(frame = 1000, easing = 'linear', transition = 1000, redraw = TRUE)
    }

    return(plot)
})
