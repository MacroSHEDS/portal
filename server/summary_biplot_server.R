

files <- site_data %>%
    filter(site_type == "stream_gauge") %>%
    select(domain, site_name) %>%
    mutate(path_m = glue('data/biplot/month/{d}/{s}.feather',
                         d = domain,
                         s = site_name),
           path_y = glue('data/biplot/year/{d}/{s}.feather',
                         d = domain,
                         s = site_name))

observe({
    doms <- input$DOMAINS2_S
    
    dom_sites <- site_data %>%
        filter(site_type == 'stream_gauge') %>%
        filter(domain %in%doms) %>%
        pull(site_name)
    
    updateSelectInput(session, 'SITES2', choices = dom_sites)
})

observe({
    data_type <- input$X_TYPE2
    
    if(data_type == 'Concentration') {
        select <- chemvars_display_subset 
        units <- conc_units}
    
    if(data_type == 'Flux') {
        select <- fluxvars
        units <- flux_units_bi }
    
    if(data_type == 'Discharge') {
        select <- 'Q' 
        units <- 'm^3' }
    
    updateSelectInput(session, 'X_VAR2', choices = select)
    updateSelectInput(session, 'X_UNIT2', choices = units)
    
    })

observe({
    data_type <- input$Y_TYPE2
    
    if(data_type == 'Concentration') {
        select <- chemvars_display_subset 
        units <- conc_units}
    
    if(data_type == 'Flux') {
        select <- fluxvars
        units <- flux_units_bi }
    
    if(data_type == 'Discharge') {
        select <- 'Q' 
        units <- 'm^3' }
    
    updateSelectInput(session, 'Y_VAR2', choices = select)
    updateSelectInput(session, 'Y_UNIT2', choices = units)
    
})

observe({
    data_type <- input$SIZE_TYPE2
    
    if(data_type == 'Concentration') {
        select <- chemvars_display_subset 
        units <- conc_units}
    
    if(data_type == 'Flux') {
        select <- fluxvars
        units <- flux_units_bi }
    
    if(data_type == 'Discharge') {
        select <- 'Q' 
        units <- 'm^3' }
    
    updateSelectInput(session, 'SIZE_VAR2', choices = select)
    updateSelectInput(session, 'SIZE_UNIT2', choices = units)
    
})

summary <- reactive({
    
    agg <- input$AGG2
    
    if(agg == 'YEARLY2') {
        sum <- read_feather('data/biplot/year.feather')
    }
    
    if(agg == 'MONTHLY2') {
        sum <- read_feather('data/biplot/month.feather')
    }
    
    return(sum)
})

filtered_bi <- reactive({
    x_var <- input$X_VAR2
    y_var <- input$Y_VAR2
    size_var <- input$SIZE_VAR2
    x_unit <- input$X_UNIT2
    y_unit <- input$Y_UNIT2
    size_unit <- input$SIZE_UNIT2
    date1 <- input$DATES2_INTER[1]
    date2 <- input$DATES2_INTER[2]
    chem_x <- input$X_TYPE2
    chem_y <- input$Y_TYPE2
    chem_size <- input$SIZE_TYPE2
    domains <- input$DOMAINS2
    sites <- input$SITES2
    type <- switch(input$SITE_SELECTION2,
                    ALL_SITES2 = 'all',
                    DOMINE_NETWORK2 = 'dom',
                    BY_SITE2 = 'site')
    raw <- summary()
    
    # x_var <<- input$X_VAR2
    # y_var <<- input$Y_VAR2
    # size_var <<- input$SIZE_VAR2
    # x_unit <<- input$X_UNIT2
    # y_unit <<- input$Y_UNIT2
    # size_unit <<- input$SIZE_UNIT2
    # date1 <<- input$DATES2_INTER[1]
    # date2 <<- input$DATES2_INTER[2]
    # chem_x <<- input$X_TYPE2
    # chem_y <<- input$Y_TYPE2
    # chem_size <<- input$SIZE_TYPE2
    # domains <<- input$DOMAINS2
    # sites <<- input$SITES2
    # type <<- switch(input$SITE_SELECTION2,
    #                ALL_SITES2 = 'all',
    #                DOMINE_NETWORK2 = 'dom',
    #                BY_SITE2 = 'site')
    # raw <<- summary()
    
    if(chem_x == 'Flux') {
        x_var <- paste(x_var, 'flux', sep = '_')
    }
    
    if(chem_y == 'Flux') {
        y_var <- paste(y_var, 'flux', sep = '_')
    }
    
    if(chem_size == 'Flux') {
        size_var <- paste(size_var, 'flux', sep = '_')
    }
    
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
    
    final <- fill %>%
        select(site_name, domain, date, year, month, contains('month_an'), !!x_var, !!y_var, !!size_var) %>%
        filter(date >= !!date1,
               date <= !!date2) %>%
        filter(complete.cases(.)) 

    
    if(chem_x == 'Concentration') {
        final <- convert_conc_units_bi(final, x_var, 'mg/L', x_unit) 
    } 
    if(chem_x == 'Flux') {
        final <- convert_flux_units_bi(final, x_var, 'kg/ha', x_unit)
    }
    
    if(chem_y == 'Concentration') {
        final <- convert_conc_units_bi(final, y_var, 'mg/L', y_unit) 
    } 
    if(chem_y == 'Flux') {
        final <- convert_flux_units_bi(final, y_var, 'kg/ha', y_unit)
    }
    
    if(chem_size == 'Concentration') {
        final <- convert_conc_units_bi(final, size_var, 'mg/L', size_unit) 
    } 
    if(chem_size == 'Flux') {
        final <- convert_flux_units_bi(final, size_var, 'kg/ha', size_unit)
    }
    
    return(final)
})

n_sites <- reactive({
    sites <- filtered_bi()
    
    sites_num <- sites %>%
        distinct(site_name) %>%
        pull(site_name) 
    
    num <- length(sites_num)
    
    return(num)
})


output$SUMMARY_BIPLOT = renderPlotly({
    table <- filtered_bi()
    domains <- isolate(input$DOMAINS2) 
    sites <- isolate(input$SITES2)
    x_var <- isolate(input$X_VAR2)
    y_var <- isolate(input$Y_VAR2)
    size_var <- isolate(input$SIZE_VAR2)
    x_unit <- isolate(input$X_UNIT2)
    y_unit <- isolate(input$Y_UNIT2)
    size_unit <- isolate(input$SIZE_UNIT2)
    agg <- switch(isolate(input$AGG2),
                   MONTHLY2 = 'm',
                   YEARLY2 = 'year')
    chem_x <- isolate(input$X_TYPE2)
    chem_y <- isolate(input$Y_TYPE2)
    chem_size <- isolate(input$SIZE_TYPE2)
    
    # table <<- filtered_bi()
    # domains <<- isolate(input$DOMAINS2) 
    # sites <<- isolate(input$SITES2)
    # x_var <<- isolate(input$X_VAR2)
    # y_var <<- isolate(input$Y_VAR2)
    # size_var <<- isolate(input$SIZE_VAR2)
    # x_unit <<- isolate(input$X_UNIT2)
    # y_unit <<- isolate(input$Y_UNIT2)
    # size_unit <<- isolate(input$SIZE_UNIT2)
    # agg <<- switch(isolate(input$AGG2),
    #               MONTHLY2 = 'm',
    #               YEARLY2 = 'year')
    # chem_x <<- isolate(input$X_TYPE2)
    # chem_y <<- isolate(input$Y_TYPE2)
    # chem_size <<- isolate(input$SIZE_TYPE2)
    
    if(chem_x == 'Flux') {
        x_tvar <- paste(x_var, 'flux', sep = '_')
        x_unit <- paste0(x_unit, '/', agg)
    } else {x_tvar <- x_var}
    
    if(chem_y == 'Flux') {
        y_tvar <- paste(y_var, 'flux', sep = '_')
        y_unit <- paste0(y_unit, '/', agg)
    } else {y_tvar <- y_var}
    
    if(chem_size == 'Flux') {
        size_tvar <- paste(size_var, 'flux', sep = '_')
        size_unit <- paste0(size_unit, '/', agg)
    } else {size_tvar <- size_var}
    
    num_sites <- n_sites()
    
    if(num_sites > 8) {
        col_by <- 'domain'
        
    } else {col_by <- 'site_name'}
    
    if(agg == 'm') {
    
        plot <- table %>%
            plotly::plot_ly(x = ~get(x_tvar),
                y = ~get(y_tvar),
                size = ~get(size_tvar),
                color = ~get(col_by),
                frame = ~month_an,
                type = 'scatter',
                mode = 'markers',
                text = ~paste0(size_var, ' ', size_unit, ':', get(size_var))) %>%
        plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit)),
                       yaxis = list(title = paste0(y_var, ' ', y_unit))) 
            #animation_opts(frame = 1000, easing = 'linear', transition = 1000, redraw = TRUE) 
    } 
    if(agg == 'year') {
        plot <- table %>%
            plotly::plot_ly(x = ~get(x_tvar),
                        y = ~get(y_tvar),
                        size = ~get(size_tvar),
                        color = ~get(col_by),
                        frame = ~year,
                        type = 'scatter',
                        mode = 'markers',
                        text = ~paste0(size_var, ' ', size_unit, ':', get(size_var))) %>%
            plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit)),
                           yaxis = list(title = paste0(y_var, ' ', y_unit))) 
            #animation_opts(frame = 1000, easing = 'linear', transition = 1000, redraw = TRUE)
    }
    
    return(plot)
})
