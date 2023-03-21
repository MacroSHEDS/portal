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

sum <- read_feather("data/general/biplot/year.feather")

reactive_vals <- reactiveValues()

# filter for sites and years. This file is used to update options for axis
# in if only a few domains are selected and not all variables are available
# at that site
pre_filtered_bi <- reactive({

    #     date1 <<- input$DATES2_INTER[1]
    #     date2 <<- input$DATES2_INTER[2]
    #     domains <<- input$DOMAINS2
    #     domains_s <<- input$DOMAINS2_S
    #     sites <<- input$SITES2
    #     type <<- switch(input$SITE_SELECTION2,
    #                     ALL_SITES2 = 'all',
    #                     DOMINE_NETWORK2 = 'dom',
    #                     BY_SITE2 = 'site')
    #     # raw <- summary()
    #     raw <- sum

    date1 <- year(input$DATES2_INTER[1])
    date2 <- year(input$DATES2_INTER[2])
    domains <- input$DOMAINS2
    domains_s <- input$DOMAINS2_S
    domains_b <- input$DOMAINS2_B
    sites_b <- input$SITES2_B
    sites <- input$SITES2
    reselect <- input$SITE_SELECTION2

    type <- switch(input$SITE_SELECTION2,
        ALL_SITES2 = "all",
        DOMINE_NETWORK2 = "dom",
        BY_SITE2 = "site",
        BY_BUCKET2 = "bucket"
    )

    # raw <- summary()
    raw <- sum %>%
      filter(!var %in% ms_vars_blocked,
             !paste(domain, site_code) %in% c("suef C2", "suef C3", "suef C4"))

    if (type == "dom") {
        fill <- raw %>%
            filter(domain %in% domains)
    }

    if (type == "site") {
        fill <- raw %>%
            filter(domain %in% domains_s) %>%
            filter(site_code %in% sites)
    }

    if (type == "all") {
        fill <- raw
    }

    if (type == "bucket") {
        fill <- raw %>%
            filter(domain %in% domains_b) %>%
            filter(site_code %in% sites_b)
    }

    final <- fill %>%
        filter(is.na(Year) | (Year >= !!date1 & Year <= !!date2))

    return(final)
})

# Final filtering for variables and configures table for plotly graph
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
    # year1 <<- year(isolate(input$DATES2_INTER[1]))
    # year2 <<- year(isolate(input$DATES2_INTER[2]))
    # pfb <<- pre_filtered_bi()
    # #raw <- isolate(summary())
    # raw <- sum

    input$SITE_SELECTION2
    x_var <- input$X_VAR2
    y_var <- input$Y_VAR2
    include_size <- input$ADD_SIZE2
    size_var <- input$SIZE_VAR2
    x_unit <- input$X_UNIT2
    y_unit <- input$Y_UNIT2
    size_unit <- input$SIZE_UNIT2
    chem_x <- isolate(input$X_TYPE2)
    chem_y <- isolate(input$Y_TYPE2)
    chem_size <- isolate(input$SIZE_TYPE2)
    agg <- isolate(input$AGG2)
    domains <- isolate(input$DOMAINS2)
    sites <- isolate(input$SITES2)
    year1 <- year(isolate(input$DATES2_INTER[1]))
    year2 <- year(isolate(input$DATES2_INTER[2]))
    pfb <- pre_filtered_bi()

    # raw <- isolate(summary())
    raw <- sum

    if (nrow(pfb) == 0) {
        final <- tibble()
        return(final)
    }

    x_var_ <- biplot_selection_to_name(
        chem = chem_x,
        unit = x_unit,
        var = x_var
    )

    y_var_ <- biplot_selection_to_name(
        chem = chem_y,
        unit = y_unit,
        var = y_var
    )

    size_var_ <- biplot_selection_to_name(
        chem = chem_size,
        unit = size_unit,
        var = size_var
    )

    if (include_size) {
        if (size_var_ == "missing" && agg == "WHOLE2") {
            filter_vars <- c(x_var_, y_var_)
        } else {
            filter_vars <- c(x_var_, y_var_, size_var_)
        }
    } else {
        filter_vars <- c(x_var_, y_var_)
    }

    # Filter summary table for needed vars and spread to wide format
    if (agg == "YEARLY2") {

        final <- pfb %>%
            filter(! is.na(Year)) %>%
            filter(var %in% !!filter_vars) %>%
            select(-Date, -pctCellErr) %>%
            pivot_wider(names_from = "var", values_from = "val")

    } else {

        final <- pfb %>%
            # filter(! is.na(Year)) %>%
            # select(-missing) %>%
            filter(var %in% !!filter_vars) %>%
            group_by(site_code, var, domain) %>%
            # group_by(site_code, Date, Year, var, domain) %>%
            summarise(val = mean(val, na.rm = TRUE),
                      missing = mean(missing, na.rm = TRUE)) %>%
            ungroup() %>%
            pivot_wider(names_from = "var", values_from = "val")
    }

    # Filter to include only sites with all variables available
    ## final <- final[complete.cases(final),]

    if (any(!filter_vars %in% names(final))) {
        return(tibble())
    }

    x_var__ <- if(chem_x %in% c('Watershed Characteristics',
                                'Discharge', 'Precipitation')) x_var_ else x_var
    x_unit_start <- variables %>%
        filter(variable_code == x_var__)

    # Unit conversions. Could be improved for sure
    if (chem_x %in% c("Stream Chemistry", "Precipitation Chemistry")) {

        x_unit_start <- variables %>%
            filter(variable_code == x_var) %>%
            select(unit) %>%
            pull()

        final <- convert_conc_units_bi(final, x_var_, x_unit_start, x_unit)
    }
    if (chem_x %in% c("Stream Chemistry Flux", "Precipitation Chemistry Flux")) {
        final <- convert_flux_units_bi(
            df = final,
            col = x_var_,
            input_unit = "kg/ha/year",
            desired_unit = x_unit,
            summary_file = raw
        )
    }
    if (chem_x == "Discharge" && x_unit == "mm/d") {
        final <- final %>%
            mutate(discharge_a = discharge_a / 365)
    }

    if (chem_y %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        y_unit_start <- variables %>%
            filter(variable_code == y_var) %>%
            select(unit) %>%
            pull()

        final <- convert_conc_units_bi(final, y_var_, y_unit_start, y_unit)
    }
    if (chem_y %in% c("Stream Chemistry Flux", "Precipitation Chemistry Flux")) {
        final <- convert_flux_units_bi(
            df = final,
            col = y_var_,
            input_unit = "kg/ha/year",
            desired_unit = y_unit,
            summary_file = raw
        )
    }
    if (chem_y == "Discharge" && y_unit == "mm/d") {
        final <- final %>%
            mutate(discharge_a = discharge_a / 365)
    }

    if (include_size) {
        if (chem_size %in% c("Stream Chemistry", "Precipitation Chemistry")) {
            size_unit_start <- variables %>%
                filter(variable_code == size_var) %>%
                select(unit) %>%
                pull()

            final <- convert_conc_units_bi(final, size_var_, size_unit_start, size_unit)
        }
        if (chem_size %in% c("Stream Chemistry Flux", "Precipitation Chemistry Flux")) {
            final <- convert_flux_units_bi(
                df = final,
                col = size_var_,
                input_unit = "kg/ha/year",
                desired_unit = size_unit,
                summary_file = raw
            )
        }
        if (chem_size == "Discharge" && size_unit == "mm/d") {
            final <- final %>%
                mutate(discharge_a = discharge_a / 365)
        }
    }

    # If the whole record summary is selected, summarize across dates; compute missing here
    if (agg == "WHOLE2") {
        if (include_size && size_var_ == "missing") {
            year_dif <- (year2 - year1) + 1
            final <- final %>%
                group_by(site_code, domain) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                    n = n()
                ) %>%
                mutate(missing = round((((year_dif - n) / year_dif) * 100), 1)) %>%
                select(-any_of('Year'), -n)
        } else {
            final <- final %>%
                select(-any_of('Year')) %>%
                group_by(site_code, domain) %>%
                summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
        }
    }

    return(final)
})

## observeEvent(
##     eventExpr = input$SITE_SELECTION2,
##     priority = 90,
##     ignoreNULL = FALSE,
##   handlerExpr = {
##     shinyjs::runjs('$("span:contains("Map Selections")").css("color", "#485580");
##                     $("span:contains("Map Selections")").prev().prev().css("color", "#485580")')
##   }
## )

# Update axis options ####
# remove year as an axis option when aggregation the whole records

observeEvent(input$AGG2, {
    if (length(reactive_vals$facet) == 0) {
        reactive_vals$facet <- 0
    } else {
        reactive_vals$facet <- reactive_vals$facet + 1
    }
    print(reactive_vals$facet)
})

observe({
    agg <- input$AGG2
    yearly <- "Year"
    data <- isolate(pre_filtered_bi())

    if (reactive_vals$facet == 0) {
        return()
    }
    if (agg == "YEARLY2") {
        updateSelectInput(session, "X_TYPE2", choices = yearly)

        biplot_data_types_size <- append(biplot_data_types, "Proportion of Record Missing", after = 0)
        updateSelectInput(session, "SIZE_TYPE2", choices = biplot_data_types_size)
        updateRadioButtons(session, inputId = "LOG_X2", selected = "XAXIS_sta2")
    } else {
        updateSelectInput(session, "X_TYPE2", choices = biplot_data_types, selected = "Discharge")
        updateSelectInput(session, "X_VAR2", choices = "Q", selected = "Q")
        updateSelectInput(session, "SIZE_TYPE2", choices = biplot_data_types_size)
    }
    # }
})

# reactivre value saves varibles so when conc is changed to flux, it will not change the
# var unless it is not flux convertable
current_selection <- reactiveValues(
    old_x = "old",
    old_y = "old",
    old_size = "old"
)
observeEvent(input$X_VAR2, {
    current_selection$old_x <- input$X_VAR2
})


# update individual options for variables based on variable type
observe({
    data <- isolate(pre_filtered_bi())
    data_type <- input$X_TYPE2
    doms <- input$DOMAINS2_S
    input$DOMAINS2
    sites <- input$SITES2
    site_select <- input$SITE_SELECTION2
    old_selection <- isolate(current_selection$old_x)

    if (data_type == "Stream Chemistry") {
        select <- filter_dropdown_varlist_bi(data, vartype = "conc")
        units <- conc_units_bi
        choose <- "mg/L"
    }

    if (data_type == "Precipitation Chemistry") {
        select <- filter_dropdown_varlist_bi(data, vartype = "precip_conc")
        units <- conc_units_bi
        choose <- "mg/L"
    }

    if (data_type %in% c("Stream Chemistry Flux", "Precipitation Chemistry Flux")) {
        select <- filter_dropdown_varlist_bi(data, vartype = "flux")
        units <- flux_units_bi
        choose <- "kg/ha/year"
    }

    if (data_type == "Discharge") {
        select <- "Q"
        units <- discharge_units_bi
        choose <- "mm/year"
    }

    if (data_type == "Watershed Characteristics") {
        select <- ws_trait_types
        units <- subset_ws_traits(ws_trait_types[2], ws_traits)
        choose <- ""
    }

    if (data_type == "Year") {
        select <- "Year"
        units <- ""
        choose <- "Year"
    }

    if (data_type == "Precipitation") {
        select <- "P"
        units <- "mm"
        choose <- "mm"
    }

    if (data_type == "") {

    } else {
        if (old_selection %in% unlist(select)) {
            updateSelectInput(session, "X_VAR2", choices = select, selected = old_selection)
        } else {
            updateSelectInput(session, "X_VAR2", choices = select)
        }

        updateSelectInput(session, "X_UNIT2", choices = units, selected = choose)
    }
})

observeEvent(input$Y_VAR2, {
    current_selection$old_y <- input$Y_VAR2
})

observe({
    data <- isolate(pre_filtered_bi())
    data_type <- input$Y_TYPE2
    doms <- input$DOMAINS2_S
    input$DOMAINS2
    sites <- input$SITES2
    site_select <- isolate(input$SITE_SELECTION2)
    old_selection <- isolate(current_selection$old_y)

    if (data_type %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        select <- filter_dropdown_varlist_bi(data, vartype = "conc")
        units <- conc_units_bi
        choose <- "mg/L"
    }

    if (data_type %in% c("Stream Chemistry Flux", "Precipitation Chemistry Flux")) {
        select <- filter_dropdown_varlist_bi(data, vartype = "flux")
        units <- flux_units_bi
        choose <- "kg/ha/year"
    }

    if (data_type == "Discharge") {
        select <- "Q"
        var <- "Q"
        units <- discharge_units_bi
        choose <- "mm/year"
    }

    if (data_type == "Watershed Characteristics") {
        select <- ws_trait_types
        units <- subset_ws_traits(ws_trait_types[2], ws_traits)
        choose <- ""
    }

    if (data_type == "Precipitation") {
        select <- "P"
        units <- "mm"
        choose <- "mm"
    }

    if (data_type == "") {

    } else {
        if (old_selection %in% unlist(select)) {
            updateSelectInput(session, "Y_VAR2", choices = select, selected = old_selection)
        } else {
            updateSelectInput(session, "Y_VAR2", choices = select)
        }

        updateSelectInput(session, "Y_UNIT2", choices = units, selected = choose)
    }
})

observeEvent(input$SIZE_VAR2, {
    current_selection$old_size <- input$SIZE_VAR2
})

observe({
    data <- isolate(pre_filtered_bi())
    data_type <- input$SIZE_TYPE2
    doms <- input$DOMAINS2_S
    input$DOMAINS2
    sites <- input$SITES2
    site_select <- isolate(input$SITE_SELECTION2)
    old_selection <- isolate(current_selection$old_size)
    x_var <- isolate(input$X_VAR2)
    y_var <- isolate(input$Y_VAR2)

    if (data_type %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        select <- filter_dropdown_varlist_bi(data, vartype = "conc")
        units <- conc_units_bi
        choose <- "mg/L"
    }

    if (data_type %in% c("Stream Chemistry Flux", "Precipitation Chemistry Flux")) {
        select <- filter_dropdown_varlist_bi(data, vartype = "flux")
        units <- flux_units_bi
        choose <- "kg/ha/year"
    }

    if (data_type == "Discharge") {
        select <- "Q"
        units <- discharge_units_bi
        choose <- "mm/year"
    }

    if (data_type == "Watershed Characteristics") {
        select <- ws_trait_types
        units <- subset_ws_traits(ws_trait_types[2], ws_traits)
        choose <- ""
    }

    if (data_type == "Precipitation") {
        select <- "P"
        units <- "mm"
        choose <- "mm"
    }

    if (data_type == "Proportion of Record Missing") {
        select <- "% of record missing"
        units <- ""
        choose <- "% of record missing"
    }

    if (data_type == "") {

    } else {
        if (old_selection %in% unlist(select)) {
            updateSelectInput(session, "SIZE_VAR2", choices = select, selected = old_selection)
        } else {
            updateSelectInput(session, "SIZE_VAR2", choices = select)
        }

        updateSelectInput(session, "SIZE_UNIT2", choices = units, selected = choose)
    }
})

# update sites based on domains selected
current_site_selection <- reactiveValues(sites = c())
observeEvent(input$X_VAR2, {
    current_site_selection$sites <- input$X_VAR2
})

observe({
    data_type <- input$DOMAINS2_S
    current_sites <- isolate(input$SITES2)

    domain_choices <- site_data %>%
        filter(
            site_type == "stream_gauge",
            domain %in% !!data_type
        ) %>%
        pull(domain) %>%
        unique()

    domain_site_list <- generate_dropdown_sitelist(domain_vec = domain_choices)

    sites_in_domains <- site_data %>%
        filter(domain %in% domain_choices) %>%
        pull(site_code)

    new_sites <- current_sites[current_sites %in% sites_in_domains]

    updateSelectInput(session, "SITES2", choices = domain_site_list, selected = new_sites)
})

observe({
    data_type <- input$DOMAINS2_B
    current_sites <- isolate(input$SITES2_B)

    biplot_trigger()

    domain_choices <- site_data %>%
        filter(
            site_type == "stream_gauge",
            domain %in% !!data_type
        ) %>%
        pull(domain) %>%
        unique()

    domain_site_list <- generate_dropdown_sitelist(domain_vec = domain_choices)

    sites_in_domains <- site_data %>%
        filter(domain %in% domain_choices) %>%
        pull(site_code)

    new_sites <- current_sites[current_sites %in% sites_in_domains]

    updateSelectInput(session, "SITES2_B", choices = domain_site_list, selected = new_sites)
})

bucket_sites <- reactive({input$SITES2_B})
observe({
  bucket_contents <- input$SITES2_B
})

# update unit options if they are not convertable
observe({
    x_var <- input$X_VAR2
    y_var <- input$Y_VAR2
    size_var <- input$SIZE_VAR2

    chem_x <- isolate(input$X_TYPE2)
    chem_y <- isolate(input$Y_TYPE2)
    chem_size <- isolate(input$SIZE_TYPE2)

    if (!convertible(x_var) && chem_x %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        updateSelectInput(session, "X_UNIT2", choices = "")
    }

    if (convertible(x_var) && chem_x %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        updateSelectInput(session, "X_UNIT2", choices = conc_units_bi, selected = conc_units_bi[3])
    }

    if (!convertible(y_var) && chem_y %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        updateSelectInput(session, "Y_UNIT2", choices = "")
    }

    if (convertible(y_var) && chem_y %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        updateSelectInput(session, "Y_UNIT2", choices = conc_units_bi, selected = conc_units_bi[3])
    }

    if (!convertible(size_var) && chem_size %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        updateSelectInput(session, "SIZE_UNIT2", choices = "")
    }

    if (convertible(size_var) && chem_size %in% c("Stream Chemistry", "Precipitation Chemistry")) {
        updateSelectInput(session, "SIZE_UNIT2", choices = conc_units_bi, selected = conc_units_bi[3])
    }
})

# Update ws_chars options if the category is changed
observe({
    input$SITE_SELECTION2
    input$DOMAINS2
    input$SITES2

    # X axis react
    x_var <- input$X_VAR2
    chem_x <- isolate(input$X_TYPE2)

    if (chem_x == "Watershed Characteristics") {
        updateSelectInput(session, "X_UNIT2", choices = subset_ws_traits(x_var, ws_traits))
    }

    # Y axis react
    y_var <- input$Y_VAR2
    chem_y <- isolate(input$Y_TYPE2)

    if (chem_y == "Watershed Characteristics") {
        updateSelectInput(session, "Y_UNIT2", choices = subset_ws_traits(y_var, ws_traits))
    }
})

observe({
    input$SITE_SELECTION2
    input$DOMAINS2
    input$SITES2

    size_var <- input$SIZE_VAR2
    chem_size <- isolate(input$SIZE_TYPE2)

    if (chem_size == "Watershed Characteristics") {
        updateSelectInput(session, "SIZE_UNIT2", choices = subset_ws_traits(size_var, ws_traits))
    }
})

# Plot data ####

# color by sites or domain
n_sites <- reactive({
    sites <- filtered_bi()

    if(nrow(sites) == 0) {
        num <- 1
    } else {
        sites_num <- sites %>%
            distinct(site_code) %>%
            pull(site_code)

        num <- length(sites_num)
    }

    return(num)
})

# Biplot updates when the underlying data changes, the data change based on
# Interacting selection so sometimes this cause a cascade of reactivity that
# updates data many times, this delay allows all those changes to happen before
# updating the graph, eliminating blank graph
biplot_trigger <- reactive({
    filtered_bi()
    return()
}) %>%
    debounce(250)

output$SUMMARY_BIPLOT <- renderPlotly({

    # biplot_trigger()
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

    biplot_trigger()
    bi_table <- isolate(filtered_bi())
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
        "MONTHLY2" = "m",
        "YEARLY2" = "year",
        "WHOLE2" = "year "
    )

    chem_x <- isolate(input$X_TYPE2)
    chem_y <- isolate(input$Y_TYPE2)
    chem_size <- isolate(input$SIZE_TYPE2)
    num_sites <- isolate(n_sites())

    empty_msg <- if (! length(sites)) {
        "No sites selected"
    } else {
        "No data available for \nthe selected variables"
    }

    if (x_var == "Year" & y_var %in% c("Soil", "Geochemistry", "Hydrology", "Terrain", "Lithology")) {
        empty_msg <- 'Non-temporal data selected. \nSet Aggregation to "Full record"'
    }

    empty_plot <- plotly::plot_ly(type = "scatter") %>%
        plotly::layout(
            annotations = list(
                text = empty_msg,
                xref = "paper",
                yref = "paper",
                opacity = 0.4,
                "showarrow" = F,
                font = list(size = 30)
            ),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
        )


    if (nrow(bi_table) == 0 || x_var == "" || y_var == "" || size_var == "") {
        return(empty_plot)
    }

    # high contrast pallete, original colors genereated by: https://mokole.com/palette.html
    safe_cols <- c(
      '#800000',
      '#00ff00',
      '#ba55d3',
      '#ffd700',
      '#00ffff',
      '#ff1493',
      '#0000ff',
      '#2e8b57',
      '#ff00ff',
      '#9acd32',
      '#00bfff',
      '#2f4f4f',
      '#00fa9a',
      '#00008b',
      '#ff0000',
      '#ff8c00',
      '#dda0dd',
      '#ffa07a',
      '#bdb76b'
    )

    x_tvar <- biplot_selection_to_name(
        chem = chem_x,
        unit = x_unit,
        var = x_var
    )

    y_tvar <- biplot_selection_to_name(
        chem = chem_y,
        unit = y_unit,
        var = y_var
    )

    size_tvar <- biplot_selection_to_name(
        chem = chem_size,
        unit = size_unit,
        var = size_var
    )

    emplty_blank <- plotly::plot_ly(type = "scatter") %>%
        plotly::layout()

    if (!include_size) {
        if (any(!c(x_tvar, y_tvar) %in% names(bi_table))) {
            return(emplty_blank)
        }
    } else {
        if (any(!c(x_tvar, y_tvar, size_tvar) %in% names(bi_table))) {
            return(emplty_blank)
        }
    }

    # if(chem_x %in% c('Stream Chemistry Flux', 'Precipitation Chemistry Flux')) {
    #     x_unit <- paste0(x_unit, '/', agg)
    # }
    #
    # if(chem_y %in% c('Stream Chemistry Flux', 'Precipitation Chemistry Flux')) {
    #     y_unit <- paste0(y_unit, '/', agg)
    # }
    #
    # if(chem_size %in% c('Stream Chemistry Flux', 'Precipitation Chemistry Flux')) {
    #     size_unit <- paste0(size_unit, '/', agg)
    # }

    if (chem_x == "Watershed Characteristics") {
        display_names <- subset_ws_traits(x_var, ws_traits)
        x_unit <- names(display_names[x_unit == display_names])
        x_var <- ""
    }

    if (chem_y == "Watershed Characteristics") {
        display_names <- subset_ws_traits(y_var, ws_traits)
        y_unit <- names(display_names[y_unit == display_names])
        y_var <- ""
    }

    if (chem_size == "Watershed Characteristics") {
        display_names <- subset_ws_traits(size_var, ws_traits)
        size_unit <- names(display_names[size_unit == display_names])
        size_var <- ""
    }

    if (num_sites > 12) {
        col_by <- "pretty_domain"

        networks_cite <- network_domain_default_sites$pretty_network[network_domain_default_sites$domain %in% unique(bi_table$domain)]

        networks_cite <- unique(networks_cite)

        networks_cite <- paste(networks_cite, collapse = ", ")
    } else {
        col_by <- "legend_name"

        domains_cite <- network_domain_default_sites$pretty_domain[network_domain_default_sites$domain %in% unique(bi_table$domain)]
        domains_n_cite <- network_domain_default_sites$pretty_network[network_domain_default_sites$domain %in% unique(bi_table$domain)]

        networks_cite <- paste(paste(domains_cite, domains_n_cite, sep = " "), collapse = ", ")
    }

    bi_table <- network_domain_default_sites %>%
        select(domain, pretty_domain) %>%
        right_join(., bi_table, by = "domain")

    if (col_by == "legend_name") {
        bi_table <- bi_table %>%
            mutate(legend_name = paste0(pretty_domain, " - ", site_code))
    } else {
        bi_table <- bi_table %>%
            mutate(legend_name = site_code)
    }

    # Currently disabled
    if (agg == "m") {
        plot <- bi_table %>%
            plotly::plot_ly(
                x = ~ get(x_tvar),
                y = ~ get(y_tvar),
                size = ~ get(size_tvar),
                color = ~ get(col_by),
                colors = safe_cols,
                frame = ~Date,
                type = "scatter",
                mode = "markers",
                text = ~ paste0(size_var, " ", size_unit, ":", get(size_var))
            ) %>%
            plotly::layout(
                xaxis = list(title = paste0(x_var, " ", x_unit)),
                yaxis = list(title = paste0(y_var, " ", y_unit)),
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)"
            )
    }

    if (agg == "year") {

        # define ticks if gtreater than 15 years
        min_year <- min(as.numeric(pull(bi_table[x_tvar])))
        max_year <- max(as.numeric(pull(bi_table[x_tvar])))

        year_length <- max_year - min_year
        all_years <- unique(as.numeric(pull(bi_table[x_tvar])))

        if (year_length >= 15) {
            tick_vals <- all_years[nchar(all_years / 5) == 3]
        } else {
            tick_vals <- all_years
        }

        if (include_size) {
            if (col_by == "pretty_domain") {
                plot <- bi_table %>%
                    plotly::plot_ly(
                        x = ~ get(x_tvar),
                        y = ~ get(y_tvar),
                        type = "scatter",
                        mode = "lines+markers",
                        alpha = 0.8,
                        size = ~ get(size_tvar),
                        color = ~ get(col_by),
                        colors = safe_cols,
                        split = ~site_code,
                        line = list(width = 2, color = ~ get(col_by)),
                        text = ~ paste0(size_var, " ", size_unit, ":", round(get(size_tvar), digits = 2), "\nSite:", site_code, ", \nDomain:", pretty_domain),
                        legendgroup = ~ get(col_by)
                    ) %>%
                    plotly::layout(
                        xaxis = list(
                            title = paste0(x_var, " ", x_unit),
                            range = c(min_year, max_year),
                            tickvals = tick_vals
                        ),
                        yaxis = list(title = paste0(y_var, " ", y_unit)),
                        paper_bgcolor = "rgba(0,0,0,0)",
                        plot_bgcolor = "rgba(0,0,0,0)",
                        legend = list(
                            itemsizing = "constant",
                            traceorder = "grouped"
                        )
                    )
            } else {
                plot <- bi_table %>%
                    plotly::plot_ly(
                        x = ~ get(x_tvar),
                        y = ~ get(y_tvar),
                        type = "scatter",
                        mode = "lines+markers",
                        alpha = 0.8,
                        size = ~ get(size_tvar),
                        color = ~ get(col_by),
                        colors = safe_cols,
                        line = list(width = 2, color = ~ get(col_by)),
                        text = ~ paste0(size_var, " ", size_unit, ":", round(get(size_tvar), digits = 2), "\nSite:", site_code, ", \nDomain:", pretty_domain),
                        legendgroup = ~ get(col_by)
                    ) %>%
                    plotly::layout(
                        xaxis = list(
                            title = paste0(x_var, " ", x_unit),
                            range = c(min_year, max_year),
                            tickvals = tick_vals
                        ),
                        yaxis = list(title = paste0(y_var, " ", y_unit)),
                        paper_bgcolor = "rgba(0,0,0,0)",
                        plot_bgcolor = "rgba(0,0,0,0)",
                        legend = list(
                            itemsizing = "constant",
                            traceorder = "grouped"
                        )
                    )
            }
        } else {

            # site_to_domain <- bi_table %>%
            #     select(site_code, pretty_domain) %>%
            #     distinct(site_code, .keep_all = TRUE)
            #
            # one_site <- bi_table %>%
            #     #filter(domain %in% c('boulder', 'hbef')) %>%
            #     pivot_wider(names_from = 'site_code', values_from = 'SO4_S_conc',
            #                 id_cols = 'Year') %>%
            #     arrange(Year)
            #
            # site_cols <- one_site %>%
            #     select(-Year) %>%
            #     colnames()
            #
            # fin_test <- plotly::plot_ly()
            #
            # all_doms <- unique(site_to_domain$pretty_domain)
            #
            # for(s in 1:length(site_cols)){
            #
            #     this_site <- site_cols[s]
            #
            #     this_pretty_domain <- site_to_domain %>%
            #         filter(site_code == !!this_site) %>%
            #         pull(pretty_domain)
            #
            #     colo_pos  <- grep(this_pretty_domain, all_doms)
            #
            #     fin_test <- plotly::add_trace(p = fin_test,
            #                                   x = one_site[['Year']],
            #                                   y = one_site[[this_site]],
            #                                   type = 'scatter',
            #                                   mode = 'lines',
            #                                   alpha = 0.8,
            #                                   line = list(width = 2, color = safe_cols[colo_pos]),
            #                                   text = ~paste0('Site:', this_site, ', \nDomain:', this_pretty_domain),
            #                                   name = ~this_pretty_domain,
            #                                   #legendgroup = ~this_pretty_domain,
            #                                   showlegend = T
            #         )
            # }

            # plotly::plot_ly(one_site, x = ~get(x_tvar)) %>%
            #     plotly::add_trace(y = ~w1,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[1]),
            #                       text = ~paste0('Site:', 'w1', ', \nDomain:', pretty_domain),
            #                       name = 'HBEF',
            #                       legendgroup = ~pretty_domain,
            #                       connectgaps=T
            #     ) %>%
            #     plotly::add_trace(y = ~w4,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[1]),
            #                       legendgroup = ~pretty_domain,
            #                       showlegend = F,
            #                       connectgaps=T
            #     ) %>%
            #     plotly::add_trace(y = ~w3,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[1]),
            #                       legendgroup = ~pretty_domain,
            #                       showlegend = F,
            #                       connectgaps=T
            #     ) %>%
            #     plotly::add_trace(y = ~w5,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[1]),
            #                       legendgroup = ~pretty_domain,
            #                       showlegend = F,
            #                       connectgaps=T
            #     ) %>%
            #     plotly::add_trace(y = ~w7,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[1]),
            #                       legendgroup = ~pretty_domain,
            #                       showlegend = F,
            #                       connectgaps=T
            #     ) %>%
            #     plotly::add_trace(y = ~GGU,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[2])
            #     ) %>%
            #     plotly::add_trace(y = ~GGL,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[2])
            #     ) %>%
            #     plotly::add_trace(y = ~BC_SW_4,
            #                       type = 'scatter',
            #                       mode = 'lines',
            #                       alpha = 0.8,
            #                       line = list(width = 2, color = safe_cols[2])
            #     ) %>%
            #     plotly::layout(xaxis = list(title = paste0(x_var, ' ', x_unit),
            #                                 range = c(min_year, max_year),
            #                                 tickvals = tick_vals),
            #                    yaxis = list(title = paste0(y_var, ' ', y_unit)),
            #                    paper_bgcolor = 'rgba(0,0,0,0)',
            #                    plot_bgcolor = 'rgba(0,0,0,0)'
            #     )

            if (col_by == "pretty_domain") {
                plot <- bi_table %>%
                    plotly::plot_ly(
                        x = ~ get(x_tvar),
                        y = ~ get(y_tvar),
                        type = "scatter",
                        mode = "lines",
                        alpha = 0.8,
                        color = ~ get(col_by),
                        colors = safe_cols,
                        split = ~site_code,
                        connectgaps = F,
                        line = list(width = 2, color = ~ get(col_by)),
                        text = ~ paste0("Site:", site_code, ", \nDomain:", pretty_domain),
                        legendgroup = ~ get(col_by)
                        # name = paste0(~pretty_domain, '>', ~site_code)
                    ) %>%
                    plotly::layout(
                        xaxis = list(
                            title = paste0(x_var, " ", x_unit),
                            range = c(min_year, max_year),
                            tickvals = tick_vals
                        ),
                        yaxis = list(title = paste0(y_var, " ", y_unit)),
                        paper_bgcolor = "rgba(0,0,0,0)",
                        plot_bgcolor = "rgba(0,0,0,0)"
                    )
            } else {
                plot <- bi_table %>%
                    plotly::plot_ly(
                        x = ~ get(x_tvar),
                        y = ~ get(y_tvar),
                        type = "scatter",
                        mode = "lines",
                        alpha = 0.8,
                        color = ~ get(col_by),
                        colors = safe_cols,
                        connectgaps = F,
                        line = list(width = 2, color = ~ get(col_by)),
                        text = ~ paste0("Site:", site_code, ", \nDomain:", pretty_domain),
                        legendgroup = ~ get(col_by)
                        # name = paste0(~pretty_domain, '>', ~site_code)
                    ) %>%
                    plotly::layout(
                        xaxis = list(
                            title = paste0(x_var, " ", x_unit),
                            range = c(min_year, max_year),
                            tickvals = tick_vals
                        ),
                        yaxis = list(title = paste0(y_var, " ", y_unit)),
                        paper_bgcolor = "rgba(0,0,0,0)",
                        plot_bgcolor = "rgba(0,0,0,0)"
                    )
            }
        }
    }

    if (agg == "year ") {
        if (include_size) {
            plot <- bi_table %>%
                plotly::plot_ly(
                    x = ~ get(x_tvar),
                    y = ~ get(y_tvar),
                    size = ~ get(size_tvar),
                    color = ~ get(col_by),
                    colors = safe_cols,
                    fill = "",
                    type = "scatter",
                    mode = "markers",
                    text = ~ paste0(size_var, " ", size_unit, ":", round(get(size_tvar), digits = 2), "\nSite:", site_code, ", \nDomain:", pretty_domain)
                ) %>%
                plotly::layout(
                    # xaxis = list(title = paste0("Mean", " ", x_var, " ", x_unit)),
                    xaxis = list(title = paste(x_var, x_unit)),
                    yaxis = list(title = paste(y_var, y_unit)),
                    paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor = "rgba(0,0,0,0)",
                    legend = list(itemsizing = "constant")
                )
        } else {
            plot <- bi_table %>%
                plotly::plot_ly(
                    x = ~ get(x_tvar),
                    y = ~ get(y_tvar),
                    size = 2,
                    color = ~ get(col_by),
                    colors = safe_cols,
                    fill = "",
                    type = "scatter",
                    mode = "markers",
                    text = ~ paste0("\nSite:", site_code, ", \nDomain:", pretty_domain)
                ) %>%
                plotly::layout(
                    xaxis = list(title = paste(x_var, x_unit)),
                    yaxis = list(title = paste(y_var, y_unit)),
                    paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor = "rgba(0,0,0,0)"
                )
        }
    }

    if (input$LOG_X2 == "XAXIS_log2") {
        plot <- plot %>%
            layout(xaxis = list(type = "log"))
    }

    if (input$LOG_Y2 == "YAXIS_log2") {
        plot <- plot %>%
            layout(yaxis = list(type = "log"))
    }

    plot <- plot %>%
        plotly::layout(annotations = list(
            text = paste0("macrosheds.org data provided by:\n", networks_cite),
            xref = "paper",
            yref = "paper",
            opacity = 0.10,
            showarrow = FALSE,
            textangle = -45,
            font = list(size = 28)
        ))

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
#         filter(site_code %in% selected) %>%
#         pull(domain)
#
#     updateSelectInput(session, 'DOMAINS2_S', selected=map_dom)
#     updateSelectInput(session, 'SITES2', selected=selected)
#
#     updateSelectInput(session, )
# })

# reduce the reactivity sensitivity of the timeslider, so that intermediate inputs
# don't trigger plot updates
                                        #
# biplot ideas
# 1) add in much more informative warnings than "no data" to help
#    user know what dates/data avialable
# 2) add coloring and other devices to help user use the site selected
#    in the biplot as well

timeSliderChanged <- eventReactive(
    {
        input$DATES2_INTER
    },
    {

        # this is the ultimate gatekeeper for plot rendering. it can be triggered by
        # the Update Plots button or by the user directly. to ensure that the plots
        # still update if the dates selected don't change, a random date is appended
        # to input$DATES3. this random date is then ignored in all the data preppers

        print("BIPLOT: slider update for plots.")

        datevec_rando_append <- c(input$DATES2_INTER, as.Date(runif(1, 0, 10000)))
        return(datevec_rando_append)
    }
) %>%
    debounce(1000)
