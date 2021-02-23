
#TODO: add a line like this to all renderers to attempt popout windows again
# output$GRAPH_PRECIP3a = output$GRAPH_PRECIP3aEXP = renderDygraph({

## govern showing/hiding of facets ####

reactive_vals = reactiveValues()
reactive_vals$facet3a = 0
reactive_vals$facet3b = 0
reactive_vals$facet3c = 0
# reactive_vals$facet3aP = 0
# reactive_vals$facet3bP = 0
# reactive_vals$facet3cP = 0
reactive_vals$update_basedata = 0

#main facets
observeEvent(input$REFRESH, {
    print('REFRESH')
    reactive_vals$facet3a = reactive_vals$facet3a + 1
    reactive_vals$facet3b = reactive_vals$facet3b + 1
    reactive_vals$facet3c = reactive_vals$facet3c + 1
})

observeEvent({
    if(
        ! is.null(input$SITES3) &&
        ! is.null(get_domains3()) &&
        ! is.null(input$CONC_FLUX3) &&
        ! is.null(input$FLUX_UNIT3) &&
        ! is.null(input$CONC_UNIT3) &&
        ! is.null(input$SHOW_PCHEM3) &&
        ! is.null(input$AGG3) &&
        is.null(timeSliderUpdate()) &&
        # ! is.null(input$DATES3) &&
        ! is.null(input$SHOW_QC3) &&
        ! is.null(input$INSTALLED_V_GRAB3) &&
        ! is.null(input$SENSOR_V_NONSENSOR3) &&
        ! is.null(input$SHOW_UNCERT3) &&
        ! is.null(input$FLAGS3) &&
        ! is.null(input$INTERP3) &&
        length(input$VARS3) == 1
    ){ TRUE } else NULL
}, {
    print('rvalA')
    reactive_vals$facet3a = reactive_vals$facet3a + 1
})

observeEvent({
    if(
        ! is.null(input$SITES3) &&
        ! is.null(get_domains3()) &&
        ! is.null(input$CONC_FLUX3) &&
        ! is.null(input$FLUX_UNIT3) &&
        ! is.null(input$CONC_UNIT3) &&
        ! is.null(input$SHOW_PCHEM3) &&
        ! is.null(input$AGG3) &&
        # ! is.null(input$DATES3) &&
        is.null(timeSliderUpdate()) &&
        ! is.null(input$SHOW_QC3) &&
        ! is.null(input$INSTALLED_V_GRAB3) &&
        ! is.null(input$SENSOR_V_NONSENSOR3) &&
        ! is.null(input$SHOW_UNCERT3) &&
        ! is.null(input$FLAGS3) &&
        ! is.null(input$INTERP3) &&
        length(input$VARS3) == 2
    ){ TRUE } else NULL
    # if(length(input$VARS3) == 2){
    #     TRUE
    # } else return()
}, {
    print('rvalB')
    reactive_vals$facet3a = reactive_vals$facet3a + 1
    reactive_vals$facet3b = reactive_vals$facet3b + 1
})

observeEvent({
    if(
        ! is.null(input$SITES3) &&
        ! is.null(get_domains3()) &&
        ! is.null(input$CONC_FLUX3) &&
        ! is.null(input$FLUX_UNIT3) &&
        ! is.null(input$CONC_UNIT3) &&
        ! is.null(input$SHOW_PCHEM3) &&
        ! is.null(input$AGG3) &&
        # ! is.null(input$DATES3) &&
        is.null(timeSliderUpdate()) &&
        ! is.null(input$SHOW_QC3) &&
        ! is.null(input$INSTALLED_V_GRAB3) &&
        ! is.null(input$SENSOR_V_NONSENSOR3) &&
        ! is.null(input$SHOW_UNCERT3) &&
        ! is.null(input$FLAGS3) &&
        ! is.null(input$INTERP3) &&
        length(input$VARS3) == 3
    ){ TRUE } else NULL
    # if(length(input$VARS3) == 3){
    #     TRUE
    # } else return()
}, {
    print('rvalC')
    reactive_vals$facet3a = reactive_vals$facet3a + 1
    reactive_vals$facet3b = reactive_vals$facet3b + 1
    reactive_vals$facet3c = reactive_vals$facet3c + 1
})

# #precip facets
# observeEvent({
#     if(length(input$SITES3) >= 1){ TRUE } else return()
# }, {
#     reactive_vals$facet3aP = reactive_vals$facet3aP + 1
# })
#
# observeEvent({
#     if(length(input$SITES3) >= 2){ TRUE } else return()
# }, {
#     reactive_vals$facet3aP = reactive_vals$facet3aP + 1
#     reactive_vals$facet3bP = reactive_vals$facet3bP + 1
# })
#
# observeEvent({
#     if(length(input$SITES3) == 3){ TRUE } else return()
# }, {
#     reactive_vals$facet3aP = reactive_vals$facet3aP + 1
#     reactive_vals$facet3bP = reactive_vals$facet3bP + 1
#     reactive_vals$facet3cP = reactive_vals$facet3cP + 1
# })

## reactivity flow control ####

#when domain(s) change, site options and basedata change, but not site selections
get_domains3 <- eventReactive(input$DOMAINS3, {

    domains <- input$DOMAINS3

    reactive_vals$update_basedata <- reactive_vals$update_basedata + 1

    updateSelectizeInput(session,
                         'SITES3',
                         choices = generate_dropdown_sitelist(domains),
                         selected = input$SITES3,
                         options = list(maxItems = 3))

    return(domains)
})

# #when domain(s) change and SHOW_PCHEM3==TRUE, update basedata reactive value
# observeEvent({
#     if(
#         ! is.null(get_domains3()) &&
#         input$SHOW_PCHEM3
#     ){ TRUE } else return()
# }, {
#     reactive_vals$update_basedata = reactive_vals$update_basedata + 1
# })

#when site(s) change or basedata reactive value updates, basedata changes
load_basedata <- eventReactive({

    input$SITES3
    reactive_vals$update_basedata
    input$TIME_SCHEME3
    input$INSTALLED_V_GRAB3
    input$SENSOR_V_NONSENSOR3
    # input$SHOW_UNCERT3
    input$FLAGS3
    input$INTERP3

}, {

    # #NOTICE: may not behave as expected (might have to globally define the datasets below)
    # time_scheme <<- input$TIME_SCHEME3
    # agg <<- isolate(input$AGG3)
    # dmns <<- get_domains3()
    #
    # if(is.null(dmns)){ #for empty domain dropdown
    #     dmns <<- init_vals$recent_domain
    #     sites <<- get_default_site(domain = dmns[1])
    # } else {
    #     sites <<- input$SITES3
    # }

    time_scheme <- input$TIME_SCHEME3
    agg <- isolate(input$AGG3)
    dmns <- get_domains3()

    if(is.null(dmns)){ #for empty domain dropdown
        dmns <- init_vals$recent_domain
        sites <- get_default_site(domain = dmns[1])
    } else {
        sites <- input$SITES3
    }

    Q <- read_combine_feathers('discharge',
                               dmns = dmns,
                               sites = sites)
    chem <- read_combine_feathers('stream_chemistry',
                                  dmns = dmns,
                                  sites = sites)
    flux <- read_combine_feathers('stream_flux_inst_scaled',
                                  dmns = dmns,
                                  sites = sites)
    P <- read_combine_feathers('precipitation',
                               dmns = dmns,
                               sites = sites)
    pchem <- read_combine_feathers('precip_chemistry',
                                   dmns = dmns,
                                   sites = sites)
    pflux <- read_combine_feathers('precip_flux_inst',
                                   dmns = dmns,
                                   sites = sites)

    init_vals$recent_domain <- dmns[1] #needed?

    basedata <- list(Q = Q,
                     chem = chem,
                     flux = flux,
                     P = P,
                     pchem = pchem,
                     pflux = pflux)

    if(time_scheme != 'UTC' && agg == 'Instantaneous') {
        basedata <- purrr::modify2(basedata,
                                   get_local_solar_time,
                                   .y = time_scheme)
    }

    return(basedata)
})

#when basedata changes, variable list and time slider change, but not selections
observe({

    print('basedata change')
    # basedata <<- load_basedata()
    # vars_ <<- isolate(input$VARS3)
    # dates <<- isolate(input$DATES3)

    basedata <- load_basedata()
    vars_ <- isolate(input$VARS3)
    dates <- isolate(input$DATES3)

    chemvars_display_subset <- filter_dropdown_varlist(basedata$chem)

    updateSelectizeInput(session = session,
                         inputId = 'VARS3',
                         choices = chemvars_display_subset,
                         selected = vars_)

    dtrng = get_timeslider_extent(basedata, dates)

    updateSliderInput(session = session,
                      inputId = 'DATES3',
                      min = dtrng[1],
                      max = dtrng[2],
                      value = dates,
                      timeFormat = '%b %Y')
})

#reduce the reactivity sensitivity of the slider, so that intermediate inputs
#don't trigger plot updates
timeSliderUpdate <- reactive({
    input$DATES3
    return()
}) %>%
    debounce(1000)


#if variables(s), aggregation, units, site, or time window change, re-filter datasets
dataChem <- reactive({

    print('dataChem')

    # basedata <<- load_basedata()
    # dates <<- isolate(input$DATES3)
    # timeSliderUpdate()
    # vars_ <<- input$VARS3
    # conc_flux <<- input$CONC_FLUX3
    # conc_unit <<- input$CONC_UNIT3 #
    # flux_unit <<- input$FLUX_UNIT3 #
    # agg <<- input$AGG3
    # # sites <<- input$SITES3
    # #time_scheme <<- input$TIME_SCHEME3
    # igsn <<- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    # show_uncert <<- input$SHOW_UNCERT3
    # show_flagged <<- input$FLAGS3
    # show_imputed <<- input$INTERP3
    # enable_unitconvert <<- init_vals$enable_unitconvert

    basedata <- load_basedata()
    dates <- isolate(input$DATES3)
    timeSliderUpdate()
    vars_ <- input$VARS3
    conc_flux <- input$CONC_FLUX3
    conc_unit <- input$CONC_UNIT3
    flux_unit <- input$FLUX_UNIT3
    agg <- input$AGG3
    # sites <- input$SITES3
    #time_scheme <- input$TIME_SCHEME3
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3
    enable_unitconvert <- init_vals$enable_unitconvert

    datachem <- if(conc_flux == 'Flux') basedata$flux else basedata$chem

    datachem <- filter_agg_widen_unprefix(d = datachem,
                                          selected_vars = vars_,
                                          selected_datebounds = dates,
                                          selected_agg = agg,
                                          selected_prefixes = igsn,
                                          show_uncert = show_uncert,
                                          show_flagged = show_flagged,
                                          show_imputed = show_imputed,
                                          conc_or_flux = conc_flux)

    datachem <- convert_portal_units(d = datachem,
                                     conversion_enabled = enable_unitconvert,
                                     conc_flux_selection = conc_flux,
                                     conc_unit = conc_unit,
                                     flux_unit = flux_unit)

    return(datachem)
})

dataPchem <- reactive({

    print('dataPchem')

    # basedata <<- load_basedata()
    # dates <<- isolate(input$DATES3)
    # timeSliderUpdate()
    # vars_ <<- input$VARS3
    # conc_flux <<- input$CONC_FLUX3
    # conc_unit <<- input$CONC_UNIT3 #
    # flux_unit <<- input$FLUX_UNIT3 #
    # agg <<- input$AGG3
    # # sites <<- input$SITES3
    # #time_scheme <<- input$TIME_SCHEME3
    # igsn <<- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    # show_uncert <<- input$SHOW_UNCERT3
    # show_flagged <<- input$FLAGS3
    # show_imputed <<- input$INTERP3
    # enable_unitconvert <<- init_vals$enable_unitconvert

    basedata <- load_basedata()
    dates <- isolate(input$DATES3)
    timeSliderUpdate()
    vars_ <- input$VARS3
    conc_flux <- input$CONC_FLUX3
    conc_unit <- input$CONC_UNIT3
    flux_unit <- input$FLUX_UNIT3
    agg <- input$AGG3
    # sites <- input$SITES3
    #time_scheme <- input$TIME_SCHEME3
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3
    enable_unitconvert <- init_vals$enable_unitconvert


    dataPchem <- if(conc_flux == 'Flux') basedata$pflux else basedata$pchem

    dataPchem <- filter_agg_widen_unprefix(d = dataPchem,
                                           selected_vars = vars_,
                                           selected_datebounds = dates,
                                           selected_agg = agg,
                                           selected_prefixes = igsn,
                                           show_uncert = show_uncert,
                                           show_flagged = show_flagged,
                                           show_imputed = show_imputed,
                                           conc_or_flux = conc_flux)

    dataPchem <- convert_portal_units(d = dataPchem,
                                      conversion_enabled = enable_unitconvert,
                                      conc_flux_selection = conc_flux,
                                      conc_unit = conc_unit,
                                      flux_unit = flux_unit)


    return(dataPchem)
})

dataPrecip <- reactive({

    print('Precip')

    # basedata <<- load_basedata()
    # dates <<- isolate(input$DATES3)
    # timeSliderUpdate()
    # agg <<- input$AGG3
    # conc_flux <<- input$CONC_FLUX3
    # igsn <<- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    # show_uncert <<- input$SHOW_UNCERT3
    # show_flagged <<- input$FLAGS3
    # show_imputed <<- input$INTERP3

    basedata <- load_basedata()
    dates <- isolate(input$DATES3)
    timeSliderUpdate()
    agg <- input$AGG3
    conc_flux <- input$CONC_FLUX3
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3

    dataP <- basedata$P

    dataP <- filter_agg_widen_unprefix(d = dataP,
                                       selected_vars = 'precipitation',
                                       selected_datebounds = dates,
                                       selected_agg = agg,
                                       selected_prefixes = igsn,
                                       show_uncert = show_uncert,
                                       show_flagged = show_flagged,
                                       show_imputed = show_imputed,
                                       conc_or_flux = conc_flux)

    warning('does precip need to be aggregated by median and sum?')

    # dates <- isolate(input$DATES3)
    # timeSliderUpdate()
    # agg = input$AGG3
    # sites = input$SITES3
    # basedata = load_basedata()
    # dmns = isolate(get_domains3())
    # input$TIME_SCHEME3

    # dataprecip = basedata$P
    # # dataprecip <<- basedata$P
    #
    # if(nrow(dataprecip) == 0) return(dataprecip)
    #
    # dataprecip = dataprecip %>%
    #     filter(datetime >= dates[1], datetime <= dates[2]) %>%
    #     select(one_of('datetime', 'site_name', 'precip'))
    #
    # if(nrow(dataprecip) == 0) return(dataprecip)
    #
    # dataprecip = pad_ts(dataprecip, vars='precip', datebounds=dates)
    # dataprecip = ms_aggregate(dataprecip, agg, which_dataset='p')
    #
    # dataprecip = dataprecip %>%
    #     group_by(datetime, site_name) %>%
    #     summarize(sumPrecip=sum(precip, na.rm=TRUE),
    #         medianPrecip=median(precip, na.rm=TRUE)) %>%
    #     ungroup()
    #
    # #append rows for selected sites with no data
    # missing_sites = sites[! sites %in% unique(dataprecip$site_name)]
    # if(length(missing_sites)){
    #     for(m in missing_sites){
    #         fake_date = lubridate::force_tz(as.POSIXct(dates[2]), tzone='UTC')
    #         dataprecip = bind_rows(dataprecip,
    #             tibble(datetime=fake_date, site_name=m, precip=as.numeric(NA)))
    #     }
    # }

    return(dataP)
})

dataQ <- reactive({

    print('dataQ')

    # basedata <<- load_basedata()
    # dates <<- isolate(input$DATES3)
    # timeSliderUpdate()
    # agg <<- input$AGG3
    # igsn <<- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    # show_uncert <<- input$SHOW_UNCERT3
    # show_flagged <<- input$FLAGS3
    # show_imputed <<- input$INTERP3

    basedata <- load_basedata()
    dates <- isolate(input$DATES3)
    timeSliderUpdate()
    agg <- input$AGG3
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3

    dataQ <- basedata$Q

    dataQ <- filter_agg_widen_unprefix(d = dataQ,
                                       selected_vars = 'discharge',
                                       selected_datebounds = dates,
                                       selected_agg = agg,
                                       selected_prefixes = igsn,
                                       show_uncert = show_uncert,
                                       show_flagged = show_flagged,
                                       show_imputed = show_imputed,
                                       conc_or_flux = conc_flux)

    # if(nrow(dataq) == 0) return(dataq)
    #
    # dataq = dataq %>%
    #     filter(datetime > dates[1], datetime < dates[2])
    #     # select(datetime, site_name, discharge)
    #
    # if(nrow(dataq) == 0) return(dataq)
    #
    # dataq = pad_ts(dataq, vars='discharge', datebounds=dates)
    # dataq = ms_aggregate(dataq, agg, which_dataset='q')
    #
    # if(agg == 'Instantaneous'){ #revisit this. needed?
    #     dataq = dataq %>%
    #         group_by(datetime, site_name) %>%
    #         summarise(discharge=max(discharge, na.rm=TRUE)) %>%
    #         ungroup()
    # }

    return(dataQ)
})

## post-filtering data modifications ####

#these should only update when prerequisite reactive data (above) updates, so
#all user inputs should be isolated

#calculate VWC (volume weighted concentration) from chem and q
#only possible at monthly and yearly agg. conditionals controlled by ui
volWeightedChem3 <- reactive({

    datachem <- dataChem()
    dataQ <- dataQ()
    agg <- isolate(input$AGG3)
    # datachem <<- dataChem()
    # dataQ <<- dataQ()
    # agg <<- isolate(input$AGG3)

    samplevel <- datachem %>%
        left_join(dataQ,
                  by = c('datetime', 'site_name')) %>%
        mutate(
            across(matches('^val_(?!discharge)',
                           perl = TRUE),
                   ~(. * val_discharge)),
            across(matches('^ms_status_(?!discharge)',
                           perl = TRUE),
                   ~(bitwOr(., ms_status_discharge))),
            across(matches('^ms_interp_(?!discharge)',
                           perl = TRUE),
                   ~(bitwOr(., ms_interp_discharge))))

    if(agg == 'Monthly'){

        samplevel <- samplevel %>%
            mutate(year = lubridate::year(datetime))

        agglevel <- samplevel %>%
            select(site_name, year, val_discharge) %>%
            group_by(year, site_name) %>%
            summarize(Qsum = sum(val_discharge,
                                 na.rm = TRUE),
                      .groups = 'drop')

        volWeightedConc <- samplevel %>%
            select(-ends_with('discharge')) %>%
            left_join(agglevel,
                      by = c('year', 'site_name')) %>%
            mutate(across(starts_with('val_'),
                          ~(. / Qsum))) %>%
            select(-Qsum, -year)

    } else if(agg == 'Yearly'){

        agglevel <- samplevel %>%
            group_by(site_name) %>%
            summarize(Qsum = sum(val_discharge,
                                 na.rm = TRUE),
                      .groups = 'drop')

        volWeightedConc <- samplevel %>%
            select(-ends_with('discharge')) %>%
            left_join(agglevel,
                      by = 'site_name') %>%
            mutate(across(starts_with('val_'),
                          ~(. / Qsum))) %>%
            select(-Qsum)
    }

    return(volWeightedConc)
})

#calculate VWC (volume weighted concentration) from pchem and p
#only possible at monthly and yearly agg. conditionals controlled by ui
volWeightedPchem3 <- reactive({

    # datapchem <<- dataPchem()
    # dataP <<- dataPrecip()
    # agg <<- isolate(input$AGG3)
    datapchem <- dataPchem()
    dataP <- dataPrecip()
    agg <- isolate(input$AGG3)

    samplevel <- datapchem %>%
        left_join(dataP,
                  by = c('datetime', 'site_name')) %>%
        mutate(
            across(matches('^val_(?!precipitation)',
                           perl = TRUE),
                   ~(. * val_precipitation)),
            across(matches('^ms_status_(?!precipitation)',
                           perl = TRUE),
                   ~(bitwOr(., ms_status_precipitation))),
            across(matches('^ms_interp_(?!precipitation)',
                           perl = TRUE),
                   ~(bitwOr(., ms_interp_precipitation))))

    # samplevel = samplevel %>%
    #     left_join(select(dataprecip, -medianPrecip),
    #               by=c('datetime', 'site_name')) %>%
    #     left_join(select(site_data, site_name, ws_area_ha),
    #               by='site_name') %>%
    #     mutate(precipVol=sumPrecip * ws_area_ha) %>%
    #     mutate_at(vars(one_of(vars_)), ~(. * precipVol)) %>%
    #     select(datetime, site_name, one_of(vars_), sumPrecip) %>%
    #     rename(P=sumPrecip)

    if(agg == 'Monthly'){

        samplevel <- samplevel %>%
            mutate(year = lubridate::year(datetime))

        agglevel <- samplevel %>%
            select(site_name, year, val_precipitation) %>%
            group_by(year, site_name) %>%
            summarize(Psum = sum(val_precipitation,
                                 na.rm = TRUE),
                      .groups = 'drop')

        volWeightedConc <- samplevel %>%
            select(-ends_with('precipitation')) %>%
            left_join(agglevel,
                      by = c('year', 'site_name')) %>%
            mutate(across(starts_with('val_'),
                          ~(. / Psum))) %>%
            select(-Psum, -year)

    } else if(agg == 'Yearly'){

        agglevel <- samplevel %>%
            group_by(site_name) %>%
            summarize(Psum = sum(val_precipitation,
                                 na.rm = TRUE),
                      .groups = 'drop')

        volWeightedConc <- samplevel %>%
            select(-ends_with('precipitation')) %>%
            left_join(agglevel,
                      by = 'site_name') %>%
            mutate(across(starts_with('val_'),
                          ~(. / Psum))) %>%
            select(-Psum)
    }

    return(volWeightedConc)
})

## plot generators ####
#these should only update when prerequisite reactive data or facets change
#def could use better abstraction, efficiency measures

output$GRAPH_PRECIP3 <- renderDygraph({

    sites <- input$SITES3
    dates <- isolate(input$DATES3)
    dataP <- dataPrecip()

    # sites <<- input$SITES3[1]
    # dates <<- isolate(input$DATES3)
    # dataP <<- dataPrecip()

    tryCatch(
        {
            dataP <- dataP %>%
                dplyr::rename_with(~ gsub('_precipitation', '', .x)) %>%
                tidyr::pivot_wider(names_from = site_name,
                                   values_from = c('val', 'ms_status',
                                                   'ms_interp')) %>%
                dplyr::rename_with(~ gsub('val_', '', .x))
        },
        error = function(e) NULL
    )

    # reactive_vals$facet3aP

    if(nrow(dataP)){

        colnms <- colnames(dataP)
        displabs <- colnms[colnms %in% sites]

        dydat <- xts(dataP[, displabs],
                     order.by = dataP$datetime,
                     tzone = lubridate::tz(dataP$datetime[1]))

        dimnames(dydat) <- list(NULL, displabs)
        # dimnames(dydat) <- list(NULL, site)

        ymax <- max(dydat,
                    na.rm = TRUE)

        dg <- dygraph(dydat,
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = TRUE,
                      fillGraph = TRUE,
                      retainDateWindow = TRUE,
                      labelsKMB = TRUE,
                      # stackedGraph = TRUE,

                      # #if precip panels are separated, use these specifications
                      # fillAlpha = 1,
                      # colors = raincolors[1],
                      # strokeWidth = 3,
                      # plotter = hyetograph_js,

                      #if not showing points, use these
                      drawPoints = FALSE,
                      strokeWidth = 1,
                      fillAlpha = 0.4,
                      colors = selection_color_match(sites,
                                                     displabs[displabs %in% sites],
                                                     pchemcolors),
                      drawGapEdgePoints = TRUE

                      # #if showing points, use these (needs work)
                      # drawPoints = TRUE,
                      # strokeWidth = 0.01,
                      # pointSize = 1,
                      # strokeBorderWidth = 1,
                      # colors = 'white',
                      # fillAlpha = 0.4,
                      # strokeBorderColor = selection_color_match(sites,
                      #                                           displabs,
                      #                                           linecolors)
            ) %>%
            dyLegend(show = 'always',
            # dyLegend(show = 'onmouseover',
                     labelsSeparateLines = FALSE,
                     # labelsDiv = 'main3aP') %>%
                     labelsDiv = 'P3') %>%
            dyAxis('y',
                   label = 'P (mm)',
                   valueRange = c(ymax + ymax * 0.1,
                                  0),
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 10,
                   rangePad = 10)

        #alternative way to show points in different color? also needs work)
        # dg2 <- dySeries(dg,
        #                name = 'w1',
        #                # group = 'nSiteNVar',
        #                fillGraph = FALSE,
        #                color = 'red',
        #                axis = 'y',
        #                drawPoints = TRUE,
        #                strokeWidth = 0,
        #                pointSize = 1) %>%
        #     dyOptions(stackedGraph = TRUE)
        # dg2

    } else {

        dg <- plot_empty_dygraph(dates,
                                 plotgroup = 'nSiteNVar',
                                 ylab = 'P (mm)',
                                 px_per_lab = 10)
    }

    return(dg)
})

# output$GRAPH_MAIN3a <- output$GRAPH_MAIN3aFULL <- renderDygraph({
output$GRAPH_MAIN3a <- renderDygraph({

    # sites <<- na.omit(isolate(input$SITES3))
    # varA <<- isolate(input$VARS3[1])
    # conc_flux <<- isolate(input$CONC_FLUX3)
    # flux_unit <<- isolate(input$FLUX_UNIT3)
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)
    # show_uncert <<- isolate(input$SHOW_UNCERT3)

    sites <- na.omit(isolate(input$SITES3))
    varA <- isolate(input$VARS3[1])#
    conc_flux <- isolate(input$CONC_FLUX3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    conc_unit <- isolate(input$CONC_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(input$DATES3)
    show_uncert <- isolate(input$SHOW_UNCERT3)

    if(reactive_vals$facet3a == 0) return()#
    print('mainA')

    if(conc_flux == 'VWC'){
        streamdata <- volWeightedChem3()
    } else {
        streamdata <- dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata <- volWeightedPchem3()
        } else {
            raindata <- dataPchem()
        }

    } else {
        raindata <- tibble()
    }

    # streamdata <<- streamdata
    # raindata <<- raindata
    # print(head(streamdata))
    # print(head(raindata))

    alldata <- pad_widen_join(v = varA,#
                              sites = sites,
                              dates = dates,
                              streamdata = streamdata,
                              raindata = raindata,
                              show_input_concentration = show_pchem)

    rainsites <- get_rainsites(alldata = alldata,
                               streamsites = sites,
                               show_input_concentration = show_pchem)

    ylabel <- get_ylab(v = varA,#
                       conc_flux = conc_flux,
                       conc_unit = conc_unit,
                       flux_unit = flux_unit)

    if(nrow(alldata)){

        colnms <- colnames(alldata)
        included_cols <- colnms[colnms %in% c(sites,
                                              paste0('P_', sites))]

        if(show_uncert){

            alldata <- alldata %>%
                mutate(across(any_of(included_cols),
                       .fns = list(errhi = ~(errors::drop_errors(.) +
                                                 errors::errors(.))))) %>%
                mutate(across(any_of(included_cols),
                       .fns = list(errlo = ~(errors::drop_errors(.) -
                                                 errors::errors(.)))))

            included_cols <- c(included_cols,
                               paste0(included_cols, '_errhi'),
                               paste0(included_cols, '_errlo'))
        }

        dydat <- xts(alldata[, included_cols],
                     order.by = alldata$datetime,
                     tzone = lubridate::tz(alldata$datetime[1]))

        # dimnames(dydat) <- list(NULL, included_cols)

        is_inst <- ifelse(agg == 'Instantaneous',
                          TRUE,
                          FALSE)

        dg <- dygraph(dydat,#[,1:2],
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = FALSE,
                      retainDateWindow = TRUE,

                      #if not showing all points, use these specifications.
                      drawPoints = FALSE,
                      colors = selection_color_match(sites,
                                                     included_cols,
                                                     linecolors),
                      strokeWidth = 2,
                      pointSize = 2,
                      drawGapEdgePoints = TRUE,
                      labelsKMB = TRUE,

                      # #if showing points, use these
                      # drawPoints = TRUE,
                      # strokeWidth = 0.01,
                      # pointSize = 1,
                      # strokeBorderWidth = 1,
                      # colors = 'white',
                      # strokeBorderColor = selection_color_match(sites,
                      #                                           included_cols,
                      #                                           linecolors),

                      connectSeparatedPoints = is_inst) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3a') %>%#
            dyAxis('y',
                   label = ylabel,
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 20,
                   rangePad = 10)

        if(show_pchem){

            rain_or_pchem_colors <- selection_color_match(
                sites_selected = paste0('P_', sites),
                sites_all = paste0('P_', included_cols[included_cols %in% sites]),
                colorvec = pchemcolors
            ) #minimally tested. might need to work with rainsites instead

            if(show_uncert){

                rain_names <- lapply(rainsites,
                                     function(x){
                                         c(paste0(x, '_errlo'),
                                           x,
                                           paste0(x, '_errhi'))
                                     })
            } else {
                rain_names <- as.list(rainsites)
            }

            for(i in 1:length(rainsites)){

                dg <- dySeries(dg,
                               name = rain_names[[i]],
                               color = rain_or_pchem_colors[i],
                               axis = 'y',
                               drawPoints = FALSE,
                               strokeWidth = 2,
                               pointSize = 2,
                               strokePattern = 'dashed')
            }
        }

        if(show_uncert){

            stream_names <- lapply(sites,
                                   function(x){
                                       c(paste0(x, '_errlo'),
                                         x,
                                         paste0(x, '_errhi'))
                                   })

            for(i in 1:length(sites)){

                dg <- dySeries(dg,
                               name = stream_names[[i]])
                               # color = rain_or_pchem_colors[i],
                               # axis = 'y',
                               # drawPoints = FALSE,
                               # strokeWidth = 2,
                               # pointSize = 2,
                               # strokePattern = 'dashed')
            }
        }

    } else {

        dg <- plot_empty_dygraph(dates,
                                 mainlab = colnames(alldata)[-1],
                                 maindiv = 'main3a',#
                                 plotgroup = 'nSiteNVar',
                                 ylab = ylabel,
                                 px_per_lab = 20)
    }

    return(dg)
})

output$GRAPH_QC3a <- renderPlot({

    print('QC3a')

    # show_qc <<- input$SHOW_QC3
    # sites <<- na.omit(isolate(input$SITES3[1:3]))
    # varA <<- isolate(input$VARS3[1])
    # # dmns <<- isolate(get_domains3())
    # conc_flux <<- isolate(input$CONC_FLUX3)
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # flux_unit <<- isolate(input$FLUX_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # show_uncert <<- isolate(input$SHOW_UNCERT3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)
    # datachem <<- dataChem()
    # dataq <<- dataQ()

    show_qc <- input$SHOW_QC3
    sites <- na.omit(isolate(input$SITES3[1:3]))
    varA <- isolate(input$VARS3[1])
    conc_flux <- isolate(input$CONC_FLUX3)
    conc_unit <- isolate(input$CONC_UNIT3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(input$DATES3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    datachem <- dataChem()
    dataq <- dataQ()

    if(reactive_vals$facet3a == 0 || ! show_qc) return()

    # datachem <- pad_widen_join(v = varA,
    #                           sites = sites,
    #                           dates = dates,
    #                           streamdata = datachem)

    alldata <- datachem %>%
        select(c('datetime', 'site_name', ends_with(varA))) %>%
        inner_join(dataq,
                   by = c("datetime", "site_name"))
        # rename(discharge = val_discharge)
#
#     ylabel <- get_ylab(v = varA,
#                        conc_flux = conc_flux,
#                        conc_unit = conc_unit,
#                        flux_unit = flux_unit)

    n_val_cols <- sum(grepl('^val_', colnames(alldata)))
    if(n_val_cols < 2){
        return(plot_empty_qc(ylab = varA))
    }

    if(show_uncert){

        alldata <- alldata %>%
            mutate(across(starts_with('val_'),
                          # .fns = list(~errors::drop_errors(.)),
                          # .names = '{.col}'))
                          .fns = list(errhi = ~(errors::drop_errors(.) +
                                                    errors::errors(.))))) %>%
            mutate(across(starts_with('val_') &
                              ! ends_with('_errhi'),
                          .fns = list(errlo = ~(errors::drop_errors(.) -
                                                    errors::errors(.))))) %>%
            mutate(across(starts_with('val_') &
                              ! ends_with(c('_errhi', '_errlo')),
                          errors::drop_errors))
    }

    cq <- ggplot(alldata,
                 aes(x = val_discharge,
                     y = !!sym(paste0('val_', varA)),
                     colour = site_name)) +
        # environment = environment()) +
        geom_point(na.rm = TRUE,
                   size = 1)

    if(show_uncert){

        cq <- cq +
            geom_linerange(aes(ymin = !!sym(paste0('val_', varA, '_errlo')),
            # geom_pointrange(aes(ymin = !!sym(paste0('val_', varA, '_errlo')),
                               ymax = !!sym(paste0('val_', varA, '_errhi')))) +
            geom_errorbarh(aes(xmin = val_discharge_errlo,
                               xmax = val_discharge_errhi))
    }

    cq <- cq +
        scale_colour_manual(values = linecolors,
                            breaks = c(sites)) +
        ggthemes::theme_few() +
        scale_y_continuous(position = "right") +
        ylab(paste('Q', 'vs.', varA)) +
        # ylab(paste('Q (L/s)',
        #            'vs.',
        #            ylabel)) +
        theme(legend.position = 'none',
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=10),
              # axis.title.y.right = element_text('C v. Q'),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_rect(fill = '#f5f5f5',
                                              color = '#f5f5f5'),
              plot.background = element_rect(fill = '#f5f5f5',
                                             color = '#f5f5f5'))

    return(cq)
})

output$GRAPH_MAIN3b <- renderDygraph({

    # sites <<- na.omit(isolate(input$SITES3))
    # varB <<- isolate(input$VARS3[1])
    # conc_flux <<- isolate(input$CONC_FLUX3)
    # flux_unit <<- isolate(input$FLUX_UNIT3)
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)
    # show_uncert <<- isolate(input$SHOW_UNCERT3)

    sites <- na.omit(isolate(input$SITES3))
    varB <- isolate(input$VARS3[2])#
    conc_flux <- isolate(input$CONC_FLUX3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    conc_unit <- isolate(input$CONC_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(input$DATES3)
    show_uncert <- isolate(input$SHOW_UNCERT3)

    if(reactive_vals$facet3b == 0) return()#
    print('mainA')

    if(conc_flux == 'VWC'){
        streamdata <- volWeightedChem3()
    } else {
        streamdata <- dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata <- volWeightedPchem3()
        } else {
            raindata <- dataPchem()
        }

    } else {
        raindata <- tibble()
    }

    # streamdata <<- streamdata
    # raindata <<- raindata
    # print(head(streamdata))

    alldata <- pad_widen_join(v = varB,#
                              sites = sites,
                              dates = dates,
                              streamdata = streamdata,
                              raindata = raindata,
                              show_input_concentration = show_pchem)

    rainsites <- get_rainsites(alldata = alldata,
                               streamsites = sites,
                               show_input_concentration = show_pchem)

    ylabel <- get_ylab(v = varB,#
                       conc_flux = conc_flux,
                       conc_unit = conc_unit,
                       flux_unit = flux_unit)

    if(nrow(alldata)){

        colnms <- colnames(alldata)
        included_cols <- colnms[colnms %in% c(sites,
                                              paste0('P_', sites))]

        if(show_uncert){

            alldata <- alldata %>%
                mutate(across(any_of(included_cols),
                       .fns = list(errhi = ~(errors::drop_errors(.) +
                                                 errors::errors(.))))) %>%
                mutate(across(any_of(included_cols),
                       .fns = list(errlo = ~(errors::drop_errors(.) -
                                                 errors::errors(.)))))

            included_cols <- c(included_cols,
                               paste0(included_cols, '_errhi'),
                               paste0(included_cols, '_errlo'))
        }

        dydat <- xts(alldata[, included_cols],
                     order.by = alldata$datetime,
                     tzone = lubridate::tz(alldata$datetime[1]))

        # dimnames(dydat) <- list(NULL, included_cols)

        is_inst <- ifelse(agg == 'Instantaneous',
                          TRUE,
                          FALSE)

        dg <- dygraph(dydat,#[,1:2],
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = FALSE,
                      retainDateWindow = TRUE,

                      #if not showing all points, use these specifications.
                      drawPoints = FALSE,
                      colors = selection_color_match(sites,
                                                     included_cols,
                                                     linecolors),
                      strokeWidth = 2,
                      pointSize = 2,
                      drawGapEdgePoints = TRUE,
                      labelsKMB = TRUE,

                      # #if showing points, use these
                      # drawPoints = TRUE,
                      # strokeWidth = 0.01,
                      # pointSize = 1,
                      # strokeBorderWidth = 1,
                      # colors = 'white',
                      # strokeBorderColor = selection_color_match(sites,
                      #                                           included_cols,
                      #                                           linecolors),

                      connectSeparatedPoints = is_inst) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3b') %>%#
            dyAxis('y',
                   label = ylabel,
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 20,
                   rangePad = 10)

        if(show_pchem){

            rain_or_pchem_colors <- selection_color_match(
                sites_selected = paste0('P_', sites),
                sites_all = paste0('P_', included_cols[included_cols %in% sites]),
                colorvec = pchemcolors
            ) #minimally tested. might need to work with rainsites instead

            if(show_uncert){

                rain_names <- lapply(rainsites,
                                     function(x){
                                         c(paste0(x, '_errlo'),
                                           x,
                                           paste0(x, '_errhi'))
                                     })
            } else {
                rain_names <- as.list(rainsites)
            }

            for(i in 1:length(rainsites)){

                dg <- dySeries(dg,
                               name = rain_names[[i]],
                               color = rain_or_pchem_colors[i],
                               axis = 'y',
                               drawPoints = FALSE,
                               strokeWidth = 2,
                               pointSize = 2,
                               strokePattern = 'dashed')
            }
        }

        if(show_uncert){

            stream_names <- lapply(sites,
                                   function(x){
                                       c(paste0(x, '_errlo'),
                                         x,
                                         paste0(x, '_errhi'))
                                   })

            for(i in 1:length(sites)){

                dg <- dySeries(dg,
                               name = stream_names[[i]])
                               # color = rain_or_pchem_colors[i],
                               # axis = 'y',
                               # drawPoints = FALSE,
                               # strokeWidth = 2,
                               # pointSize = 2,
                               # strokePattern = 'dashed')
            }
        }

    } else {

        dg <- plot_empty_dygraph(dates,
                                 mainlab = colnames(alldata)[-1],
                                 maindiv = 'main3b',#
                                 plotgroup = 'nSiteNVar',
                                 ylab = ylabel,
                                 px_per_lab = 20)
    }

    return(dg)
})

output$GRAPH_QC3b <- renderPlot({

    print('QC3b')

    # show_qc <<- input$SHOW_QC3
    # sites <<- na.omit(isolate(input$SITES3[1:3]))
    # varB <<- isolate(input$VARS3[1])
    # # dmns <<- isolate(get_domains3())
    # conc_flux <<- isolate(input$CONC_FLUX3)
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # flux_unit <<- isolate(input$FLUX_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # show_uncert <<- isolate(input$SHOW_UNCERT3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)
    # datachem <<- dataChem()
    # dataq <<- dataQ()

    show_qc <- input$SHOW_QC3
    sites <- na.omit(isolate(input$SITES3[1:3]))
    varB <- isolate(input$VARS3[2])
    conc_flux <- isolate(input$CONC_FLUX3)
    conc_unit <- isolate(input$CONC_UNIT3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(input$DATES3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    datachem <- dataChem()
    dataq <- dataQ()

    if(reactive_vals$facet3b == 0 || ! show_qc) return()

    alldata <- datachem %>%
        select(c('datetime', 'site_name', ends_with(varB))) %>%
        inner_join(dataq,
                   by = c("datetime", "site_name"))

    n_val_cols <- sum(grepl('^val_', colnames(alldata)))
    if(n_val_cols < 2){
        return(plot_empty_qc(ylab = varB))
    }

    if(show_uncert){

        alldata <- alldata %>%
            mutate(across(starts_with('val_'),
                          .fns = list(errhi = ~(errors::drop_errors(.) +
                                                    errors::errors(.))))) %>%
            mutate(across(starts_with('val_') &
                              ! ends_with('_errhi'),
                          .fns = list(errlo = ~(errors::drop_errors(.) -
                                                    errors::errors(.))))) %>%
            mutate(across(starts_with('val_') &
                              ! ends_with(c('_errhi', '_errlo')),
                          errors::drop_errors))
    }

    cq <- ggplot(alldata,
                 aes(x = val_discharge,
                     y = !!sym(paste0('val_', varB)),
                     colour = site_name)) +
        geom_point(na.rm = TRUE,
                   size = 1)

    if(show_uncert){

        cq <- cq +
            geom_linerange(aes(ymin = !!sym(paste0('val_', varB, '_errlo')),
                               ymax = !!sym(paste0('val_', varB, '_errhi')))) +
            geom_errorbarh(aes(xmin = val_discharge_errlo,
                               xmax = val_discharge_errhi))
    }

    cq <- cq +
        scale_colour_manual(values = linecolors,
                            breaks = c(sites)) +
        ggthemes::theme_few() +
        scale_y_continuous(position = "right") +
        ylab(paste('Q', 'vs.', varB)) +
        theme(legend.position = 'none',
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=10),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_rect(fill = '#f5f5f5',
                                              color = '#f5f5f5'),
              plot.background = element_rect(fill = '#f5f5f5',
                                             color = '#f5f5f5'))

    return(cq)
})

output$GRAPH_MAIN3c <- renderDygraph({

    # sites <<- na.omit(isolate(input$SITES3))
    # varC <<- isolate(input$VARS3[1])
    # conc_flux <<- isolate(input$CONC_FLUX3)
    # flux_unit <<- isolate(input$FLUX_UNIT3)
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)
    # show_uncert <<- isolate(input$SHOW_UNCERT3)

    sites <- na.omit(isolate(input$SITES3))
    varC <- isolate(input$VARS3[3])#
    conc_flux <- isolate(input$CONC_FLUX3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    conc_unit <- isolate(input$CONC_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(input$DATES3)
    show_uncert <- isolate(input$SHOW_UNCERT3)

    if(reactive_vals$facet3c == 0) return()#
    print('mainA')

    if(conc_flux == 'VWC'){
        streamdata <- volWeightedChem3()
    } else {
        streamdata <- dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata <- volWeightedPchem3()
        } else {
            raindata <- dataPchem()
        }

    } else {
        raindata <- tibble()
    }

    # streamdata <<- streamdata
    # raindata <<- raindata
    # print(head(streamdata))

    alldata <- pad_widen_join(v = varC,#
                              sites = sites,
                              dates = dates,
                              streamdata = streamdata,
                              raindata = raindata,
                              show_input_concentration = show_pchem)

    rainsites <- get_rainsites(alldata = alldata,
                               streamsites = sites,
                               show_input_concentration = show_pchem)

    ylabel <- get_ylab(v = varC,#
                       conc_flux = conc_flux,
                       conc_unit = conc_unit,
                       flux_unit = flux_unit)

    if(nrow(alldata)){

        colnms <- colnames(alldata)
        included_cols <- colnms[colnms %in% c(sites,
                                              paste0('P_', sites))]

        if(show_uncert){

            alldata <- alldata %>%
                mutate(across(any_of(included_cols),
                       .fns = list(errhi = ~(errors::drop_errors(.) +
                                                 errors::errors(.))))) %>%
                mutate(across(any_of(included_cols),
                       .fns = list(errlo = ~(errors::drop_errors(.) -
                                                 errors::errors(.)))))

            included_cols <- c(included_cols,
                               paste0(included_cols, '_errhi'),
                               paste0(included_cols, '_errlo'))
        }

        dydat <- xts(alldata[, included_cols],
                     order.by = alldata$datetime,
                     tzone = lubridate::tz(alldata$datetime[1]))

        # dimnames(dydat) <- list(NULL, included_cols)

        is_inst <- ifelse(agg == 'Instantaneous',
                          TRUE,
                          FALSE)

        dg <- dygraph(dydat,#[,1:2],
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = FALSE,
                      retainDateWindow = TRUE,

                      #if not showing all points, use these specifications.
                      drawPoints = FALSE,
                      colors = selection_color_match(sites,
                                                     included_cols,
                                                     linecolors),
                      strokeWidth = 2,
                      pointSize = 2,
                      drawGapEdgePoints = TRUE,
                      labelsKMB = TRUE,

                      # #if showing points, use these
                      # drawPoints = TRUE,
                      # strokeWidth = 0.01,
                      # pointSize = 1,
                      # strokeBorderWidth = 1,
                      # colors = 'white',
                      # strokeBorderColor = selection_color_match(sites,
                      #                                           included_cols,
                      #                                           linecolors),

                      connectSeparatedPoints = is_inst) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3c') %>%#
            dyAxis('y',
                   label = ylabel,
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 20,
                   rangePad = 10)

        if(show_pchem){

            rain_or_pchem_colors <- selection_color_match(
                sites_selected = paste0('P_', sites),
                sites_all = paste0('P_', included_cols[included_cols %in% sites]),
                colorvec = pchemcolors
            ) #minimally tested. might need to work with rainsites instead

            if(show_uncert){

                rain_names <- lapply(rainsites,
                                     function(x){
                                         c(paste0(x, '_errlo'),
                                           x,
                                           paste0(x, '_errhi'))
                                     })
            } else {
                rain_names <- as.list(rainsites)
            }

            for(i in 1:length(rainsites)){

                dg <- dySeries(dg,
                               name = rain_names[[i]],
                               color = rain_or_pchem_colors[i],
                               axis = 'y',
                               drawPoints = FALSE,
                               strokeWidth = 2,
                               pointSize = 2,
                               strokePattern = 'dashed')
            }
        }

        if(show_uncert){

            stream_names <- lapply(sites,
                                   function(x){
                                       c(paste0(x, '_errlo'),
                                         x,
                                         paste0(x, '_errhi'))
                                   })

            for(i in 1:length(sites)){

                dg <- dySeries(dg,
                               name = stream_names[[i]])
                               # color = rain_or_pchem_colors[i],
                               # axis = 'y',
                               # drawPoints = FALSE,
                               # strokeWidth = 2,
                               # pointSize = 2,
                               # strokePattern = 'dashed')
            }
        }

    } else {

        dg <- plot_empty_dygraph(dates,
                                 mainlab = colnames(alldata)[-1],
                                 maindiv = 'main3c',#
                                 plotgroup = 'nSiteNVar',
                                 ylab = ylabel,
                                 px_per_lab = 20)
    }

    return(dg)
})

output$GRAPH_QC3c <- renderPlot({

    print('QC3a')

    # show_qc <<- input$SHOW_QC3
    # sites <<- na.omit(isolate(input$SITES3[1:3]))
    # varC <<- isolate(input$VARS3[1])
    # # dmns <<- isolate(get_domains3())
    # conc_flux <<- isolate(input$CONC_FLUX3)
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # flux_unit <<- isolate(input$FLUX_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # show_uncert <<- isolate(input$SHOW_UNCERT3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)
    # datachem <<- dataChem()
    # dataq <<- dataQ()

    show_qc <- input$SHOW_QC3
    sites <- na.omit(isolate(input$SITES3[1:3]))
    varC <- isolate(input$VARS3[3])
    conc_flux <- isolate(input$CONC_FLUX3)
    conc_unit <- isolate(input$CONC_UNIT3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(input$DATES3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    datachem <- dataChem()
    dataq <- dataQ()

    if(reactive_vals$facet3a == 0 || ! show_qc) return()

    alldata <- datachem %>%
        select(c('datetime', 'site_name', ends_with(varC))) %>%
        inner_join(dataq,
                   by = c("datetime", "site_name"))

    n_val_cols <- sum(grepl('^val_', colnames(alldata)))
    if(n_val_cols < 2){
        return(plot_empty_qc(ylab = varC))
    }

    if(show_uncert){

        alldata <- alldata %>%
            mutate(across(starts_with('val_'),
                          .fns = list(errhi = ~(errors::drop_errors(.) +
                                                    errors::errors(.))))) %>%
            mutate(across(starts_with('val_') &
                              ! ends_with('_errhi'),
                          .fns = list(errlo = ~(errors::drop_errors(.) -
                                                    errors::errors(.))))) %>%
            mutate(across(starts_with('val_') &
                              ! ends_with(c('_errhi', '_errlo')),
                          errors::drop_errors))
    }

    cq <- ggplot(alldata,
                 aes(x = val_discharge,
                     y = !!sym(paste0('val_', varC)),
                     colour = site_name)) +
        geom_point(na.rm = TRUE,
                   size = 1)

    if(show_uncert){

        cq <- cq +
            geom_linerange(aes(ymin = !!sym(paste0('val_', varC, '_errlo')),
                               ymax = !!sym(paste0('val_', varC, '_errhi')))) +
            geom_errorbarh(aes(xmin = val_discharge_errlo,
                               xmax = val_discharge_errhi))
    }

    cq <- cq +
        scale_colour_manual(values = linecolors,
                            breaks = c(sites)) +
        ggthemes::theme_few() +
        scale_y_continuous(position = "right") +
        ylab(paste('Q', 'vs.', varC)) +
        theme(legend.position = 'none',
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=10),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_rect(fill = '#f5f5f5',
                                              color = '#f5f5f5'),
              plot.background = element_rect(fill = '#f5f5f5',
                                             color = '#f5f5f5'))

    return(cq)
})

output$GRAPH_Q3 <- renderDygraph({

    dataq <- dataQ()
    dates <- isolate(input$DATES3)
    sites <- na.omit(isolate(input$SITES3[1:3]))

    # dataq <<- dataQ()
    # dates <<- isolate(input$DATES3)
    # sites <<- na.omit(isolate(input$SITES3[1:3]))

    tryCatch(
        {
            dataq <- dataq %>%
                dplyr::rename_with(~ gsub('_discharge', '', .x)) %>%
                tidyr::pivot_wider(names_from = site_name,
                                   values_from = c('val', 'ms_status',
                                                   'ms_interp')) %>%
                dplyr::rename_with(~ gsub('val_', '', .x))
        },
        error = function(e) NULL
    )

    if(nrow(dataq)){

        colnms <- colnames(dataq)
        displabs <- colnms[colnms %in% sites]

        dydat <- xts(dataq[, displabs],
                     order.by = dataq$datetime,
                     tzone = lubridate::tz(dataq$datetime[1]))

        dimnames(dydat) <- list(NULL, displabs)

        dg <- dygraph(dydat,
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = TRUE,
                      drawPoints = FALSE,
                      fillGraph = TRUE,
                      strokeWidth = 1,
                      fillAlpha = 0.4,
                      retainDateWindow = TRUE,
                      colors = selection_color_match(sites,
                                                     displabs[displabs %in% sites],
                                                     linecolors),
                      labelsKMB = TRUE,
                      drawGapEdgePoints = TRUE) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'Q3') %>%
            dyAxis('y',
                   label = 'Q (L/s)',
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 10,
                   rangePad = 10)

    } else {

        dg <- plot_empty_dygraph(dates,
                                 plotgroup = 'nSiteNVar',
                                 ylab = 'Q (L/s)',
                                 px_per_lab = 10)
    }

    return(dg)
})

# #manage popout windows. currently buggy. disabled.
# observeEvent(input$EXPAND_PRECIP3, {
#     showModal(
#         modalDialog(title=NULL, size='l', id='modal3precip', easyClose=TRUE,
#             footer=NULL,
#             fluidRow(class='text-center',
#                 div(id='precip3DUPE'),
#                 dygraphOutput('GRAPH_PRECIP3aEXP'),
#                 br()
#             )
#         )
#     )
#     runjs("$('#precip3').clone().appendTo('#precip3DUPE')")
# })
#
# observeEvent(input$EXPAND_MAIN3a, {
#     showModal(
#         modalDialog(title=NULL, size='l', id='modal3a', easyClose=TRUE,
#             footer=NULL,
#             fluidRow(class='text-center',
#                 div(id='main3aDUPE'),
#                 dygraphOutput('GRAPH_MAIN3aEXP'),
#                 br()
#             )
#         )
#     )
#     runjs("$('#main3a').clone().appendTo('#main3aDUPE')")
# }) #must be duplicated for b and c if reinstating
#
# observeEvent(input$EXPAND_FLOW3, {
#     showModal(
#         modalDialog(title=NULL, size='l', id='Q3', easyClose=TRUE,
#             footer=NULL,
#             fluidRow(class='text-center',
#                 div(id='flow3DUPE'),
#                 dygraphOutput('GRAPH_FLOW3EXP'),
#                 br()
#             )
#         )
#     )
#     runjs("$('#Q3').clone().appendTo('#flow3DUPE')")
# })

# observe({
#     input$DEBUG
#     print(rlang::last_error())
#     print(traceback())
# })


