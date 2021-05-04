
#TODO: add a line like this to all renderers to attempt popout windows again
# output$GRAPH_PRECIP3a = output$GRAPH_PRECIP3aEXP = renderDygraph({

## govern showing/hiding of facets ####

reactive_vals = reactiveValues()
reactive_vals$facet3a = 0
reactive_vals$facet3b = 0
reactive_vals$facet3c = 0
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
get_domains3 <- eventReactive(eventExpr = input$DOMAINS3,
                              ignoreNULL = FALSE,
                              valueExpr = {

    domains <- input$DOMAINS3
    sites <- input$SITES3

    # domains <<- input$DOMAINS3
    # sites <<- input$SITES3

    reactive_vals$update_basedata <- reactive_vals$update_basedata + 1

    site_opts <- generate_dropdown_sitelist(domains)
    selection <- if(is.null(site_opts)) '' else sites

    updateSelectizeInput(session,
                         'SITES3',
                         choices = site_opts,
                         selected = selection,
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
    pflux <- read_combine_feathers('precip_flux_inst_scaled',
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
    # agg <<- input$AGG3
    # vars_ <<- isolate(input$VARS3)
    # dates <<- isolate(input$DATES3)
    # sites <<- isolate(input$SITES3)

    basedata <- load_basedata()
    agg <- input$AGG3
    vars_ <- isolate(input$VARS3)
    dates <- isolate(input$DATES3)
    sites <- isolate(input$SITES3)

    chemvars_display_subset <- filter_dropdown_varlist(basedata$chem)

    updateSelectizeInput(session = session,
                         inputId = 'VARS3',
                         choices = chemvars_display_subset,
                         selected = vars_)

    dtrng <- get_timeslider_extent(basedata, dates)

    if(length(sites) == 1){

        if(agg == 'Yearly'){
            selected_dtrng <- dtrng
        } else {
            vardates <- filter(basedata$chem, drop_var_prefix(var) %in% vars_)$datetime
            selected_dtrng <- as.Date(most_recent_year(range(vardates)))
        }

    } else {
        selected_dtrng <- dates
    }

    updateSliderInput(session = session,
                      inputId = 'DATES3',
                      min = dtrng[1],
                      max = dtrng[2],
                      value = selected_dtrng,
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
    # agg <- isolate(input$AGG3)
    # sites <- input$SITES3
    #time_scheme <- input$TIME_SCHEME3
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3
    enable_unitconvert <- init_vals$enable_unitconvert

    datachem <- if(conc_flux == 'Flux') basedata$flux else basedata$chem

    datachem <- filter_and_unprefix(d = datachem,
                                    selected_vars = vars_,
                                    selected_datebounds = dates,
                                    selected_prefixes = igsn,
                                    show_uncert = show_uncert,
                                    show_flagged = show_flagged,
                                    show_imputed = show_imputed)

    datachem <- ms_aggregate(d = datachem,
                             agg_selection = agg)

    if(nrow(datachem) == 0) return(datachem)

    datachem <- pivot_wider(datachem,
                            names_from = var,
                            values_from = c('val', 'ms_status', 'ms_interp'))

    datachem <- convert_portal_units(d = datachem,
                                     conversion_enabled = enable_unitconvert,
                                     conc_flux_selection = conc_flux,
                                     conc_unit = conc_unit,
                                     flux_unit = flux_unit)

    return(datachem)
})

dataVWC <- reactive({

    # basedata <<- load_basedata()
    # dates <<- isolate(input$DATES3)
    # timeSliderUpdate()
    # vars_ <<- input$VARS3
    # conc_flux <<- input$CONC_FLUX3
    # conc_unit <<- input$CONC_UNIT3
    # flux_unit <<- input$FLUX_UNIT3
    # agg <<- input$AGG3
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
    # agg <- isolate(input$AGG3)
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3
    enable_unitconvert <- init_vals$enable_unitconvert

    agg_unit <- ifelse(agg == 'Monthly', 'month', 'year') #this won't run if agg < monthly

    dataq <- filter_and_unprefix(d = basedata$Q,
                                 selected_vars = 'discharge',
                                 selected_datebounds = dates,
                                 # selected_agg = agg,
                                 selected_prefixes = igsn,
                                 show_uncert = show_uncert,
                                 show_flagged = show_flagged,
                                 show_imputed = show_imputed)
                                 # conc_or_flux = conc_flux)

    first_two_rows <- dataq[1:2,]
    if(first_two_rows$site_name[1] == first_two_rows$site_name[2]){
        dtdiff <- diff(first_two_rows$datetime)
        units(dtdiff) <- 'mins'
        if(as.numeric(dtdiff) == 15){
            stop('are we in high-res mode? if so, time to write pre-aggregated (by day) flux, P, and Q datasets to portal/data, so that vwc (and maybe other stuff) can be calculated without having to do a daily groupby
    the two VWC functions should only ever receive P, Q, and flux data in daily increments')
        }
    }

    #NOTE: the "traditional" way to compute VWC is:
    #VWC = sum(concentrations * volumes) / sum(volumes)
    #over a given period. Because we have discharge (a rate), rather than volume,
    #we could scale it to the sample interval and then use the equation
    #above. For simplicity, I use this instead, which works out the same:
    #VWC = mean(concentrations * discharges) / mean(discharges)
    #Note also that we're starting from flux here, for computational
    #efficiency, so the "concentrations * volumes" part is already computed,
    #and it's a rate too. That means the modified equation is:
    #VWC = mean(fluxes) / mean(discharges). There are two minor introductions of
    #error. The first is that we don't omit discharge values from the denominator
    #that lack a corresponding flux value (this is expensive, and not doing it
    #should rarely skew the results). The second is that a "month" is considered
    #30 days here, which only affects coverage filtering.

    if(agg == 'Monthly'){
        agg_unit <- 'month'
        period_complete_n <- 30 #for filtering below. see justification in comment below
    } else if(agg == 'Yearly'){
        agg_unit <- 'year'
        period_complete_n <- 365
    } else {
        stop('this should never run when agg < monthly')
    }

    dataflux <- filter_and_unprefix(d = basedata$flux,
                                    selected_vars = vars_,
                                    selected_datebounds = dates,
                                    selected_prefixes = igsn,
                                    show_uncert = show_uncert,
                                    show_flagged = show_flagged,
                                    show_imputed = show_imputed)

    period_mean_Q <- dataq %>%
        mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
        group_by(site_name, datetime) %>%
        #divisor of vwc is MEAN(Q) (unlike Pvwc), because Q is expressed as a rate (L/s)
        summarize(val = mean(val, na.rm = TRUE),
                  ms_status = numeric_any(ms_status),
                  ms_interp = numeric_any(ms_interp),
                  .groups = 'drop')

    datavwc <- dataflux %>%
        mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
        group_by(site_name, var, datetime) %>%
        summarize(nday = sum(! is.na(val)),
                  #vwc dividend is a mean, not a sum, because flux is also a rate
                  val = mean(val, na.rm = TRUE),
                  ms_status = numeric_any(ms_status),
                  ms_interp = numeric_any(ms_interp),
                  .groups = 'drop') %>%
        filter(nday > period_complete_n * 0.1) %>%
        left_join(period_mean_Q,
                  by = c('datetime', 'site_name'),
                  suffix = c('.flux', '.Q')) %>%
        left_join(site_data[c('site_name', 'ws_area_ha')],
                  by = 'site_name') %>%
        #     mg/L = kg/ha/d  /  L/s  *  ha          ...
        mutate(val = val.flux / val.Q * ws_area_ha * 1e6 / 86400,
               ms_status = numeric_any_v(ms_status.flux, ms_status.Q),
               ms_interp = numeric_any_v(ms_interp.flux, ms_interp.Q)) %>%
        select(datetime, site_name, var, val, ms_status, ms_interp)

    if(nrow(datavwc) == 0) return(datavwc)

    datavwc <- pivot_wider(datavwc,
                           names_from = var,
                           values_from = c('val', 'ms_status', 'ms_interp'))

    datavwc <- convert_portal_units(d = datavwc,
                                    conversion_enabled = enable_unitconvert,
                                    conc_flux_selection = conc_flux,
                                    conc_unit = conc_unit,
                                    flux_unit = flux_unit)

    return(datavwc)
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
    # agg <- isolate(input$AGG3)
    # sites <- input$SITES3
    #time_scheme <- input$TIME_SCHEME3
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3
    enable_unitconvert <- init_vals$enable_unitconvert

    dataPchem <- if(conc_flux == 'Flux') basedata$pflux else basedata$pchem

    dataPchem <- filter_and_unprefix(d = dataPchem,
                                     selected_vars = vars_,
                                     selected_datebounds = dates,
                                     selected_prefixes = igsn,
                                     show_uncert = show_uncert,
                                     show_flagged = show_flagged,
                                     show_imputed = show_imputed)

    dataPchem <- ms_aggregate(d = dataPchem,
                              agg_selection = agg)

    if(nrow(dataPchem) == 0) return(dataPchem)

    dataPchem <- pivot_wider(dataPchem,
                          names_from = var,
                          values_from = c('val', 'ms_status', 'ms_interp'))

    dataPchem <- convert_portal_units(d = dataPchem,
                                      conversion_enabled = enable_unitconvert,
                                      conc_flux_selection = conc_flux,
                                      conc_unit = conc_unit,
                                      flux_unit = flux_unit)


    return(dataPchem)
})

dataPVWC <- reactive({

    # basedata <<- load_basedata()
    # dates <<- isolate(input$DATES3)
    # timeSliderUpdate()
    # vars_ <<- input$VARS3
    # conc_flux <<- input$CONC_FLUX3
    # conc_unit <<- input$CONC_UNIT3
    # flux_unit <<- input$FLUX_UNIT3
    # agg <<- input$AGG3
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
    # agg <- isolate(input$AGG3)
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3
    enable_unitconvert <- init_vals$enable_unitconvert

    #NOTE: the "traditional" way to compute VWC is:
    #VWC = sum(concentrations * volumes) / sum(volumes)
    #over a given period. We're starting from flux here, for computational
    #efficiency, so the "concentrations * volumes" part is already computed,
    #and it's a rate, which means the modified equation is:
    #VWC = mean(fluxes) * ndays / sum(volumes). There are two minor introductions of
    #error. The first is that we don't omit precip volumes from the denominator
    #that lack a corresponding flux value (this is expensive, and not doing it
    #should rarely skew the results). The second is that a "month" is considered
    #30 days here. So the mean daily flux for Feb will get multiplied by 30
    #instead of 28, etc. That avoids a slow dplyr conditional.

    if(agg == 'Monthly'){
        agg_unit <- 'month'
        period_complete_n <- 30 #for filtering below. not worth using actual n days per month
    } else if(agg == 'Yearly'){
        agg_unit <- 'year'
        period_complete_n <- 365
    } else {
        stop('this should never run when agg < monthly')
    }

    datap <- filter_and_unprefix(d = basedata$P,
                                 selected_vars = 'precipitation',
                                 selected_datebounds = dates,
                                 selected_prefixes = igsn,
                                 show_uncert = show_uncert,
                                 show_flagged = show_flagged,
                                 show_imputed = show_imputed)

    dataPflux <- filter_and_unprefix(d = basedata$pflux,
                                     selected_vars = vars_,
                                     selected_datebounds = dates,
                                     selected_prefixes = igsn,
                                     show_uncert = show_uncert,
                                     show_flagged = show_flagged,
                                     show_imputed = show_imputed)

    period_sum_P <- datap %>%
        mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
        group_by(site_name, datetime) %>%
        #divisor of Pvwc is SUM(P) (unlike vwc), because P is expressed as a length (mm)
        summarize(val = sum(val, na.rm = TRUE),
                  ms_status = numeric_any(ms_status),
                  ms_interp = numeric_any(ms_interp),
                  .groups = 'drop')

    dataPvwc <- dataPflux %>%
        mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
        group_by(site_name, var, datetime) %>%
        summarize(nday = sum(! is.na(val)),
                  #Pvwc dividend is a mean, not a sum, because flux is a rate
                  val = mean(val, na.rm = TRUE),
                  ms_status = numeric_any(ms_status),
                  ms_interp = numeric_any(ms_interp),
                  .groups = 'drop') %>%
        filter(nday > period_complete_n * 0.5) %>%
        left_join(period_sum_P,
                  by = c('datetime', 'site_name'),
                  suffix = c('.flux', '.P')) %>%
             #mg/L = kg/ha/d  /  mm (~ per month or per year) * 1e6 / 1e4 * d
        mutate(val = val.flux / val.P * 100 * period_complete_n,
               ms_status = numeric_any_v(ms_status.flux, ms_status.P),
               ms_interp = numeric_any_v(ms_interp.flux, ms_interp.P)) %>%
        select(datetime, site_name, var, val, ms_status, ms_interp)

    if(nrow(dataPvwc) == 0) return(dataPvwc)

    dataPvwc <- pivot_wider(dataPvwc,
                            names_from = var,
                            values_from = c('val', 'ms_status', 'ms_interp'))

    dataPvwc <- convert_portal_units(d = dataPvwc,
                                     conversion_enabled = enable_unitconvert,
                                     conc_flux_selection = conc_flux,
                                     conc_unit = conc_unit,
                                     flux_unit = flux_unit)

    return(dataPvwc)
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
    # agg <- isolate(input$AGG3)
    conc_flux <- input$CONC_FLUX3
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3

    dataP <- filter_and_unprefix(d = basedata$P,
                                 selected_vars = 'precipitation',
                                 selected_datebounds = dates,
                                 selected_prefixes = igsn,
                                 show_uncert = show_uncert,
                                 show_flagged = show_flagged,
                                 show_imputed = show_imputed)

    dataP <- ms_aggregate(d = dataP,
                          agg_selection = agg)

    if(nrow(dataP) == 0) return(dataP)

    dataP <- pivot_wider(dataP,
                         names_from = var,
                         values_from = c('val', 'ms_status', 'ms_interp'))

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
    # conc_flux <<- input$CONC_FLUX3

    basedata <- load_basedata()
    dates <- isolate(input$DATES3)
    timeSliderUpdate()
    agg <- input$AGG3
    # agg <- isolate(input$AGG3)
    igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
    show_uncert <- input$SHOW_UNCERT3
    show_flagged <- input$FLAGS3
    show_imputed <- input$INTERP3
    conc_flux <- input$CONC_FLUX3

    dataQ <- filter_and_unprefix(d = basedata$Q,
                                 selected_vars = 'discharge',
                                 selected_datebounds = dates,
                                 selected_prefixes = igsn,
                                 show_uncert = show_uncert,
                                 show_flagged = show_flagged,
                                 show_imputed = show_imputed)

    dataQ <- ms_aggregate(d = dataQ,
                          agg_selection = agg)

    if(nrow(dataQ) == 0) return(dataQ)

    dataQ <- pivot_wider(dataQ,
                         names_from = var,
                         values_from = c('val', 'ms_status', 'ms_interp'))

    return(dataQ)
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

    # if(reactive_vals$precip3 == 0) return()

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

    if(nrow(dataP)){

        dataP <- select(dataP,
                        datetime, any_of(sites)) #preserve order

        colnms <- colnames(dataP)
        displabs <- colnms[! colnms == 'datetime']

        dydat <- xts(dataP[, displabs],
                     order.by = dataP$datetime,
                     tzone = lubridate::tz(dataP$datetime[1]))

        dimnames(dydat) <- list(NULL, displabs)
        # dimnames(dydat) <- list(NULL, site)

        ymax <- max(dydat,
                    na.rm = TRUE)

        # watermark_specs <- get_watermark_specs(dydat = dydat,
        #                                        displabs = displabs)

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
                      colors = selection_color_match(
                          sites_selected = sites,
                          sites_all = displabs[displabs %in% sites],
                          colorvec = pchemcolors
                      ),
                      drawGapEdgePoints = TRUE

                      # #if showing points, use these (needs work)
                      # drawPoints = TRUE,
                      # strokeWidth = 0.01,
                      # pointSize = 1,
                      # strokeBorderWidth = 1,
                      # colors = 'white',
                      # fillAlpha = 0.4,
                      # strokeBorderColors = selection_color_match(
                      #     sites_selected = sites,
                      #     sites_all = displabs,
                      #     colorvec = linecolors
                      # ),

            ) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'P3') %>%
            dyAxis('y',
                   label = 'P (mm)',
                   valueRange = c(ymax + ymax * 0.1,
                                  0),
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 10,
                   rangePad = 10)
            # # dyCSS('~/git/macrosheds/portal/www/dygraph.css') %>%
            # dyAnnotation(x = watermark_specs$dt,
            #              text = 'macrosheds.org',
            #              attachAtBottom = TRUE,
            #              # cssClass = 'dygraph-watermark',
            #              tickHeight = 0,
            #              width = 0,
            #              series = watermark_specs$series,
            #              tooltip = '')

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
                                 px_per_lab = 10) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'P3')
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
    # dmns <<- get_domains3()

    sites <- na.omit(isolate(input$SITES3))
    varA <- isolate(input$VARS3[1])#
    conc_flux <- isolate(input$CONC_FLUX3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    conc_unit <- isolate(input$CONC_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(input$DATES3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    dmns <- get_domains3()

    if(reactive_vals$facet3a == 0) return()#
    print('mainA')

    if(conc_flux == 'VWC'){
        streamdata <- dataVWC()
    } else {
        streamdata <- dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata <- dataPVWC()
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

    ylabel <- get_ylab(v = varA,#
                       conc_flux = conc_flux,
                       conc_unit = conc_unit,
                       flux_unit = flux_unit)

    if(nrow(alldata)){

        streamsites_rainsites <- c(sites, paste0('P_', sites))
        colnms <- colnames(alldata)
        included_cols <- streamsites_rainsites[streamsites_rainsites %in% colnms]
        alldata <- select(alldata,
                          datetime, any_of(streamsites_rainsites)) #preserve order

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

        dydat <- xts(select(alldata, -datetime),
                     order.by = alldata$datetime,
                     tzone = lubridate::tz(alldata$datetime[1]))

        is_inst <- ifelse(agg == 'Instantaneous',
                          TRUE,
                          FALSE)

        dg <- dygraph(dydat,#[,1:2],
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = TRUE,
                      retainDateWindow = TRUE,

                      #if not showing all points, use these specifications.
                      drawPoints = FALSE,
                      colors = selection_color_match( #pchem NAs required here
                          sites_selected = sites,
                          sites_all = included_cols,
                          sites_missing = sites[! sites %in% colnms],
                          colorvec = linecolors),
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
                      # strokeBorderColor = selection_color_match(
                      #     sites_selected = sites,
                      #     sites_all = included_cols,
                      #     colorvec = linecolors
                      # ),

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
            # # dyCSS('~/git/macrosheds/portal/www/dygraph.css') %>%
            # # dyCSS('www/dygraph.css') %>%
            # dyAnnotation(x = watermark_specs$dt,
            #              text = 'macrosheds.org',
            #              attachAtBottom = TRUE,
            #              # cssClass = 'dygraphDefaultAnnotation',
            #              # cssClass = 'dygraph-watermark',
            #              tickHeight = 0,
            #              width = 0,
            #              series = watermark_specs$series,
            #              tooltip = '')

        rainsites <- paste0('P_', sites)

        if(show_pchem && any(rainsites %in% colnms)){

            rain_or_pchem_colors <- selection_color_match(
                sites_selected = rainsites,
                sites_all = rainsites,
                sites_missing = rainsites[! rainsites %in% colnms],
                colorvec = pchemcolors
            )

            rainsites <- rainsites[rainsites %in% colnms]

            if(show_uncert){

                rainsites <- lapply(rainsites,
                                    function(x){
                                        c(paste0(x, '_errlo'),
                                          x,
                                          paste0(x, '_errhi'))
                                    })
            } else {
                rainsites <- as.list(rainsites)
            }

            for(i in 1:length(rainsites)){

                dg <- dySeries(dg,
                               name = rainsites[[i]],
                               color = rain_or_pchem_colors[i],
                               axis = 'y',
                               drawPoints = FALSE,
                               strokeWidth = 2,
                               pointSize = 2,
                               strokePattern = 'dashed')
            }
        }

        if(show_uncert){

            sites <- sites[sites %in% colnms]

            stream_names <- lapply(sites,
                                   function(x){
                                       c(paste0(x, '_errlo'),
                                         x,
                                         paste0(x, '_errhi'))
                                   })

            for(i in 1:length(sites)){

                dg <- dySeries(dg,
                               name = stream_names[[i]])
            }
        }

    } else {

        dg <- plot_empty_dygraph(dates,
                                 mainlab = colnames(alldata)[-1],
                                 maindiv = 'main3a',#
                                 plotgroup = 'nSiteNVar',
                                 ylab = ylabel,
                                 px_per_lab = 20) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3a')
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
    # datachem <<- if(conc_flux == 'VWC') dataVWC() else dataChem()
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
    dataq <- dataQ()
    datachem <- if(conc_flux == 'VWC') dataVWC() else dataChem()

    if(reactive_vals$facet3a == 0 || ! show_qc) return()

    alldata <- datachem %>%
        select(c('datetime', 'site_name', ends_with(varA))) %>%
        inner_join(dataq,
                   by = c("datetime", "site_name"))

    n_val_cols <- sum(grepl('^val_', colnames(alldata)))
    if(n_val_cols < 2){
        return(plot_empty_qc(ylab = varA))
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
                     y = !!sym(paste0('val_', varA)),
                     colour = site_name)) +
        geom_point(na.rm = TRUE,
                   size = 1)

    if(show_uncert){

        cq <- cq +
            geom_linerange(aes(ymin = !!sym(paste0('val_', varA, '_errlo')),
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
        streamdata <- dataVWC()
    } else {
        streamdata <- dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata <- dataPVWC()
        } else {
            raindata <- dataPchem()
        }

    } else {
        raindata <- tibble()
    }

    # streamdata <<- streamdata
    # raindata <<- raindata
    # print(head(streamdata))

    alldata <- pad_widen_join(v = varB,
                              sites = sites,
                              dates = dates,
                              streamdata = streamdata,
                              raindata = raindata,
                              show_input_concentration = show_pchem)

    ylabel <- get_ylab(v = varB,
                       conc_flux = conc_flux,
                       conc_unit = conc_unit,
                       flux_unit = flux_unit)

    if(nrow(alldata)){

        streamsites_rainsites <- c(sites, paste0('P_', sites))
        colnms <- colnames(alldata)
        included_cols <- streamsites_rainsites[streamsites_rainsites %in% colnms]
        alldata <- select(alldata,
                          datetime, any_of(streamsites_rainsites)) #preserve order

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

        dydat <- xts(select(alldata, -datetime),
                     order.by = alldata$datetime,
                     tzone = lubridate::tz(alldata$datetime[1]))

        is_inst <- ifelse(agg == 'Instantaneous',
                          TRUE,
                          FALSE)

        dg <- dygraph(dydat,,
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = TRUE,
                      retainDateWindow = TRUE,

                      drawPoints = FALSE,
                      colors = selection_color_match( #pchem NAs required here
                          sites_selected = sites,
                          sites_all = included_cols,
                          sites_missing = sites[! sites %in% colnms],
                          colorvec = linecolors),
                      strokeWidth = 2,
                      pointSize = 2,
                      drawGapEdgePoints = TRUE,
                      labelsKMB = TRUE,

                      connectSeparatedPoints = is_inst) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3b') %>%
            dyAxis('y',
                   label = ylabel,
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 20,
                   rangePad = 10)

        rainsites <- paste0('P_', sites)

        if(show_pchem && any(rainsites %in% colnms)){

            rain_or_pchem_colors <- selection_color_match(
                sites_selected = rainsites,
                sites_all = rainsites,
                sites_missing = rainsites[! rainsites %in% colnms],
                colorvec = pchemcolors
            )

            rainsites <- rainsites[rainsites %in% colnms]

            if(show_uncert){

                rainsites <- lapply(rainsites,
                                    function(x){
                                        c(paste0(x, '_errlo'),
                                          x,
                                          paste0(x, '_errhi'))
                                    })
            } else {
                rainsites <- as.list(rainsites)
            }

            for(i in 1:length(rainsites)){

                dg <- dySeries(dg,
                               name = rainsites[[i]],
                               color = rain_or_pchem_colors[i],
                               axis = 'y',
                               drawPoints = FALSE,
                               strokeWidth = 2,
                               pointSize = 2,
                               strokePattern = 'dashed')
            }
        }

        if(show_uncert){

            sites <- sites[sites %in% colnms]

            stream_names <- lapply(sites,
                                   function(x){
                                       c(paste0(x, '_errlo'),
                                         x,
                                         paste0(x, '_errhi'))
                                   })

            for(i in 1:length(sites)){
                dg <- dySeries(dg,
                               name = stream_names[[i]])
            }
        }

    } else {

        dg <- plot_empty_dygraph(dates,
                                 mainlab = colnames(alldata)[-1],
                                 maindiv = 'main3b',
                                 plotgroup = 'nSiteNVar',
                                 ylab = ylabel,
                                 px_per_lab = 20) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3b')
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
    # datachem <<- if(conc_flux == 'VWC') dataVWC() else dataChem()
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
    dataq <- dataQ()
    datachem <- if(conc_flux == 'VWC') dataVWC() else dataChem()

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
        streamdata <- dataVWC()
    } else {
        streamdata <- dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata <- dataPVWC()
        } else {
            raindata <- dataPchem()
        }

    } else {
        raindata <- tibble()
    }

    # streamdata <<- streamdata
    # raindata <<- raindata
    # print(head(streamdata))

    alldata <- pad_widen_join(v = varC,
                              sites = sites,
                              dates = dates,
                              streamdata = streamdata,
                              raindata = raindata,
                              show_input_concentration = show_pchem)

    ylabel <- get_ylab(v = varC,
                       conc_flux = conc_flux,
                       conc_unit = conc_unit,
                       flux_unit = flux_unit)

    if(nrow(alldata)){

        streamsites_rainsites <- c(sites, paste0('P_', sites))
        colnms <- colnames(alldata)
        included_cols <- streamsites_rainsites[streamsites_rainsites %in% colnms]
        alldata <- select(alldata,
                          datetime, any_of(streamsites_rainsites)) #preserve order

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

        dydat <- xts(select(alldata, -datetime),
                     order.by = alldata$datetime,
                     tzone = lubridate::tz(alldata$datetime[1]))

        is_inst <- ifelse(agg == 'Instantaneous',
                          TRUE,
                          FALSE)

        dg <- dygraph(dydat,,
                      group = 'nSiteNVar') %>%
            dyOptions(useDataTimezone = TRUE,
                      retainDateWindow = TRUE,

                      drawPoints = FALSE,
                      colors = selection_color_match( #pchem NAs required here
                          sites_selected = sites,
                          sites_all = included_cols,
                          sites_missing = sites[! sites %in% colnms],
                          colorvec = linecolors),
                      strokeWidth = 2,
                      pointSize = 2,
                      drawGapEdgePoints = TRUE,
                      labelsKMB = TRUE,

                      connectSeparatedPoints = is_inst) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3c') %>%
            dyAxis('y',
                   label = ylabel,
                   labelWidth = 16,
                   labelHeight = 10,
                   pixelsPerLabel = 20,
                   rangePad = 10)

        rainsites <- paste0('P_', sites)

        if(show_pchem && any(rainsites %in% colnms)){

            rain_or_pchem_colors <- selection_color_match(
                sites_selected = rainsites,
                sites_all = rainsites,
                sites_missing = rainsites[! rainsites %in% colnms],
                colorvec = pchemcolors
            )

            rainsites <- rainsites[rainsites %in% colnms]

            if(show_uncert){

                rainsites <- lapply(rainsites,
                                    function(x){
                                        c(paste0(x, '_errlo'),
                                          x,
                                          paste0(x, '_errhi'))
                                    })
            } else {
                rainsites <- as.list(rainsites)
            }

            for(i in 1:length(rainsites)){

                dg <- dySeries(dg,
                               name = rainsites[[i]],
                               color = rain_or_pchem_colors[i],
                               axis = 'y',
                               drawPoints = FALSE,
                               strokeWidth = 2,
                               pointSize = 2,
                               strokePattern = 'dashed')
            }
        }

        if(show_uncert){

            sites <- sites[sites %in% colnms]

            stream_names <- lapply(sites,
                                   function(x){
                                       c(paste0(x, '_errlo'),
                                         x,
                                         paste0(x, '_errhi'))
                                   })

            for(i in 1:length(sites)){
                dg <- dySeries(dg,
                               name = stream_names[[i]])
            }
        }

    } else {

        dg <- plot_empty_dygraph(dates,
                                 mainlab = colnames(alldata)[-1],
                                 maindiv = 'main3c',
                                 plotgroup = 'nSiteNVar',
                                 ylab = ylabel,
                                 px_per_lab = 20) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'main3c')
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
    # datachem <<- if(conc_flux == 'VWC') dataVWC() else dataChem()
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
    dataq <- dataQ()
    datachem <- if(conc_flux == 'VWC') dataVWC() else dataChem()

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

        # d0 = filter(dataq, datetime <= as.POSIXct('2001-10-01', tz='UTC'))
        # d1 = tibble(datetime = c(as.POSIXct('2001-10-02', tz='UTC'),
        #                          as.POSIXct('2002-07-09', tz='UTC')),
        #             C2 = c(NA, NA),
        #             ms_status_C2 = c(0, 0),
        #             ms_interp_C2 = c(0, 0))
        # d2 = filter(dataq, datetime >= as.POSIXct('2002-07-10', tz='UTC'))
        # dataq = rbind(d0, d1, d2)

        dataq <- select(dataq,
                        datetime, any_of(sites)) #preserve order

        colnms <- colnames(dataq)
        displabs <- colnms[! colnms == 'datetime']

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
                      colors = selection_color_match(
                          sites_selected = sites,
                          sites_all = displabs[displabs %in% sites],
                          colorvec = linecolors
                      ),
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
                                 px_per_lab = 10) %>%
            dyLegend(show = 'always',
                     labelsSeparateLines = FALSE,
                     labelsDiv = 'Q3')
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


