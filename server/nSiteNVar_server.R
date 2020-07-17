
#govern showing/hiding of facets ####

reactive_vals = reactiveValues()
reactive_vals$facet3a = 0
reactive_vals$facet3b = 0
reactive_vals$facet3c = 0
reactive_vals$facet3aP = 0
reactive_vals$facet3bP = 0
reactive_vals$facet3cP = 0
reactive_vals$update_basedata = 0

#main facets
observeEvent({
    if(
        ! is.null(input$SITES3) &&
        ! is.null(get_domains3()) &&
        ! is.null(input$CONC_FLUX3) &&
        ! is.null(input$FLUX_UNIT3) &&
        ! is.null(input$CONC_UNIT3) &&
        ! is.null(input$SHOW_PCHEM3) &&
        ! is.null(input$AGG3) &&
        ! is.null(input$DATES3) &&
        length(input$VARS3) == 1
    ){ TRUE } else return()
}, {
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
        ! is.null(input$DATES3) &&
        length(input$VARS3) == 2
    ){ TRUE } else return()
    # if(length(input$VARS3) == 2){
    #     TRUE
    # } else return()
}, {
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
        ! is.null(input$DATES3) &&
        length(input$VARS3) == 3
    ){ TRUE } else return()
    # if(length(input$VARS3) == 3){
    #     TRUE
    # } else return()
}, {
    reactive_vals$facet3a = reactive_vals$facet3a + 1
    reactive_vals$facet3b = reactive_vals$facet3b + 1
    reactive_vals$facet3c = reactive_vals$facet3c + 1
})

#precip facets
observeEvent({
    if(length(input$DOMAINS3) >= 1){ TRUE } else return()
}, {
    reactive_vals$facet3aP = reactive_vals$facet3aP + 1
})

observeEvent({
    if(length(input$DOMAINS3) >= 2){ TRUE } else return()
}, {
    reactive_vals$facet3aP = reactive_vals$facet3aP + 1
    reactive_vals$facet3bP = reactive_vals$facet3bP + 1
})

observeEvent({
    if(length(input$DOMAINS3) == 3){ TRUE } else return()
}, {
    reactive_vals$facet3aP = reactive_vals$facet3aP + 1
    reactive_vals$facet3bP = reactive_vals$facet3bP + 1
    reactive_vals$facet3cP = reactive_vals$facet3cP + 1
})

#reactivity flow control ####

#when domain(s) change, site options and basedata change, but not site selections
get_domains3 = eventReactive(input$DOMAINS3, {

    domains = input$DOMAINS3

    reactive_vals$update_basedata = reactive_vals$update_basedata + 1

    updateSelectizeInput(session, 'SITES3',
        choices=generate_dropdown_sitelist(domains),
        selected=input$SITES3,
        options=list(maxItems=3))

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
load_basedata = eventReactive({
    input$SITES3
    reactive_vals$update_basedata
}, {

    dmns = get_domains3()

    if(is.null(dmns)){ #for empty domain dropdown
        dmns = init_vals$recent_domain
        sites = default_sites_by_domain[[dmns[1]]] #overkill?
    } else {
        sites = input$SITES3
    }

    #NOTE: read_combine_feathers will have to be modified once rain data are
    #no longer aggregated for each domain
    pchem = read_combine_feathers('pchem', dmns=dmns)
    P = read_combine_feathers('precip', dmns=dmns)
    chem = read_combine_feathers('chemistry', dmns=dmns, sites=sites)
    flux = read_combine_feathers('flux', dmns=dmns, sites=sites)
    Q = read_combine_feathers('discharge', dmns=dmns, sites=sites)

    init_vals$recent_domain = dmns[1] #needed?

    basedata = list(chem=chem, P=P, Q=Q, pchem=pchem, flux=flux)
    return(basedata) })

#when basedata changes, variable list and time slider change, but not selections
observe({

    basedata = load_basedata()
    vars_ = isolate(input$VARS3)
    dates = isolate(input$DATES3)

    chemvars_display_subset = filter_dropdown_varlist(basedata$chem)
    updateSelectizeInput(session, 'VARS3',
        choices=chemvars_display_subset, selected=vars_)

    dtrng = get_timeslider_extent(basedata, dates)

    updateSliderInput(session, 'DATE3', min=dtrng[1], max=dtrng[2],
        value=dates, timeFormat='%b %Y')
})

#if variables(s), aggregation, units, site, or time window change, re-filter datasets
dataChem = reactive({

    dates = input$DATES3
    vars_ = input$VARS3
    conc_flux = input$CONC_FLUX3
    conc_unit = input$CONC_UNIT3
    flux_unit = input$FLUX_UNIT3
    agg = input$AGG3
    sites = input$SITES3
    basedata = load_basedata()

    datachem = if(conc_flux == 'Flux') basedata$flux else basedata$chem

    if(nrow(datachem) == 0) return(datachem)

    datachem = datachem %>%
        filter(datetime >= dates[1], datetime <= dates[2]) %>%
        select(one_of('datetime', 'site_name', vars_))

    if(nrow(datachem) == 0) return(datachem)

    datachem = pad_ts(datachem, vars=vars_, datebounds=dates)
    datachem = ms_aggregate(datachem, agg, which_dataset='chem', conc_flux)

    if(init_vals$enable_unitconvert){
        if(conc_flux %in% c('Concentration', 'VWC')){
            datachem = convert_conc_units(datachem, desired_unit=conc_unit)
        } else if(conc_flux == 'Flux'){
            datachem = convert_flux_units(datachem, desired_unit=flux_unit)
        }
    }

    return(datachem)
})

dataPchem = reactive({

    # dates <<- input$DATES3
    # vars_ <<- input$VARS3
    # conc_flux <<- input$CONC_FLUX3
    # conc_unit <<- input$CONC_UNIT3
    # agg <<- input$AGG3
    # dmns <<- get_domains3()
    # basedata <<- load_basedata()
    dates = input$DATES3
    vars_ = input$VARS3
    conc_flux = input$CONC_FLUX3
    conc_unit = input$CONC_UNIT3
    agg = input$AGG3
    dmns = get_domains3()
    basedata = load_basedata()

    datapchem = basedata$pchem

    if(nrow(datapchem) == 0) return(datapchem)

    datapchem = datapchem %>%
        filter(datetime >= dates[1], datetime <= dates[2]) %>%
        select(datetime, domain, one_of(vars_))

    if(nrow(datapchem) == 0) return(datapchem)

    datapchem = pad_ts(datapchem, vars=vars_, datebounds=dates)
    datapchem = ms_aggregate(datapchem, agg, which_dataset='pchem', conc_flux)

    if(init_vals$enable_unitconvert){
        datapchem = convert_conc_units(datapchem, desired_unit=conc_unit)
    }#temporary? modify the above if rain flux and rain units become modifiable

    #format domain name for display as a "site name"
    datapchem = datapchem %>%
        mutate(domain = paste(domain, 'pchem')) %>%
        rename(site_name=domain)

    return(datapchem)
})

dataPrecip = reactive({

    dates = input$DATES3
    agg = input$AGG3
    basedata = load_basedata()
    dmns = isolate(get_domains3())

    dataprecip = basedata$P

    if(nrow(dataprecip) == 0) return(dataprecip)

    dataprecip = dataprecip %>%
        filter(datetime >= dates[1], datetime <= dates[2]) %>%
        select(one_of('datetime', 'domain', 'precip'))

    if(nrow(dataprecip) == 0) return(dataprecip)

    dataprecip = pad_ts(dataprecip, vars='precip', datebounds=dates)
    dataprecip = ms_aggregate(dataprecip, agg, which_dataset='p')

    dataprecip = dataprecip %>%
        group_by(datetime, domain) %>%
        summarize(sumPrecip=sum(precip, na.rm=TRUE),
            medianPrecip=median(precip, na.rm=TRUE)) %>%
        ungroup()

    #append rows for selected domains with no data
    missing_domains = dmns[! dmns %in% unique(dataprecip$domain)]
    if(length(missing_domains)){
        for(m in missing_domains){
            fake_date = lubridate::force_tz(as.POSIXct(dates[2]), tzone='UTC')
            dataprecip = bind_rows(dataprecip,
                    tibble(datetime=fake_date, domain=m, precip=as.numeric(NA)))
        }
    }

    return(dataprecip)
})

dataQ = reactive({

    dates <<- input$DATES3
    sites <<- input$SITES3
    agg <<- input$AGG3
    basedata <<- load_basedata()
    # dates = input$DATES3
    # sites = input$SITES3
    # agg = input$AGG3
    # basedata = load_basedata()

    # dataq = basedata$Q
    dataq <<- basedata$Q %>%
        rename(discharge = Q)

    if(nrow(dataq) == 0) return(dataq)

    dataq = dataq %>%
        filter(datetime > dates[1], datetime < dates[2]) %>%
        select(datetime, site_name, discharge)

    if(nrow(dataq) == 0) return(dataq)

    dataq = pad_ts(dataq, vars='discharge', datebounds=dates)
    dataq = ms_aggregate(dataq, agg, which_dataset='q')

    if(agg == 'Instantaneous'){ #revisit this. needed?
        dataq = dataq %>%
            group_by(datetime, site_name) %>%
            summarise(discharge=max(discharge, na.rm=TRUE)) %>%
            ungroup()
    }

    return(dataq)
})

#post-filtering data modifications ####
#these should only update when prerequisite reactive data (above) updates, so
#all user inputs should be isolated

#calculate VWC (volume weighted concentration) from chem and q
#only possible at monthly and yearly agg. conditionals controlled by ui
volWeightedChem3 = reactive({

    datachem = dataChem()
    dataq = dataQ()
    agg_input = isolate(input$AGG3)

    samplevel = datachem %>%
        left_join(dataq, by=c('datetime', 'site_name')) %>%
        mutate_at(vars(-datetime, -site_name, -discharge), ~(. * discharge))

    if(agg_input == 'Monthly'){

        samplevel = samplevel %>%
            mutate(year=lubridate::year(datetime))

        agglevel = samplevel %>%
            select(site_name, year, discharge) %>%
            group_by(year, site_name) %>%
            summarize(Qsum=sum(discharge, na.rm=TRUE)) %>%
            ungroup()

        volWeightedConc = samplevel %>%
            select(-discharge) %>%
            left_join(agglevel, by=c('year', 'site_name')) %>%
            mutate_at(vars(-datetime, -site_name, -year, -Qsum),
                ~(. / Qsum)) %>%
            select(-Qsum, -year)

    } else if(agg_input == 'Yearly'){

        agglevel = samplevel %>%
            group_by(site_name) %>%
            summarize(Qsum=sum(discharge, na.rm=TRUE))

        volWeightedConc = samplevel %>%
            select(-discharge) %>%
            left_join(agglevel, by='site_name') %>%
            mutate_at(vars(-datetime, -site_name, -Qsum), ~(. / Qsum)) %>%
            select(-Qsum)
    }

    return(volWeightedConc)
})

#calculate VWC (volume weighted concentration) from pchem and p
#only possible at monthly and yearly agg. conditionals controlled by ui
volWeightedPchem3 = reactive({

    # samplevel <<- dataPchem()
    # dataprecip <<- dataPrecip()
    # agg_input <<- isolate(input$AGG3)
    # sites <<- isolate(input$SITES3)
    # vars_ <<- isolate(input$VARS3)
    samplevel = dataPchem()
    dataprecip = dataPrecip()
    agg_input = isolate(input$AGG3)
    sites = isolate(input$SITES3)
    vars_ = isolate(input$VARS3)

    #TEMPORARY SHORT-CIRCUIT UNTIL WE WORK OUT PRECIP INTERPOLATION
    #THE CODE BELOW ALSO HASN'T CHANGED SINCE IT WAS DOMAIN-AGNOSTIC
    if(isolate(input$SHOW_PCHEM3) && isolate(input$CONC_FLUX3 == 'VWC') &&
            length(isolate(get_domains3())) > 1){
        fake_tibble = tibble(datetime=as.POSIXct('2000-01-01'),
            site_name='vwc bollocks', Cl=as.numeric(NA))
        return(fake_tibble)
    }

    # artificially extend pchem dataset to represent each individual watershed
    nsites = length(sites)
    if(nsites > 1){

        samplevel$site_name = sites[1]
        dcopy = samplevel

        for(i in 2:nsites){
            dcopy$site_name = sites[i]
            samplevel = bind_rows(samplevel, dcopy)
        }

    } else {
        samplevel$site_name = sites
    }

    samplevel = samplevel %>%
        left_join(select(dataprecip, -medianPrecip), by='datetime') %>%
        left_join(site_data, by='site_name') %>%
        mutate(precipVol=sumPrecip * ws_area_ha) %>%
        mutate_at(vars(one_of(vars_)), ~(. * precipVol)) %>%
        select(datetime, site_name, one_of(vars_),
            sumPrecip, ws_area_ha) %>%
        select(-ws_area_ha) %>%
        rename(P=sumPrecip)

    if(agg_input == 'Monthly'){

        samplevel = samplevel %>%
            mutate(year=year(datetime))

        agglevel = samplevel %>%
            select(site_name, year, P) %>%
            group_by(year, site_name) %>%
            summarize(Psum=sum(P, na.rm=TRUE)) %>%
            ungroup()

        volWeightedConc = samplevel %>%
            select(-P) %>%
            left_join(agglevel, by=c('year', 'site_name')) %>%
            mutate_at(vars(-datetime, -site_name, -year, -Psum),
                ~(. / Psum)) %>%
            select(-Psum, -year)

    } else if(agg_input == 'Yearly'){

        agglevel = samplevel %>%
            group_by(site_name) %>%
            summarize(Psum=sum(P, na.rm=TRUE))

        volWeightedConc = samplevel %>%
            select(-P) %>%
            left_join(agglevel, by='site_name') %>%
            mutate_at(vars(-datetime, -site_name, -Psum), ~(. / Psum)) %>%
            select(-Psum)
    }

    return(volWeightedConc)
})

#plot generators ####
#these should only update when prerequisite reactive data or facets change
#def could use better abstraction, efficiency measures

output$GRAPH_PRECIP3a = renderDygraph({

    #add a line like this to all renderers to attempt popout windows again
    # output$GRAPH_PRECIP3a = output$GRAPH_PRECIP3aEXP = renderDygraph({

    # dates <<- isolate(input$DATES3)
    # dmn <<- isolate(get_domains3()[1])
    # dataprecip <<- dataPrecip() %>%
    #     filter(domain == dmn)
    dates = isolate(input$DATES3)
    dmn = isolate(get_domains3()[1])
    dataprecip = dataPrecip() %>%
        filter(domain == dmn)

    reactive_vals$facet3aP

    if(nrow(dataprecip)){

        dydat = xts(dataprecip$medianPrecip, order.by=dataprecip$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, dmn)
        ymax = max(dydat, na.rm=TRUE)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                fillAlpha=1, colors=raincolors[1], strokeWidth=3,
                plotter=hyetograph_js, retainDateWindow=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=FALSE,
                labelsDiv='main3aP') %>%
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(dates, plotgroup='nSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(dg)
})

output$GRAPH_PRECIP3b = renderDygraph({

    dates = isolate(input$DATES3)
    dmn = isolate(get_domains3()[2])
    dataprecip = dataPrecip() %>%
        filter(domain == dmn)

    if(reactive_vals$facet3bP == 0) return(NULL)

    if(nrow(dataprecip)){

        dydat = xts(dataprecip$medianPrecip, order.by=dataprecip$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, dmn)
        ymax = max(dydat, na.rm=TRUE)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                fillAlpha=1, colors=raincolors[1], strokeWidth=3,
                plotter=hyetograph_js, retainDateWindow=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=FALSE,
                labelsDiv='main3bP') %>%
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(dates, plotgroup='nSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(dg)
})

output$GRAPH_PRECIP3c = renderDygraph({

    dates = isolate(input$DATES3)
    dmn = isolate(get_domains3()[3])
    dataprecip = dataPrecip() %>%
        filter(domain == dmn)

    if(reactive_vals$facet3cP == 0) return(NULL)

    if(nrow(dataprecip)){

        dydat = xts(dataprecip$medianPrecip, order.by=dataprecip$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, dmn)
        ymax = max(dydat, na.rm=TRUE)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                fillAlpha=1, colors=raincolors[1], strokeWidth=3,
                plotter=hyetograph_js, retainDateWindow=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=FALSE,
                labelsDiv='main3cP') %>%
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(dates, plotgroup='nSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(dg)
})

output$GRAPH_MAIN3a <- output$GRAPH_MAIN3aFULL <- renderDygraph({

    # sites <<- na.omit(isolate(input$SITES3[1:3]))
    # varA <<- isolate(input$VARS3[1])
    # dmns <<- isolate(get_domains3())
    # conc_flux <<- isolate(input$CONC_FLUX3)
    # flux_unit <<- isolate(input$FLUX_UNIT3)
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)
    sites = na.omit(isolate(input$SITES3[1:3]))
    varA = isolate(input$VARS3[1])
    dmns = isolate(get_domains3())
    conc_flux = isolate(input$CONC_FLUX3)
    flux_unit = isolate(input$FLUX_UNIT3)
    conc_unit = isolate(input$CONC_UNIT3)
    show_pchem = isolate(input$SHOW_PCHEM3)
    agg = isolate(input$AGG3)
    dates = isolate(input$DATES3)

    reactive_vals$facet3a

    if(conc_flux == 'VWC'){
        # streamdata <<- volWeightedChem3()
          streamdata = volWeightedChem3()
    } else {
        # streamdata <<- dataChem()
        streamdata = dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            # raindata <<- volWeightedPchem3()
            raindata = volWeightedPchem3()
        } else {
            # raindata <<- dataPchem()
            raindata = dataPchem()
        }

    } else {
        raindata = tibble()
    }

    #TEMPORARY SHORT-CIRCUIT UNTIL WE WORK OUT PRECIP INTERPOLATION
    if(nrow(raindata) == 1 && 'site_name' %in% colnames(raindata) &&
        raindata$site_name == 'vwc bollocks'){
        stop('This feature will be available once we work out precip interpolation.')
    }

    alldata = prep_mainfacets3(varA, dmns, sites, streamdata, raindata,
        conc_flux_selection=conc_flux, show_input_concentration=show_pchem)

    rainsites = get_rainsites(raindata, alldata, streamsites=sites,
        conc_flux_selection=conc_flux, show_input_concentration=show_pchem)

    yunit = ifelse(conc_flux == 'Flux', flux_unit, conc_unit)
    ylab = get_ylab(varA, conc_flux, yunit)

    if(nrow(alldata)){

        displabs = colnames(alldata)[-1]
        dydat = xts(alldata[, displabs], order.by=alldata$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        is_inst = ifelse(agg == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=selection_color_match(sites, displabs, linecolors),#, pad_length=length(displabs)),
                strokeWidth=2, pointSize=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3a') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)

        if(show_pchem){

            if(conc_flux == 'Concentration'){
                rain_or_pchem_cols = selection_color_match(paste(dmns, 'pchem'),
                    rainsites, raincolors)
            } else {
                rain_or_pchem_cols = selection_color_match(paste0('P_', sites),
                    paste0('P_', displabs[displabs %in% sites]),
                    pchemcolors) #untested. might need to work with rainsites instead
            }

            for(i in 1:length(rainsites)){
                dg = dySeries(dg, name=rainsites[i], color=rain_or_pchem_cols[i],
                    axis='y', drawPoints=FALSE, strokeWidth=2,
                    pointSize=2, strokePattern='dashed')
            }
        }

    } else {

        dg = plot_empty_dygraph(dates, mainlab=colnames(alldata)[-1],
            maindiv='main3a', plotgroup='nSiteNVar', ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_QC3a <- renderPlot({

    show_qc = isolate(input$SHOW_QC3)
    sites = na.omit(isolate(input$SITES3[1:3]))
    varA = isolate(input$VARS3[1])
    dmns = isolate(get_domains3())
    conc_unit = isolate(input$CONC_UNIT3)
    show_pchem = isolate(input$SHOW_PCHEM3)
    agg = isolate(input$AGG3)
    dates = isolate(input$DATES3)
    # sites <<- na.omit(isolate(input$SITES3[1:3]))
    # varA <<- isolate(input$VARS3[1])
    # dmns <<- isolate(get_domains3())
    # conc_unit <<- isolate(input$CONC_UNIT3)
    # show_pchem <<- isolate(input$SHOW_PCHEM3)
    # agg <<- isolate(input$AGG3)
    # dates <<- isolate(input$DATES3)

    # reactive_vals$facet3aQC
    reactive_vals$facet3a

    # streamdata <<- dataChem()
    streamdata = dataChem()

    # dischargedata <<- dataQ()
    dischargedata = dataQ()

    alldata <- inner_join(streamdata,
                          dischargedata,
                          by = c("datetime", "site_name")) %>%
        rename(value=3) %>%
        select(datetime, site_name, value, discharge)

    qc <- ggplot(alldata,
                 aes(x = discharge, y = value, colour = site_name),
                 environment=environment()) +
        geom_point() +
        scale_colour_manual(values = linecolors,
                            breaks = c(sites)) +
        labs(y = "") +
        ggthemes::theme_few() +
        theme(legend.position = 'none') 

    return(qc)
})

output$GRAPH_MAIN3b <- output$GRAPH_MAIN3bFULL <- renderDygraph({

    sites = na.omit(isolate(input$SITES3[1:3]))
    varB = isolate(input$VARS3[2])
    dmns = isolate(get_domains3())
    conc_flux = isolate(input$CONC_FLUX3)
    flux_unit = isolate(input$FLUX_UNIT3)
    conc_unit = isolate(input$CONC_UNIT3)
    show_pchem = isolate(input$SHOW_PCHEM3)
    agg = isolate(input$AGG3)
    dates = isolate(input$DATES3)

    if(reactive_vals$facet3b == 0) return(NULL)

    if(conc_flux == 'VWC'){
        streamdata = volWeightedChem3()
    } else {
        streamdata = dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata = volWeightedPchem3()
        } else {
            raindata = dataPchem()
        }

        rainsites = raindata$site_name

    } else {
        raindata = NULL
    }

    #TEMPORARY SHORT-CIRCUIT UNTIL WE WORK OUT PRECIP INTERPOLATION
    if(nrow(raindata) == 1 && 'site_name' %in% colnames(raindata) &&
            raindata$site_name == 'vwc bollocks'){
        stop('This feature will be available once we work out precip interpolation.')
    }

    alldata = prep_mainfacets3(varB, dmns, sites, streamdata, raindata,
        conc_flux_selection=conc_flux, show_input_concentration=show_pchem)

    rainsites = get_rainsites(raindata, alldata, streamsites=sites,
        conc_flux_selection=conc_flux, show_input_concentration=show_pchem)

    yunit = ifelse(conc_flux == 'Flux', flux_unit, conc_unit)
    ylab = get_ylab(varB, conc_flux, yunit)

    if(nrow(alldata)){

        displabs = colnames(alldata)[-1]
        dydat = xts(alldata[, displabs], order.by=alldata$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        is_inst = ifelse(agg == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=selection_color_match(sites, displabs, linecolors),
                strokeWidth=2, pointSize=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3b') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)

        if(show_pchem){

            if(conc_flux == 'Concentration'){
                rain_or_pchem_cols = selection_color_match(paste(dmns, 'pchem'),
                    rainsites, raincolors)
            } else {
                rain_or_pchem_cols = selection_color_match(paste0('P_', sites),
                    paste0('P_', displabs[displabs %in% sites]),
                    pchemcolors)
            }

            for(i in 1:length(rainsites)){
                dg = dySeries(dg, name=rainsites[i], color=rain_or_pchem_cols[i],
                    axis='y', drawPoints=FALSE, strokeWidth=2,
                    pointSize=2, strokePattern='dashed')
            }
        }

    } else {
        dg = plot_empty_dygraph(dates, mainlab=colnames(alldata)[-1],
            maindiv='main3b', plotgroup='nSiteNVar', ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_QC3b <- renderPlot({
    
    # show_qc = isolate(input$SHOW_QC3)
    # sites = na.omit(isolate(input$SITES3[1:3]))
    # varA = isolate(input$VARS3[1])
    # dmns = isolate(get_domains3())
    # conc_unit = isolate(input$CONC_UNIT3)
    # show_pchem = isolate(input$SHOW_PCHEM3)
    # agg = isolate(input$AGG3)
    # dates = isolate(input$DATES3)
    sites <<- na.omit(isolate(input$SITES3[1:3]))
    varA <<- isolate(input$VARS3[1])
    dmns <<- isolate(get_domains3())
    conc_unit <<- isolate(input$CONC_UNIT3)
    show_pchem <<- isolate(input$SHOW_PCHEM3)
    agg <<- isolate(input$AGG3)
    dates <<- isolate(input$DATES3)
    
    # reactive_vals$facet3aQC
    reactive_vals$facet3a
    
    streamdata <<- dataChem()
    # streamdata = dataChem()
    
    dischargedata <<- dataQ()
    # dischargedata = dataQ()
    
    alldata <- inner_join(streamdata,
                          dischargedata,
                          by = c("datetime", "site_name")) %>%
        rename(value=4) %>%
        select(datetime, site_name, value, discharge)
    
    qc <- ggplot(alldata,
                 aes(x = discharge, y = value, colour = site_name),
                 environment=environment()) +
        geom_point() +
        scale_colour_manual(values = linecolors,
                            breaks = c(sites)) +
        labs(y = "") +
        ggthemes::theme_few() +
        theme(legend.position = 'none') 
    
    return(qc)
})

output$GRAPH_MAIN3c <- output$GRAPH_MAIN3cFULL <- renderDygraph({

    sites = na.omit(isolate(input$SITES3[1:3]))
    varC = isolate(input$VARS3[3])
    dmns = isolate(get_domains3())
    conc_flux = isolate(input$CONC_FLUX3)
    flux_unit = isolate(input$FLUX_UNIT3)
    conc_unit = isolate(input$CONC_UNIT3)
    show_pchem = isolate(input$SHOW_PCHEM3)
    agg = isolate(input$AGG3)
    dates = isolate(input$DATES3)

    if(reactive_vals$facet3c == 0) return(NULL)

    if(conc_flux == 'VWC'){
        streamdata = volWeightedChem3()
    } else {
        streamdata = dataChem()
    }

    if(show_pchem){

        if(conc_flux == 'VWC'){
            raindata = volWeightedPchem3()
        } else {
            raindata = dataPchem()
        }

        rainsites = raindata$site_name

    } else {
        raindata = NULL
    }

    #TEMPORARY SHORT-CIRCUIT UNTIL WE WORK OUT PRECIP INTERPOLATION
    if(nrow(raindata) == 1 && 'site_name' %in% colnames(raindata) &&
            raindata$site_name == 'vwc bollocks'){
        stop('This feature will be available once we work out precip interpolation.')
    }

    alldata = prep_mainfacets3(varC, dmns, sites, streamdata, raindata,
        conc_flux_selection=conc_flux, show_input_concentration=show_pchem)

    rainsites = get_rainsites(raindata, alldata, streamsites=sites,
        conc_flux_selection=conc_flux, show_input_concentration=show_pchem)

    yunit = ifelse(conc_flux == 'Flux', flux_unit, conc_unit)
    ylab = get_ylab(varC, conc_flux, yunit)

    if(nrow(alldata)){

        displabs = colnames(alldata)[-1]
        dydat = xts(alldata[, displabs], order.by=alldata$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        is_inst = ifelse(agg == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=selection_color_match(sites, displabs, linecolors),
                strokeWidth=2, pointSize=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3c') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)

        if(show_pchem){

            if(conc_flux == 'Concentration'){
                rain_or_pchem_cols = selection_color_match(paste(dmns, 'pchem'),
                    rainsites, raincolors)
            } else {
                rain_or_pchem_cols = selection_color_match(paste0('P_', sites),
                    paste0('P_', displabs[displabs %in% sites]),
                    pchemcolors)
            }

            for(i in 1:length(rainsites)){
                dg = dySeries(dg, name=rainsites[i], color=rain_or_pchem_cols[i],
                    axis='y', drawPoints=FALSE, strokeWidth=2,
                    pointSize=2, strokePattern='dashed')
            }
        }

    } else {
        dg = plot_empty_dygraph(dates, mainlab=colnames(alldata)[-1],
            maindiv='main3a', plotgroup='nSiteNVar', ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_QC3c <- renderPlot({
    
    # show_qc = isolate(input$SHOW_QC3)
    # sites = na.omit(isolate(input$SITES3[1:3]))
    # varA = isolate(input$VARS3[1])
    # dmns = isolate(get_domains3())
    # conc_unit = isolate(input$CONC_UNIT3)
    # show_pchem = isolate(input$SHOW_PCHEM3)
    # agg = isolate(input$AGG3)
    # dates = isolate(input$DATES3)
    sites <<- na.omit(isolate(input$SITES3[1:3]))
    varA <<- isolate(input$VARS3[1])
    dmns <<- isolate(get_domains3())
    conc_unit <<- isolate(input$CONC_UNIT3)
    show_pchem <<- isolate(input$SHOW_PCHEM3)
    agg <<- isolate(input$AGG3)
    dates <<- isolate(input$DATES3)
    
    # reactive_vals$facet3aQC
    reactive_vals$facet3a
    
    streamdata <<- dataChem()
    # streamdata = dataChem()
    
    dischargedata <<- dataQ()
    # dischargedata = dataQ()
    
    alldata <- inner_join(streamdata,
                          dischargedata,
                          by = c("datetime", "site_name")) %>%
        rename(value=5) %>%
        select(datetime, site_name, value, discharge)
    
    qc <- ggplot(alldata,
                 aes(x = discharge, y = value, colour = site_name),
                 environment=environment()) +
        geom_point() +
        scale_colour_manual(values = linecolors,
                            breaks = c(sites)) +
        labs(y = "") +
        ggthemes::theme_few() +
        theme(legend.position = 'none')
    
    return(qc)
})

output$GRAPH_Q3 = renderDygraph({

    dataq = dataQ()
    #zz <<- dataQ()
    #dataq <- zz
    tryCatch({
        dataq = spread(dataq, site_name, discharge)
    }, error=function(e) NULL)
    dates = isolate(input$DATES3)
    sites = na.omit(isolate(input$SITES3[1:3]))
    
    #ii <<- dates
    #dates <- ii
    #ww <<- sites
    #sites <- ww

    if(nrow(dataq)){

        displabs = colnames(dataq)[-1]
        dydat = xts(dataq[, displabs], order.by=dataq$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                strokeWidth=1, fillAlpha=0.4, retainDateWindow=TRUE,
                colors=selection_color_match(sites,
                    displabs[displabs %in% sites],
                    linecolors),
                drawGapEdgePoints=TRUE) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='Q3') %>%
            dyAxis('y', label='Q (L/s)', labelWidth=16, labelHeight=10,
                pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(dates, plotgroup='nSiteNVar',
            ylab='Q (L/s)', px_per_lab=10)
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


