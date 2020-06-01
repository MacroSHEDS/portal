
#govern showing/hiding of facets ####

reactive_vals = reactiveValues()
reactive_vals$facetA3 = 0
reactive_vals$facetB3 = 0
reactive_vals$facetC3 = 0
reactive_vals$update_basedata = 0

# observeEvent({
#     input$DATE3
#     input$CONC_FLUX3
#     input$CONC_UNIT3
#     input$FLUX_UNIT3
#     input$AGG3
#     input$INCONC3
# }, {
#     reactive_vals$facetA3 = reactive_vals$facetA3 + 1
#     reactive_vals$facetB3 = reactive_vals$facetB3 + 1
#     reactive_vals$facetC3 = reactive_vals$facetC3 + 1
# })

observeEvent({
    if(
        ! is.null(input$SITES3) &&
        ! is.null(get_domains3()) &&
        ! is.null(input$CONC_FLUX3) &&
        ! is.null(input$FLUX_UNIT3) &&
        ! is.null(input$CONC_UNIT3) &&
        ! is.null(input$INCONC3) &&
        ! is.null(input$AGG3) &&
        ! is.null(input$DATE3) &&
        length(input$SOLUTES3) == 1
    ){ TRUE } else return()
}, {
    reactive_vals$facetA3 = reactive_vals$facetA3 + 1
})

observeEvent({
    if(
        ! is.null(input$SITES3) &&
        ! is.null(get_domains3()) &&
        ! is.null(input$CONC_FLUX3) &&
        ! is.null(input$FLUX_UNIT3) &&
        ! is.null(input$CONC_UNIT3) &&
        ! is.null(input$INCONC3) &&
        ! is.null(input$AGG3) &&
        ! is.null(input$DATE3) &&
        length(input$SOLUTES3) == 2
    ){ TRUE } else return()
    # if(length(input$SOLUTES3) == 2){
    #     TRUE
    # } else return()
}, {
    reactive_vals$facetA3 = reactive_vals$facetA3 + 1
    reactive_vals$facetB3 = reactive_vals$facetB3 + 1
})

observeEvent({
    if(
        ! is.null(input$SITES3) &&
        ! is.null(get_domains3()) &&
        ! is.null(input$CONC_FLUX3) &&
        ! is.null(input$FLUX_UNIT3) &&
        ! is.null(input$CONC_UNIT3) &&
        ! is.null(input$INCONC3) &&
        ! is.null(input$AGG3) &&
        ! is.null(input$DATE3) &&
        length(input$SOLUTES3) == 3
    ){ TRUE } else return()
    # if(length(input$SOLUTES3) == 3){
    #     TRUE
    # } else return()
}, {
    reactive_vals$facetA3 = reactive_vals$facetA3 + 1
    reactive_vals$facetB3 = reactive_vals$facetB3 + 1
    reactive_vals$facetC3 = reactive_vals$facetC3 + 1
})

#reactivity flow control ####

#when domain(s) change, site options change, but not site selections
get_domains3 = eventReactive(input$DOMAINS3, {

    domains = input$DOMAINS3

    updateSelectizeInput(session, 'SITES3',
        choices=generate_dropdown_sitelist(domains),
        selected=input$SITES3,
        options=list(maxItems=3))

    return(domains)
})

#when domain(s) change and INCONC3==TRUE, update basedata reactive value
observeEvent({
    if(
        ! is.null(get_domains3()) &&
        input$INCONC3
    ){ TRUE } else return()
}, {
    reactive_vals$update_basedata = reactive_vals$update_basedata + 1
})

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
    grab = read_combine_feathers('chemistry', dmns=dmns, sites=sites)
    flux = read_combine_feathers('flux', dmns=dmns, sites=sites)
    Q = read_combine_feathers('discharge', dmns=dmns, sites=sites)

    init_vals$recent_domain = dmns[1] #needed?

    basedata = list(grab=grab, P=P, Q=Q, pchem=pchem, flux=flux)
    return(basedata)
})

#when basedata changes, variable list and time slider change, but not selections
observe({

    # basedata <<- load_basedata()
    # solutes3 <<- isolate(input$SOLUTES3)
    # dates3 <<- isolate(input$DATE3)
    basedata = load_basedata()
    solutes3 = isolate(input$SOLUTES3)
    dates3 = isolate(input$DATE3)

    grabvars_display_subset = filter_dropdown_varlist(basedata$grab)
    updateSelectizeInput(session, 'SOLUTES3',
        choices=grabvars_display_subset, selected=solutes3)

    dtrng = get_timeslider_extent(basedata, dates3)

    updateSliderInput(session, 'DATE3', min=dtrng[1], max=dtrng[2],
        value=dates3, timeFormat='%b %Y')
})

#when variable(s) change, time slider changes (disabled)
# observe({
#
#     var = input$SOLUTES3
#     dates3 = isolate(input$DATE3)
#     basedata = isolate(load_basedata())
#
#     dtrng = get_timeslider_extent(basedata, dates3)
#
#     updateSliderInput(session, 'DATE3', min=dtrng[1], max=dtrng[2],
#         value=most_recent_year(dtrng), timeFormat='%b %Y')
# })

#if variables(s), aggregation, units, site, or time window change, re-filter datasets
data3 = reactive({

    dates3 = input$DATE3
    solutes3 = input$SOLUTES3
    conc_flux3 = input$CONC_FLUX3
    conc_unit3 = input$CONC_UNIT3
    flux_unit3 = input$FLUX_UNIT3
    agg3 = input$AGG3
    sites3 = input$SITES3
    basedata = load_basedata()

    data3 = if(conc_flux3 == 'Flux') basedata$flux else basedata$grab

    if(nrow(data3) == 0) return(data3)

    data3 = data3 %>%
        filter(datetime >= dates3[1], datetime <= dates3[2]) %>%
        select(one_of('datetime', 'site_name', solutes3))

    if(nrow(data3) == 0) return(data3)

    data3 = pad_ts(data3, vars=solutes3, datebounds=dates3)
    data3 = ms_aggregate(data3, agg3, which_dataset='grab', conc_flux3)

    if(init_vals$enable_unitconvert){
        if(conc_flux3 %in% c('Concentration', 'VWC')){
            data3 = convert_conc_units(data3, desired_unit=conc_unit3)
        } else if(conc_flux3 == 'Flux'){
            data3 = convert_flux_units(data3, desired_unit=flux_unit3)
        }
    }

    return(data3)
})

dataPchem3 = reactive({

    # dates3 <<- input$DATE3
    # solutes3 <<- input$SOLUTES3
    # conc_flux3 <<- input$CONC_FLUX3
    # conc_unit3 <<- input$CONC_UNIT3
    # agg3 <<- input$AGG3
    # dmns <<- get_domains3()
    # basedata <<- load_basedata()
    dates3 = input$DATE3
    solutes3 = input$SOLUTES3
    conc_flux3 = input$CONC_FLUX3
    conc_unit3 = input$CONC_UNIT3
    agg3 = input$AGG3
    dmns = get_domains3()
    basedata = load_basedata()

    pchem3 = basedata$pchem

    if(nrow(pchem3) == 0) return(pchem3)

    pchem3 = pchem3 %>%
        filter(datetime >= dates3[1], datetime <= dates3[2]) %>%
        select(datetime, domain, one_of(solutes3))

    if(nrow(pchem3) == 0) return(pchem3)

    pchem3 = pad_ts(pchem3, vars=solutes3, datebounds=dates3)
    pchem3 = ms_aggregate(pchem3, agg3, which_dataset='pchem', conc_flux3)

    if(init_vals$enable_unitconvert){
        pchem3 = convert_conc_units(pchem3, desired_unit=conc_unit3)
    }#temporary? modify the above if rain flux and rain units become modifiable

    #format domain name for display as a "site name"
    pchem3 = pchem3 %>%
        mutate(domain = paste(domain, 'pchem')) %>%
        rename(site_name=domain)

    return(pchem3)
})

dataPrecip3 = reactive({

    dates3 = input$DATE3
    agg3 = input$AGG3
    basedata = load_basedata()

    dataPrecip3 = basedata$P

    if(nrow(dataPrecip3) == 0) return(dataPrecip3)

    dataPrecip3 = dataPrecip3 %>%
        filter(datetime >= dates3[1], datetime <= dates3[2]) %>%
        select(one_of("datetime", "site_name", 'precip'))

    if(nrow(dataPrecip3) == 0) return(dataPrecip3)

    # dataPrecip3 = pad_ts(dataPrecip3, unique(dataPrecip3$site_name),
    #     'precip', dates3)
    dataPrecip3 = pad_ts(dataPrecip3, vars='precip', datebounds=dates3)
    dataPrecip3 = ms_aggregate(dataPrecip3, agg3, which_dataset='p')

    dataPrecip3 = dataPrecip3 %>%
        group_by(datetime) %>%
        summarize(sumPrecip=sum(precip, na.rm=TRUE),
            medianPrecip=median(precip, na.rm=TRUE)) %>%
        ungroup()

    return(dataPrecip3)
})

dataFlow3 = reactive({

    # dates3 <<- input$DATE3
    # sites3 <<- input$SITES3
    # agg3 <<- input$AGG3
    # basedata <<- load_basedata()
    dates3 = input$DATE3
    sites3 = input$SITES3
    agg3 = input$AGG3
    basedata = load_basedata()

    dataFlow3 = basedata$Q

    if(nrow(dataFlow3) == 0) return(dataFlow3)

    dataFlow3 = dataFlow3 %>%
        filter(datetime > dates3[1], datetime < dates3[2]) %>%
        select(datetime, site_name, Q)

    if(nrow(dataFlow3) == 0) return(dataFlow3)

    dataFlow3 = pad_ts(dataFlow3, vars='Q', datebounds=dates3)
    dataFlow3 = ms_aggregate(dataFlow3, agg3, which_dataset='q')

    if(agg3 == 'Instantaneous'){ #revisit this. needed?
        dataFlow3 = dataFlow3 %>%
            group_by(datetime, site_name) %>%
            summarise(Q=max(Q, na.rm=TRUE)) %>%
            ungroup()
    }

    return(dataFlow3)
})

#post-filtering data modifications ####
#these should only update when prerequisite reactive data (above) updates, so
#all user inputs should be isolated

#calculate VWC (volume weighted concentration) from chem and q
#only possible at monthly and yearly agg. conditionals controlled by ui
volWeighted3 = reactive({

    data3 = data3()
    dataflow3 = dataFlow3()
    agg_input = isolate(input$AGG3)

    samplevel = data3 %>%
        left_join(dataflow3, by=c('datetime', 'site_name')) %>%
        mutate_at(vars(-datetime, -site_name, -Q), ~(. * Q))

    if(agg_input == 'Monthly'){

        samplevel = samplevel %>%
            mutate(year=lubridate::year(datetime))

        agglevel = samplevel %>%
            select(site_name, year, Q) %>%
            group_by(year, site_name) %>%
            summarize(Qsum=sum(Q, na.rm=TRUE)) %>%
            ungroup()

        volWeightedConc = samplevel %>%
            select(-Q) %>%
            left_join(agglevel, by=c('year', 'site_name')) %>%
            mutate_at(vars(-datetime, -site_name, -year, -Qsum),
                ~(. / Qsum)) %>%
            select(-Qsum, -year)

    } else if(agg_input == 'Yearly'){

        agglevel = samplevel %>%
            group_by(site_name) %>%
            summarize(Qsum=sum(Q, na.rm=TRUE))

        volWeightedConc = samplevel %>%
            select(-Q) %>%
            left_join(agglevel, by='site_name') %>%
            mutate_at(vars(-datetime, -site_name, -Qsum), ~(. / Qsum)) %>%
            select(-Qsum)
    }

    return(volWeightedConc)
})

#calculate VWC (volume weighted concentration) from pchem and p
#only possible at monthly and yearly agg. conditionals controlled by ui
volWeightedPrecip3 = reactive({

    # samplevel <<- dataPchem3()
    # dataprecip3 <<- dataPrecip3()
    # agg_input <<- isolate(input$AGG3)
    # sites <<- isolate(input$SITES3)
    # solutes <<- isolate(input$SOLUTES3)
    samplevel = dataPchem3()
    dataprecip3 = dataPrecip3()
    agg_input = isolate(input$AGG3)
    sites = isolate(input$SITES3)
    solutes = isolate(input$SOLUTES3)

    #TEMPORARY SHORT-CIRCUIT UNTIL WE WORK OUT PRECIP INTERPOLATION
    if(isolate(input$INCONC3) && isolate(input$CONC_FLUX3 == 'VWC') &&
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
        left_join(select(dataprecip3, -medianPrecip), by='datetime') %>%
        left_join(site_data, by='site_name') %>%
        mutate(precipVol=sumPrecip * ws_area_ha) %>%
        mutate_at(vars(one_of(solutes)), ~(. * precipVol)) %>%
        select(datetime, site_name, one_of(solutes),
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

output$GRAPH_PRECIP3 = output$GRAPH_PRECIP3EXP = renderDygraph({

    data = dataPrecip3()
    date3 = isolate(input$DATE3)

    if(nrow(data)){

        dydat = xts(data$medianPrecip, order.by=data$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, 'P')
        ymax = max(dydat, na.rm=TRUE)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                fillAlpha=1, colors=raincolors[1], strokeWidth=3,
                plotter=hyetograph_js, retainDateWindow=TRUE) %>%
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(date3, plotgroup='nSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(dg)
})

output$GRAPH_MAIN3a = output$GRAPH_MAIN3aEXP = renderDygraph({

    # sites <<- na.omit(isolate(input$SITES3[1:3]))
    # varA <<- isolate(input$SOLUTES3[1])
    # dmns <<- isolate(get_domains3())
    # conc_flux3 <<- isolate(input$CONC_FLUX3)
    # flux_unit3 <<- isolate(input$FLUX_UNIT3)
    # conc_unit3 <<- isolate(input$CONC_UNIT3)
    # inconc3 <<- isolate(input$INCONC3)
    # agg3 <<- isolate(input$AGG3)
    # date3 <<- isolate(input$DATE3)
    sites = na.omit(isolate(input$SITES3[1:3]))
    varA = isolate(input$SOLUTES3[1])
    dmns = isolate(get_domains3())
    conc_flux3 = isolate(input$CONC_FLUX3)
    flux_unit3 = isolate(input$FLUX_UNIT3)
    conc_unit3 = isolate(input$CONC_UNIT3)
    inconc3 = isolate(input$INCONC3)
    agg3 = isolate(input$AGG3)
    date3 = isolate(input$DATE3)

    reactive_vals$facetA3

    if(conc_flux3 == 'VWC'){
        # streamdata <<- volWeighted3()
        streamdata = volWeighted3()
    } else {
        # streamdata <<- data3()
        streamdata = data3()
    }

    if(inconc3){

        if(conc_flux3 == 'VWC'){
            # raindata <<- volWeightedPrecip3()
            raindata = volWeightedPrecip3()
        } else {
            # raindata <<- dataPchem3()
            raindata = dataPchem3()
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
        conc_flux_selection=conc_flux3, show_input_concentration=inconc3)

    rainsites = get_rainsites(raindata, alldata, streamsites=sites,
        conc_flux_selection=conc_flux3, show_input_concentration=inconc3)

    yunit = ifelse(conc_flux3 == 'Flux', flux_unit3, conc_unit3)
    ylab = get_ylab(varA, conc_flux3, yunit)

    if(nrow(alldata)){

        displabs = colnames(alldata)[-1]
        dydat = xts(alldata[, displabs], order.by=alldata$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        is_inst = ifelse(agg3 == 'Instantaneous', TRUE, FALSE)
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

        if(inconc3 == TRUE){

            if(conc_flux3 == 'Concentration'){
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

        dg = plot_empty_dygraph(date3, mainlab=colnames(alldata)[-1],
            maindiv='main3a', plotgroup='nSiteNVar', ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN3b = output$GRAPH_MAIN3bEXP = renderDygraph({

    sites = na.omit(isolate(input$SITES3[1:3]))
    varB = isolate(input$SOLUTES3[2])
    dmns = isolate(get_domains3())
    conc_flux3 = isolate(input$CONC_FLUX3)
    flux_unit3 = isolate(input$FLUX_UNIT3)
    conc_unit3 = isolate(input$CONC_UNIT3)
    inconc3 = isolate(input$INCONC3)
    agg3 = isolate(input$AGG3)
    date3 = isolate(input$DATE3)

    if(reactive_vals$facetB3 == 0) return(NULL)

    if(conc_flux3 == 'VWC'){
        streamdata = volWeighted3()
    } else {
        streamdata = data3()
    }

    if(inconc3){

        if(conc_flux3 == 'VWC'){
            raindata = volWeightedPrecip3()
        } else {
            raindata = dataPchem3()
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
        conc_flux_selection=conc_flux3, show_input_concentration=inconc3)

    rainsites = get_rainsites(raindata, alldata, streamsites=sites,
        conc_flux_selection=conc_flux3, show_input_concentration=inconc3)

    yunit = ifelse(conc_flux3 == 'Flux', flux_unit3, conc_unit3)
    ylab = get_ylab(varB, conc_flux3, yunit)

    if(nrow(alldata)){

        displabs = colnames(alldata)[-1]
        dydat = xts(alldata[, displabs], order.by=alldata$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        is_inst = ifelse(agg3 == 'Instantaneous', TRUE, FALSE)
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

        if(inconc3 == TRUE){

            if(conc_flux3 == 'Concentration'){
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
        dg = plot_empty_dygraph(date3, mainlab=colnames(alldata)[-1],
            maindiv='main3b', plotgroup='nSiteNVar', ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN3c = output$GRAPH_MAIN3cEXP = renderDygraph({

    sites = na.omit(isolate(input$SITES3[1:3]))
    varC = isolate(input$SOLUTES3[3])
    dmns = isolate(get_domains3())
    conc_flux3 = isolate(input$CONC_FLUX3)
    flux_unit3 = isolate(input$FLUX_UNIT3)
    conc_unit3 = isolate(input$CONC_UNIT3)
    inconc3 = isolate(input$INCONC3)
    agg3 = isolate(input$AGG3)
    date3 = isolate(input$DATE3)

    if(reactive_vals$facetC3 == 0) return(NULL)

    if(conc_flux3 == 'VWC'){
        streamdata = volWeighted3()
    } else {
        streamdata = data3()
    }

    if(inconc3){

        if(conc_flux3 == 'VWC'){
            raindata = isolate(volWeightedPrecip3())
        } else {
            raindata = isolate(dataPchem3())
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
        conc_flux_selection=conc_flux3, show_input_concentration=inconc3)

    rainsites = get_rainsites(raindata, alldata, streamsites=sites,
        conc_flux_selection=conc_flux3, show_input_concentration=inconc3)

    yunit = ifelse(conc_flux3 == 'Flux', flux_unit3, conc_unit3)
    ylab = get_ylab(varC, conc_flux3, yunit)

    if(nrow(alldata)){

        displabs = colnames(alldata)[-1]
        dydat = xts(alldata[, displabs], order.by=alldata$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        is_inst = ifelse(agg3 == 'Instantaneous', TRUE, FALSE)
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

        if(inconc3 == TRUE){

            if(conc_flux3 == 'Concentration'){
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
        dg = plot_empty_dygraph(date3, mainlab=colnames(alldata)[-1],
            maindiv='main3a', plotgroup='nSiteNVar', ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_FLOW3 = output$GRAPH_FLOW3EXP = renderDygraph({

    dataflow3 = dataFlow3()
    tryCatch({
        dataflow3 = spread(dataflow3, site_name, Q)
    }, error=function(e) NULL)
    date3 = isolate(input$DATE3)
    sites = na.omit(isolate(input$SITES3[1:3]))

    if(nrow(dataflow3)){

        displabs = colnames(dataflow3)[-1]
        dydat = xts(dataflow3[, displabs], order.by=dataflow3$datetime,
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
                labelsDiv='flow3') %>%
            dyAxis('y', label='Q (L/s)', labelWidth=16, labelHeight=10,
                pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(date3, plotgroup='nSiteNVar',
            ylab='Q (L/s)', px_per_lab=10)
    }

    return(dg)
})

#manage popout windows. currently buggy. disabled.
observeEvent(input$EXPAND_PRECIP3, {
    showModal(
        modalDialog(title=NULL, size='l', id='modal3precip', easyClose=TRUE,
            footer=NULL,
            fluidRow(class='text-center',
                div(id='precip3DUPE'),
                dygraphOutput('GRAPH_PRECIP3EXP'),
                br()
            )
        )
    )
    runjs("$('#precip3').clone().appendTo('#precip3DUPE')")
})

observeEvent(input$EXPAND_MAIN3a, {
    showModal(
        modalDialog(title=NULL, size='l', id='modal3a', easyClose=TRUE,
            footer=NULL,
            fluidRow(class='text-center',
                div(id='main3aDUPE'),
                dygraphOutput('GRAPH_MAIN3aEXP'),
                br()
            )
        )
    )
    runjs("$('#main3a').clone().appendTo('#main3aDUPE')")
})

observeEvent(input$EXPAND_MAIN3b, {
    showModal(
        modalDialog(title=NULL, size='l', id='modal3b', easyClose=TRUE,
            footer=NULL,
            fluidRow(class='text-center',
                div(id='main3bDUPE'),
                dygraphOutput('GRAPH_MAIN3bEXP'),
                br()
            )
        )
    )
    runjs("$('#main3b').clone().appendTo('#main3bDUPE')")
})

observeEvent(input$EXPAND_MAIN3c, {
    showModal(
        modalDialog(title=NULL, size='l', id='modal3c', easyClose=TRUE,
            footer=NULL,
            fluidRow(class='text-center',
                div(id='main3cDUPE'),
                dygraphOutput('GRAPH_MAIN3cEXP'),
                br()
            )
        )
    )
    runjs("$('#main3c').clone().appendTo('#main3cDUPE')")
})

observeEvent(input$EXPAND_FLOW3, {
    showModal(
        modalDialog(title=NULL, size='l', id='flow3', easyClose=TRUE,
            footer=NULL,
            fluidRow(class='text-center',
                div(id='flow3DUPE'),
                dygraphOutput('GRAPH_FLOW3EXP'),
                br()
            )
        )
    )
    runjs("$('#flow3').clone().appendTo('#flow3DUPE')")
})

# observe({
#     input$DEBUG
#     print(rlang::last_error())
#     print(traceback())
# })
