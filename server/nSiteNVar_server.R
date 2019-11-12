
changesInSelections3 = reactiveValues()
changesInSelections3$facetA3 = 0
changesInSelections3$facetB3 = 0
changesInSelections3$facetC3 = 0
# dt_extent = rep(Sys.time(), 2)
# attr(dt_extent, 'tzone') = 'UTC'
# changesInSelections3$dt_extent = dt_extent


observeEvent({
    input$DATE3
    input$CONC_FLUX3
    input$CONC_UNIT3
    input$FLUX_UNIT3
    input$AGG3
    input$INCONC3
    input$RAINSITES3
    input$RAINVARS3
}, {
    changesInSelections3$facetA3 = changesInSelections3$facetA3 + 1
    changesInSelections3$facetB3 = changesInSelections3$facetB3 + 1
    changesInSelections3$facetC3 = changesInSelections3$facetC3 + 1
})

observeEvent({
    if(length(input$SOLUTES3) == 1){
        TRUE
    } else return()
}, {
    changesInSelections3$facetA3 = changesInSelections3$facetA3 + 1
})

observeEvent({
    if(length(input$SOLUTES3) == 2){
        TRUE
    } else return()
}, {
    changesInSelections3$facetB3 = changesInSelections3$facetB3 + 1
})

observeEvent({
    if(length(input$SOLUTES3) == 3){
        TRUE
    } else return()
}, {
    changesInSelections3$facetC3 = changesInSelections3$facetC3 + 1
})

grab = reactive({

    domain = input$DOMAINS3

    if(is.null(domain)){
        grab = read_feather(glue('data/{dmn}/grab.feather',
            dmn=init_vals$recent_domain))
    } else {
        grab = read_feather(glue('data/{dmn}/grab.feather', dmn=domain))
    }

    grab = filter(grab, site_name %in% sites_with_Q)

    init_vals$recent_domain = domain

    new_sitelist = site_data %>%
        filter(domain == input$DOMAINS3, site_type == 'stream_gauge') %>%
        pull(site_name)

    updateSelectizeInput(session, 'SITES3', choices=new_sitelist,
        selected=default_site[[input$DOMAINS3]])

    # dt_extent = changesInSelections3$dt_extent
    # changesInSelections3$dt_extent = range(c(dt_extent, input$DATE3))

    return(grab)
})

pchem = reactive({

    domain = input$DOMAINS3

    if(is.null(domain)){
        pchem = read_feather(glue('data/{dmn}/pchem.feather',
            dmn=init_vals$recent_domain))
    } else {
        pchem = read_feather(glue('data/{dmn}/pchem.feather', dmn=domain))
    }

    init_vals$recent_domain = domain

    return(pchem)
})

flux = reactive({

    domain = input$DOMAINS3

    if(is.null(domain)){
        flux = read_feather(glue('data/{dmn}/flux.feather',
            dmn=init_vals$recent_domain))
    } else {
        flux = read_feather(glue('data/{dmn}/flux.feather', dmn=domain))
    }

    # flux = flux %>%
    #     rename(datetime=date) %>% #temporary
    #     select(-Q_Ld) #temporary
        # filter(datetime < as.Date('2013-01-01')) #temporary

    return(flux)
})

P = reactive({

    domain = input$DOMAINS3

    if(is.null(domain)){
        P = read_feather(glue('data/{dmn}/precip.feather',
            dmn=init_vals$recent_domain))
    } else {
        P = read_feather(glue('data/{dmn}/precip.feather', dmn=domain))
    }

    # P = P %>%
    #     filter(datetime < as.Date('2013-01-01')) #temporary

    return(P)
})

Q = reactive({

    domain = input$DOMAINS3

    if(is.null(domain)){
        Q = read_feather(glue('data/{dmn}/discharge.feather',
            dmn=init_vals$recent_domain))
    } else {
        Q = read_feather(glue('data/{dmn}/discharge.feather', dmn=domain))
    }

    # Q = Q %>%
    #     filter(datetime < as.Date('2013-01-01')) #temporary

    return(Q)
})

observe({

    grab_subset = filter(grab(), site_name %in% default_site[[input$DOMAINS3]])
    grabvars_display_subset = populate_vars(grab_subset[-(1:2)])

    updateSelectizeInput(session, 'SOLUTES3',
        choices=grabvars_display_subset,
        selected=grabvars_display_subset[[1]][[1]])
})

observe({

    dmn = input$DOMAINS3
    dmnsites = sites_with_pchem[[dmn]]

    updateSelectizeInput(session, 'RAINSITES3', choices=dmnsites,
        selected=dmnsites[1])

    pchem_subset = filter(pchem(), site_name %in% dmnsites[1])
    pchemvars_display_subset = populate_vars(pchem_subset[-(1:2)],
        vartype='precip')

    updateSelectizeInput(session, 'RAINVARS3',
        choices=pchemvars_display_subset,
        selected=pchemvars_display_subset[[1]][[1]])
})

data3 = reactive({

    data3 = if(input$CONC_FLUX3 == 'Flux') flux() else grab()
    data3 = data3 %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% input$SITES3) %>%
        select(one_of("datetime", "site_name", input$SOLUTES3))
        # mutate(datetime=as.POSIXct(datetime))

    if(nrow(data3) == 0) return(data3)

    # isi <<- input$SITES3
    # ida <<- input$DATE3
    # iso <<- input$SOLUTES3
    # dd <<- data3
    # input = list(SITES3=isi, DATE3=ida, SOLUTES3=iso)
    # sites=input$SITES3; vars=input$SOLUTES3; datebounds=input$DATE3

    data3 = pad_ts3(data3, input$SITES3, input$SOLUTES3, input$DATE3)

    if(input$AGG3 != 'Instantaneous'){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')
        data3 = data3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
            summarize_all(list(~mean(., na.rm=TRUE))) %>%
            ungroup()
    }

    if(init_vals$enable_unitconvert){
        if(input$CONC_FLUX3 %in% c('Concentration', 'VWC')){
            data3 = convert_conc_units(data3, desired_unit=input$CONC_UNIT3)
        } else if(input$CONC_FLUX3 == 'Flux'){
            data3 = convert_flux_units(data3, desired_unit=input$FLUX_UNIT3)
        }
    }

    return(data3)

})

dataPchem3 = reactive({

    pchem3 = pchem() %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% input$RAINSITES3) %>%
        select(one_of("datetime", "site_name", input$RAINVARS3))

    if(nrow(pchem3) == 0) return(pchem3)

    pchem3 = pad_ts3(pchem3, input$RAINSITES3, input$RAINVARS3, input$DATE3)

    agg_period = switch(input$AGG3, 'Monthly'='month', 'Yearly'='year')
    pchem3 = pchem3 %>%
        group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
        summarize_all(list(~mean(., na.rm=TRUE))) %>%
        ungroup()

    if(init_vals$enable_unitconvert){
        pchem3 = convert_conc_units(pchem3, desired_unit=input$CONC_UNIT3)
    }#temporary: modify the above when RAIN_UNIT3 and PCHEM_CONC_FLUX3 exist

    return(pchem3)

})

dataPrecip3 = reactive({

    # pp <<- P()
    # ida <<- input$DATE3
    # isi <<- input$SITES3
    # ido <<- input$DOMAINS3
    # iag <<- input$AGG3
    # input = list(DATE3=ida, DOMAINS3=ido, AGG3=iag, SITES3=isi)
    # sites=input$SITES3; vars=input$SOLUTES3; datebounds=input$DATE3; tsdf=pp

    dataPrecip3 = P() %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% sites_with_P[[input$DOMAINS3]]) %>%
        select(one_of("datetime", "site_name", 'precip'))
        # mutate(datetime=as.POSIXct(datetime))

    dataPrecip3 = pad_ts3(dataPrecip3, unique(dataPrecip3$site_name),
        'precip', input$DATE3)
    # nsites = length(input$SITES3)
    # dt_ext_rows = tibble(datetime=rep(as.POSIXct(input$DATE3), nsites),
    #     site_name=rep(input$SITES3, each=nsites),
    #     precip=rep(NA, nsites * 2))
    # dataPrecip3 = bind_rows(dt_ext_rows[1,], dataPrecip3, dt_ext_rows[2,])

    if(input$AGG3 != 'Instantaneous'){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')
        dataPrecip3 = dataPrecip3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
            summarize_all(list(~mean(., na.rm=TRUE))) %>%
            ungroup()
    }

    dataPrecip3 = dataPrecip3 %>%
        # group_by(lubridate::yday(datetime)) %>%
        group_by(datetime) %>%
        summarise(medianPrecip=median(precip, na.rm=TRUE)) %>%
        ungroup()
})

dataFlow3 = reactive ({

    dataFlow3 = Q() %>%
        filter(datetime > input$DATE3[1],
        datetime < input$DATE3[2], site_name %in% input$SITES3) %>%
        select(datetime, site_name, Q)
        # mutate(datetime=as.POSIXct(datetime, tz='UTC'))

    if(nrow(dataFlow3) == 0) return(dataFlow3)

    dataFlow3 = pad_ts3(dataFlow3, input$SITES3, 'Q', input$DATE3)

    if(input$AGG3 != 'Instantaneous'){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')
        dataFlow3 = dataFlow3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
            summarize_all(list(~max(., na.rm=TRUE))) %>%
            ungroup()
    } else {
        dataFlow3 = dataFlow3 %>%
            group_by(datetime, site_name) %>%
            summarise(Q=max(Q, na.rm=TRUE)) %>%
            ungroup()
    }

    return(dataFlow3)
})

volWeighted3 = reactive({

    if(input$INCONC3){
        d = dataPchem3()
        pp = dataPrecip3()
        d <<- d
        pp <<- pp
        d %>%
            full_join(pp, by='datetime') %>%
            #HERE; GOTTA EITHER BRING IN RAIN VOLUME OR CALCULATE IT
            mutate_at(vars(-datetime, -site_name, -precipVol), ~(. * precipVol))
    } else {
        samplevel = data3() %>%
            full_join(dataFlow3(), by=c('datetime', 'site_name')) %>%
            mutate_at(vars(-datetime, -site_name, -Q), ~(. * Q))
    }


    if(input$AGG3 == 'Monthly'){ #otherwise Yearly; conditionals controlled by js

        samplevel = samplevel %>%
            mutate(year = year(datetime))

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

    } else {

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

output$GRAPH_PRECIP3 = renderDygraph({

    data = dataPrecip3()

    if(nrow(data)){

        dydat = xts(data$medianPrecip, order.by=data$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, 'P')
        ymax = max(dydat, na.rm=TRUE)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                # fillAlpha=1, colors='#4b92cc', strokeWidth=3,
                fillAlpha=1, colors=raincolor, strokeWidth=3,
                plotter=hyetograph_js, retainDateWindow=TRUE) %>%
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(dg)
})

output$GRAPH_MAIN3a = renderDygraph({

    changesInSelections3$facetA3
    sites = na.omit(input$SITES3[1:3])
    rainsites = na.omit(input$RAINSITES3[1:3])
    varA = isolate(input$SOLUTES3[1])
    rainvarA = isolate(input$RAINVARS3[1])
    # ss <<- sites
    # rr <<- rainsites
    # vv <<- varA
    # rv <<- rainvarA

    if(input$CONC_FLUX3 == 'VWC'){
        streamwide = volWeighted3()
    } else {
        streamwide = isolate(data3())
    }

    # plot_rain = ifelse(isolate(input$INCONC3) && nrow(rainwide), TRUE, FALSE)

    streamwide = streamwide %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(varA)) %>%
        group_by(datetime, site_name) %>%
        summarize_all(mean, na.rm=TRUE) %>%
        spread(site_name, !!varA) %>%
        data.table()
    # sw <<- streamwide
    # print(streamwide)

    if(isolate(input$INCONC3)){
        # linecolors = c(linecolors, 'green')
        # pp <<- dataPchem3() %>%
        rainwide = dataPchem3() %>%
            filter(site_name %in% rainsites) %>%
            select(datetime, site_name, one_of(rainvarA)) %>%
            spread(site_name, !!rainvarA) %>%
            data.table()
        # rw <<- rainwide

        if(! nrow(rainwide)){
            rainwide = data.table(datetime=streamwide$datetime, x=NA)
            colnames(rainwide)[-1] = rainsites
        }

        # rainwide = data.table(rainwide)
        # streamwide = data.table(streamwide)
        #forward rolling join by datetime
        setkey(rainwide, 'datetime')
        setkey(streamwide, 'datetime')
        allwide = rainwide[streamwide, roll=TRUE]
        allwide = as_tibble(allwide) %>%
            select(datetime, one_of(sites), one_of(rainsites))
        gratuitous_end_roll = streamwide$datetime >
            rainwide$datetime[nrow(rainwide) - 1]
        allwide[gratuitous_end_roll, rainsites] = NA
        # p2 = data.table(pp)
        # w2 = data.table(ww)
        # setkey(p2, 'datetime')
        # setkey(w2, 'datetime')
        # z2 = p2[w2, roll=TRUE]
        # z3 = w2[p2, roll=TRUE]
    } else {
        allwide = as_tibble(streamwide)
    }

    # aa <<- allwide
    # print(allwide)

    if(nrow(allwide)){

        sites = colnames(allwide)[-1]
        dydat = xts(allwide[, sites], order.by=allwide$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, sites)

        if(input$CONC_FLUX3 == 'Flux'){
            gunit = input$FLUX_UNIT3
        } else {
            gunit = ifelse(varA %in% conc_vars, input$CONC_UNIT3,
                grabvars$unit[grabvars$variable_code == varA])
        }
        # gg <<- gunit
        # ss -> sites
        # rr -> rainsites
        # vv -> varA
        # rv -> rainvarA
        # rw -> rainwide
        # sw -> streamwide
        # aa -> allwide
        # gg -> gunit
        ylab1 = glue('{var} ({unit})', var=varA, unit=gunit)

        # if(plot_rain){
        #     linecolors = c('green', linecolors) #expects a color for rain series too
        # }
        is_inst = ifelse(input$AGG3 == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[1:length(sites)], strokeWidth=2, pointSize=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3a') %>%
            dyAxis('y', label=ylab1, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)

        if(input$INCONC3 == TRUE){

            ylab2 = glue('{rv} ({unit})', rv=rainvarA, unit=input$CONC_UNIT3)
            dg = dg %>%
                dyAxis(name='y2', label=ylab2, labelWidth=16, labelHeight=10,
                    pixelsPerLabel=20, rangePad=10, axisLabelColor=raincolorbold,
                    axisLineColor=raincolorbold) %>%
                dySeries(name=rainsites, color=raincolor, axis='y2', drawPoints=FALSE,
                    strokeWidth=2, pointSize=2, strokePattern='dashed')
        }

    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN3b = renderDygraph({

    changesInSelections3$facetB3
    sites = na.omit(input$SITES3[1:3])
    varB = isolate(input$SOLUTES3[2])

    if(length(input$SOLUTES3) > 1){

        if(input$CONC_FLUX3 == 'VWC'){
            widedat = volWeighted3()
        } else {
            widedat = isolate(data3())
        }

        widedat = widedat %>%
            filter(site_name %in% sites) %>%
            select(datetime, site_name, one_of(varB)) %>%
            spread(site_name, !!varB)

    } else {
        widedat = data.frame()
    }

    if(nrow(widedat)){
        sites = colnames(widedat)[-1]
        dydat = xts(widedat[, sites], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, sites)

        if(input$CONC_FLUX3 == 'Flux'){
            gunit = input$FLUX_UNIT3
        } else {
            gunit = ifelse(varB %in% conc_vars, input$CONC_UNIT3,
                grabvars$unit[grabvars$variable_code == varB])
        }
        ylab = glue('{var} ({unit})', var=varB, unit=gunit)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors, strokeWidth=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3b') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN3c = renderDygraph({

    changesInSelections3$facetC3
    sites = na.omit(input$SITES3[1:3])
    varC = isolate(input$SOLUTES3[3])

    if(length(input$SOLUTES3) > 2){

        if(input$CONC_FLUX3 == 'VWC'){
            widedat = volWeighted3()
        } else {
            widedat = isolate(data3())
        }

        widedat = widedat %>%
            filter(site_name %in% sites) %>%
            select(datetime, site_name, one_of(varC)) %>%
            spread(site_name, !!varC)

    } else {
        widedat = data.frame()
    }

    if(nrow(widedat)){

        sites = colnames(widedat)[-1]
        dydat = xts(widedat[, sites], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, sites)

        if(input$CONC_FLUX3 == 'Flux'){
            gunit = input$FLUX_UNIT3
        } else {
            gunit = ifelse(varC %in% conc_vars, input$CONC_UNIT3,
                grabvars$unit[grabvars$variable_code == varC])
        }
        ylab = glue('{var} ({unit})', var=varC, unit=gunit)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors, strokeWidth=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3c') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_FLOW3 = renderDygraph({

    widedat = dataFlow3() %>%
        spread(site_name, Q)

    if(nrow(widedat)){

        sites = colnames(widedat)[-1]
        dydat = xts(widedat[, sites], order.by=widedat$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, sites)
        # dimnames(dydat) = list(NULL, 'Q')

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                # strokeBorderColor='#4b92cc', strokeBorderWidth=1,
                strokeWidth=1, fillAlpha=0.4, retainDateWindow=TRUE,
                colors=linecolors, drawGapEdgePoints=TRUE) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='flow3') %>%
            dyAxis('y', label='Q (L/s)', labelWidth=16, labelHeight=10,
                pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab='Q (L/s)', px_per_lab=10)
    }

    return(dg)
})
