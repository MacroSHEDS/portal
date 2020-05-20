
#govern the appearance and disappearance of facets ####
changesInSelections3 = reactiveValues()
changesInSelections3$facetA3 = 0
changesInSelections3$facetB3 = 0
changesInSelections3$facetC3 = 0

observeEvent({
    input$DATE3
    input$CONC_FLUX3
    input$CONC_UNIT3
    input$FLUX_UNIT3
    input$AGG3
    input$INCONC3
    # input$RAINSITES3
    # input$RAINVARS3
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

get_domains3 = eventReactive(input$DOMAINS3, {

    domain = input$DOMAINS3

    updateSelectizeInput(session, 'SITES3',
        choices=sitelist_from_domain(domain, site_type='stream_gauge'),
        selected=default_sites_by_domain[[domain]],
        options=list(maxItems=3))

    return(domain)
})

# get_sites3 = eventReactive(input$SITES3, {
#
#     sites = input$SITES3
#
#     # updateSelectizeInput(session, 'SOLUTES3',
#     #     choices=,
#     #     selected=,
#     #     options=list(maxItems=3))
#
#     return(sites)
# })

grab = eventReactive(input$SITES3, {

    domain = get_domains3()

    if(is.null(domain)){ #for empty domain dropdown

        rd = init_vals$recent_domain
        grab = read_feather(glue('data/{d}/chemistry/{s}.feather',
            d=rd, s=default_sites_by_domain[[rd]]))

        domain = 'hbef'

    } else {

        grab = tibble()
        for(s in input$SITES3){
            grab = read_feather(glue('data/{d}/chemistry/{s}.feather',
                    d=domain, s=s)) %>%
                bind_rows(grab)
        }
    }

    init_vals$recent_domain = domain

    # test commented. is next line useful?
    # grab = filter(grab, site_name %in% sites_with_Q)

    #test commented this too
    # new_sitelist = site_data %>%
    #     filter(domain == get_domains3(), site_type == 'stream_gauge') %>%
    #     pull(site_name)
    #
    # updateSelectizeInput(session, 'SITES3', choices=new_sitelist,
    #     selected=default_sites_by_domain[[domain]])

    # dt_extent = changesInSelections3$dt_extent
    # changesInSelections3$dt_extent = range(c(dt_extent, input$DATE3))

    return(grab)
})

pchem = eventReactive(input$SITES3, {

    domain = get_domains3()

    if(is.null(domain)){
        pchem = read_feather(glue('data/{d}/pchem.feather',
            d=init_vals$recent_domain))
    } else {
        pchem = read_feather(glue('data/{d}/pchem.feather', d=domain))
    }

    # init_vals$recent_domain = domain

    return(pchem)
})

flux = eventReactive(input$SITES3, {

    domain = get_domains3()

    if(is.null(domain)){
        rd = init_vals$recent_domain
        flux = read_feather(glue('data/{d}/flux/{s}.feather',
            d=rd, s=default_sites_by_domain[[rd]]))
    } else {
        flux = tibble()
        for(s in input$SITES3){
            flux = read_feather(glue('data/{d}/flux/{s}.feather',
                    d=domain, s=s)) %>%
                bind_rows(flux)
        }
    }

    return(flux)
})

P = eventReactive(input$SITES3, {

    domain = get_domains3()

    if(is.null(domain)){
        P = read_feather(glue('data/{d}/precip.feather',
            d=init_vals$recent_domain))
    } else {
        P = read_feather(glue('data/{d}/precip.feather', d=domain))
    }

    return(P)
})

Q = eventReactive(input$SITES3, {

    domain = get_domains3()

    if(is.null(domain)){
        rd = init_vals$recent_domain
        Q = read_feather(glue('data/{d}/discharge/{s}.feather',
            d=rd, s=default_sites_by_domain[[rd]]))
    } else {
        Q = tibble()
        for(s in input$SITES3){
            Q = read_feather(glue('data/{d}/discharge/{s}.feather',
                d=domain, s=s)) %>%
                bind_rows(Q)
        }
    }

    return(Q)
})

observe({

    # grab_subset = filter(grab(), site_name %in% default_sites_by_domain[[get_domains3()]])
    grabvars_display_subset = populate_display_vars(grab()[, -(1:2)])

    selected = unname(unlist(grabvars_display_subset)[1])

    updateSelectizeInput(session, 'SOLUTES3',
        choices=grabvars_display_subset, selected=selected)
})

observe({

    dmn = get_domains3()
    dmnsites = sites_with_pchem[[dmn]]

    updateSelectizeInput(session, 'RAINSITES3', choices=dmnsites,
        selected=dmnsites[1])

    pchem_subset = filter(pchem(), site_name %in% dmnsites[1])
    pchemvars_display_subset = populate_display_vars(pchem_subset[-(1:2)],
        vartype='precip')

    selected = unname(unlist(pchemvars_display_subset)[1])

    updateSelectizeInput(session, 'RAINVARS3',
        choices=pchemvars_display_subset, selected=selected)
})

data3 = reactive({

    data3 = if(input$CONC_FLUX3 == 'Flux') flux() else grab()

    if(nrow(data3) == 0) return(data3)

    data3 = data3 %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        # filter(site_name %in% input$SITES3) %>%
        select(one_of("datetime", "site_name", input$SOLUTES3))

    if(nrow(data3) == 0) return(data3)

    data3 = pad_ts3(data3, input$SITES3, input$SOLUTES3, input$DATE3)

    if(input$AGG3 != 'Instantaneous'){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')

        data3 = data3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name)

        if(input$CONC_FLUX3 == 'VWC'){
            data3 = data3 %>%
                summarize_all(list(~sum(., na.rm=TRUE))) %>%
                ungroup()
        } else {
            data3 = data3 %>%
                summarize_all(list(~mean(., na.rm=TRUE))) %>%
                ungroup()
        }
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

    # ppp <<- pchem() %>%
    #     filter(datetime >= input$DATE3[1]) %>%
    #     filter(datetime <= input$DATE3[2])
    pchem3 = pchem() %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        # filter(site_name %in% input$SITES3) %>%
        # pchem3 = ppp %>%
        group_by(datetime) %>%
        summarize_at(vars(-site_name), ~mean(., na.rm=TRUE)) %>%
        ungroup() %>%
        select(one_of('datetime', input$SOLUTES3))
        # filter(site_name %in% input$RAINSITES3) %>%
        # select(one_of("datetime", "site_name", input$RAINVARS3))
    # ppp <<- pchem3

    if(nrow(pchem3) == 0) return(pchem3)
    # si <<- input$SITES3
    # so <<- input$SOLUTES3
    # da <<- input$DATE3
    # tsdf = ppp; sites=NULL; vars=so; datebounds=da

    pchem3 = pad_ts3(pchem3, sites=NULL, vars=input$SOLUTES3,
        datebounds=input$DATE3)
    # pchem3 = pad_ts3(pchem3, input$RAINSITES3, input$RAINVARS3, input$DATE3)

    agg_period = switch(input$AGG3, 'Monthly'='month', 'Yearly'='year')

    pchem3 = pchem3 %>%
        group_by(datetime=floor_date(datetime, agg_period))

    if(input$CONC_FLUX3 == 'VWC'){
        pchem3 = pchem3 %>%
            summarize_all(list(~sum(., na.rm=TRUE))) %>%
            ungroup()
    } else {
        pchem3 = pchem3 %>%
            summarize_all(list(~mean(., na.rm=TRUE))) %>%
            ungroup()
    }

    if(init_vals$enable_unitconvert){
        pchem3 = convert_conc_units(pchem3, desired_unit=input$CONC_UNIT3)
    }#temporary: modify the above when RAIN_UNIT3 and PCHEM_CONC_FLUX3 exist

    #artificially extend pchem dataset to represent each individual watershed
    # nsites = length(input$SITES3)
    # if(nsites > 1){
    #
    #     pchem3$site_name = input$SITES3[1]
    #     pcopy = pchem3
    #
    #     for(i in 2:nsites){
    #         pcopy$site_name = input$SITES3[i]
    #         pchem3 = bind_rows(pchem3, pcopy)
    #     }
    #
    # } else {
    #     pchem3$site_name = input$SITES3
    # }
    pchem3$site_name = paste(isolate(get_domains3()), 'pchem')

    return(pchem3)

})

dataPrecip3 = reactive({

    dp3 = P()
    show_precip = nrow(dp3) > 1
    dataPrecip3 = dp3 %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% sites_with_P[[get_domains3()]]) %>% #superfluous?
        select(one_of("datetime", "site_name", 'precip'))
        # mutate(datetime=as.POSIXct(datetime))

    dataPrecip3 = pad_ts3(dataPrecip3, unique(dataPrecip3$site_name),
        'precip', input$DATE3)

    if(input$AGG3 != 'Instantaneous' & show_precip){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')
        dataPrecip3 = dataPrecip3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
            summarize_all(list(~mean(., na.rm=TRUE))) %>%
            ungroup()
    }

    dataPrecip3 = dataPrecip3 %>%
        group_by(datetime) %>%
        summarize(sumPrecip=sum(precip, na.rm=TRUE),
            medianPrecip=median(precip, na.rm=TRUE)) %>%
        ungroup()
})

dataFlow3 = reactive ({

    dataFlow3 = Q() %>%
        filter(datetime > input$DATE3[1], datetime < input$DATE3[2]) %>%
        #, site_name %in% input$SITES3) %>%
        select(datetime, site_name, Q)

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

    samplevel = data3() %>%
        left_join(dataFlow3(), by=c('datetime', 'site_name')) %>%
        mutate_at(vars(-datetime, -site_name, -Q), ~(. * Q))

    if(input$AGG3 == 'Monthly'){ #otherwise Yearly; conditionals controlled by js

        samplevel = samplevel %>%
            mutate(year=year(datetime))

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

volWeightedPrecip3 = reactive({

    # samplevel = dataPchem3()
    samplevel = dataPchem3()
    # pp = dataPrecip3()
    # d <<- samplevel
    # d -> samplevel
    # pp <<- pp
    # input = list(SITES3=c('W1'), SOLUTES3=c('Cl'))
    # site_df = tibble(site_name=input$

    # artificially extend pchem dataset to represent each individual watershed
    nsites = length(input$SITES3)
    if(nsites > 1){

        samplevel$site_name = input$SITES3[1]
        dcopy = samplevel

        for(i in 2:nsites){
            dcopy$site_name = input$SITES3[i]
            samplevel = bind_rows(samplevel, dcopy)
        }

    } else {
        samplevel$site_name = input$SITES3
    }

    samplevel = samplevel %>%
        left_join(select(dataPrecip3(), -medianPrecip), by='datetime') %>%
        # left_join(select(pp, -medianPrecip), by='datetime') %>%
        # group_by(datetime) %>%
        # summarize_at(vars(-site_name), mean(., na.rm=TRUE)) %>%
        # ungroup() %>%
        left_join(site_data, by='site_name') %>%
        mutate(precipVol=sumPrecip * ws_area_ha) %>%
        # mutate_at(vars(-datetime, -site_name, -precipVol, -sumPrecip, -ws_area_ha),
        mutate_at(vars(one_of(isolate(input$SOLUTES3))), ~(. * precipVol)) %>%
        select(datetime, site_name, one_of(isolate(input$SOLUTES3)),
            sumPrecip, ws_area_ha) %>%
        select(-ws_area_ha) %>%
        # rename_at(vars(one_of(input$SOLUTES3)), ~paste0('p_', .)) %>%
        rename(P=sumPrecip)

    # samplevel$site_name = paste(isolate(get_domains3()), 'pchem')

    if(input$AGG3 == 'Monthly'){ #otherwise Yearly; conditionals controlled by js

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

    } else {

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

output$GRAPH_PRECIP3 = output$GRAPH_PRECIP3EXP = renderDygraph({

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

output$GRAPH_MAIN3a = output$GRAPH_MAIN3aEXP = renderDygraph({

    changesInSelections3$facetA3
    sites = na.omit(input$SITES3[1:3])
    varA = isolate(input$SOLUTES3[1])

    # ss <<- input$SOLUTES3
    if(input$CONC_FLUX3 == 'VWC'){
        streamwide = volWeighted3()
    } else {
        streamwide = isolate(data3())
    }

    # ss <<- streamwide
    # vv <<- varA
    streamwide = streamwide %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(varA)) %>%
        group_by(datetime, site_name) %>%
        summarize_all(mean, na.rm=TRUE) %>%
        spread(site_name, !!varA) %>%
        data.table()
    # ss -> streamwide
    # cc <<- input$CONC_UNIT3
    # aa <<- input$AGG3

    if(isolate(input$INCONC3)){

        if(input$CONC_FLUX3 == 'VWC'){
            rainwide = volWeightedPrecip3()
        } else {
            rainwide = isolate(dataPchem3())
        }

        rainwide = rainwide %>%
            select(datetime, site_name, one_of(varA)) %>%
            spread(site_name, !!varA) %>%
            rename_at(vars(-one_of('datetime', 'hbef pchem')),
                ~paste0('P_', .)) %>%
            data.table()

        if(input$CONC_FLUX3 == 'VWC'){
            rainsites = colnames(rainwide)[-1]
        } else {
            rainsites = paste(isolate(get_domains3()), 'pchem')
        }

        if(! nrow(rainwide)){
            rainwide = tibble(datetime=streamwide$datetime)
            rainwide[rainsites] = NA
            rainwide = as.data.table(rainwide)
        }

        allwide = rolljoin(rainwide, streamwide, rainsites, sites)

    } else {
        allwide = as_tibble(streamwide)
    }

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

        ylab = glue('{var} ({unit})', var=varA, unit=gunit)

        is_inst = ifelse(input$AGG3 == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[1:length(sites)], strokeWidth=2, pointSize=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3a') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)

        if(input$INCONC3 == TRUE){

            if(input$CONC_FLUX3 == 'Concentration'){

                dg = dg %>%
                    dySeries(name=rainsites[1], color=raincolor, axis='y',
                        drawPoints=FALSE, strokeWidth=2, pointSize=2,
                        strokePattern='dashed')

            } else {

                for(i in 1:length(rainsites)){
                    dg = dg %>%
                        dySeries(name=rainsites[i], color=pchemcolors[i],
                            axis='y', drawPoints=FALSE, strokeWidth=2,
                            pointSize=2, strokePattern='dashed')
                }
            }
        }

    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN3b = output$GRAPH_MAIN3bEXP = renderDygraph({

    changesInSelections3$facetB3
    sites = na.omit(input$SITES3[1:3])
    varB = isolate(input$SOLUTES3[2])
    if(is.na(varB)) return()

    if(input$CONC_FLUX3 == 'VWC'){
        streamwide = volWeighted3()
    } else {
        streamwide = isolate(data3())
    }

    streamwide = streamwide %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(varB)) %>%
        group_by(datetime, site_name) %>%
        summarize_all(mean, na.rm=TRUE) %>%
        spread(site_name, !!varB) %>%
        data.table()

    if(isolate(input$INCONC3)){

        if(input$CONC_FLUX3 == 'VWC'){
            rainwide = volWeightedPrecip3()
        } else {
            rainwide = isolate(dataPchem3())
        }

        rainwide = rainwide %>%
            select(datetime, site_name, one_of(varB)) %>%
            spread(site_name, !!varB) %>%
            rename_at(vars(-one_of('datetime', 'hbef pchem')),
                ~paste0('P_', .)) %>%
            data.table()

        if(input$CONC_FLUX3 == 'VWC'){
            rainsites = colnames(rainwide)[-1]
        } else {
            rainsites = paste(isolate(get_domains3()), 'pchem')
        }

        if(! nrow(rainwide)){
            rainwide = tibble(datetime=streamwide$datetime)
            rainwide[rainsites] = NA
            rainwide = as.data.table(rainwide)
        }

        allwide = rolljoin(rainwide, streamwide, rainsites, sites)

    } else {
        allwide = as_tibble(streamwide)
    }

    if(nrow(allwide)){

        sites = colnames(allwide)[-1]
        dydat = xts(allwide[, sites], order.by=allwide$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, sites)

        if(input$CONC_FLUX3 == 'Flux'){
            gunit = input$FLUX_UNIT3
        } else {
            gunit = ifelse(varB %in% conc_vars, input$CONC_UNIT3,
                grabvars$unit[grabvars$variable_code == varB])
        }

        ylab = glue('{var} ({unit})', var=varB, unit=gunit)

        is_inst = ifelse(input$AGG3 == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[1:length(sites)], strokeWidth=2, pointSize=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3b') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)

        if(isolate(input$INCONC3) == TRUE){

            if(input$CONC_FLUX3 == 'Concentration'){

                dg = dg %>%
                    dySeries(name=rainsites[1], color=raincolor, axis='y',
                        drawPoints=FALSE, strokeWidth=2, pointSize=2,
                        strokePattern='dashed')

            } else {

                for(i in 1:length(rainsites)){
                    dg = dg %>%
                        dySeries(name=rainsites[i], color=pchemcolors[i],
                            axis='y', drawPoints=FALSE, strokeWidth=2,
                            pointSize=2, strokePattern='dashed')
                }
            }
        }

    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN3c = output$GRAPH_MAIN3cEXP = renderDygraph({

    changesInSelections3$facetC3
    sites = na.omit(input$SITES3[1:3])
    varC = isolate(input$SOLUTES3[3])
    # print(varC)
    if(is.na(varC)) return()

    if(input$CONC_FLUX3 == 'VWC'){
        streamwide = volWeighted3()
    } else {
        streamwide = isolate(data3())
    }

    streamwide = streamwide %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(varC)) %>%
        group_by(datetime, site_name) %>%
        summarize_all(mean, na.rm=TRUE) %>%
        spread(site_name, !!varC) %>%
        data.table()

    if(isolate(input$INCONC3)){

        if(input$CONC_FLUX3 == 'VWC'){
            rainwide = volWeightedPrecip3()
        } else {
            rainwide = isolate(dataPchem3())
        }

        rainwide = rainwide %>%
            select(datetime, site_name, one_of(varC)) %>%
            spread(site_name, !!varC) %>%
            rename_at(vars(-one_of('datetime', 'hbef pchem')),
                ~paste0('P_', .)) %>%
            data.table()

        if(input$CONC_FLUX3 == 'VWC'){
            rainsites = colnames(rainwide)[-1]
        } else {
            rainsites = paste(isolate(get_domains3()), 'pchem')
        }

        if(! nrow(rainwide)){
            rainwide = tibble(datetime=streamwide$datetime)
            rainwide[rainsites] = NA
            rainwide = as.data.table(rainwide)
        }

        allwide = rolljoin(rainwide, streamwide, rainsites, sites)

    } else {
        allwide = as_tibble(streamwide)
    }

    if(nrow(allwide)){

        sites = colnames(allwide)[-1]
        dydat = xts(allwide[, sites], order.by=allwide$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, sites)

        if(input$CONC_FLUX3 == 'Flux'){
            gunit = input$FLUX_UNIT3
        } else {
            gunit = ifelse(varC %in% conc_vars, input$CONC_UNIT3,
                grabvars$unit[grabvars$variable_code == varC])
        }

        ylab = glue('{var} ({unit})', var=varC, unit=gunit)

        is_inst = ifelse(input$AGG3 == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[1:length(sites)], strokeWidth=2, pointSize=2,
                retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3c') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)

        if(isolate(input$INCONC3) == TRUE){

            if(input$CONC_FLUX3 == 'Concentration'){

                dg = dg %>%
                    dySeries(name=rainsites[1], color=raincolor, axis='y',
                        drawPoints=FALSE, strokeWidth=2, pointSize=2,
                        strokePattern='dashed')

            } else {

                for(i in 1:length(rainsites)){
                    dg = dg %>%
                        dySeries(name=rainsites[i], color=pchemcolors[i],
                            axis='y', drawPoints=FALSE, strokeWidth=2,
                            pointSize=2, strokePattern='dashed')
                }
            }
        }

    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_FLOW3 = output$GRAPH_FLOW3EXP = renderDygraph({

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
