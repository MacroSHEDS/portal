
#this one keeps track of n vars/sites selected, for faceting
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
    grab = read_feather(glue('data/{dmn}/grab.feather', dmn=domain)) %>%
        filter(datetime < as.Date('2013-01-01')) %>% #temporary
        filter(site_name %in% sites_with_Q) #temporary
})

flux = reactive({
    domain = input$DOMAINS3
    flux = read_feather(glue('data/{dmn}/flux.feather', dmn=domain)) %>%
        rename(datetime=date) %>% #temporary
        select(-Q_Ld) %>% #temporary
        filter(datetime < as.Date('2013-01-01')) #temporary
})

P = reactive({
    domain = input$DOMAINS3
    P = read_feather(glue('data/{dmn}/precip.feather', dmn=domain)) %>%
        filter(datetime < as.Date('2013-01-01')) #temporary
})

Q = reactive({
    domain = input$DOMAINS3
    Q = read_feather(glue('data/{dmn}/discharge.feather', dmn=domain)) %>%
        filter(datetime < as.Date('2013-01-01')) #temporary
})

observe({

    grab_subset = filter(grab(), site_name %in% input$SITES3)
    grabvars_display_subset = populate_vars(grab_subset[-(1:2)])

    updateSelectizeInput(session, 'SOLUTES3',
        choices=grabvars_display_subset,
        selected=grabvars_display_subset[[1]][[1]])
})

data3 <- reactive({

    data3 = if(input$CONC_FLUX3 == 'Flux') flux() else grab()
    data3 <- data3 %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% input$SITES3) %>%
        select(one_of("datetime", "site_name", input$SOLUTES3))

    if(input$AGG3 != 'Instantaneous'){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')
        data3 = data3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
            summarize_all(list(~mean(., na.rm=FALSE)))
            # ungroup()
    }

    if(init_vals$enable_unitconvert){
        if(input$CONC_FLUX3 == 'Concentration'){
            data3 = convert_conc_units(data3, desired_unit=input$CONC_UNIT3)
        } else if(input$CONC_FLUX3 == 'Flux'){
            data3 = convert_flux_units(data3, desired_unit=input$FLUX_UNIT3)
        }
    }

    return(data3)

})

dataPrecip3 <- reactive({

    dataPrecip3 = P() %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% sites_precip) %>%
        select(one_of("datetime", "site_name", 'P'))

    if(input$AGG3 != 'Instantaneous'){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')
        dataPrecip3 = dataPrecip3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
            summarize_all(list(~mean(., na.rm=FALSE))) %>%
            ungroup()
    }

    dataPrecip3 = dataPrecip3 %>%
        # group_by(lubridate::yday(datetime)) %>%
        group_by(datetime) %>%
        summarise(medianPrecip=median(P, na.rm=TRUE)) %>%
        ungroup()
})

dataFlow3 <- reactive ({

    dataFlow3 = Q() %>%
        filter(datetime > input$DATE3[1],
        datetime < input$DATE3[2], site_name %in% input$SITES3) %>%
        select(datetime, Q, site_name)

    if(input$AGG3 != 'Instantaneous'){
        agg_period = switch(input$AGG3, 'Daily'='day', 'Monthly'='month',
            'Yearly'='year')
        dataFlow3 = dataFlow3 %>%
            group_by(datetime=floor_date(datetime, agg_period), site_name) %>%
            summarize_all(list(~max(., na.rm=FALSE))) %>%
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

    samplevel =  data3() %>%
    # samplevel = dd %>%
        full_join(dataFlow3(), by=c('datetime', 'site_name')) %>%
        # full_join(ff, by=c('datetime', 'site_name')) %>%
        mutate_at(vars(-datetime, -site_name, -Q), ~(. * Q))

    if(input$AGG3 == 'Monthly'){ #otherwise Yearly; flow controlled by js

        samplevel = samplevel %>%
            mutate(year = year(datetime))

        agglevel = samplevel %>%
            select(site_name, year, Q) %>%
            group_by(year, site_name) %>%
            summarize(Qsum=sum(Q, na.rm=TRUE))

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

output$GRAPH_PRECIP3 <- renderDygraph({

    data = dataPrecip3()

    if(nrow(data)){

        dydat = xts(data$medianPrecip, order.by=data$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, 'P')
        ymax = max(dydat, na.rm=TRUE)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                fillAlpha=1, colors='#4b92cc', strokeWidth=3,
                plotter=hyetograph_js, retainDateWindow=TRUE) %>%
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(dg)
})

output$GRAPH_MAIN3a <- renderDygraph({

    changesInSelections3$facetA3
    sites = na.omit(input$SITES3[1:3])
    varA = isolate(input$SOLUTES3[1])

    varnames = filter(grabvars, variable_code == varA) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    if(input$CONC_FLUX3 == 'VWC'){
        widedat = volWeighted3()
    } else {
        widedat = isolate(data3())
    }

    widedat = widedat %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(varA)) %>%
        spread(site_name, !!varA)

    if(nrow(widedat)){

        sites = colnames(widedat)[-1]
        dydat = xts(widedat[, sites], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, sites)

        if(input$CONC_FLUX3 == 'Flux'){
            gunit = input$FLUX_UNIT3
        } else {
            gunit = ifelse(varA %in% conc_vars, input$CONC_UNIT3,
                grabvars$unit[grabvars$variable_code == varA])
        }
        ylab = glue('{var} ({unit})', var=varA, unit=gunit)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors, strokeWidth=2,
                retainDateWindow=TRUE, connectSeparatedPoints=TRUE) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main3a') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=ylab, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN3b <- renderDygraph({

    changesInSelections3$facetB3
    sites = na.omit(input$SITES3[1:3])
    varB = isolate(input$SOLUTES3[2])

    varnames = filter(grabvars, variable_code == varB) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(data3()) %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(varB)) %>%
        spread(site_name, !!varB)

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
                retainDateWindow=TRUE, connectSeparatedPoints=TRUE) %>%
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

output$GRAPH_MAIN3c <- renderDygraph({

    changesInSelections3$facetC3
    sites = na.omit(input$SITES3[1:3])
    varC = isolate(input$SOLUTES3[3])

    varnames = filter(grabvars, variable_code == varC) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(data3()) %>%
        filter(site_name %in% sites) %>%
        select(datetime, site_name, one_of(varC)) %>%
        spread(site_name, !!varC)

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
                retainDateWindow=TRUE, connectSeparatedPoints=TRUE) %>%
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

output$GRAPH_FLOW3 <- renderDygraph({

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
                colors=linecolors, connectSeparatedPoints=TRUE) %>%
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
