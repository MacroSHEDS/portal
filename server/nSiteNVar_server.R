
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

observeEvent(input$SITES3, {

    grab_subset = filter(grab, site_name %in% input$SITES3)
    grabvars_display_subset = populate_vars(grab_subset[-(1:2)])

    updateSelectizeInput(session, 'SOLUTES3',
        choices=grabvars_display_subset,
        selected=grabvars_display_subset[[1]][[1]])

    # site_dtrng = as.Date(range(grab$datetime[grab$site_name %in% input$SITES3],
    #     na.rm=TRUE))
    #
    # updateSliderInput(session, "DATE3",
    #     label="Date Range", min=site_dtrng[1], max=site_dtrng[2], step=30,
    #     value=c(max(site_dtrng[2] - lubridate::days(365),
    #         site_dtrng[1], na.rm=TRUE),
    #         site_dtrng[2]))
})

## Filter data to desired dates
data3 <- reactive ({

    data3 = if(input$CONC_FLUX3 == 'Concentration') grab else flux
    data3 <- data3 %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% input$SITES3) %>%
        select(one_of("datetime", "site_name", input$SOLUTES3))

    if(init_vals$enable_unitconvert){
        if(input$CONC_FLUX3 == 'Concentration'){
            data3 = convert_conc_units(data3, desired_unit=input$CONC_UNIT3)
        } else if(input$CONC_FLUX3 == 'Flux'){
            data3 = convert_flux_units(data3, desired_unit=input$FLUX_UNIT3)
        }
    }

})

dataPrecip3 <- reactive ({

    dataPrecip3 = P %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% sites_precip) %>%
        select(one_of("datetime", "site", 'precipCatch')) %>%
        # group_by(lubridate::yday(datetime)) %>%
        group_by(datetime) %>%
        summarise(medianPrecip=median(precipCatch, na.rm=TRUE)) %>%
        ungroup()
})

dataFlow3 <- reactive ({

    dataFlow3 = filter(sensor, datetime > input$DATE3[1],
        datetime < input$DATE3[2], site_name %in% input$SITES3) %>%
        # mutate(datetime=as.Date(datetime)) %>%
        select(datetime, Q_Ls, site_name) %>%
        group_by(datetime, site_name) %>%
        summarise(flowMaxPerDate=max(Q_Ls, na.rm=TRUE)) %>%
        ungroup()

    return(dataFlow3)
})

output$GRAPH_PRECIP3 <- renderDygraph({

    data <- dataPrecip3()

    if(nrow(data)){

        dydat = xts(data$medianPrecip, order.by=data$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, 'P')
        ymax = max(dydat, na.rm=TRUE)

        p = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                fillAlpha=1, colors='#4b92cc', strokeWidth=3,
                plotter=hyetograph_js, retainDateWindow=TRUE) %>%
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)
    } else {
        p = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(p)
})

output$GRAPH_MAIN3a <- renderDygraph({

    changesInSelections3$facetA3
    sites = na.omit(input$SITES3[1:3])
    varA = isolate(input$SOLUTES3[1])

    varnames = filter(grabvars, variable_code == varA) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(data3()) %>%
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
        spread(site_name, flowMaxPerDate)

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
