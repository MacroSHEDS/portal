
#keep track of # vars selected, for faceting
changesInSelections4 = reactiveValues()
changesInSelections4$n_vars = 1
changesInSelections4$facetA4 = 0
changesInSelections4$facetB4 = 0
changesInSelections4$facetC4 = 0

observeEvent({
    input$SITES4
    input$DATE4
    input$CONC_FLUX4
    input$CONC_UNIT4
    input$FLUX_UNIT4
}, {
    changesInSelections4$facetA4 = changesInSelections4$facetA4 + 1
    changesInSelections4$facetB4 = changesInSelections4$facetB4 + 1
    changesInSelections4$facetC4 = changesInSelections4$facetC4 + 1
})

observeEvent({
    if(length(input$SOLUTES4) %in% 1:3){
        TRUE
    } else return()
}, {
    changesInSelections4$facetA4 = changesInSelections4$facetA4 + 1
    changesInSelections4$n_vars = length(input$SOLUTES4)
})

observeEvent({
    if(length(input$SOLUTES4) %in% 4:6){
        TRUE
    } else return()
}, {
    changesInSelections4$facetB4 = changesInSelections4$facetB4 + 1
    changesInSelections4$n_vars = length(input$SOLUTES4)
})

observeEvent({
    if(length(input$SOLUTES4) %in% 7:9){
        TRUE
    } else return()
}, {
    changesInSelections4$facetC4 = changesInSelections4$facetC4 + 1
    changesInSelections4$n_vars = length(input$SOLUTES4)
})

observeEvent(input$SITES4, {

    grab_subset = filter(grab, site_code == input$SITES4)
    grabvars_display_subset = filter_dropdown_varlist(grab_subset)

    updateSelectizeInput(session, 'SOLUTES4',
        choices=grabvars_display_subset,
        selected=input$SOLUTES4)

    # site_dtrng = as.Date(range(grab$datetime[grab$site_code == input$SITES4],
    #     na.rm=TRUE))
    #
    # updateSliderInput(session, "DATE4",
    #     label="Date Range", min=site_dtrng[1], max=site_dtrng[2], step=30,
    #     value=c(max(site_dtrng[2] - lubridate::days(365),
    #         site_dtrng[1], na.rm=TRUE),
    #         site_dtrng[2]))
})

data4 <- reactive({

    data4 = if(input$CONC_FLUX4 == 'Concentration') grab else flux
    data4 = data4 |>
        filter(site_code %in% input$SITES4) |>
        filter(datetime >= input$DATE4[1]) |>
        filter(datetime <= input$DATE4[2]) |>
        select(one_of("datetime", "site_code", input$SOLUTES4))

    if(init_vals$enable_unitconvert){
        if(input$CONC_FLUX4 == 'Concentration'){
            data4 = convert_conc_units(data4, desired_unit=input$CONC_UNIT4)
        } else if(input$CONC_FLUX4 == 'Flux'){
            data4 = convert_flux_units(data4, desired_unit=input$FLUX_UNIT4)
        }
    }

    return(data4)
})

dataPrecip4 <- reactive({

    dataPrecip4 = P |>
        filter(datetime >= input$DATE4[1]) |>
        filter(datetime <= input$DATE4[2]) |>
        filter(site_code %in% sites_with_P) |>
        select(one_of("datetime", "site_code", 'P')) |>
        # group_by(lubridate::yday(datetime)) |>
        group_by(datetime) |>
        summarise(medianPrecip=median(P, na.rm=TRUE)) |>
        ungroup()
})

dataFlow4 <- reactive({

    if (input$FLOW_SOURCE4 == "flowSens") {
        dataFlow4 = filter(Q, datetime > input$DATE4[1],
                datetime < input$DATE4[2],
                site_code %in% input$SITES4) |>
            select(datetime, Q) |>
            group_by(datetime) |>
            summarise(flowMaxPerDate=max(Q, na.rm=TRUE)) |>
            ungroup()
    }

    dataFlow4
})

output$GRAPH_PRECIP4 <- renderDygraph({

    data <- dataPrecip4()

    if(nrow(data)){

        dydat = xts(data$medianPrecip, order.by=data$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, 'P')
        ymax = max(dydat, na.rm=TRUE)

        p = dygraph(dydat, group='oneSiteNVar') |>
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                fillAlpha=1, colors='#4b92cc', strokeWidth=3,
                plotter=hyetograph_js) |>
            dyAxis('y', label='P (mm)', valueRange=c(ymax + ymax * 0.1, 0),
                labelWidth=16, labelHeight=10, pixelsPerLabel=10,
                rangePad=10)
    } else {
        p = plot_empty_dygraph(isolate(input$DATE4), plotgroup='oneSiteNVar',
            ylab='P (in)', px_per_lab=10)
    }

    return(p)
})

output$GRAPH_MAIN4a <- renderDygraph({

    changesInSelections4$facetA4
    n_vars = isolate(changesInSelections4$n_vars)
    plotvars = isolate(input$SOLUTES4)[1:min(c(n_vars, 3))]

    varnames = filter(grabvars, variable_code %in% plotvars) |>
        mutate(combined=case_when(! variable_code %in% conc_vars ~
                paste0(variable_name, ' (', unit, ')'),
            variable_code %in% conc_vars & input$CONC_FLUX4 == 'Concentration' ~
                paste0(variable_name, ' (', input$CONC_UNIT4, ')'),
            variable_code %in% conc_vars & input$CONC_FLUX4 == 'Flux' ~
                paste0(variable_name, ' (', input$FLUX_UNIT4, ')'))) |>
        select(variable_name, unit, combined)

    # widedat = isolate(dataMain4()) |>
    widedat = isolate(data4()) |>
        # filter(solute %in% plotvars) |>
        select(datetime, one_of(plotvars))

    if(nrow(widedat)){

        dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, varnames$combined)

        dg = dygraph(dydat, group='oneSiteNVar') |>
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors, strokeWidth=2) |> #, pointSize=2) |>
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main4a') |>
            dyAxis('y', label=NULL, pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE4), plotgroup='oneSiteNVar',
            ylab='', px_per_lab=20)
    }

    return(dg)

    # ordsOfMag = apply(select_if(data, is.numeric), 2, function(x) {
    #     rng = max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    #     log10_ceiling(rng)
    # })

    # if(length(datal) > 1){
    #     for(i in 2:length(datal)){
    #         data = datal[[i]]
    #
    #         for(pv in plotvars){
    #             dydat = xts(data[, pv], order.by=data$datetime)
    #             dimnames(dydat) = list(NULL, varnames$combined)
    #
    #             m = m |>
    #                 dySeries('')
    #         }
    #
    #     }
    # }
})

output$GRAPH_MAIN4b <- renderDygraph({

    if(! init_vals$enable_facets){
        return()
    }

    changesInSelections4$facetB4
    n_vars = isolate(changesInSelections4$n_vars)
    plotvars = isolate(input$SOLUTES4)[4:min(c(n_vars, 6))]

    varnames = filter(grabvars, variable_code %in% plotvars) |>
        mutate(combined=case_when(! variable_code %in% conc_vars ~
                paste0(variable_name, ' (', unit, ')'),
            variable_code %in% conc_vars & input$CONC_FLUX4 == 'Concentration' ~
                paste0(variable_name, ' (', input$CONC_UNIT4, ')'),
            variable_code %in% conc_vars & input$CONC_FLUX4 == 'Flux' ~
                paste0(variable_name, ' (', input$FLUX_UNIT4, ')'))) |>
        select(variable_name, unit, combined)

    widedat = isolate(data4()) |>
        select(datetime, one_of(plotvars))

    if(nrow(widedat)){

        dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, varnames$combined)

        dg = dygraph(dydat, group='oneSiteNVar') |>
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors, strokeWidth=2) |>
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main4b') |>
            dyAxis('y', label=NULL, pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE4), plotgroup='oneSiteNVar',
            ylab='', px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_MAIN4c <- renderDygraph({

    if(! init_vals$enable_facets){
        return()
    }

    changesInSelections4$facetC4
    n_vars = isolate(changesInSelections4$n_vars)
    plotvars = isolate(input$SOLUTES4)[7:min(c(n_vars, 9))]

    input = list(CONC_FLUX4='Concentration', CONC_UNIT4='ug/L')
    varnames = filter(grabvars, variable_code %in% plotvars) |>
        mutate(combined=case_when(! variable_code %in% conc_vars ~
                paste0(variable_name, ' (', unit, ')'),
            variable_code %in% conc_vars & input$CONC_FLUX4 == 'Concentration' ~
                paste0(variable_name, ' (', input$CONC_UNIT4, ')'),
            variable_code %in% conc_vars & input$CONC_FLUX4 == 'Flux' ~
                paste0(variable_name, ' (', input$FLUX_UNIT4, ')'))) |>
        select(variable_name, unit, combined)

    widedat = isolate(data4()) |>
        select(datetime, one_of(plotvars))

    if(nrow(widedat)){

        dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, varnames$combined)

        dg = dygraph(dydat, group='oneSiteNVar') |>
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors, strokeWidth=2) |>
            dyLegend(show='always', labelsSeparateLines=FALSE,
                labelsDiv='main4c') |>
            dyAxis('y', label=NULL, pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE4), plotgroup='oneSiteNVar',
            ylab='', px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_FLOW4 <- renderDygraph({

    widedat <- dataFlow4()

    if(nrow(widedat)){

        dydat = xts(widedat[, 'flowMaxPerDate'], order.by=widedat$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, 'Q')

        dg = dygraph(dydat, group='oneSiteNVar') |>
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                colors='#4b92cc', strokeWidth=2, fillAlpha=0.25) |>
            dyAxis('y', label='Q (L/s)', labelWidth=16, labelHeight=10,
                pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE4), plotgroup='oneSiteNVar',
            ylab='Q (L/s)', px_per_lab=10)
    }

    return(dg)
})
