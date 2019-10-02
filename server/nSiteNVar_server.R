
#this one keeps track of n vars/sites selected, for faceting
changesInSelections3 = reactiveValues()
changesInSelections3$facetA3 = 0
changesInSelections3$facetB3 = 0
changesInSelections3$facetC3 = 0

observeEvent(input$DATE3, {
        changesInSelections3$facetA3 = changesInSelections3$facetA3 + 1
        changesInSelections3$facetB3 = changesInSelections3$facetB3 + 1
        changesInSelections3$facetC3 = changesInSelections3$facetC3 + 1
})

observeEvent({
    if(length(input$SITES3) == 1){
        TRUE
    } else return()
}, {
    changesInSelections3$facetA3 = changesInSelections3$facetA3 + 1
})

observeEvent({
    if(length(input$SITES3) == 2){
        TRUE
    } else return()
}, {
    changesInSelections3$facetB3 = changesInSelections3$facetB3 + 1
})

observeEvent({
    if(length(input$SITES3) == 3){
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

    data3 <- grab %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2])
})

## Extract data for Precip plot
dataPrecip3 <- reactive ({

    # dataPrecip3 <- data3() %>%
    #     select(precipCatch) %>%
    #     filter(! is.na(precipCatch)) %>%
    #     select(one_of("datetime", "site_name", 'precipCatch')) %>%
    #     group_by(lubridate::days(datetime)) %>%
    #     summarise(medianPrecip = median(precipCatch, na.rm=TRUE)) %>%
    #     ungroup()

    dataPrecip3 <- data3() %>%
        filter(site_name %in% sites_precip) %>%
        select(one_of("datetime", "site", 'precipCatch')) %>%
        # group_by(lubridate::yday(datetime)) %>%
        group_by(datetime) %>%
        summarise(medianPrecip=median(precipCatch, na.rm=TRUE)) %>%
        ungroup()
})

## Extract data for Solutes (Main) plot
dataMain3 <- reactive ({

    dataMain3 <- data3() %>%
        filter(site_name %in% input$SITES3) %>%
        select(one_of("datetime", "site_name", input$SOLUTES3)) %>%
        group_by(datetime, site_name) %>%
        gather(key = solute, value = solute_value, -site_name, -datetime)
})

## Extract data for Discharge (Flow) plot
dataFlow3 <- reactive ({

    dataFlow3 <- data3() %>%
        filter(site_name %in% input$SITES3)

    dataFlow3 <- dataFlow3 %>%
        select(one_of("datetime", 'flowGageHt')) %>%
        group_by(datetime) %>%
        summarise(flowMaxPerDate = max(flowGageHt, na.rm=TRUE))

    # if (SENSORFLOW) {
    #     dataFlow3 = filter(dataSensor, datetime > input$DATE3[1],
    #         datetime < input$DATE3[2], watershedID %in% input$SITES3) %>%
    #         mutate(datetime=as.Date(datetime)) %>%
    #         select(datetime, Q_Ls) %>%
    #         group_by(datetime) %>%
    #         summarise(flowMaxPerDate = max(Q_Ls, na.rm=TRUE))
    # }

    return(dataFlow3)
})

output$GRAPH_PRECIP3 <- renderDygraph({

    data <- dataPrecip3()
    # ind_col <- which(input$PRECIP_SOURCE3 == colnames(data), arr.ind = TRUE)
    dydat = xts(data$medianPrecip, order.by=data$datetime, tzone='UTC')
    dimnames(dydat) = list(NULL, 'P')
    ymax = max(dydat, na.rm=TRUE)

    p = dygraph(dydat, group='nSiteNVar') %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
            fillAlpha=1, colors='#4b92cc', strokeWidth=3,
            plotter=hyetograph_js,
            retainDateWindow=TRUE) %>%
        dyAxis('y', label='Daily mean precip (in.)',
            valueRange=c(ymax + ymax * 0.1, 0),
            labelWidth=16, labelHeight=10)

    return(p)
})

output$GRAPH_MAIN3a <- renderDygraph({

    changesInSelections3$facetA3
    plotvars = na.omit(input$SOLUTES3[1:3])
    siteA = isolate(input$SITES3[1])

    varnames = filter(grabvars, variable_code %in% plotvars) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(dataMain3()) %>%
        filter(site_name == siteA, solute %in% plotvars) %>%
        group_by(datetime, solute) %>%
        summarize(solute_value=mean(solute_value)) %>%
        spread(solute, solute_value)

    if(nrow(widedat)){
        dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, varnames$combined)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[1:3], strokeWidth=2,
                retainDateWindow=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
            dyAxis('y', label=siteA, labelWidth=16, labelHeight=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=siteA)
    }

    return(dg)
})

output$GRAPH_MAIN3b <- renderDygraph({

    changesInSelections3$facetB3
    plotvars = na.omit(input$SOLUTES3[1:3])
    siteB = isolate(input$SITES3[2])

    varnames = filter(grabvars, variable_code %in% plotvars) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(dataMain3()) %>%
        filter(site_name == siteB, solute %in% plotvars) %>%
        group_by(datetime, solute) %>%
        summarize(solute_value=mean(solute_value)) %>%
        spread(solute, solute_value)

    if(nrow(widedat)){
        dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, varnames$combined)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[1:3], strokeWidth=2,
                retainDateWindow=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
            dyAxis('y', label=siteB, labelWidth=16, labelHeight=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=siteB)
    }

    return(dg)
})

output$GRAPH_MAIN3c <- renderDygraph({

    changesInSelections3$facetC3
    plotvars = na.omit(input$SOLUTES3[1:3])
    siteC = isolate(input$SITES3[3])

    varnames = filter(grabvars, variable_code %in% plotvars) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(dataMain3()) %>%
        filter(site_name == siteC, solute %in% plotvars) %>%
        group_by(datetime, solute) %>%
        summarize(solute_value=mean(solute_value)) %>%
        spread(solute, solute_value)

    if(nrow(widedat)){
        dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, varnames$combined)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[1:3], strokeWidth=2,
                retainDateWindow=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
            dyAxis('y', label=siteC, labelWidth=16, labelHeight=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=siteC)
    }

    return(dg)
})

output$GRAPH_FLOW3 <- renderDygraph({

    widedat <- dataFlow3()
    dydat = xts(widedat[, 'flowMaxPerDate'], order.by=widedat$datetime,
        tzone='UTC')
    dimnames(dydat) = list(NULL, 'Q')

    dg = dygraph(dydat, group='nSiteNVar') %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
            colors='#4b92cc', strokeWidth=2, fillAlpha=0.25,
            retainDateWindow=TRUE) %>%
        dyAxis('y', label='Discharge (L/s)', labelWidth=16, labelHeight=10)

    return(dg)
})
