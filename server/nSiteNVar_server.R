
#this one keeps track of n vars/sites selected, for faceting
changesInSelections3 = reactiveValues()
changesInSelections3$facetA3 = 0
changesInSelections3$facetB3 = 0
changesInSelections3$facetC3 = 0

observeEvent({
    input$DATE3
    input$CONC_FLUX3
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

    data3 = if(input$CONC_FLUX3 == 'concentration') grab else flux
    data3 <- data3 %>%
        filter(datetime >= input$DATE3[1]) %>%
        filter(datetime <= input$DATE3[2]) %>%
        filter(site_name %in% input$SITES3) %>%
        select(one_of("datetime", "site_name", input$SOLUTES3))
})

## Extract data for Precip plot
dataPrecip3 <- reactive ({

    # dataPrecip3 <- data3() %>%
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

# ## Extract data for Solutes (Main) plot
# dataMain3 <- reactive ({
#
#     dataMain3 <- data3() %>%
#         filter(site_name %in% input$SITES3) %>%
#         select(one_of("datetime", "site_name", input$SOLUTES3))
# })

## Extract data for Discharge (Flow) plot
dataFlow3 <- reactive ({

    # dataFlow3 <- data3() %>%
    #     filter(site_name %in% input$SITES3)
    #
    # dataFlow3 <- dataFlow3 %>%
    #     select(one_of("datetime", 'flowGageHt')) %>%
    #     group_by(datetime) %>%
    #     summarise(flowMaxPerDate = max(flowGageHt, na.rm=TRUE))

    dataFlow3 = filter(sensor, datetime > input$DATE3[1],
        datetime < input$DATE3[2], watershedID %in% input$SITES3) %>%
        mutate(datetime=as.Date(datetime)) %>%
        select(datetime, Q_Ls) %>%
        group_by(datetime) %>%
        summarise(flowMaxPerDate = max(Q_Ls, na.rm=TRUE))

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
            plotter=hyetograph_js, retainDateWindow=TRUE) %>%
        dyAxis('y', label='P (in)', valueRange=c(ymax + ymax * 0.1, 0),
            labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)

    return(p)
})

output$GRAPH_MAIN3a <- renderDygraph({

    changesInSelections3$facetA3
    sites = na.omit(input$SITES3[1:3])
    varA = isolate(input$SOLUTES3[1])
    # plotvars = na.omit(input$SOLUTES3[1:3])
    # siteA = isolate(input$SITES3[1])

    # varnames = filter(grabvars, variable_code %in% plotvars) %>%
    varnames = filter(grabvars, variable_code == varA) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(data3()) %>%
        filter(site_name %in% sites) %>%
        # filter(site_name == siteA) %>%
        select(datetime, site_name, one_of(varA)) %>%
        spread(site_name, !!varA)
        # select(datetime, one_of(plotvars))

    if(nrow(widedat)){
        sites = colnames(widedat)[-1]
        # dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
        dydat = xts(widedat[, sites], order.by=widedat$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, sites)
        # dimnames(dydat) = list(NULL, varnames$combined)

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[c(4, 5, 7)], strokeWidth=2,
                retainDateWindow=TRUE, connectSeparatedPoints=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
            dyAxis('y', label=varA, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)
            # dyAxis('y', label=siteA, labelWidth=16, labelHeight=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=varA, px_per_lab=20)
            # ylab=siteA)
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

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[c(4, 5, 7)], strokeWidth=2,
                retainDateWindow=TRUE, connectSeparatedPoints=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
            dyAxis('y', label=varB, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=varB, px_per_lab=20)
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

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
                colors=linecolors[c(4, 5, 7)], strokeWidth=2,
                retainDateWindow=TRUE, connectSeparatedPoints=TRUE) %>%
            dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
            dyAxis('y', label=varC, labelWidth=16, labelHeight=10,
                pixelsPerLabel=20, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab=varC, px_per_lab=20)
    }

    return(dg)
})

output$GRAPH_FLOW3 <- renderDygraph({

    widedat <- dataFlow3()

    if(nrow(widedat)){
        dydat = xts(widedat[, 'flowMaxPerDate'], order.by=widedat$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, 'Q')

        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                colors='#4b92cc', strokeWidth=2, fillAlpha=0.25,
                retainDateWindow=TRUE) %>%
            dyAxis('y', label='Q (L/s)', labelWidth=16, labelHeight=10,
                pixelsPerLabel=10, rangePad=10)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE3), plotgroup='nSiteNVar',
            ylab='Q (L/s)', px_per_lab=10)
    }

    return(dg)
})
