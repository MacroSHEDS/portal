
# # Replaces codes -999.9, -1, -2, and -3 from data (used before graphing)
# removeCodes <- function(dataSet) {
#     # if value -999.9 is present in certain columns, replace with NA
#     for (i in 1:6) {
#         # test data set when needed:
#         # test<-dataAll[which(dataAll$temp == -999.9),] #selects all temp -999.9
#         current_col_ofData <- codes999.9[i]
#         if (current_col_ofData %in% names(dataSet)) {
#             ind_col <- which(current_col_ofData == colnames(dataSet), arr.ind = TRUE)
#             if (current_col_ofData == "timeEST") {
#                 dataSet[ind_col][dataSet[ind_col] == "-9999"] <- NA
#                 # above is essentially the same as:
#                 # dataAll2$timeEST[dataAll2$timeEST==-999.9] <- NA
#             } else {
#                 dataSet[ind_col][dataSet[ind_col] == -999.9] <- NA
#             }
#         }
#     }
#     # if values are -1, -2, or -3, replace with NA
#     for (i in 1:23) {
#         current_col_ofData <- codes123[i]
#         if (current_col_ofData %in% names(dataSet)) {
#             ind_col <- which(current_col_ofData == colnames(dataSet), arr.ind = TRUE)
#             dataSet[ind_col][dataSet[ind_col] == -1] <- NA
#             dataSet[ind_col][dataSet[ind_col] == -2] <- NA
#             dataSet[ind_col][dataSet[ind_col] == -3] <- NA
#         }
#     }
#
#     return(dataSet)
# }

# # Graph theme
# my_theme <- theme_fivethirtyeight() +
#     theme(rect = element_rect(fill = NA),
#         panel.grid.major = element_line(colour = "#dddddd"),
#         text = element_text(family = "Arial", size = 14),
#         legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal",
#         legend.box.just = "left", legend.title = element_blank(),
#         #legend.key.size = unit(2.5, "native"),
#         strip.text = element_text(hjust = 1, size = 20, face = "bold"),
#         axis.title= element_text(NULL), axis.title.x= element_blank(),
#         axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

# Set up color palette for solutes (using 'qual', or qualitative, color palette)
# n <- 30 # number of colors
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=(col_vector[1:n])) # to see color wheel


# # Create reactive value which will be used to signal when core data (e.g. 'current')
# # has changed and should be updated. Anytime current data is changed, the value
# # of this variable should be increased by 1.
# changesInData <- reactiveValues()
# changesInData$change_dataCurrent <- 0
# changesInData$change_dataAll <- 0

#this one keeps track of n vars/sites selected, for faceting
changesInSelections4 = reactiveValues()
changesInSelections4$n_vars = 1
changesInSelections4$facetA4 = 0
changesInSelections4$facetB4 = 0
changesInSelections4$facetC4 = 0

observeEvent({
        input$SITES4
        input$DATE4
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

# Make a reactive dataAll2 data frame, to be called whenever data is updated
# (R in dataCurrentR stands for reactive)
# dataCurrentR <- eventReactive(changesInData$change_dataCurrent, {
#
#     # # Open database connection
#     # y = RMariaDB::MariaDB()
#     # con = dbConnect(y,
#     #     user = 'root',
#     #     password = pass,
#     #     host = 'localhost',
#     #     dbname = 'hbef')
#
#     # Read current data and disconnect from table
#     # dataCurrentR <- dbReadTable(con, "current")
#     # dataCurrentR = read_feather('temp_shinyappsio/dataCurrent.feather')
#     dataCurrentR = dataCurrent
#     dataCurrentR <- as.data.frame(dataCurrentR)
#     # dbDisconnect(con)
#
#     # Clean up data
#     # dataCurrentR <- standardizeClasses(dataCurrentR)
#
#     # substituting commas with semi-colons. (necessary to prevent problems when downloading csv files)
#     # dataCurrentR$notes <- gsub(",", ";", dataCurrentR$notes)
#     # dataCurrentR$sampleType <- gsub(",", ";", dataCurrentR$sampleType)
#
#     # Re-calculate and assign water year variable for current data
#     # wy_current <- levels(as.factor(dataCurrentR$waterYr))
#     # wy1_current <- c()
#     # for (i in 1:length(wy_current)) {
#     #     wy1_current <- c(wy1_current, wy_current[i])
#     # }
#     # wateryears_current <<- as.list(wy1_current) #assign it globally
#
#     # Update Panel 5 user interface
#     # updateSelectInput(session, "WATERYEAR5", label = "Water Year", choices = wateryears_current)
#
#     # Trigger update in dataAll
#     changesInData$change_dataAll <- changesInData$change_dataAll + 1
#
#     dataCurrentR
#
# })

# dataAllR <- eventReactive(changesInData$change_dataAll, {
#     # dataAllR <- bind_rows(select(dataHistorical, -canonical), dataCurrentR())
#     # dataAllR <- standardizeClasses(dataAllR)
#
#     # # Re-calculate and assign water year variable for all data
#     # wy <- levels(as.factor(dataAllR$waterYr))
#     # wy1 <- c()
#     # for (i in 1:length(wy)) {
#     #     wy1 <- c(wy1, wy[i])
#     # }
#     # #wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
#     # wateryears <<- as.list(wy1) #assign it globally
#
#     # Get new maximum date ----
#     # used in ui.R for Panel 4 (QA/QC "Free-for-all" graph)
#     maxDate <- max(dataAllR$date, na.rm=TRUE)
#
#     # Update Panel 1-4 user interfaces
#     # updateSelectInput(session, "WATERYEAR1", label = "Water Year", choices = wateryears)
#     # updateSelectInput(session, "WATERYEAR2", label = "Water Year", choices = wateryears)
#     # updateSelectInput(session, "WATERYEAR3", label = "Water Year", choices = wateryears)
#     updateSliderInput(session, "DATE4",
#         label = "Date Range",
#         value = as.Date(c(maxDate-365, maxDate)),
#         min =as.Date("1963-06-01"),
#         max = as.Date(maxDate),
#         step = 30)
#
#     dataAllR
#
# })

observeEvent(input$SITES4, {

    grab_subset = filter(grab, site_name == input$SITES4)
    grabvars_display_subset = populate_vars(grab_subset[-(1:2)])

    updateSelectizeInput(session, 'SOLUTES4',
        choices=grabvars_display_subset,
        selected=input$SOLUTES4)

    # site_dtrng = as.Date(range(grab$datetime[grab$site_name == input$SITES4],
    #     na.rm=TRUE))
    #
    # updateSliderInput(session, "DATE4",
    #     label="Date Range", min=site_dtrng[1], max=site_dtrng[2], step=30,
    #     value=c(max(site_dtrng[2] - lubridate::days(365),
    #         site_dtrng[1], na.rm=TRUE),
    #         site_dtrng[2]))
})

## Filter data to desired dates
data4 <- reactive ({

    data4 <- grab %>%
        filter(datetime >= input$DATE4[1]) %>%
        filter(datetime <= input$DATE4[2])
    return(data4)
})

dataPrecip4 <- reactive ({

    # dataPrecip4 <- data4() %>%
    #     select(precipCatch) %>%
    #     filter(! is.na(precipCatch)) %>%
    #     select(one_of("datetime", "site_name", 'precipCatch')) %>%
    #     group_by(lubridate::days(datetime)) %>%
    #     summarise(medianPrecip = median(precipCatch, na.rm=TRUE)) %>%
    #     ungroup()

    dataPrecip4 <- data4() %>%
        filter(site_name %in% sites_precip) %>%
        select(one_of("datetime", "site_name", 'precipCatch')) %>%
        # group_by(lubridate::yday(datetime)) %>%
        group_by(datetime) %>%
        summarise(medianPrecip=median(precipCatch, na.rm=TRUE)) %>%
        ungroup()
})

dataMain4 <- reactive ({

    dataMain4 <- data4() %>%
        filter(site_name %in% input$SITES4) %>%
        select(one_of("datetime", "site_name", input$SOLUTES4))
})

dataFlow4 <- reactive ({

    dataFlow4 <- data4() %>%
        filter(site_name %in% input$SITES4)

    if (input$FLOW_SOURCE4 == "flowGageHt") {
        dataFlow4 <- dataFlow4 %>%
            select(one_of("datetime", input$FLOW_SOURCE4)) %>%
            group_by(datetime) %>%
            summarise(flowMaxPerDate = max(flowGageHt, na.rm=TRUE))
    }

    if (input$FLOW_SOURCE4 == "flowSens") {
        dataFlow4 = filter(sensor, datetime > input$DATE4[1],
                datetime < input$DATE4[2], watershedID %in% input$SITES4) %>%
            mutate(datetime=datetime) %>%
            select(datetime, Q_Ls) %>%
            group_by(datetime) %>%
            summarise(flowMaxPerDate = max(Q_Ls, na.rm=TRUE))
    }

    dataFlow4
})

output$GRAPH_PRECIP4 <- renderDygraph({

    data <- dataPrecip4()
    # ind_col <- which(input$PRECIP_SOURCE4 == colnames(data), arr.ind = TRUE)
    dydat = xts(data$medianPrecip, order.by=data$datetime, tzone='UTC')
    dimnames(dydat) = list(NULL, 'P')
    ymax = max(dydat, na.rm=TRUE)

    p = dygraph(dydat, group='oneSiteNVar') %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
            fillAlpha=1, colors='#4b92cc', strokeWidth=3,
            plotter=hyetograph_js) %>%
        dyAxis('y', label='P (in)',
            valueRange=c(ymax + ymax * 0.1, 0),
            labelWidth=16, labelHeight=10, pixelsPerLabel=10, rangePad=10)

    return(p)
})

output$GRAPH_MAIN4a <- renderDygraph({

    changesInSelections4$facetA4
    n_vars = isolate(changesInSelections4$n_vars)
    plotvars = isolate(input$SOLUTES4)[1:min(c(n_vars, 3))]

    varnames = filter(grabvars, variable_code %in% plotvars) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(dataMain4()) %>%
        # filter(solute %in% plotvars) %>%
        select(datetime, one_of(plotvars))

    dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
    dimnames(dydat) = list(NULL, varnames$combined)

    dg = dygraph(dydat, group='oneSiteNVar') %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
            colors=linecolors[1:3], strokeWidth=2) %>%#, pointSize=2) %>%
        dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
        dyAxis('y', label=NULL, pixelsPerLabel=20, rangePad=10)
    return(dg)
    # }

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
    #             m = m %>%
    #                 dySeries('')
    #         }
    #
    #     }
    # }

    # if(input$SOLUTES4_COLOR == "Solutes") {


    # } else {
    #     m <- ggplot(data, aes(x, y, shape=data$solute, color=data$site)) +
    #         my_theme +
    #         geom_point(size = 2.5) +
    #         geom_line(alpha = 0.5) +
    #         scale_x_date(date_labels = "%Y-%b")+
    #         coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
    #         scale_color_manual(values = c("black", "#307975", "#691476", "#735E1F", "#6F0D2F", "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")) +
    #         labs(x = "", y = "Solutes")
    # }

})

output$GRAPH_MAIN4b <- renderDygraph({

    changesInSelections4$facetB4
    n_vars = isolate(changesInSelections4$n_vars)
    plotvars = isolate(input$SOLUTES4)[4:min(c(n_vars, 6))]

    varnames = filter(grabvars, variable_code %in% plotvars) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(dataMain4()) %>%
        select(datetime, one_of(plotvars))

    dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
    dimnames(dydat) = list(NULL, varnames$combined)

    dg = dygraph(dydat, group='oneSiteNVar') %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
            colors=linecolors[4:6], strokeWidth=2) %>%
        dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
        dyAxis('y', label=NULL, pixelsPerLabel=20, rangePad=10)
    return(dg)
})

output$GRAPH_MAIN4c <- renderDygraph({

    changesInSelections4$facetC4
    n_vars = isolate(changesInSelections4$n_vars)
    plotvars = isolate(input$SOLUTES4)[7:min(c(n_vars, 9))]

    varnames = filter(grabvars, variable_code %in% plotvars) %>%
        select(variable_name, unit) %>%
        mutate(combined = paste0(variable_name, ' (', unit, ')'))

    widedat = isolate(dataMain4()) %>%
        select(datetime, one_of(plotvars))

    dydat = xts(widedat[, plotvars], order.by=widedat$datetime, tzone='UTC')
    dimnames(dydat) = list(NULL, varnames$combined)

    dg = dygraph(dydat, group='oneSiteNVar') %>%
        dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
            colors=linecolors[7:9], strokeWidth=2) %>%
        dyLegend(show='onmouseover', labelsSeparateLines=TRUE) %>%
        dyAxis('y', label=NULL, pixelsPerLabel=20, rangePad=10)

    return(dg)
})

output$GRAPH_FLOW4 <- renderDygraph({

    widedat <- dataFlow4()

    if(nrow(widedat)){
        dydat = xts(widedat[, 'flowMaxPerDate'], order.by=widedat$datetime,
            tzone='UTC')
        dimnames(dydat) = list(NULL, 'Q')

        dg = dygraph(dydat, group='oneSiteNVar') %>%
            dyOptions(useDataTimezone=TRUE, drawPoints=FALSE, fillGraph=TRUE,
                colors='#4b92cc', strokeWidth=2, fillAlpha=0.25) %>%
            dyAxis('y', label='Q (L/s)', labelWidth=16, labelHeight=10,
                pixelsPerLabel=10, rangePad=10)
            # dyLegend(show='onmouseover', labelsSeparateLines=TRUE)
    } else {
        dg = plot_empty_dygraph(isolate(input$DATE4), plotgroup='oneSiteNVar',
            ylab='Q (L/s)', px_per_lab=10)
    }

    return(dg)
})
