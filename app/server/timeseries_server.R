
# Replaces codes -999.9, -1, -2, and -3 from data (used before graphing)
removeCodes <- function(dataSet) {
    # if value -999.9 is present in certain columns, replace with NA
    for (i in 1:6) {
        # test data set when needed:
        # test<-dataAll[which(dataAll$temp == -999.9),] #selects all temp -999.9
        current_col_ofData <- codes999.9[i]
        if (current_col_ofData %in% names(dataSet)) {
            ind_col <- which(current_col_ofData == colnames(dataSet), arr.ind = TRUE)
            if (current_col_ofData == "timeEST") {
                dataSet[ind_col][dataSet[ind_col] == "-9999"] <- NA
                # above is essentially the same as:
                # dataAll2$timeEST[dataAll2$timeEST==-999.9] <- NA
            } else {
                dataSet[ind_col][dataSet[ind_col] == -999.9] <- NA
            }
        }
    }
    # if values are -1, -2, or -3, replace with NA
    for (i in 1:23) {
        current_col_ofData <- codes123[i]
        if (current_col_ofData %in% names(dataSet)) {
            ind_col <- which(current_col_ofData == colnames(dataSet), arr.ind = TRUE)
            dataSet[ind_col][dataSet[ind_col] == -1] <- NA
            dataSet[ind_col][dataSet[ind_col] == -2] <- NA
            dataSet[ind_col][dataSet[ind_col] == -3] <- NA
        }
    }

    return(dataSet)
}

# Graph theme
my_theme <- theme_fivethirtyeight() +
    theme(rect = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#dddddd"),
        text = element_text(family = "Arial", size = 14),
        legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal",
        legend.box.just = "left", legend.title = element_blank(),
        #legend.key.size = unit(2.5, "native"),
        strip.text = element_text(hjust = 1, size = 20, face = "bold"),
        axis.title= element_text(NULL), axis.title.x= element_blank(),
        axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

# Set up color palette for solutes (using 'qual', or qualitative, color palette)
n <- 30 # number of colors
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=(col_vector[1:n])) # to see color wheel


# Create reactive value which will be used to signal when core data (e.g. 'current')
# has changed and should be updated. Anytime current data is changed, the value
# of this variable should be increased by 1.
changesInData <- reactiveValues()
changesInData$change_dataCurrent <- 0
changesInData$change_dataAll <- 0

# Make a reactive dataAll2 data frame, to be called whenever data is updated
# (R in dataCurrentR stands for reactive)
dataCurrentR <- eventReactive(changesInData$change_dataCurrent, {

    # Open database connection
    y = RMariaDB::MariaDB()
    con = dbConnect(y,
        user = 'root',
        password = pass,
        host = 'localhost',
        dbname = 'hbef')

    # Read current data and disconnect from table
    dataCurrentR <- dbReadTable(con, "current")
    message(print(class(dataCurrentR)))
    message(head(dataCurrentR))
    dataCurrentR <- as.data.frame(dataCurrentR)
    message(print(class(dataCurrentR)))
    message(head(dataCurrentR))
    dbDisconnect(con)

    # Clean up data
    dataCurrentR <- standardizeClasses(dataCurrentR)
    # substituting commas with semi-colons. (necessary to prevent problems when downloading csv files)
    dataCurrentR$notes <- gsub(",", ";", dataCurrentR$notes)
    dataCurrentR$sampleType <- gsub(",", ";", dataCurrentR$sampleType)

    # Re-calculate and assign water year variable for current data
    wy_current <- levels(as.factor(dataCurrentR$waterYr))
    wy1_current <- c()
    for (i in 1:length(wy_current)) {
        wy1_current <- c(wy1_current, wy_current[i])
    }
    #wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
    wateryears_current <<- as.list(wy1_current) #assign it globally

    # Update Panel 5 user interface
    updateSelectInput(session, "WATERYEAR5", label = "Water Year", choices = wateryears_current)

    # Trigger update in dataAll
    changesInData$change_dataAll <- changesInData$change_dataAll + 1

    dataCurrentR

})


dataAllR <- eventReactive(changesInData$change_dataAll, {
    dataAllR <- bind_rows(select(dataHistorical, -canonical), dataCurrentR())
    dataAllR <- standardizeClasses(dataAllR)

    # Re-calculate and assign water year variable for all data
    wy <- levels(as.factor(dataAllR$waterYr))
    wy1 <- c()
    for (i in 1:length(wy)) {
        wy1 <- c(wy1, wy[i])
    }
    #wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
    wateryears <<- as.list(wy1) #assign it globally

    # Get new maximum date ----
    # used in ui.R for Panel 4 (QA/QC "Free-for-all" graph)
    maxDate <- max(dataAllR$date, na.rm=TRUE)

    # Update Panel 1-4 user interfaces
    updateSelectInput(session, "WATERYEAR1", label = "Water Year", choices = wateryears)
    updateSelectInput(session, "WATERYEAR2", label = "Water Year", choices = wateryears)
    updateSelectInput(session, "WATERYEAR3", label = "Water Year", choices = wateryears)
    updateSliderInput(session, "DATE4",
        label = "Date Range",
        value = as.Date(c(maxDate-365, maxDate)),
        min =as.Date("1963-06-01"),
        max = as.Date(maxDate),
        step = 30)

    dataAllR

})



## Filter data to desired dates
data4 <- reactive ({
    if (changesInData$change_dataAll > 0) dataAll <- dataAllR()
    data4 <- dataAll %>%
        filter(date >= input$DATE4[1]) %>%
        filter(date <= input$DATE4[2])
    data4 <- removeCodes(data4)
})
## Extract data for Precip plot
dataPrecip4 <- reactive ({
    dataPrecip4 <- data4() %>%
        #filter(site %in% input$PRECIP_SITE4) %>%
        filter(site %in% sites_precip) %>%
        select(one_of("date", "site", input$PRECIP_SOURCE4))
    if (input$PRECIP_SOURCE4 == "precipCatch") {
        dataPrecip4 <- dataPrecip4 %>%
            group_by(date) %>%
            summarise(medianPrecip = median(precipCatch, na.rm=TRUE))
    }
    if (input$PRECIP_SOURCE4 == "precipETI") {
        dataPrecip4 <- dataPrecip4 %>%
            group_by(date) %>%
            summarise(medianPrecip = median(precipETI, na.rm=TRUE))
    }
    dataPrecip4
})
## Extract data for Solutes (Main) plot
dataMain4 <- reactive ({
    dataMain4 <- data4() %>%
        filter(site %in% input$SITES4) %>%
        select(one_of("date", "site", input$SOLUTES4, "fieldCode")) %>%  # Keep date, site, solute & fieldcode data
        group_by(date, site) %>%
        gather(key = solute, value = solute_value, -site, -date, -fieldCode)  # Reshape data for ggplot2 plotting
})
## Extract data for Discharge (Flow) plot
dataFlow4 <- reactive ({
    dataFlow4 <- data4() %>%
        filter(site %in% input$FLOW_SITE4)
    # flow values need to be summarized with median per date,
    # because multiple values for one date make flow graph look strange
    if (input$FLOW_SOURCE4 == "gageHt") {
        dataFlow4 <- dataFlow4 %>%
            select(one_of("date", input$FLOW_SOURCE4)) %>%
            group_by(date) %>%
            summarise(flowMaxPerDate = max(gageHt, na.rm=TRUE))
    }
    if (input$FLOW_SOURCE4 == "flowGageHt") {
        dataFlow4 <- dataFlow4 %>%
            select(one_of("date", input$FLOW_SOURCE4)) %>%
            group_by(date) %>%
            summarise(flowMaxPerDate = max(flowGageHt, na.rm=TRUE))
    }
    if (input$FLOW_SOURCE4 == "flowSens") {
        #!!! Not selecting by input$FLOW_SITE4, but by all SITES4 selected - is this intentional?
        dataFlow4 = filter(dataSensor, datetime > input$DATE4[1],
            datetime < input$DATE4[2], watershedID %in% input$FLOW_SITE4) %>%
            mutate(date=as.Date(datetime)) %>%
            select(date, Q_Ls) %>%
            group_by(date) %>%
            summarise(flowMaxPerDate = max(Q_Ls, na.rm=TRUE))

    }
    dataFlow4
})
## Additional data for Flow plot: hydroGraph labels
dataFlowHydroGraph4 <- reactive ({
    dataFlowHydroGraph4 <- data4() %>%
        filter(site %in% input$FLOW_SITE4) %>%
        select(one_of("date", "hydroGraph", input$FLOW_SOURCE4))
    # group_by(date) %>%
    # summarise(hydroGraph = first(hydroGraph, na.rm=TRUE), flowSource = max(flowSource, na.rm=TRUE))
    dataFlowHydroGraph4
})


output$GRAPH_PRECIP4 <- renderPlot({
    if (input$PRECIP4_OPTION == TRUE) {
        data <- dataPrecip4()
        x <- data$date
        # get column number of selected precipitation source
        # ind_col <- which(input$PRECIP_SOURCE4 == colnames(data), arr.ind = TRUE)
        y <- data$medianPrecip
        p <- ggplot(data, aes(x, y)) + my_theme +
            geom_col(fill = "cadetblue3", width = 4, na.rm=TRUE) +
            labs(x = "", y = "Precipitation") +
            coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
            scale_y_reverse()
        p
    }
}, height = 100) # end of output$GRAPH_PRECIP4
output$GRAPH_MAIN4 <- renderPlot({
    data <- dataMain4()
    x <- data$date
    y <- data$solute_value
    # build ggplot function
    # design <- my_theme +
    #   geom_point(size = 2.5) +
    #   geom_line(alpha = 0.5) +
    #   scale_x_date(date_labels = "%Y-%b")+
    #   coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
    #   scale_color_manual(values = c("black", "#307975", "#691476", "#735E1F", "#6F0D2F", "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")) +
    #   labs(x = "", y = "Solutes")
    if(input$SOLUTES4_COLOR == "Solutes") {
        m <- ggplot(data, aes(x, y, shape=data$site, color=data$solute)) +
            my_theme +
            geom_point(size = 2.5) +
            geom_line(alpha = 0.5) +
            scale_x_date(date_labels = "%Y-%b")+
            coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
            scale_color_manual(values = c("black", "#307975", "#691476", "#735E1F", "#6F0D2F", "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")) +
            labs(x = "", y = "Solutes")
    } else {
        m <- ggplot(data, aes(x, y, shape=data$solute, color=data$site)) +
            my_theme +
            geom_point(size = 2.5) +
            geom_line(alpha = 0.5) +
            scale_x_date(date_labels = "%Y-%b")+
            coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
            scale_color_manual(values = c("black", "#307975", "#691476", "#735E1F", "#6F0D2F", "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")) +
            labs(x = "", y = "Solutes")
    }

    # If show field code is selected, add to ggplot
    if (input$FIELDCODE4 == TRUE) {
        m <- m + geom_text(aes(label=data$fieldCode),
            nudge_y = (max(data$solute_value, na.rm = TRUE) - min(data$solute_value, na.rm = TRUE))/15,
            check_overlap = TRUE)
    }
    # plot
    m
}, height = 350) # end of output$GRAPH_MAIN4
output$GRAPH_FLOW4 <- renderPlot({
    if (input$DISCHARGE4_OPTION == TRUE) {
        data <- dataFlow4()
        x <- data$date
        y <- data$flowMaxPerDate
        f <- ggplot(data, aes(x, y)) + my_theme +
            geom_area(fill = "cadetblue3", na.rm=TRUE) +
            coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
            labs(x = "", y = "Discharge")
        if (input$HYDROLIMB4 == TRUE) {
            data.hl <- dataFlowHydroGraph4()
            if (input$FLOW_SOURCE4 == "gageHt") y.hl <- data.hl$gageHt
            if (input$FLOW_SOURCE4 == "flowGageHt") y.hl <- data.hl$flowGageHt
            if (input$FLOW_SOURCE4 == "flowSens") y.hl <- data.hl$flowSensor
            f <- f + geom_text(data = data.hl,
                aes(x = date,
                    y = y.hl,
                    label = hydroGraph),
                nudge_y = (max(y.hl, na.rm = TRUE) - min(y.hl, na.rm = TRUE))/15,
                check_overlap = TRUE)
        }
        f
    }
}, height = 100) # end of output$GRAPH_FLOW4

output$TABLE4 <- renderDataTable({
    dataFlowHydroGraph4()
    #head(dataCurrentR())
}) # end of output$TABLE4
