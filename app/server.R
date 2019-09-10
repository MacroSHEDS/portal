library(shiny)
library(Cairo)
# library(shinyjs)

options(shiny.usecairo=TRUE)

shinyServer(function(input, output, session){

    # #hacky way to specify div height by % with js
    # height50 = reactive({
    #     ifelse(is.null(input$height50), 0, input$height50)
    # })
    # js$getHeight50()

    output$MAIN_BIPLOT = renderPlot({
        main_biplot()
    })

})
