output$MAIN_BIPLOT = renderPlot({
    main_biplot(input$X_VAR, input$Y_VAR)
})
