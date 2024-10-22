.speSelected <- function(input, output, session, rv) {
speSelected <- shiny::eventReactive(input$selectedNorm, {
    spe <- switch(input$selectedNorm,
                          "CPM" = rv$speCpm(),
                          "Q3" = rv$speQ3(),
                          "RUV4" = rv$speRuv())

})


    return(speSelected)
}
