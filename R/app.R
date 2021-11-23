ui <- shiny::fixedPage(
  shiny::textAreaInput(inputId = "decodeplz", label = "Text to be decoded"),
  shiny::sliderInput(inputId = "iter", label = "Number of iterations", min = 5000, max = 20000, value = 10000),
  shiny::actionButton(inputId = "decode", label = "Decode!"),
  shiny::verbatimTextOutput(outputId = "decodedtxt"),
  shiny::textAreaInput(inputId = "codeplz", label = "Text to be encoded"),
  shiny::actionButton(inputId = "code", label = "Encode!"),
  shiny::verbatimTextOutput(outputId = "encodedtxt")
)

server <- function(input, output) {
  shiny::observeEvent(input$decode, {
    output$decodedtxt <- shiny::renderText({
      decode(shiny::isolate(input$decodeplz), iter = shiny::isolate(input$iter))
    })
  })

  shiny::observeEvent(input$code, {
    output$encodedtxt <- shiny::renderText({
      encode(shiny::isolate(input$codeplz))
    })
  })
}

shiny::shinyApp(ui = ui, server = server)
