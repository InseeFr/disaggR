ui <- function(x) fluidPage(
    titlePanel(paste("displaying", x, "*m")),
    sidebarLayout(
        sidebarPanel(
            sliderInput("m",
                        "m:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(textOutput("product"))
    )
)
server <- function(x) {
    function(input, output) {
        output$product <- renderText(as.character(x*input$m))
    }
}
run_app <- function(x) runApp(shinyApp(ui = ui(x), server = server(x)))
run_app(4)
