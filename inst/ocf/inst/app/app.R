library(shiny)
library(ocf)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data (In Color!)"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      actionButton(inputId = "color", label = "Add Color")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  color_palette <- eventReactive(input$color, {
    get_pal()
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = color_palette()[[1]][1],
         border = color_palette()[[1]][5],
         main = paste0("Histogram in ", names(color_palette())))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
