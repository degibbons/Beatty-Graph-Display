library(shiny)
library(bslib)

# Define UI ----
ui <- page_sidebar(
  title = "Beatty Graph Display Tool",
  actionButton("files.load", "Load Files"), 
  tableOutput("file.list"),
  colourInput("color.pick", "Select Color", "black"),
  actionButton("color.load", "Select Color"),
  tableOutput("color.list")
  plotOutput("plot.out")
)

# Define server logic ----
server <- function(input, output) {
  
  names(data_folder) <- c("File Names")
  output$file.list <- renderTable(data_folder, striped = TRUE) 
  
  output$color.list <- renderTable(graph_colors, striped = TRUE)
  output$plot.out <- renderPlot({ 
    mpg |> 
      ggplot(aes(hwy, cty)) + 
      geom_point() + 
      labs(title = "Click anywhere on the plot")
  }) 
}

# Run the app ----
shinyApp(ui = ui, server = server)