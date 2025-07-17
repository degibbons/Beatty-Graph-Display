library(shiny)
library(bslib)

# Define UI ----
ui <- page_fluid(
  titlePanel("Beatty Graph Display Tool"),
  layout_columns(
    col_width = 2,
    card(
      card_header("Files"),
      actionButton("files.load", "Load Files"), 
      tableOutput("file.list"),
    ),
    card(
      card_header("Colors"),
    # colourInput("color.pick", "Select Color", "black"),
      actionButton("color.load", "Select Color"),
      tableOutput("color.list"),
    ),
    card(
      card_header("Plot"),
    plotOutput("plot.out"),
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # names(data_folder) <- c("File Names")
  # output$file.list <- renderTable(data_folder, striped = TRUE) 
  # 
  # output$color.list <- renderTable(graph_colors, striped = TRUE)
  # output$plot.out <- renderPlot({ 
  #   mpg |> 
  #     ggplot(aes(hwy, cty)) + 
  #     geom_point() + 
  #     labs(title = "Click anywhere on the plot")
  # }) 
}

# Run the app ----
shinyApp(ui = ui, server = server)