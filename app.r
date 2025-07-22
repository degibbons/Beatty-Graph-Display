library(shiny)
library(bslib)
library(colourpicker)

# Define UI ----
ui <- page_fillable(
  titlePanel("Beatty Graph Display Tool"),
  layout_columns(
    width=1/2,
    card(
      card_header("Files"),
      # actionButton("files.load", "Load Files"), 
      fileInput("file1", "Choose TXT Files", accept = ".txt",multiple = TRUE),
      tableOutput("file.list"),
    ),

    card(
      card_header("Colors"),
      colourInput("color.pick", NULL, "black",palette="limited"),
      # actionButton("color.load", "Select Color"),
      tableOutput("color.list"),
    )),
  layout_columns(
    width=1,
    card(
      card_header("Plot"),
    actionButton("generate.plot","Generate Plot"),
    plotOutput("plot.out"),

      layout_columns(
        width=1/2,
    textInput( 
      "out.file.name", 
      "Export File Name", 
      placeholder = "Enter name (no .ext)..."
    ),
    selectInput( 
      "select", 
      "Select Export File Option Below:", 
      list("EPS" = ".eps", "PS" = ".ps", "PDF" = ".pdf", "JPEG"=".jpg","TIFF"=".tiff","PNG"=".png","BMP"=".bmp","SVG"=".svg") 
    ), 
    ),

    ),
  )
)

# Define server logic ----
server <- function(input, output) {
  # file <- input$file1
  # ext <- tools::file_ext(file$datapath)
  # req(file)
  # validate(need(ext == "txt", "Please upload a .txt file"))
  # all.data.files <- reactive({
  #   for (each.file in input$file1){
  #     print(each.file)
  #   }
  # })  file <- input$file1
  # ext <- tools::file_ext(file$datapath)
  # req(file)
  # validate(need(ext == "txt", "Please upload a .txt file"))
  # all.data.files <- reactive({
  #   for (each.file in input$file1){
  #     print(each.file)
  #   }
  # })
  
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