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
  
  ###############################################################
  file <- input$file1
  ext <- tools::file_ext(file$datapath)
  req(file)
  validate(need(ext == "txt", "Please upload a .txt file"))
  
  #### Function List ----
  extract_title <- function(filename) {
    # Remove file extension
    name <- tools::file_path_sans_ext(basename(filename))
    
    # Split into characters
    chars <- strsplit(name, "")[[1]]
    
    # Identify non-alphabet characters (excluding space)
    is_non_alpha <- !grepl("[A-Za-z]", chars) & chars != " "
    
    # Find index of the 4th non-alpha character
    non_alpha_pos <- which(is_non_alpha)
    
    if (length(non_alpha_pos) >= 4) {
      cut_pos <- non_alpha_pos[4] - 1
      
      # If the character before is a space, trim that too
      if (chars[cut_pos] == " ") {
        cut_pos <- cut_pos - 1
      }
      
      return(substr(name, 1, cut_pos))
    } else {
      return(name)  # If fewer than 4 non-alpha characters, return the whole thing
    }
  } # Function for extracting title from file name
  
  
  #### Data Extraction & Organize Dataset ----
  data_folder <- list.files(path = "C:\\Users\\dgibbo03\\Beatty-Graph-Display\\Data", pattern = "\\.txt$", full.names = TRUE) # Extract title from txt files
  titles <- sapply(data_folder, extract_title) # Only extract sample name from the title
  titles
  
  # Initialize a list to hold each table
  dataset <- list()
  
  # Loop over files and read each into a named slot in the list
  for (i in seq_along(data_folder)) {
    raw_lines <- readLines(data_folder[i])
    data_lines <- raw_lines[grep("^[0-9]", raw_lines)]  # Extract only data lines
    df <- read.table(text = data_lines, fill = TRUE)
    colnames(df) <- c("Scale", "RelativeLength", "FractalComplexity", "R2")
    
    # Make sure all columns are numeric
    df <- df %>%
      mutate(
        Scale = as.numeric(as.character(Scale)),
        RelativeLength = as.numeric(as.character(RelativeLength)),
        FractalComplexity = as.numeric(as.character(FractalComplexity)),
        R2 = as.numeric(as.character(R2))
      ) %>% 
      slice(-1) # Delete first N/A row
    
    # Store the table in the list using auto name
    dataset[[paste0("table: ", titles[i])]] <- df
  }
  
  
  #### Combine Data----
  combined_data <- bind_rows(dataset, .id = "Sample")
  
  
  #### Plot ----
  ggplot(combined_data %>% filter(Sample == "table: LH001 Upper Ureter"), aes(x = Scale, y = RelativeLength)) +
    geom_line(color = "blue", linewidth = 0.7) +
    scale_x_log10(labels = comma_format(accuracy = 1)) +
    labs(title = "Scale-Sensitive Fractal Analysis", x = "Scale (µm)", y = "Relative Length") +
    theme_minimal(base_size = 14) +
    geom_hline(yintercept = 1.086, linetype = "dashed", color = "red") +
    annotate("text", x = 5, y = 1.1, label = "SRC Threshold", color = "red", vjust = -1)
  
  # Color method 1
  ggplot(combined_data, aes(x = Scale, y = RelativeLength, color = Sample)) +
    geom_line(linewidth = 0.4) +
    scale_x_log10(labels = comma_format(accuracy = 1)) +
    labs(title = "Scale-Sensitive Fractal Analysis", x = "Scale (µm)", y = "Relative Length") +
    theme_minimal(base_size = 14)
  
  # TODO: Change maybe color, titles, lines for logarithmic 10
  
  # Color method 2
  color_palette <- c(
    "#FF0000", "blue", "green", "purple", "orange", "darkorange",
    "#641e16", "brown", "black", "#b6df5c", "darkgreen",
    "#d4ac0d", "deepskyblue", "darkred"
  )
  
  ###############################################################
  
  output$scatterplot <- renderPlot({
    ggplot(combined_data, aes(x = Scale, y = RelativeLength, color = Sample)) +
      geom_line(linewidth = 0.7) +
      scale_color_manual(values = color_palette) +
      scale_x_log10(labels = comma_format(accuracy = 1)) +
      labs(title = "Scale-Sensitive Fractal Analysis", x = "Scale (µm)", y = "Relative Length") +
      theme_minimal(base_size = 14)
  })
  
  
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