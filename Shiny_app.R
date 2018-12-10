frames3 <- list()
for(i in 3:50){
  print(i)
  frames3[[i-2]] <- list()
  for(j in 1:2){
    frames3[[i-2]][[j]] <- magick::image_read(paste0("Plots/ShinyPlots_small/Shiny_", i, "MY_", j, ".pdf"), density=300)
  }
}
#animation <- magick::image_animate(magick::image_join(frames), fps=4)
#magick::image_write(animation, "Plots/animated_fig.gif")


# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("Timeframe", "Timeframe (MYA to present):",
                  min = 3, max = 50,
                  value = 5),
      
      # Input: Decimal interval with step value ----
      sliderInput("Tree", "Phylogenetic hypothesis:",
                  min = 1, max = 2,
                  value = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput("my_plot", height = 1000, width = 'auto')
      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Timeframe",
               "Tree"),
      Value = as.character(c(input$Timeframe,
                             input$Tree)),
      stringsAsFactors = FALSE)
    
  })
  
  output$my_plot <- renderPlot({
    plot(frames3[[input$Timeframe - 2]][[input$Tree]])
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)