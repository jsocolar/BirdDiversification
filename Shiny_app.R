# ENTER PATH TO THE FOLDER "ShinyPlots_small" (produced by unarchiving ShinyPlots_small.zip)
setwd("/Users/JacobSocolar/Dropbox/Work/Diversity_accum/Plots/ShinyPlots_small")


library(shiny)
library(magick)

frames <- list()
for(i in 3:50){
  print(i)
  frames[[i-2]] <- list()
  for(j in 1:30){
    frames[[i-2]][[j]] <- magick::image_read_pdf(paste0("Shiny_", i, "MY_", j, ".pdf"), density=150)
  }
}


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
                  value = 5, step=1,
                  animate = animationOptions(interval = 1100, loop = TRUE)),
      
      # Input: Decimal interval with step value ----
      sliderInput("Tree", "Phylogenetic hypothesis:",
                  min = 1, max = 30,
                  value = 1, step=1)
      
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
    plot(frames[[input$Timeframe - 2]][[input$Tree]])
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
