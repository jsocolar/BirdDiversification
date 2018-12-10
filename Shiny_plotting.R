library(shiny)
library(magick)
"%ni%" <- Negate("%in%")
setwd("/Users/TingleyLab/Dropbox/Work/Diversity_accum")


for(i in 3:50){
  print(i)
  for(j in 1:30){
    print(j)
    p1 <- magick::image_read(paste0("Plots/SF_", i, "MY_", j, ".pdf"), density=300)
    p2 <- image_border(p1,"white","700x500")
    p3 <- image_annotate(p2, "All", size = 70, color = "gray20",
                         location = "+3600+785")
    p3 <- image_annotate(p3, "Tropical forest", size = 70, color = "gray20",
                         location = "+3600+1054")
    p3 <- image_annotate(p3, "Temperate forest", size = 70, color = "gray20",
                         location = "+3600+1323")
    p3 <- image_annotate(p3, "Taiga", size = 70, color = "gray20",
                         location = "+3600+1592")
    p3 <- image_annotate(p3, "Savanna", size = 70, color = "gray20",
                         location = "+3600+1861")
    p3 <- image_annotate(p3, "Grassland", size = 70, color = "gray20",
                         location = "+3600+2130")
    p3 <- image_annotate(p3, "Desert", size = 70, color = "gray20",
                         location = "+3600+2399")
    
    p4 <- image_annotate(p3, "BLOCK", size = 70, color = "white", boxcolor = "white",
                         location = "+1985+550")
    p4 <- image_annotate(p4, "Americas", size = 80, color = "gray20", degrees = -50, location="+920+600")
    p4 <- image_annotate(p4, "N. America", size = 80, color = "gray20", degrees = -50, location="+1396+600")
    p4 <- image_annotate(p4, "S. America", size = 80, color = "gray20", degrees = -50, location="+1872+600")
    p4 <- image_annotate(p4, "Africa", size = 80, color = "gray20", degrees = -50, location="+2348+600")
    p4 <- image_annotate(p4, "Eurasia", size = 80, color = "gray20", degrees = -50, location="+2824+600")
    p4 <- image_annotate(p4, "Australasia", size = 80, color = "gray20", degrees = -50, location="+3300+600")
    p5 <- image_annotate(p4, "M BOX", size=400, color="black", boxcolor="black", location = "+2700+2630")
    p5 <- image_annotate(p5, "M BOX", size=380, color="white", boxcolor="white", location = "+2710+2640")
    p5 <- image_annotate(p5, "M BOX", size=380, color="white", boxcolor="white", location = "+2755+2640")
    p5 <- image_annotate(p5, "Process", size=80, color="black", location = "+3200+2660")
    p5 <- image_annotate(p5, "Combined", size=65, color="gray20", location = "+2900+2850")
    p5 <- image_annotate(p5, "Diversification-rate", size=65, color="gray20", location = "+3420+2790")
    p5 <- image_annotate(p5, "Sympatry-based", size=65, color="gray20", location = "+3420+2920")
    p5 <- image_annotate(p5, "                                                                                                                                                                             ", 
                         size=5, boxcolor="dodgerblue", location = "+2730+2950", degrees = -45)
    p5 <- image_annotate(p5, "                                                                                                                                                   ", 
                         size=5, boxcolor="coral", location = "+3280+2880", degrees = -45)
    p5 <- image_annotate(p5, "                                                                                                                                                    ", 
                         size=5, boxcolor="brown", location = "+3280+3010", degrees = -45)
    
    p5 <- image_annotate(p5, "Species Richness", size=90, color="black", location = "+1530+2630")
    p5 <- image_annotate(p5, "Process contribution", size=95, degrees=-90, color="black", location = "+550+2000")
    pdf(file = paste0("Plots/Shiny_", i, "MY_", j, ".pdf"), width=9.5, height=7, family="sans")
    trimmed <- image_trim(p5)
    par(mar=c(0.1,0.1,0.1,0.1))
    plot(trimmed)
    dev.off()
  } 
}

frames <- list()
for(i in 3:50){
  print(i)
  frames[[i-2]] <- list()
  for(j in 1:30){
    print(j)
    frames[[i-2]][[j]] <- magick::image_read(paste0("Plots/Shiny_", i, "MY_", j, ".pdf"), density=150)
  }
}

for(i in 3:50){
  print(i)
  for(j in 1:30){
    print(j)
    pdf(file = paste0("Plots/ShinyPlots_small/Shiny_", i, "MY_", j, ".pdf"), width=9.5, height=7, family="sans")
    par(mar=c(0.1,0.1,0.1,0.1))
    plot(frames[[i-2]][[j]])
    dev.off()
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
                  value = 5),
      
      # Input: Decimal interval with step value ----
      sliderInput("Tree", "Phylogenetic hypothesis:",
                  min = 1, max = 30,
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
    plot(frames[[input$Timeframe - 2]][[input$Tree]])
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)