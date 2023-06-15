library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

#AnalySlice
#Shiny app for interactive processing of LoadSol pressure insole force data
#Jake Melaro
#https://github.com/KennethTM/FluxBandit

version <- "AnalySlice-v0.2"
options(shiny.maxRequestSize=50*1024^2)


ui <- fluidPage(
  
  # use a gradient in background
  shinyWidgets::setBackgroundColor(
    color = c("#F7FBFF", "#FF8200"),
    gradient = "radial",
    direction = "top"
  ),
  
  titlePanel(version),
  
  em("'This stupid thing is gonna make us rich' -- Claude Shannon"),
  
  p("Follow these steps to select, crop and export your LoadSol data!"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file", "Step 1 | Upload CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(style = "border-top: 1px solid #000000;"),
      
      tags$b("Step 5 | Reset your time index"),
      
      actionButton("reset", "Reset!"),
      
      tags$hr(style = "border-top: 1px solid #000000;"),
      
      tags$b("Step 6 | Download your fresh, new cropped data!"),
      
      downloadButton("download", "Download!"),
      
      tags$hr(style = "border-top: 1px solid #000000;"),
      
      img(src = "Novel.png", height = 95, width = 95),  # Add your logo here
    
      img(src = "Vols.png", height = 95, width = 95),  # Add your logo here
      
      img(src = "Bmch.jpg", height = 95, width = 95),
    ),
    mainPanel(
      
      tags$b("Step 3 | Crop your data by dragging the mouse to highlight the important stuff!"),
      
      plotOutput("plot", brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
      
      tags$b("Step 4 | Make sure everything looks good!"),
      
      plotOutput("plot_zoom")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$plot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = Time_L, y = Force_L, color = "Left")) +
      geom_line() +
      geom_line(aes(x = Time_R, y = Force_R, color = "Right")) +
      scale_color_manual(values = c("Left" = "blue", "Right" = "red")) +
      labs(x = "Time (s)", y = "Force (N)", color = "Leg") +
      theme(
        text = element_text(size = 20,
                            face = 'bold')
      )
  })
  
  data_zoom <- reactiveVal()
  
  observe({
    req(input$plot_brush)
    brushed <- brushedPoints(data(), input$plot_brush)
    data_zoom(brushed)
  })
  
  output$plot_zoom <- renderPlot({
    req(data_zoom())
    ggplot(data_zoom(), aes(x = Time_L, y = Force_L, color = "Left")) +
      geom_line() +
      geom_line(aes(x = Time_R, y = Force_R, color = "Right")) +
      scale_color_manual(values = c("Left" = "blue", "Right" = "red")) +
      labs(x = "Time (s)", y = "Force (N)", color = "Leg") +
      theme(
        text = element_text(size = 20,
                            face = 'bold')
      )
  })
  
  observeEvent(input$reset, {
    df <- data_zoom()
    df$Time_L <- seq(0, by = 0.01, length.out = nrow(df))
    df$Time_R <- seq(0, by = 0.01, length.out = nrow(df))
    data_zoom(df)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(basename(input$file$name)), "_cropped.csv", sep = "")
    },
    content = function(file) {
      write.csv(data_zoom(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
