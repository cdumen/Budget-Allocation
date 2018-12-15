library(shiny)

ui <-  fluidPage(
  
  headerPanel('Budget Optimiser'),
  
  sidebarLayout(position='left',
                
                sidebarPanel(
                  # column(12,sidebarPanel(numericInput("total_budget", label="Total Budget", value=""))),
                  # h4('Input for each investment details and click'),
                  # column(12, helpText("Enter details on individual investments.")),
                  column(4, textInput("investment", label="Investment *", value="")),
                  column(4, textInput("cat1", label=span("Category 1", style="color:grey"), value="")),
                  column(4, textInput("cat2", label=span("Category 2", style="color:grey"), value="")),
                  column(6, numericInput("cur_spend", label="Current Spend *", value="")),
                  column(6, numericInput("cur_ret", label="Current Return *", value="")),
                  column(12, numericInput("dim_ret", label=span("Diminishing Return", style="color:grey"), value="")),
                  column(6, numericInput("min_budget", label=span("Minimum Budget", style="color:grey"), value="")),
                  column(6, numericInput("max_budget", label=span("Maximum Budget", style="color:grey"), value="")),
                  actionButton("addButton", "Add"),
                  position='right'
                ),
                
                mainPanel(
                  tableOutput("table")
                )
  )
  
)


server <-  function(input, output, session) { 
  
  dt <- data.frame(Investment = character(),
                   `Category1` = character(),
                   `Category2` = character(),
                   `Current Spend` = numeric(),
                   `Current Return` = numeric(),
                   `Diminishing Return` = numeric(),
                   `Minimum Budget` = numeric(),
                   `Maximum Budget` = numeric(),
                   check.names = F)
  
  data <- reactiveValues(file=dt)
  
  addData <- observeEvent(
    
    input$addButton,
    {
      newLine <- data.frame(Investment = input$investment,
                            `Category1` = input$cat1,
                            `Category2` = input$cat2,
                            `Current Spend` = input$cur_spend,
                            `Current Return` = input$cur_ret,
                            `Diminishing Return` = input$dim_ret,
                            `Minimum Budget` = input$min_budget,
                            `Maximum Budget` = input$max_budget,
                            check.names = F)
      
      dt <- rbind(dt, newLine)
      
      data$file <- dt
      
      updateTextInput(session, "fielda", value = "") 
      updateTextInput(session, "fieldb", value = "") 
      sapply(c('investment', 'cat1', 'cat2', 'cur_spend', 'cur_ret', 'dim_ret', 'min_budget', 'max_budget'), 
             function(x) updateTextInput(session, x, value=""))
    }
  )
  
  
  output$table <- 
    renderTable(data$file, include.rownames=FALSE)
  
}

shinyApp(ui, server)
