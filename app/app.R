# rm(list = ls())

library(shiny)
library(data.table)
library(DT)
library(plotly)
library(shinycssloaders)

# disable the scientific formatting of numbers
options(scipen = 999)

# UI ----
ui <-  fluidPage(
  
  # theme <- 'bootstrap.css',
  includeCSS("styles.css"),
  
  # tabs ----
  navbarPage(title = div('Budget Allocator', style='color:#4b84ce;font-size:35px;width:300px;'), id='navbar',
              
    # tabs: input ----          
    tabPanel('Input',
             
             sidebarLayout(position='left',
                           
                           # side bar ----
                           sidebarPanel(
                             
                             # side bar: add invesment ----
                             column(12, span(h4("Enter details of each investment"), style="color:#DEB887; padding-top:0px;")),
                             
                             column(4, textInput("investment", label=span("Investment *", style='color:#4b84ce'), value="")),
                             column(4, textInput("cat1", label=span("Group 1", style="color:#4b84ce;opacity:0.6;"), value="")),
                             column(4, textInput("cat2", label=span("Group 2", style="color:#4b84ce;opacity:0.6;"), value="")),
                             column(6, numericInput("cur_spend", label=span("Current Spend *", style='color:#4b84ce'), value="")),
                             column(6, numericInput("cur_ret", label=span("Current Return *", style='color:#4b84ce'), value="")),
                             column(12, numericInput("dim_ret", label=span("Diminishing Return", style="color:#4b84ce;opacity:0.6;"), value="")),
                             column(6, numericInput("min_budget", label=span("Minimum Budget", style="color:#4b84ce;opacity:0.6;"), value="")),
                             column(6, numericInput("max_budget", label=span("Maximum Budget", style="color:#4b84ce;opacity:0.6;"), value="")),
                             
                             actionButton("addButton", "Add", class='lpan'),
                             
                             tags$hr(),
                             
                             # side bar: upload CSV ----
                             # Make sure columns are in the same order as table on the right
                             fileInput("file1", label=HTML('<h4 style="color:#DEB887;">And/or upload CSV file
                                                           </br><span><h5><lw></lw></h5></span>
                                                           </h4>'),
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             
                             tags$hr(),
                             
                             # side bar: enter budget and allocate ----
                             numericInput("total_budget", label=span(h4("Total Budget *", style='color:#4b84ce; margin-top:0px;')), value="", width='74%'),
                             
                             div(style='position:absolute;left:24.6em;bottom:4em',actionButton('allocateButton', strong('ALLOCATE'), class='opt'))
                             
                             ), # sidebarPanel end
                           
                           
                           # main panel ----
                           mainPanel(
                             
                             actionButton('clear_all', 'Clear all', class='clear'),
                             
                             tags$hr(),
                             
                             DT::DTOutput("table"),
                             
                             # create proxy variable ('input$lastClickId') in JS client which updates on button click
                             tags$script("$(document).on('click', '#table button', function () {
                                         Shiny.onInputChange('lastClickId',this.id);
                                         Shiny.onInputChange('lastClick', Math.random())
                                         });")
                  
                           ) # mainPanel
                    ) # siderbarLayout
              ), # tabPanel 
    
    # tabs: stacked chart ----
    tabPanel('Allocation Process', sidebarLayout(position='left',
                                   
                                    sidebarPanel(
                                      h2('The allocation process', style='color:#DEB887;'),
                                      
                                      tags$hr(), tags$hr(), tags$hr(),
                                      
                                      p('Your budget is allocated using a step-by-step process.', 
                                        style='font-size:18px;color:grey'),
                                      
                                      p('At each step, we increase the spend by a small increment and look at the return it gives 
                                        (how we estimate the return is outlined in the Cumulative Return section).', 
                                        style='font-size:18px;color:grey'),
                                      
                                      # paste()
                                      
                                      p('From this, we can calculate the return of investment (ROI) and the rate of return for the increase in spend (marginal return).', 
                                        style='font-size:18px;color:grey'),
                                      
                                      p('We do this for every investment at each step for 1000 steps until we reach the total budget.', 
                                        style='font-size:18px;color:grey'),
                                      
                                      tags$hr(), tags$hr(), tags$hr(),
                                      
                                      p('This chart shows how your budget is allocated and the optimal spend combinations every step of the way - 
                                        hover over it to see which investments the colours corresponds to!', 
                                        style='font-size:18px;color:#DEB887;font-style:italic;'),
                                      
                                      style='margin-top:40px;;'
                                      
                                      ),
                                   
                                    mainPanel(div(withSpinner(plotlyOutput('stacked_ch')), align='center')))),
    
    # tabs: Tab2 ----
    tabPanel('Tab2', plotlyOutput('pie_ch'))
    
  ) # tabsetPanel

) # fluidPage


# server ----
server <-  function(input, output, session) { 
  
  # hide non-input tabs by default
  hideTab('navbar', 'Allocation Process')
  hideTab('navbar', 'Tab2')
  
  # refresh ----
  observeEvent(input$clear_all, {session$reload()})
  
  # csv data cols ----
  dt <- 'Investment,Group1,Group2,Current Spend,Current Return,Diminishing Return,Minimum Budget,Maximum Budget'
  write(dt, file='dt.csv')
  
  # set data as reactive values
  data <- reactiveValues()
  data$file <-  read.csv('dt.csv', check.names = F)
  
  # observe event: add data ----
  addData <- observeEvent(input$addButton,{
    
    # record manual user input
    newLine <- data.frame(Investment = input$investment,
                          `Group1` = input$cat1,
                          `Group2` = input$cat2,
                          `Current Spend` = input$cur_spend,
                          `Current Return` = input$cur_ret,
                          `Diminishing Return` = input$dim_ret,
                          `Minimum Budget` = input$min_budget,
                          `Maximum Budget` = input$max_budget,
                          check.names = F)
    
    # and add on top of existing data
    data$file <- data.table(rbind(newLine, data$file))
    
    # define input type for cols
    for(i in 1:ncol(data$file)) {
      if(i %in% 4:8) {
        data$file[[i]] <- as.numeric(data$file[[i]])
      } else {
        data$file[[i]] <- as.character(data$file[[i]])
      }
    }
    
    # clear fields
    sapply(c('investment', 'cat1', 'cat2', 'cur_spend', 'cur_ret', 'dim_ret', 'min_budget', 'max_budget'), 
           function(x) updateTextInput(session, x, value=""))
    
  })# observeEvent: addButton
  
  
  # observe event: addCSV ----
  addCSV <- observeEvent(input$file1, {
    
    # read in csv
    csv <- read.csv(input$file1$datapath, check.names = FALSE)
    
    # and add on top of existing data
    colnames(csv) <- colnames(data$file)
    data$file <- data.table(rbind(csv, data$file))
  })
  
  
  # render table: main output ----
  output$table <- renderDataTable({
    
    dt <- data$file
    
    # modification buttons
    if(nrow(dt)>0) {
      
      # column of edit buttons
      dt[[" "]] <- paste0('
                           <div class="btn-group" role="group" aria-label="Basic example">
                           <button type="button" class="btn btn-secondary edit" id=edit_',1:nrow(data$file),'>Edit</button></div>
                           ')
      # column of delete buttons
      dt[["  "]] <- paste0('
                           <div class="btn-group" role="group" aria-label="Basic example">
                           <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(data$file),'>x</button></div>
                           ')

      # column of checkboxes
      # dt[[' ']] <- paste0('<input type="checkbox" name="select_row" value="Row', 1:nrow(dt), '">')
      
    }# if nrow(dt)>0
    
    # return dt formatted
    datatable(format(dt, digits=7), 
              # DO NOT escape strings
              escape=F, 
              # disable selection
              selection='none', 
              # show all input but limit to 10 entries per page
              options = list(pageLength = 10, dom = 'tip',
                             
                             # change default empty table message
                             language = list(zeroRecords = "No details entered"),
                             
                             # call JS to format header
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4b84ce', 'color': '#fff'});",
                               "}"),
                             
                             # call JS to format cells: display no more than 10 characters in each cell
                             columnDefs = list(list(
                               targets = c(0,1,2,3,4,5,6,7),
                               render = JS(
                                 "function(data, type, row, meta) {",
                                 "return type === 'display' && data.length > 10 ?",
                                 "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                                 "}") # JS
                             ) # list: inner columnDefs list
                             ) # list: outer columnDefs list
              ), # list: options
              
              callback = JS('table.page(3).draw(false);')
              
    ) # datatable
  }) # renderDataTable
  
  
  # UI: edit modal ----
  edit_modal <- modalDialog(
    
    fluidPage(
      
      h3('Edit input', align='center', style='color:#DEB887'),
      hr(),
      
      # return edit table
      dataTableOutput('edit_table'),
      
      # adjust width of modal
      tags$head(tags$style(HTML('.modal-lg{width:1100px;}'))),
      
      # changes pushed to a list in HTML and assign newValue as inputs
      tags$script(HTML("$(document).on('click', '#save', function() {
                       var list_value=[]
                       for (i = 0; i < $('.new_input').length; i++)
                       {
                       list_value.push($('.new_input')[i].value)
                       }
                       Shiny.onInputChange('newValue', list_value)
                       });") # HTML
                  ), # tags$script
      
      # close button
      actionButton('close_modal', 'X', class='close')
      
    ), # fluidPage
    
    # add save button
    footer=actionButton('save', "Save"),
    
    size = 'l', easyClose = TRUE
    
  )# modalDialog
  
  
  # observe event: modification button clicks ----
  observeEvent(input$lastClick, {
    
    # delete row
    if(input$lastClickId%like%'delete') {
      delete_row <- as.numeric(gsub('delete_', '', input$lastClickId))
      data$file <- data$file[-delete_row]
      
      # show edit modal
    } else if(input$lastClickId %like% 'edit') {
      showModal(edit_modal)
    }
    
  }) # observeEvent

  
  # render table: edit modal ----
  output$edit_table <- renderDataTable({
    
    # extract original values of selected row
    edit_row <- as.numeric(gsub('edit_', '', input$lastClickId))
    old_row <- data$file[edit_row]
    
    # loop through each column to generate HTML input for corresponding types
    # pre-fill new input fields with original values
    new_row <- list()
    for(i in 1:ncol(data$file)) {
      if(i %in% 4:8) {
        new_row[[i]] <- paste(c('<input class="new_input" type="number" id=new_', i, ' value=', old_row[[i]], '>'), collapse='')
      } else {
        new_row[[i]] <- paste(c('<input class="new_input" type="text" id=new_', i, ' value="', as.character(old_row[[i]]), '">'), collapse='')
      } # else
    } # for

    # output table as row of the new fields
    new_row <- as.data.table(new_row)
    setnames(new_row, colnames(old_row))
    new_row

  }, # renderDataTable reactive
  
  escape=F, selection='none', rownames=FALSE, options=list(dom='t',ordering=F)
  
  )# renderDataTable: edit modal
  
  
  # observe event: save ----
  observeEvent(input$newValue, {
    
    # convert input into correct type
    newValue <- list()
    
    for(i in 1:length(input$newValue)) {
      if(i %in% 4:8) {
        newValue[[i]] <- as.numeric(as.character(input$newValue[i]))
      } else {
        newValue[[i]] <- input$newValue[i]
      }
    }

    # put values into data frame and substitue row in data
    dt <- data.frame(newValue, stringsAsFactors = FALSE)
    colnames(dt) <- colnames(data$file)
    suppressWarnings(data$file[as.numeric(gsub('edit_', '', input$lastClickId))] <- dt)
    
    # close modal
    removeModal()
    
  }) # observeEvent:save
  
  
  # close modal ----
  observeEvent(input$close_modal, {removeModal()})
  
  
  # allocate budget ----
  observeEvent(input$allocateButton, {
    
    total_budget <- input$total_budget
    
    dt <- data.frame(data$file)
    
    # increment will be set to generate 1000 iterations by default
    increment <- (total_budget - sum(dt$`Minimum Budget`))/1000
    
    # call source of calculations
    source('calcs/main.R', local=TRUE)
    
    showTab('navbar', 'Allocation Process')
    showTab('navbar', 'Tab2')
    
    updateTabsetPanel(session, 'navbar', 'Allocation Process')
    
    output$stacked_ch <- renderPlotly(stacked_ch)
    
  })
  
  
} # server end

shinyApp(ui, server)

