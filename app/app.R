rm(list = ls())

library(shiny)
library(data.table)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyalert)

# disable the scientific formatting of numbers
options(scipen = 999)

# UI ----
ui <-  fluidPage(
  
  # theme <- 'bootstrap.css',
  includeCSS("styles.css"),
  
  # set up shinyalert
  useShinyalert(),
  
  # tabs ----
  navbarPage(title = div('Budget Allocator', style='color:#4b84ce;font-size:35px;width:300px;'), id='navbar',
              
    # tabs: input ----          
    tabPanel('Input',
             
             sidebarLayout(position='left',
                           
                           # side panel ----
                           sidebarPanel(
                             
                             # side panel: period covered ----
                             numericInput("weeks_covered", label=span("Period Investments Covered (in weeks) *", style="color:#4b84ce;;font-size:15px;"), value=""),
                             
                             tags$hr(),
                             
                             # side panel: add invesment ----
                             column(12, div(p("Enter details of each investment"), style="color:#DEB887;padding-top:0px;font-size:16px")),
                             
                             column(4, textInput("investment", label=span("Investment *", style='color:#4b84ce;font-size:12px;'), value="")),
                             column(4, textInput("cat1", label=span("Group 1", style="color:#4b84ce;opacity:0.6;font-size:12px;"), value="")),
                             column(4, textInput("cat2", label=span("Group 2", style="color:#4b84ce;opacity:0.6;font-size:12px;"), value="")),
                             column(6, numericInput("cur_spend", label=span("Current Spend *", style='color:#4b84ce;font-size:12px;'), value="")),
                             column(6, numericInput("cur_ret", label=span("Current Return *", style='color:#4b84ce;font-size:12px;'), value="")),
                             column(6, numericInput("weeks", label=span("Weeks *", style="color:#4b84ce;;font-size:12px;"), value="")),
                             column(6, numericInput("dim_ret", label=span("Spend at Diminishing Return", style="color:#4b84ce;opacity:0.6;font-size:12px;"), value="")),
                             column(6, numericInput("min_budget", label=span("Minimum Budget", style="color:#4b84ce;opacity:0.6;font-size:12px;"), value="")),
                             column(6, numericInput("max_budget", label=span("Maximum Budget", style="color:#4b84ce;opacity:0.6;font-size:12px;"), value="")),
                             
                             actionButton("addButton", "Add", class='lpan'),
                             
                             tags$hr(),
                             
                             # side panel: upload CSV ----
                             # Make sure columns are in the same order as table on the right
                             fileInput("file1", label=HTML('<div style="color:#DEB887;font-size:16px;font-weight:lighter"> And/or upload CSV file </div>'),
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             
                             tags$hr(),
                             
                             # side panel: enter budget and allocate ----
                             numericInput("total_budget", label=span("Total Budget *", style="color:#4b84ce;;font-size:15px;"), value="", width='74%'),
                             
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
                                   
                                    # side panel: allocation process description ----
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
                                      
                                      style='margin-top:40px;'
                                      
                                      ), # sidebarPanel
                                   
                                    # main panel: stacked ch ----
                                    mainPanel(div(withSpinner(plotlyOutput('stacked_ch')), align='center'))
                                    
                              ) # sidebarLayout
             ) # tabPanel
    
  ) # tabsetPanel

) # fluidPage


# server ----
server <-  function(input, output, session) { 
  
  # hide non-input tabs by default
  hideTab('navbar', 'Allocation Process')
  
  # refresh ----
  observeEvent(input$clear_all, {session$reload()})
  
  # csv data cols ----
  dt <- 'Investment,Group1,Group2,Current Spend,Current Return,Weeks,Diminishing Return,Min Budget,Max Budget'
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
                          `Weeks` = input$weeks,
                          `Diminishing Return` = input$dim_ret,
                          `Min Budget` = input$min_budget,
                          `Max Budget` = input$max_budget,
                          check.names = F)
    
    # check that all compulsory fields are filled in
    if(input$investment == '' | is.na(input$cur_spend) | is.na(input$cur_ret) | is.na(input$weeks)) {
      
      # return error modal if fail criteria check
      shinyalert('Missing/Invalid Input', 'Check that the fields "Investment", "Current Spend", "Current Return" and "Weeks" are filled in correctly.', type='error')
      
    } else {
      
      # if no error, and add on top of existing data
      data$file <- data.table(rbind(newLine, data$file))
      
      # define input type for cols
      for(i in 1:ncol(data$file)) {
        if(i %in% 4:9) {
          data$file[[i]] <- as.numeric(data$file[[i]])
        } else {
          data$file[[i]] <- as.character(data$file[[i]])
        }
      }
      
      # clear fields
      sapply(c('investment', 'cat1', 'cat2', 'cur_spend', 'cur_ret', 'weeks', 'dim_ret', 'min_budget', 'max_budget'), 
             function(x) updateTextInput(session, x, value=""))
      
    } # else
    
  })# observeEvent: addButton
  
  
  # observe event: addCSV ----
  addCSV <- observeEvent(input$file1, {
    
    # read in csv
    csv <- read.csv(input$file1$datapath, check.names = FALSE)
    
    # check number of columns match
    if(ncol(csv) != 9) {
      shinyalert('Invalid Format','Please make sure number of columns matches table shown', type='error')
      
    } else {
      
      colnames(csv) <- colnames(data$file)
      
      dt <- data.frame(csv)
      
      # check that the Investment, Current.Spend & Current.Return cols have the correct type and no NAs
      if((!all(dt$Investment != '') |
          !(is.numeric(dt$Current.Spend))) | !(all(!is.na(dt$Current.Spend))) | 
         !(is.numeric(dt$Current.Return)) | !(all(!is.na(dt$Current.Return))) |
         !(is.numeric(dt$Weeks)) | !(all(!is.na(dt$Weeks)))) {
        
        # return error modal if fail criteria check
        shinyalert("Invalid/Missing Input","The CSV must have the same columns order as table shown. 
                   'Investment', 'Current Spend', 'Current Return' and 'Week' must be completely filled in.", 
                   type="error")
        
      } else {
        
        # if no error, add csv top of existing data
        data$file <- data.table(rbind(csv, data$file))
        
      } # else: missing data
    } # else: incorrect no. of cols
    
  }) # observeEvent: addCSV
  
  
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
    datatable(format(dt, digits=20), 
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
                             
                             # call JS to format cells: limit the number of characters in each cell
                             columnDefs = list(list(
                               targets = c(0,1,2,3,4,5,6,7,8),
                               render = JS(
                                 "function(data, type, row, meta) {",
                                 "return type === 'display' && data.length > 7 ?",
                                 "'<span title=\"' + data + '\">' + data.substr(0, 7) + '...</span>' : data;",
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
      tags$head(tags$style(HTML('.modal-lg{width:1300px;}'))),
      
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
      if(i %in% 4:9) {
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
    
    weeks_covered <- input$weeks_covered
    
    total_budget <- input$total_budget
    
    dt <- data.frame(data$file)
    
    # error if no weeks covered input
    if(!is.numeric(weeks_covered)) {
      shinyalert("You forgot something!","Please enter 'Period Investments Covered'", type="error")
      
    } else {
      # error if no total budget input
      if(!is.numeric(total_budget)) {
        shinyalert("You forgot something!","Please enter 'Total Budget'", type="error")
        
      } else {
        
        # error if no investment details entered
        if(nrow(dt)==0) {
          shinyalert("You forgot something!","Please input investment details", type="error")
          
        } else {
          
          # call source of calculations
          source('calcs/main.R', local=TRUE)
          
          # unhide tabs
          showTab('navbar', 'Allocation Process')
          
          # direct user to the summary page
          updateTabsetPanel(session, 'navbar', 'Allocation Process')
          
          # render output
          output$stacked_ch <- renderPlotly(stacked_ch)
          
        } # else: no investment deets
      } # else: no total budget
    } # else: no period covered
    
    
  }) # observeEvent: allocate
  
  
} # server end

shinyApp(ui, server)

