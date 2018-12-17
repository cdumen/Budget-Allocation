library(shiny)
library(data.table)
library(DT)

# UI ----
ui <-  fluidPage(
  
  # theme = 'bootstrap.css',
  includeCSS("styles.css"),
  
  strong(h1('Budget Allocator', style='color:#4b84ce')),
  
  # side bar ----
  sidebarLayout(position='left',
                
                sidebarPanel(

                  numericInput("total_budget", label=span(h4("Total Budget *")), value=""),
                  
                  tags$hr(),
                  
                  column(12, span(h4("Enter investment details"), style="color:#DEB887")),
                  
                  column(4, textInput("investment", label="Investment *", value="")),
                  column(4, textInput("cat1", label=span("Group 1", style="color:grey"), value="")),
                  column(4, textInput("cat2", label=span("Group 2", style="color:grey"), value="")),
                  column(6, numericInput("cur_spend", label="Current Spend *", value="")),
                  column(6, numericInput("cur_ret", label="Current Return *", value="")),
                  column(12, numericInput("dim_ret", label=span("Diminishing Return", style="color:grey"), value="")),
                  column(6, numericInput("min_budget", label=span("Minimum Budget", style="color:grey"), value="")),
                  column(6, numericInput("max_budget", label=span("Maximum Budget", style="color:grey"), value="")),
                  
                  actionButton("addButton", "Add", class='lpan'),
                  
                  tags$hr(),
                  
                  fileInput("file1", label=HTML('<h4 style="color:#DEB887;">And/or upload CSV file
                                                </br><span><h5><lw>Make sure columns are in the same order shown on the right</lw></h5></span>
                                                </h4>'),
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv"))
                  
                  ), # sidebarPanel end
                
                
                # main panel ----
                mainPanel(
                  
                  DT::DTOutput("table"),
                  
                  # create proxy variable ('input$lastClickId') in JS client which updates on button click
                  tags$script("$(document).on('click', '#table button', function () {
                              Shiny.onInputChange('lastClickId',this.id);
                              Shiny.onInputChange('lastClick', Math.random())
                              });"),
                  
                  tags$hr(),
                  
                  column(3, actionButton('optButton', 'Go', class='opt'))
                  
                ) # mainPanel
  ) # siderbarLayout
) # fluidPage


# server ----
server <-  function(input, output, session) { 
  
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
    
    # and add this on top
    data$file <- data.table(rbind(newLine, data$file))
    
    # clear fields
    sapply(c('investment', 'cat1', 'cat2', 'cur_spend', 'cur_ret', 'dim_ret', 'min_budget', 'max_budget'), 
           function(x) updateTextInput(session, x, value=""))
    
  })# observeEvent: addButton
  
  
  # observe event: addCSV ----
  addCSV <- observeEvent(input$file1, {
    csv <- read.csv(input$file1$datapath, check.names = FALSE)
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
    
    # return dt formatted and DO NOT escape strings
    datatable(format(dt, digits=5), escape=F, selection='none', options = list(pageLength = 10, dom = 'tip', 
                                                             # call JS to format header
                                                             initComplete = JS(
                                                               "function(settings, json) {",
                                                               "$(this.api().table().header()).css({'background-color': '#4b84ce', 'color': '#fff'});",
                                                               "}"),
                                                             
                                                             # call JS to format cells: display no more than 10 characters in each
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
      
      h3('Edit input', align='center'),
      hr(),
      
      # return edit table
      dataTableOutput('edit_table'),
      
      # add save button
      actionButton('save_changes', "Save changes"),
      
      
      tags$script(HTML("$(document).on('click', '#save_changes', function() {
                       var list_value=[]
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       {
                       list_value.push($( '.new_input' )[i].value)
                       }
                       Shiny.onInputChange('newValue', list_value)
                       });") # HTML
                  ) # tags$script
      
    ), # fluidPage
    
    size = 'l'
    
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
    edit_row <- gsub('edit_', '', input$lastClickId)
    old_row <- dt[edit_row]
    
    # loop through each column to and record input type and id
    new_row <- list()
    for(i in colnames(old_row)) {
      if(is.numeric(old_row[i])) {
        new_row[[i]] <- paste0('<input class="new_input" type="number" id=new_', i, '>')
      } else {
        new_row[[i]] <- paste0('<input class="new_input" type="text" id=new_', i, '>')
      } # else
    } # for
    
    # output table consists of new row and old row
    new_row <- as.data.table(new_row)
    setnames(new_row, colnames(old_row))
    DT <- rbind(old_row, new_row)
    rownames(DT) <- c('Current input', 'New input')
    DT
  }, # renderDataTable reactive
  
  escape=F, options=list(dom='t',ordering=F), selection='none'
  
  )# renderDataTable: edit modal
  
  
} # server end

shinyApp(ui, server)
