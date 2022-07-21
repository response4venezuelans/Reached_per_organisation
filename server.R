#add upload capacity to shiny to 30MB

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  
  data <- reactive({
    
    df <- df5Wconsolidated
    
    df <-  if(is.null(input$countryinput)) {df}
    else {filter(df, Country %in% input$countryinput) }
    
    df <- if(is.null(input$orginput)) {df}
    else {filter(df, Appealing_org %in% input$orginput)}

    df
  })
  
  # datafinal <- reactive({
  #   
  #  reached <- source("R/ConsolidatedGenerator.R")
  # })
  # 
  
  ## Preview of NUmber of questions
  output$nactivities <- renderText({nrow(data())})
  
  # extensions='FixedColumns'
  
  ## Data Preview
  output$Preview_Data <- DT::renderDataTable({data()}, options = list(
    dom = 'lBftip', 
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 20,
    scrollX = TRUE,
    rownames = TRUE,
    buttons = c('copy', 'excel')
  ))
  
  
  ## Download Selection
  output$download <- downloadHandler(
    filename = function() {
      paste("People Reached Per Organisation", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data(), file)
    }
  )
  
  # ## Download Selection
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste("R4Vql", ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     write_xlsx(data(), file)
  #   }
  # )
  
})