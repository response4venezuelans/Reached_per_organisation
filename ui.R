dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    img(src = "r4v.png", height = 40),
    h3("People Reached per Organisation", style="color: #fff; background-color: #672D53; text-align: center"),

    h3(textOutput("nactivities"), style="color: #fff; background-color: #672D53; text-align: center")
    ,
    
    pickerInput(inputId = 'countryinput',
                label = 'Select country:',
                choices = countrylist,
                options = list(`actions-box` = TRUE),
                multiple = TRUE),
    
      pickerInput(inputId = 'orginput',
                         label = 'Select organisation:',
                         choices = orglist,
                         options = list(`actions-box` = TRUE),
                multiple = TRUE),
   

    
    downloadButton("download", "Get People Reached results", style="color: #fff; background-color: #672D53")
    
    
    
  ),
  
  dashboardBody(       fluidRow(  column(12,
                                         p("V.1, August 2022", style="color: #fff; background-color: #672D53"),
                                         
                                         br(),
                                         DT::dataTableOutput("Preview_Data"))
  )))

