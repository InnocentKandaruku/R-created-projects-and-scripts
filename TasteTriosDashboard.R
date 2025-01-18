###### ---------------------------- TASTE TRIOS DASHBOARD REPORT --------------------------------- #####

## ------------ loading the required libraries for the dashboard ---------------- #####
library("shinydashboard")
library("tidyverse")
library("plotly")
library("shiny")
library("fontawesome")
## -------------------- end of the loaded libraries ------------------------------ #####

#### ------------------- creating the dashboard for the taste trios data set -------------------- ####

##### ---------------------- User-Interface ---------------------------------- ####
ui <- dashboardPage(skin = "green",
                    
  dashboardHeader(title = "TASTE TRIOS DASHBOARD",
                  
                  ## ------------ toggling the title of the dashboard ----------- #####
                  titleWidth = "calc(100% - 40px)"),
  
  dashboardSidebar(
    ## -- creating a sidebar menu for the data set -- ####
    sidebarMenu(
      menuItem("Dashboard",tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Widgets",tabName = "Widgets",icon = icon("cogs"),
               fileInput("file",
                         "Choose the TasteTrios csv file",
                         accept = ".csv"),
               selectInput("factor","Select the category to visualize",
                           choices = c("Ingredient.1","Classification.Output"),
                           ), selected = "Ingredient.1")
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard",
              
## ------------- observation box fluid row ----------------------------------------- ####
              fluidRow(
                valueBoxOutput("observation_box")
              ),

## ----------------- frequency fluid row -------------------------------------------- ####
              fluidRow(
                valueBoxOutput("high_freq"),
                valueBoxOutput("moderate_freq"),
                valueBoxOutput("low_freq")
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Visualization of selected category plot area.",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("Column_Plot")
                )
              )              
)
    )
  )
)
## -------------------------- end of the dashboard user-interface ------------- ####

#### ------------------------ server-logic of the dashboard ------------------------ ####
server <- function(input, output){
  
  ### -------- reading the csv file from the device's memory ----------- ####
  Tt <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
   
  ### ------------ creating a value box for the count of observations used ------------ ####
  observations <- reactive({
    nrow(Tt())
  })
  
  ## --------------- updating the value box created -------------------------------- ###
  output$observation_box <- renderValueBox({
    valueBox(observations(), h4("Total Number of Ingredients"), icon = icon("cube"), color = "aqua")
  })
  
  ## ----------------- high compatibility frequency ---------------------------------- ####
  output$high_freq <- renderValueBox({
    high_freq <- sum(Tt()$Classification.Output == "Highly Compatible")
    valueBox(high_freq, h4("Highly Compatible"), icon = icon("thumbs-up"), color = "green")
  })
  
  ## ------------------ Display frequency of Moderately compatible --------------------------- ####
  output$moderate_freq <- renderValueBox({
    moderate_freq <- sum(Tt()$Classification.Output == "Moderately Compatible")
    valueBox(moderate_freq, h4("Moderately Compatible"), icon = icon("hand-paper"), color = "orange")
  })
  
  ## ----------------------- Display frequency of compatible --------------------------------- ####
  output$low_freq <- renderValueBox({
    low_freq <- sum(Tt()$Classification.Output == "Compatible")
    valueBox(low_freq, h4("Compatible"), icon = icon("thumbs-down"), color = "red")
  })
  
  ## ------------------------- Creating a funnel chart for the categories --------------------------------------- ####
  output$Column_Plot <- renderPlotly({
    p <- ggplot(Tt(), aes_string(x = input$factor)) +
      geom_bar(fill = "navy") +
      labs(title = paste("The column chart below shows the frequency of",input$factor,"categories"),
           x = input$factor, y = "frequency") +
      theme_grey() +
      theme(title = element_text(size = 9, colour = "black", face = "bold"))
    
    ##convert ggplot object to plotly
    ggplotly(p)
  })
  
 
}
### -------------------------- end of the dashboard server logic ------------------- #####

### ------------------------ Running the dashboard created --------------------------- ####
shinyApp(ui = ui, server = server)
####### ------------------------------ END OF THE TASTE TRIOS DASHBOARD REPORT -------------------------------------- #####
