#### =========================== INJURY PREDICTION DASHBOARD ========================== ######
# --- Created on 25th June 2024 --- ####

### ----------- Libraries ------------- #####
library("tidyverse")
library("shiny")
library("shinydashboard")
library("plotly")
library("gfonts")
library("fontawesome")

### ========================= Creating the dashboard ========================= #####

#### ----------------- Dashboard User Interface ---------------------- #####
ui <- dashboardPage(skin = "blue",
                    
  ## ------- Title ----------- ####
  dashboardHeader(
    title = "INJURY ANALYSIS DASHBOARD",
    
## ----------------------------- toggling the title of the dashboard -------------------------- #####
    titleWidth = "calc(100% - 60px)"
  ),
  
## -------------------------------- Sidebar of the dashboard ---------------------------------- ####
  dashboardSidebar(
### -------------------------- creating a sidebar menu for the data set ----------------------- ####
    sidebarMenu(
      menuItem("Dashboard",tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "Widgets", icon = icon("cogs"),
               fileInput("file",
                         "Load the Injury data",
                         accept = ".csv"),
               selectInput("factor",
                           "Choose category to plot",
                           choices = c("Previous_Injuries","Recovery_Time","Likelihood_of_Injury"),
                           selected = "Previous_Injuries"),
               selectInput("var",
                           "Choose measure to distribute",
                           choices = c("Player_Age","Player_Height","Training_Intensity"), 
                           selected = "Training_Intensity"),
               sliderInput("bin",
                           "Slide the rador to vary distribution",
                           min = 0, max = 300, step = 0.03, value = 0.3),
               radioButtons("colour",
                            "Vary histogram appearance",
                            choices = c("brown","navy","black"),
                            selected = "black"),
               
               selectInput("VarX",
                           "Choose first variable to relate",
                           choices = c("Player_Age","Player_Height","Training_Intensity"),
                           selected = "Player_Age"
                           ),
               selectInput("VarY",
                           "Choose second variable to relate",
                           choices = c("Player_Age","Player_Height","Training_Intensity"),
                           selected = "Training_Intensity")
               )
    )
  ),
  
## ------------------------------ Body of the dashboard -------------------------------------- ####
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard",
              
### ----------------------------- Row observations cont ------------------------------------ #####
              fluidRow(
                valueBoxOutput("IP_observations_box"),
                valueBoxOutput("Age_avg_box"),
                valueBoxOutput("average_height_box")
              ),
              
### ---------------------------- Visualization output ------------------------------------- #####
              fluidRow(
                
### --------------------------- Bar-chart box --------------------------------------------- #####
                box(
                  title = "PERCENTAGE DISTRIBUTION",
                  status = "success",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotlyOutput("pie_chart")
                ),

## ---------------------------- Histogram box --------------------------------------------- ####
              box(
                title = "MEASURE OF DISTRIBUTION",
                status = "primary",
                collapsible = TRUE,
                solidHeader = TRUE,
                plotlyOutput("num_hist")
              ),
### --------------------------- Scatter-plot box ------------------------------------------- ####
              box(
                width = 12,
                title = "CORRELATION",
                status = "info",
                collapsible = TRUE,
                solidHeader = TRUE,
                plotlyOutput("scatterplot")
              )

              )
              )
    )
  )
)

#### ------------------------ Dashboard Server-Logic --------------------------------------- #####
server <- function(input, output){
  
### ---------------------- Reading the csv file from storage ------------------------------- #####
  IP <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
### ------------------------ Total number of players recorded ------------------------------- ####
  IP_observations <- reactive({
    nrow(IP())
  })
  
### ------------------------ Updating the observation value box created --------------------- ####
  output$IP_observations_box <- renderValueBox({
    valueBox(IP_observations(), "PLAYERS", icon = icon("user"), color = "navy")
  })
  
### ------------------- Average age value of the soccer players ----------------------------- ####
  Age_avg <- reactive({
    mean(IP()$Player_Age)
  })
  
## --------------------------------- Average age value box ---------------------------------- ####
  output$Age_avg_box <- renderValueBox({
    valueBox(round(Age_avg(), 2), "AVERAGE AGE\n(years)", icon = icon("users"), color = "green")
  })
  
#### ------------- Average time of recovery -------------------------- #####
  average_height <- reactive({
    mean(IP()$Player_Height)
  })
  
#### -------------- Average time recovery value box --------------------------- #####
  output$average_height_box <- renderValueBox({
    valueBox(round(average_height(), 2), "AVERAGE HEIGHT\n(cm)", icon = icon("ruler"), color = "fuchsia")
  })
  
#### ------------------------------------------- PIE-CHART ------------------------------------------ #####
  output$pie_chart <- renderPlotly({
    plot_ly(IP(), labels = ~get(input$factor), type = "pie") %>% 
      layout(title = paste("Percentage distribution in",input$factor))
  })
  
#### ------------------------------- HISTOGRAM --------------------------------------------------------- #####
  output$num_hist <- renderPlotly(
    ggplotly(
      ggplot(IP(), aes_string(x = input$var)) +
        geom_histogram(binwidth = input$bin, colour = "white", fill = input$colour) +
        labs(title = paste(input$var,"distribution of soccer players"),
             x = input$var, y = "no. of players") +
        theme(title = element_text(size = 9, colour = "black", face = "bold"))
    )
  )
  
#### ------------------------------- SCATTER-PLOT ----------------------------------------------------- ######
  output$scatterplot <- renderPlotly(
    ggplotly(
      ggplot(IP(), aes_string(x = input$VarX, y = input$VarY, col = input$factor, size = input$factor)) +
        geom_point() +
        labs(title = paste(input$VarX,"and",input$VarY), 
             x = input$VarX, y = input$VarY) +
        geom_smooth(color = "black", se = TRUE, size = 1) +
        theme(title = element_text(size = 9, colour = "black", face = "bold")) +
        scale_x_log10() +
        scale_color_viridis_d() +
        theme_grey()
    )
  )
  
} ### ------------- closes the server function -------------- #####

### --------------- Running the dashboard created --------------------- #####
shinyApp(ui = ui, server = server)

### =================== End of the dashboard ================================= ######