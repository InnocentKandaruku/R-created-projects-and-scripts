##### ----------------------------- CREATING A VARIATION DASHBOARD FOR THE SOCCER DATA -------------------------------- #####
## --- loading the required libraries ------ ####
library("shinydashboard")
library("tidyverse")
library("plotly")
library("fontawesome")
library("shiny")
## ------------- loaded libraries ---------- ###

### --------------- user interface of the dashboard --------------- ####
ui <- dashboardPage(skin ="yellow",
                    
  dashboardHeader(title = "ASSOCIATIONS AND VARIATIONS DASHBOARD",
                  
                  ## ------ toggling the title of the dashboard ---------- #####
                  titleWidth = "calc(100% - 50px)"),
  
  ## ----------- creating a side bar menu --------------- #####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Widgets",tabName = "Widgets", icon = icon("cogs"),
               fileInput("file","Choose the Salary Prediction csv file", accept = ".csv"),
               selectInput("factor",
                        "Select the category of your choice",
                        choices = c("League","Position"), selected = "League"),
               selectInput("VarX",
                           "Select the numeric variable",
                           choices = c("Wage","Age","Apps","Caps"),
                           selected = "Wage"))
     
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Dashboard",
        fluidRow(
          valueBoxOutput("ft2_row_box"),
          valueBoxOutput("wage_avg_box"),
          valueBoxOutput("Caps_avg_box"),
          valueBoxOutput("Age_avg_box"),
          valueBoxOutput("Apps_avg_box"),
          valueBoxOutput("Wage_max_box")
        ),
        fluidRow(
          box(
            width = 12,
            status = "success",
            title = "Relationship between numeric variables",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("scatter1")
          ),
          box(
            width = 12,
            title = "Distinct relationship between numeric and categorical variables",
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("Box_plot")
          )
        )
      )
    )
  )
)
## ------------------ end of the user dashboard user-interface ----- ####

### --------------- Dashboard server logic --------------------------- #####
server <- function(input, output){
  ## ----- loading in the salary prediction csv file into the system ---- ####
  ft2 <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  ## ---------------- Info Boxes for a general overview of the loaded csv file ---------------------- #####
  ft2_row <- reactive({
    nrow(ft2())
  })
  
  ### --------------- updating the row count information box -------------------------- ####
  output$ft2_row_box <- renderValueBox({
    valueBox(ft2_row(), "Soccer Players", icon = icon("futbol"), color = "orange")
  })
  
  ### ---------- Average wage -------- ###
  Wage_avg <- reactive({
    mean(ft2()$Wage)
  })
  
  ## ---- Average wage value box ---- ####
  output$wage_avg_box <- renderValueBox({
    valueBox(round(Wage_avg(), 2), "Estimated wage", icon = icon("dollar"), color = "green")
  })
  
  ## -- Average Caps --- ####
  Caps_avg <- reactive({
    mean(ft2()$Caps)
  })
  
  ## -- Average Caps value box -- ###
  output$Caps_avg_box <- renderValueBox({
    valueBox(round(Caps_avg(), 1), "Average Caps awarded", icon = icon("trophy"), color = "teal")
  })
  
  ## - Average age - ####
  Age_avg <- reactive({
    mean(ft2()$Age)
  })
  
  ## -- Average age value box -- ####
  output$Age_avg_box <- renderValueBox({
    valueBox(round(Age_avg(), 1), "Estimated Age", icon = icon("list-alt"), color = "navy")
  })
  
  ## --- Average number of appearance ----------- #####
  Apps_avg <- reactive({
    mean(ft2()$Apps)
  })
  
  ## --- Average number of appearances value box ---- ####
  output$Apps_avg_box <- renderValueBox({
    valueBox(round(Apps_avg(), 1), "Average number of appearances", icon = icon("users"), color = "black")
  })
  
  ## -- Maximum wage of a player -------- ####
  Wage_max <- reactive({
    max(ft2()$Wage)
  })
  
  ## --- Maximum wage render value box ------- ####
  output$Wage_max_box <- renderValueBox({
    valueBox(Wage_max(),"Highest salary", icon = icon("dollar"), color = "red")
  })
  #### ------------------------ ASSOCIATION BTN NUMERICS USING SCATTER PLOT -------------------- #####
  output$scatter1 <- renderPlotly(
    ggplotly(
      ggplot(ft2(), aes_string(x = input$VarX, y = ft2()$Wage, col = input$factor)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", colour = "black") +
        labs(title = paste("The scatter plot below shows the relationship between",input$VarX,"and Wage based on a player's",input$factor,"category"),
             x = input$VarX, y = "Wage") +
        theme(title = element_text(size = 9, color = "black", face = "bold"))
    )
  )
  

  #### ------------------------ ASSOCIATION BTN CATEGORICAL AND TARGET VARIABLE USING BOX-PLOT ------------------------ #####
  output$Box_plot <- renderPlotly(
    ggplotly(
      ggplot(ft2(), aes_string(x = input$factor, y = input$VarX, fill = input$factor)) +
        geom_boxplot(varwidth = TRUE) +
        geom_jitter(alpha = 0.35, width = 0.25) +
        coord_flip() +
        labs(title = paste("The box-plot below shows the variation in palyers",input$VarX,"in the different",input$factor,"categories"),
             x = )
        
    )
  )
}
## ----------------- end of the dashboard server logic --------------- #####

### ---------------- running the dashboard created -------------------- #####
shinyApp(ui = ui, server = server)
