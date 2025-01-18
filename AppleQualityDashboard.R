### ------------------- APPLE FARM MONITORING DASHBOARD ---------------------------- #####
## --- CREATED ON 21/04/2024 --- ###

## -- loading the required libraries ----- ####
library("shiny")
library("shinydashboard")
library("plotly")
library("tidyverse")
library("fontawesome")
## --------- loaded packages ----------- ######

#### -------------------------------- DASHBOARD FEATURES CREATED BELOW ---------------------------- #######

### ------------------ DASHBOARD USER-INTERFACE --------------------------- #####
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "APPLE MONITORING DASHBOARD",
                  
                  ### ---- toggling the title ---- ###
                  titleWidth = "calc(100% - 40px)"
                  ),
  
  dashboardSidebar(
    ### ----- creating an overview of the dashboard ---------- ####
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Widget", tabName = "Widget", icon = icon("cogs"),
               fileInput("file", "Load apple csv file:", accept = ".csv"),
               selectInput("factor",
                           "Select categorical variable of choice:",
                           choices = c("Quality"),
                           selected = "Quality"),
               selectInput("Var",
                           "Select discrete variable of choice:",
                           choices = c("Size","Weight","Sweetness","Crunchiness","Juiciness","Ripeness","Acidity"),
                           selected = "Size"),
               selectInput("VarY",
                           "Select alternative variable to relate with descrete variable above",
                           choices = c("Size","Weight","Sweetness","Crunchiness","Juiciness","Ripeness","Acidity"),
                           selected = "Acidity"),
               radioButtons("col",
                            "Click to change histogram appearance:",
                            choices = c("black","navy","darkred"),
                            selected = "black"),
               sliderInput("bin",
                           "Slide the bin for a preferred distribution:",
                           min = -1, max = 10, value = 0.8, step = 0.01)
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard",
              fluidRow(
                valueBoxOutput("ap_observation_box"),
                valueBoxOutput("average_size_box"),
                valueBoxOutput("average_weight_box"),
                valueBoxOutput("average_sweet_box"),
                valueBoxOutput("average_crunchiness_box"),
                valueBoxOutput("average_juice_box"),
                valueBoxOutput("average_ripeness_box"),
                valueBoxOutput("average_acidity_box")
                
              ),
            fluidRow(
              box(
                title = "Proportion",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("pie_chart")
              ),
              box(
                title = "Distirbution",
                status = "danger",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("hist")
              ),
            box(
              title = "Relationship between variables",
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("scatter_plot")
            ),
          box(
            title = "Average distribution",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("aver_dist")
          )
            )
              )
    )
  )
)
### ------------------- END OF THE USER-INTERFACE DASHBOARD --------------- ####

### ---------------- DASHBOARD SERVER LOGIC -------------------------------- ####
server <- function(input, output){
  
  ## ------ file input function ------------------------------------------------- #####
  ap <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  ### -------------------- Creating the value boxes required ---------------------------- #####
  ## -------- number of observations in the data set -------- ####
  ap_row_count <- reactive({
    nrow(ap())
  })
  output$ap_observation_box <- renderValueBox({
    valueBox(ap_row_count(), "Total Number of apples", icon = icon("apple"), color = "green")
  })
  
  ### ------- average size of the apples -------------------- ####
  average_size <- reactive({
    mean(ap()$Size)
  })
  output$average_size_box <- renderValueBox({
    valueBox(round(average_size(), 2), "Average Size", icon = icon("arrow-up"), color = "navy")
  }) 
  
  ### --------- average weight of the apples ----------------- ####
  average_weight <- reactive({
    mean(ap()$Weight)
  })
  output$average_weight_box <- renderValueBox({
    valueBox(round(average_weight(), 2), "Average Weight", icon = icon("balance-scale"), color = "red")
  })
  
  ### ------------ average sweetness level in the apples --------------------- #####
  average_sweetness <- reactive({
    mean(ap()$Sweetness)
  })
  output$average_sweet_box <- renderValueBox({
    valueBox(round(average_sweetness(), 2), "Average Sweetness Level", icon = icon("candy-cane"), color = "fuchsia")
  })
  
  ### -------------- average crunchiness level in the apples ------------------------ #####
  average_crunhiness <- reactive({
    mean(ap()$Crunchiness)
  })
  output$average_crunchiness_box <- renderValueBox({
    valueBox(round(average_crunhiness(), 2), "Average Crunchiness Level", icon = icon("cutlery"), color = "maroon")
  })
  
  #### ------------------ average juiciness level in the apples ------------------------------ #####
  average_juiciness <- reactive({
    mean(ap()$Juiciness)
  })
  output$average_juice_box <- renderValueBox({
    valueBox(round(average_juiciness(), 2), "Average Juiciness Level", icon = icon("tint"), color = "purple")
  })
  
  #### ------------------- average ripeness level in the apples ------------------------------- ######
  average_ripeness <- reactive({
    mean(ap()$Ripeness)
  })
  output$average_ripeness_box <- renderValueBox({
    valueBox(round(average_ripeness(), 2), "Average Ripeness Level", icon = icon("traffic-light"), color = "yellow")
  })
  
  #### ---------------------- average acidity level in the apples ------------------------------- #####
  average_acidity <- reactive({
    mean(ap()$Acidity)
  })
  output$average_acidity_box <- renderValueBox({
    valueBox(round(average_acidity(), 2), "Average Acidity Levels", icon = icon("flask"), color = "lime")
  })
  
############ -------------------------- VISUALIZATION FUNCTIONS -------------------------------- ####################
  
  ### ---------- Pie-chart --------------------------------------------- #######
  output$pie_chart <- renderPlotly({ 
    plot_ly(ap(), labels = ~get(input$factor), type = "pie") %>% 
      layout(title = paste("Distribution of apple",input$factor))
  })
  
  ### -------- Histogram -------------------------------------------------- ######
  output$hist <- renderPlotly(
    ggplotly(
      ggplot(ap(), aes_string(x = input$Var)) + 
        geom_histogram(binwidth = input$bin, color = "white", fill = input$col) +
        labs(title = paste("Distribution level in",input$Var,"of apples"), 
             x = input$Var, y = "frequency") +
        theme(title = element_text(size = 8, color = "black", face = "bold"))
    )
  )
  
  ### ------------------------ Scatter plot function --------------------------------------- #####
  output$scatter_plot <- renderPlotly(
    ggplotly(
      ggplot(ap(), aes_string(x = input$Var, y = input$VarY, col = input$factor)) +
        geom_point() +
        labs(title = paste("Relationship between",input$Var,"and", input$VarY), 
             x = input$Var, y = input$VarY) +
        geom_smooth(method = "lm", color = "black") +
        theme(title = element_text(size = 8, color = "black", face = "bold"))
    )
  )
  
  #### ---------------------------- Average segmentation function --------------------------------- ######
  output$aver_dist <- renderPlotly({
    req(input$factor, input$VarY)
    
    data_summary <- ap() %>% 
      group_by_at(input$factor) %>% 
      summarise(across(all_of(input$VarY), mean, na.rm = TRUE))
    
    plot_ly(data_summary,
           labels = ~get(input$factor),
           values = ~get(input$VarY),
           type = 'pie',
           hole = 0.6) %>% 
      layout(title = paste("Average distrition of",input$VarY,"in",input$factor,"\ncategories"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
           
  })
}
### ------------------ END OF THE SERVER LOGIC ------------------------------ ####



### ---------------- RUNNING FUNCTION --------------------------------- ####
shinyApp(ui = ui, server = server)