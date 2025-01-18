###### ------------------ CAR PRICES DASHBOARD ------------------------ #######

## ----- Loading the required libraries ------- #####
library("shinydashboard")
library("shiny")
library("tidyverse")
library("fontawesome")
library("plotly")
## ----------- libraries loaded --------------- #####

### ----------------- Designing the framework of the dashboard --------------------- ####

## ---------------------- dashboard user-interface --------------------- ####
ui <- dashboardPage(skin = "purple",
                    
  dashboardHeader(title = "CAR PRICES DASHBOARD",
                  
                  ## --- toggling the title -- ###
                  titleWidth = "calc(100% - 40px)"
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "Dashboard",icon = icon("dashboard")),
      menuItem("Widgets",tabName = "Widgets",icon = icon("cogs"),
               fileInput("file","Upload the car prices csv file:", accept = ".csv"),
               selectInput("factor",
                           "Select categorical variable of choice",
                           choices = c("Make","Model","Year","Condition"),
                           selected = "Make"),
               selectInput("var",
                           "Select numerical variable of choice",
                           choices = c("Mileage","Price"),
                           selected = "Mileage"),
               radioButtons("color",
                            "Choose the histogram appearance",
                            choices = c("black","navy","darkgreen"),
                            selected = "black"),
               sliderInput("bin",
                           "Slde the bin for distribution",
                           min = 0, max = 20000, value = 20000, step = 100))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Dashboard",
        fluidRow(
          valueBoxOutput("observation_box"),
          valueBoxOutput("Price_box"),
          valueBoxOutput("Mileage_box")
        ),
        fluidRow(
          box(
            title = "Category distribution",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("column_chart")
          ),
          box(
            title = "Numeric distribution of Mileage and Price",
            status = "success",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE, 
            plotlyOutput("hist")
          ),
          box(
            title = "Variation in the different categories",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("boxplot")
          ),
          box(
            title = "Average distribution",
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("donut_chart")
          )
        ))
    )
  )
)
## ------------------- end of the dashboard user interface -------------- ###

### ----------------- dashboard server-logic -------------------------------- #####
server <- function(input, output){
  
  ## --------- reading the csv file into the system -------------------- ####
  cp <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  ### ----------- creating value boxes ----------------------- ####
  #### ---- total observation value box ---- ####
  row_count <- reactive({
    nrow(cp())
  })
  output$observation_box <- renderValueBox({
    valueBox(row_count(), "Total Number of Cars", icon = icon("car"), color = "blue")
  })
  
  ### ---- Average price value box ------ ####
  avg_price <- reactive({
    mean(cp()$Price)
  })
  output$Price_box <- renderValueBox({
    valueBox(round(avg_price(), 2), "Average price", icon = icon("money-bill-wave"), color = "green")
  })
  
  ### ---- Average mileage value box ---------- ####
  avg_mileage <- reactive({
    mean(cp()$Mileage)
  })
  output$Mileage_box <- renderValueBox({
    valueBox(round(avg_mileage(), 2), "Average mileage", icon = icon("road"), color = "navy")
  })
  
  
  #### ---------------- Column-chart function --------------------------- #####
  output$column_chart <- renderPlotly(
    ggplotly(
      ggplot(cp(), aes_string(x = input$factor)) +
        geom_bar(fill = "navy") +
        labs(title = paste("The graph below shows the distribution of cars based on their",input$factor),
             x = input$factor, y = "frequency") +
        theme(title = element_text(size = 9, colour = "black", face = "bold"))
    )
  )
  
  ### --------------- Histogram chart function ---------------------------------- #####
  output$hist <- renderPlotly({
    ggplotly(
      ggplot(cp(), aes_string(x = input$var)) +
        geom_histogram(binwidth = input$bin, colour = "white", fill = input$color) +
        labs(title = paste("The histogram below shows",input$var,"of cars"),
             x = input$var, y = "frequency") +
        theme(title = element_text(size = 9, colour = "black", face = "bold"))
    )
  })
  
  ### ----------------- Box-plot function ----------------------------------- #####
  output$boxplot <- renderPlotly({
    ggplotly(
      ggplot(cp(), aes_string(x = input$factor, y = input$var, fill = input$factor)) +
        geom_boxplot(varwidth = TRUE) +
        geom_jitter(alpha = 0.2, width = 0.2) +
        labs(title = paste("The box-plot below shows the variation in",input$var,"in the different",input$factor),
             x = input$factor, y = input$var) +
        theme(title = element_text(size = 9, color = "black", face = "bold")) +
        theme(legend.position = "none")
    )
  })
  
  ### -------------- AVERAGE SEGMENTATION -------------------------------------------- ####
  output$donut_chart <- renderPlotly({
    req(input$factor, input$var)
    
    data_summary <- cp() %>% 
      group_by_at(input$factor) %>% 
      summarise(across(all_of(input$var), mean, na.rm = TRUE))
    
    plot_ly(data_summary,
            labels = ~get(input$factor),
            values = ~get(input$var),
            type = 'pie',
            hole = 0.6) %>% 
      layout(title = paste("The donut chart below shows the average",input$var,"in the different",input$factor,"groups"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  
}
## --------------------- end of dashboard server logic ----------------------- ####

#### ------------------ dashboard running function -------------------------------- ####
shinyApp(ui = ui, server = server)

### --------------- end of the car price dashboard ------------------------------------- #####