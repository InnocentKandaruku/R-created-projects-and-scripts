###### ----------------- SOCCER DASHBOARD FOR REPORTING INSIGHTS ------------------------- ########

### ----------- Libraries ------------------------- #####
library("shinydashboard")
library("tidyverse")
library("shiny")
library("fontawesome")
library("plotly")
## ---------------- end of libraries --------------- #####

## ---- must do in this dashboard ---- ####
# - background color of the body.
# - font style of the words.


### --------------------- Creating the dashboard ------------------------- #####

### --------------- user interface -------------------------------- ######
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "SOCCER DASHBOARD REPORT",
                  
                  ## ---- toggling the title of the dashboard ----- ###
                  titleWidth = "calc(100% - 100px)"
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "Data", icon = icon("database")),
      menuItem("Widgets", tabName = "Widgets", icon = icon("cogs"),
               fileInput("file", "Choose the soccer csv file", accept = ".csv"),
               selectInput("factor",
                           "Select the categorical variable",
                           choices = c("League","Position"),
                           selected = "League"),
               selectInput("Var",
                           "Select numerical variable",
                           choices = c("Wage","Age","Apps","Caps"),
                           selected = "Caps"),
               sliderInput("bin",
                           "Slide for best binwidth",
                           min = 0, max = 2000000, value = 20, step = 0.5),
               radioButtons("col",
                            "Choose Appearance of histogram",
                            choices = c("black","navy","purple"),
                            selected = "black")),
      menuItem("Charts", tabName = "Charts", icon = icon("line-chart"))
     
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Data",
              fluidRow(
                div(style = "width: 1000px; height: 400px;", valueBoxOutput("row_count_box")),
                valueBoxOutput("wage_box"),
                valueBoxOutput("Apps_box"),
                valueBoxOutput("wage_avg_box")
              ),
              fluidRow(
                       valueBoxOutput("Age_box"),
                       valueBoxOutput("Apps_avg_box"),
                       valueBoxOutput("Caps_box")
                  
                
              )
              ),
      tabItem(
        tabName = "Charts",
        fluidRow(
          box(
            title = "Player distribution",
            plotlyOutput("barchart"),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success"
          ),
          box(
            title = "distribution",
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("histogram")
          ),
          box(
            width = 12,
            title = "Relationship",
           # status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("scatter")
          ),
          box(
            title = "variation",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("boxplot")
          ),
          box(
            title = "Frequency",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("freq")
          ),
          box(
            width = 12,
            title = "Average segmentation",
            solidHeader = TRUE,
            status = "danger",
            collapsible = TRUE,
            plotlyOutput("barchart2")
          )
        )
      
      )
    )
    
  )
)
### --------------- end of the user interface --------------------- #######

### --------------- server logic of the dashboard ------------------ #####


## --------- computing the number of players in the soccer data ------------------ ######
server <- function(input, output){
  ### ------------- reading in the data from the csv file ------------------------------------ ######
  ft <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  #### ---------- count the number of rows ------------------------------------- #####
  row_count <- reactive({
    nrow(ft())
  })
  
  ### ------------ updating the value box for the row count --------------------------------- ######
  output$row_count_box <- renderValueBox({
   valueBox(row_count(), h2("SOCCER\nPLAYERS"), icon = icon("futbol"), color = "orange") 
  
    
  })
  
  ### ------------- Maximum wage paid to the players ------------------------------- #####
  wage_max <- reactive({
    max(ft()$Wage)
  })
  
  ### ------------------ value box of the maximum wage ------------------------------ #####
  output$wage_box <- renderValueBox({
    valueBox(wage_max(), h2("MAXIMUM\nWAGE"), icon = icon("dollar"), color = "purple")
  })
  
 
  
  
  ### -------------------- Highest number of appearances made by the players --------------------- #####
  Apps_max <- reactive({
    max(ft()$Apps)
  })
  
  ### --------------------- value box of the maximum apps ----------------------------- #####
  output$Apps_box <- renderValueBox({
    valueBox(Apps_max(), h2("HIGHEST NUMBER OF APPEARANCES"), icon = icon("list-alt"), color = "navy")
  })
  
  ### --------------------- Average wage earned in the soccer --------------------------------------- #####
  wage_avg <- reactive({
    mean(ft()$Wage)
  })
  
  ### ------------------- Average wage value box ------------------------------------------- ######
  output$wage_avg_box <- renderValueBox({
    valueBox(round(wage_avg(), 2), h2("AVERAGE WAGE"), icon = icon("dollar"), color = "green")
  })
  
  ### ----------------- Average age of the soccer players ------------------------------ #####
  Age_avg <- reactive({
    mean(ft()$Age)
  })
  
  ### ----------------- Average age value box ------------------------------------------ #####
  output$Age_box <- renderValueBox({
    valueBox(round(Age_avg(), 2), h2("AVERAGE AGE"), icon = icon("users"), color = "aqua")
  })
  
  ### -------------------- Average appearances -------------------------------------- #####
  Apps_avg <- reactive({
    mean(ft()$Apps)
  })
  
  ### -------------------- Average appearance value box ------------------------------ #####
  output$Apps_avg_box <- renderValueBox({
    valueBox(round(Apps_avg(), 2),  h2("AVERAGE APPS"), icon = icon("list-alt"), color = "red")
  })
  
  ## ----------------------- Average Caps ------------------------------------------ #####
  Caps_avg <- reactive({
    mean(ft()$Caps)
  })
  
  ## -------------------------- Average Caps value box --------------------------------- ####
  output$Caps_box <- renderValueBox({
    valueBox(round(Caps_avg(), 2), 
             h2("AVERAGE CAPS"), 
             icon = icon("trophy"), 
             color = "fuchsia"
            )
  })
  
  ### ---------------- PIE-CHART -------------------------------------------------- #####
  
  output$barchart <- renderPlotly({
    plot_ly(ft(), labels = ~get(input$factor), type = 'pie') %>%
      layout(title = paste("The pie chart below illustrates the distribution of players in the different\n", input$factor, "categories"))
  })
  
  
  ### -------------------- HISTOGRAM -------------------------------------------------- ####
  output$histogram <- renderPlotly(
    ggplotly(
      ggplot(ft(), aes_string(x = input$Var)) +
        geom_histogram(binwidth = input$bin, color = "white", fill = input$col) +
        labs(title = paste("The histogram below shows",input$Var,"distribution"),
             x = input$Var, y = "frequency") +
        theme(title = element_text(size = 9, color = "black", face = "bold"))
    )
  )
  
  ### --------------------- SCATTER PLOT -------------------------------------------- #####
  output$scatter <- renderPlotly(
    ggplotly(
      ggplot(ft(), aes_string(x = input$Var, y = ft()$Wage, col = input$factor)) +
        geom_point(size = 2.5) +
        geom_smooth(color = "black", method = "lm") +
        labs(title = paste("The scatter plot below shows the linear relationship between wage and",input$Var),
             x = input$Var, y = "Wage") +
        theme(title = element_text(size = 9, color = "black", face = "bold"))
    )
  )
  
  #### ------------------- BOX-PLOT --------------------------------------------------- ####
  output$boxplot <- renderPlotly(
    ggplotly(
      ggplot(ft(), aes_string(x = input$factor, y = input$Var, col = input$factor)) +
        geom_boxplot(varwidth = TRUE) +
        geom_jitter(width = 0.25, alpha = 0.3) +
        labs(title = paste("The box-plot below shows the variation in",input$Var,"in the different\n",input$factor,"categories"),
             x = input$factor, y = input$Var) +
        theme(legend.position = "none") +
        theme(title = element_text(size = 9, color = "black", face = "bold"))
    )
  )
  
  #### ------------------- FREQUENCY POLYGON --------------------------------------------- #####
  output$freq <- renderPlotly(
    ggplotly(
      ggplot(ft(), aes_string(x = input$Var, col = input$factor)) +
        geom_freqpoly(bin = 10) +
        labs(title = paste("The graph below shows the number of players in the different\n",input$Var,"segments"),
             x = input$Var, y = input$factor) +
        theme(title = element_text(size = 9, colour = "black", face = "bold"))
    )
  )
  
  #### ------------------- AVERAGE SEGMENTATION IN THE DIFFERENT CATEGORIES ----------------------------- #####
  #output$barchart2 <- renderPlotly({ 
   # req(input$factor, input$Var)
    #data_summary <- ft() %>% 
     # group_by_at(input$factor) %>% 
      #summarise(across(all_of(input$Var), mean, na.rm = TRUE))
    
  #  gg <- ggplot(data_summary, 
   #              aes_string(x = reorder({{input$factor}}, {{input$Var}}), 
    #                        y = {{input$Var}}, fill = {{input$factor}})) +
     # geom_bar(stat = "identity") +
      #labs(title = paste("The bar chart below shows the average",input$Var,"in the different",input$factor,"categories"),
       #    x = input$factor, y = input$Var)+
      #theme_dark() +
      #theme(title = element_text(size = 8, color = "black", face = "bold")) +
      #coord_flip()
    
    #ggplotly(gg)
    
    
    
    
  #})
  
  output$barchart2 <- renderPlotly({ 
    req(input$factor, input$Var)
    
    data_summary <- ft() %>% 
      group_by_at(input$factor) %>% 
      summarise(across(all_of(input$Var), mean, na.rm = TRUE))
    
    plot_ly(data_summary, 
            labels = ~get(input$factor), 
            values = ~get(input$Var), 
            type = 'pie', 
            hole = 0.6) %>%
      layout(title = paste("The donut chart below shows the average", input$Var, "in the different", input$factor, "categories"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
}
### --------------- end of the server logic function ---------------- #####

### -------------- Running the application --------------------------- ######
shinyApp(ui = ui, server = server)