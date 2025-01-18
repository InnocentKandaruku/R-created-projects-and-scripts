####### -------------- creating a brand new ICU analysis application ----------------- #########

### ------ incase the libraries are not installed ----- ####
#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("DT")
#install.packages("plotly")
#install.packages("reshape2")

### ...... loading the required libraries for the data set ........ ######
library("tidyverse")
library("shiny")
library("shinythemes")
library("DT")
library("plotly")
library("reshape2")
## ----------------------------------------------------------------- #####

### ....... loading the ICU data set from the storage location ......... #####
DATA <- read.csv("F:/Rpractice/DATA SETS/ICUData.csv")

### ............... converting the characters to factors ............. ####
DATA$sex = as.factor(DATA$sex)
DATA$surgery = as.factor(DATA$surgery)
DATA$outcome = as.factor(DATA$outcome)
DATA$age = as.numeric(DATA$age)
DATA$SAPS.II = as.numeric(DATA$SAPS.II)
DATA$liver.failure = as.numeric(DATA$liver.failure)
DATA$LOS = as.numeric(DATA$LOS)

######### ---------------------- creating the shinny application for the new ICU data set -------------------- #####
###### =============== creating the user interface of the application ==================== ########
ui <- fluidPage(
  theme = 
    shinytheme("cyborg"),
  titlePanel(title = "ICU RESEARCH PROJECT"),
  tags$style(HTML( 
             "body
             {
             background-colour : black;
             colour : white;
             }")),
  sidebarLayout(position = "right",
                
#### ============================= SIDE BAR PANEL AREA ======================================================== ######
    sidebarPanel(
### ---------- generating the first select input for the factor variable ------------ ####
      selectInput("factor",
                  "Choose the factor variable",
                  choices = c("sex","surgery","outcome"),
                  selected = "sex"),
### ----------------------- end of the select input for the factor variable -------------------- ###
      
### ------------ generating a slider input for the histogram ---------------------------- ####
      sliderInput("bin",
                  "Scroll for the best binwidth",
                  min = 0, max = 200, value = 10, step = 0.5 ),
#### --------------- slider above helps adjust the bin width meant for the histogram -------------- #####
      
#### ------------- generating the inputs meant to feed the histogram to be created -------------------- #######
      selectInput("var",
                  "Select the numeric variable",
                  choices = c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure"),
                  selected = "age"),
###### ---------------------- end of the input for the histogram to be created -------------------------- #######

### ---------------- generating radio buttons for the color inputs in the histogram -------------------- ######
radioButtons("colour",
             "Change histogram colour",
             choices = c("purple","brown","darkgrey"),
             selected = "purple"),
### -------------------------------------- end of the color selection panel ------------------------------ #####

### --------------------------- creating the inputs for the scatter plot ---------------------------------- #####
splitLayout("Scatter_plot",  ### ----scatter plot split layout ----- ###
            selectInput("variablex",
                        "X",
                        choices = c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure"),
                        selected = "age"),
            selectInput("variabley",
                        "Y",
                        choices = c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure"),
                        selected = "heart.rate"),
            selectInput("variablez",
                        "Variation in",
                        choices = c("sex","surgery","outcome"),
                        selected = "sex")
            ), ### ---- end of the scatter plot split layout ---- ###
## ------------ box plots split layout --------------------------------------------------------- #####
splitLayout("Box_plots",
            selectInput("variablek",
                        "Variable(Y)",
                        choices = c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure"),
                        selected = "age"),
            selectInput("factorx",
                        "Variable(X)",
                        choices = c("sex","surgery","outcome"),
                        selected = "sex")), ## ----- end of the box plot split Layout ----- #####

### ------------------- creating a splitLayout for the statistical tests ------------------------------ ######
splitLayout("Hypothesis",

#### ------------------ creating a selectInput for the for the testTypes ------------------------------ #########            
            selectInput("testType",
                        "Select testType",
                        choices = c("Chi-Square test","One sample t-Test","Correlation test"),
                        selected = "Chi-Square test"), #### ---- end of the testType selectInput ----- ####

### --------- creating a conditional panel for each specific selected test type --------- #####

#### ---------------- chi-Square test -------------------------------------------- #####
            conditionalPanel(
              condition = "input.testType == 'Chi-Square test'",
              selectInput("variable1_chi",
                          "Var_1",
                          choices = c("sex","surgery","outcome"),
                          selected = "sex"),
              selectInput("variable2_chi",
                          "Var_2",
                          choices = c("sex","surgery","outcome"),
                          selected = "surgery")
            ), ## ---end of the chi-square conditional panel--- ####

#### ---------------- conditional panel for the one-sample t-test ---------------------------------------- ######
          conditionalPanel(
            condition = "input.testType == 'One sample t-Test'",
            selectInput("variable_t",
                        "var_Y",
                        choices = c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure"),
                        selected = "age"),
            numericInput("mu_t","mu_t",value = 0)
          ), ### ------- end of the one-sample t-test conditional panel ----- ####

##### ------------------------- correlation test conditional panel ----------------------------------------- #######      
          conditionalPanel(
            condition = "input.testType == 'Correlation test'",
            selectInput("variable1_corr",
                        "var_A",
                        choices = c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure"),
                        selected = "age"),
            selectInput("variable2_corr",
                        "var_B",
                        choices = c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure"),
                        selected = "heart.rate")
          )  ### ------------ end of the correlation test conditional panel --------------------- #####

), ### ------ end of the hypothesis splitLayout ------ ####

#### ------------------------------ generating the split model for the statistical model -------------------------- ####
splitLayout("Model",

### --------- generating the input for age ----------------- ###
            numericInput("age",
                         "age:",
                         value = mean(DATA$age)),

### ---------------- generating the input for sex -------------- ####
            selectInput("sex",
                        "sex:",
                        choices = unique(DATA$sex),
                        selected = unique(DATA$sex)[1]),

### --------------- generating the input for temperature ---------------- ####
            numericInput("temperature",
                         "temperature:",
                         value = mean(DATA$temperature)),

### ------------------ generating the input for surgery ---------------- ####
            selectInput("surgery",
                        "surgery:",
                        choices = unique(DATA$surgery),
                        selected = unique(DATA$surgery)[1]),

### ----------------------- generating the input for bilirubin ------------------ ####
            numericInput("bilirubin",
                         "bilirubin:",
                         value = mean(DATA$bilirubin))
) ### -------------- end of the model split layout ----------------- #####

    ), ####======= closes the bracket opened by the sidebar Panel =============== ####



###### ================================ MAIN PANEL AREA ======================================================= #####
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("ICU DATA", ## ---- ICU data tab created to enable the data get displayed ---- ###
                           DTOutput("mytable")),
                  
                  tabPanel("SUMMARY STATISTICS", ## ------ summary statistics tab is generated ------- ####
                           plotlyOutput("Bar_chart"),
                           tableOutput("factorTable")),
                  
                  tabPanel("DESCRIPTIVES", ## ------ descriptive statistics tab is generated -------- #####
                           tableOutput("continous_table"),
                           plotlyOutput("my_histogram"),
                           plotlyOutput("my_densityplot"),
                           plotlyOutput("poly")),
                  
                  tabPanel("ASSOCIATIONS", ### ---------- association statistics tab generated ----------- ####
                           plotlyOutput("my_scatterplot"),
                           actionButton("showCorrelogramBtn",
                                        "SHOW CORRELOGRAM"),
                           plotlyOutput("correlogramPlot"),
                           plotlyOutput("my_boxplot")),
                  
                  tabPanel("TESTS", #### -------- tab generated for statistical tests --------- #####
                           textOutput("result"),
                           actionButton("runTest",
                                        "RUN_TEST")),
                  tabPanel("STATISTICAL_MODEL",
                           actionButton("fitModelButton",
                                        "Fit Model"),
                           
                           h4("Model Summary"),
                           verbatimTextOutput("modelSummary"),
                           actionButton("predictButton",
                                        "Predict Heart rate"),
                           h4("Predicted Heart rate: "),
                           textOutput("prediction"))
      )
  )
  )
) ### ==== closes the fluid page of to the user interface ==== ####

### ============= creating the server function that enables the functionality of the application =========== #####

#### --------------------------------- Displaying the data table in the application ---------------------------------------- #######
server <- function(input, output){
  output$mytable <- renderDT({
    datatable(DATA,
              options = list(
                scrollX = TRUE,
                scrollY = "400px",
                paging = FALSE,
                stripeClass = c("even","odd"),
                ordering = FALSE
              ), rownames = FALSE)
  })
#### ---------------------------------------------- end of the table function --------------------------------------------- #######  
  
  
##### -------------------------------- generating summary statistics for the categorical variables ------------------------------- ######
  factorTable <- reactive({
    DATA %>% 
      group_by_at(vars(input$factor)) %>% 
      summarise(
        frquency = n(),
        proportion = n() / nrow(DATA)
      )
  })
######## ------------------------------------ end of the function that generates the summary statistics of the table --------------------------- #####
  
###### ---------------- displaying the table of each factor variable that has been selected ----------------------- ########
  output$factorTable = renderTable({
    factorTable()
  })
#### ------------------------------------------------ end of the summary statistics table displayed ----------------- ######
  
##### ========================= generating graphical plots for the graphs ========================================= ######
  output$Bar_chart <- renderPlotly(
    ggplotly(
      ggplot(DATA, aes_string(x = input$factor, fill = input$factor)) +
        geom_bar() +
        labs(title = paste("The bar chart below shows the number of patients in the different",input$factor,"categories."),
             x = input$factor, y = "Frequency") +
        theme(title = element_text(size = 9, colour = "purple", face = "bold"))
    )
  )
#### ================================ end of the bar chart that has been created above ============================ ######
  
##### ============================= generating the descriptive statistics for the continous variables ======================== #####
  continous_table <- reactive({
    DATA %>% 
      summarise(
        maximum = max(!!sym(input$var)),
        mean = mean(!!sym(input$var)),
        median = median(!!sym(input$var)),
        StdDev = sd(!!sym(input$var)),
        minimum = min(!!sym(input$var))
      )
  })
##### ============================ end of the function that generates the descriptive statistics =============================== ######

#### ------------------------------ displaying the descriptive statistics table ----------------------------------------- #####
  output$continous_table <- renderTable({
    continous_table()
  })
#### -------------------------------------- end of the function that displays the descriptive statistics table --------------------- #####

##### =================== Additionally generating the histograms for the continuous variables =========================== ######
  output$my_histogram <- renderPlotly(
    ggplotly(
      ggplot(DATA, aes_string(x = input$var)) +
        geom_histogram(binwidth = input$bin, colour = "black", fill = input$colour) +
        labs(title = paste("The histogram below illustrates the distribution in",input$var,"of ICU patients"),
             x = input$var, y = "frequency") +
        theme(title = element_text(size = 9, colour = "brown", face = "bold"))
    )
  )
#### ============================= end of the histogram that has been created ================================================ ####
  
### ================================= creating a histogram that is meant to indicate the skewness of the data ================================ ####
  output$my_densityplot <- renderPlotly(
    ggplotly(
      ggplot(DATA, aes_string(x = input$var)) +
        geom_density(alpha = 0.95, fill = input$colour) +
        labs(title = paste("The density plot below illustrates the distribution of patient's",input$var,"in the ICU")) +
        theme(title = element_text(size = 9, colour = "black",face = "bold"))
      
    )
  )
### =========================================== end of the density plot that has been created ================================================ #####
  
#### ================================================ creating relationships between the variables ============================================= ####
  ## ----- between the numeric variables, a correlogram and scatter plots ------- ##
  ## ----- between the numeric and factor variables a box plot is generated ----- ##
  
###### ============================= generating a scatter plot to illustrate between the numeric variables ============================ ######
  
##### --------------------------- scatter plot for the relationship between the variables ------------------------------------ #####
  output$my_scatterplot <- renderPlotly(
    ggplotly(
      ggplot(DATA, aes_string(x = input$variablex, y = input$variabley, col = input$variablez)) +
        geom_point(size = 2) + 
        geom_smooth(method = "lm", colour = "brown", se = FALSE) +
        theme(title = element_text(size = 9, colour = "blue", face = "bold")) +
        labs(title = paste("The scatter plot below illustrates the relationship between the",input$variablex,"and",input$variabley,"based on",input$variablez),
             x = input$variablex, y = input$variabley)
        
    )
  )
#### ========================================= end of the scatter plot ======================================================================== #####
  
##### ======================================== designing a correlogram in order to illustrate correlation ============================================ ######
showCorrelogram <- reactiveVal(FALSE)

## ---- this section creates a reactive link between the event and correlogram button ---- ###
observeEvent(input$showCorrelogramBtn, {
  showCorrelogram(TRUE)
})
  
  output$correlogramPlot <- renderPlotly({
    if(showCorrelogram()){
      corr_data <- cor(DATA[c("age","heart.rate","temperature","bilirubin","SAPS.II","liver.failure","LOS")])
      
      gg_correlogram <- ggplot(melt(corr_data), aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
        theme_minimal() +
        labs("CORRELOGRAM")
      
      ggplotly(gg_correlogram)
    }
  })
#### ========================================== END OF THE CORRELOGRAM SECTION =============================================================== ######
  
#### ============================== Box plots for the numeric and factor variable ========================================================== ####
  output$my_boxplot <- renderPlotly(
    ggplotly(
      ggplot(DATA, aes_string(x = input$factorx, y = input$variablek, fill = input$factorx)) +
        geom_boxplot(varwidth = TRUE) +
        geom_jitter(alpha = 0.25, width = 0.2) +
        theme(title = element_text(size = 9, colour = "darkgreen", face = "bold")) +
        labs(title = paste("The box plots below illustrate the variation",input$variablek,"in the different",input$factorx,"categories"),
             x = input$factorx, y = input$variablek)
    )
  )
#### ==================================== end of the box plot function ===================================================================== ####

##### ================================ creating a frequency polygon =================================== ####
  output$poly <- renderPlotly(
    ggplotly(
      ggplot(DATA, aes_string(x = input$var, col = input$factor)) +
        geom_freqpoly(bins = input$bin) + 
        labs(title = paste("The distribution of",input$var,"in the respective",input$factor,"categories"),
             x = input$var, y = "frequency") +
        theme(title = element_text(size = 9, colour = "darkred", face = "bold"))
    )
  )
##### ================================= creating the server logic for the statistical tests ======================================== ########
  observeEvent(input$runTest, {
    testType <- input$testType
    
    ### ----- chi-square test ------- ####
    if(testType == "Chi-Square test"){
     variable1 <- input$variable1_chi
     variable2 <- input$variable2_chi
     result <- chisq.test(DATA[[variable1]], DATA[[variable2]])
    } else if(testType == "One sample t-Test"){ ### ---- one sample t-test ---- ####
      variable <- input$variable_t
      mu = input$mu_t
      result <- t.test(DATA[[variable]], mu = mu)
      
    } else if(testType == "Correlation test"){ ## ------ correlation test ------ #####
      variable1 <- input$variable1_corr
      variable2 <- input$variable2_corr
      result <- cor.test(DATA[[variable1]], DATA[[variable2]])
    }
    
    ### ------ displaying the output of the testTypes ------ ####
    output$result <- renderText({
      paste(testType,"\n",
            if(testType == "Correlation test"){
              paste("Correlation Coeffecient:",
                    round(result$estimate, 2), "\nP-value:",round(result$p.value, 4))
            }else{
              paste("Test statistic:",
                    round(result$statistic, 2),"\nP-value:",round(result$p.value, 4))
            })
    })
  })
  
  ###### ================================= generating the regression model for the server function ================ #####
  model <- reactiveVal(NULL)
  
  ### ----- acquiring the user input ------ ####
  observeEvent(input$fitModelButton,{
    ##--- collect the user input ---###
    new_data <- data.frame(
      age = DATA$age,
      sex = as.factor(DATA$sex),
      temperature = DATA$temperature,
      surgery = as.factor(DATA$surgery),
      bilirubin = DATA$bilirubin,
      heart.rate = DATA$heart.rate
    )
    
    ### ---- fitting the linear regression model ---- ######
    model_val <- lm(heart.rate ~ age + sex + temperature + surgery + bilirubin, data = new_data)
    model(model_val)
    
    ## ---- displaying the model summary ------- ####
    output$modelSummary <- renderPrint({
      summary(model_val)
    })
  })
#### -------------------------- end of the function that fits the model summary ---------------- ####
  
### ------------------------------- prediction function of the model ----------------------------- ######
  observeEvent(input$predictButton, {
    if(!is.null(model())) {
      
      ### --- collecting the user input ---- ####
      new_data <- data.frame(
        age = input$age,
        sex = input$sex,
        temperature = input$temperature,
        surgery = input$surgery,
        bilirubin = input$bilirubin
        
      )
      
      ## --- making the prediction ---- #####
      prediction <- predict(model(), newdata = new_data)
      
      ## ---- displaying the predicted heart rate ------ ####
      output$prediction <- renderText({
        paste("Predicted Heart Rate: ", round(prediction, 2))
      })
    }
  })
###### ----------------------------------- end of the model's prediction function ------------------------------- #####
  
  
}  #### ===== closes the created server function ======= #####

#### =========== then creating the runner of the application ============== ####
shinyApp(ui = ui, server = server)
## ============================== end of the ICU application ======================================== ######