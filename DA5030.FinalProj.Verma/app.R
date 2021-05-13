#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(C50)



#Importing training data
train.shiny <- read.csv("finaldata.csv", stringsAsFactors = T)

#Removing unnessary columns
train.shiny <- train.shiny[,-1]

#Log transforming right skewed data
for (i in c("Arrival_Time", "Creation_Time"))
{
    train.shiny[,i] <- log(train.shiny[, i] + 1)
}

#Creating a Decision Tree model using C5.0
c50.shiny <- C5.0(x = train.shiny[,-9], y = train.shiny$gt)



ui <- pageWithSidebar(
    
    # Page header
    headerPanel('Activity Detection'),
    
    # Input values
    sidebarPanel(
        #HTML("<h3>Input parameters</h3>"),
        tags$label(h3('Input parameters')),
        numericInput("Arrival_Time", 
                     label = "Arrival Time", 
                     value = 5.1),
        numericInput("Creation_Time", 
                     label = "Creation Time", 
                     value = 3.6),
        numericInput("x", 
                     label = "x", 
                     value = 1.4),
        numericInput("y", 
                     label = "y", 
                     value = 0.2),
        numericInput("z", 
                     label = "z", 
                     value = 0.2),
        radioButtons("User", "User", c("a", "b", "c","e", "f", "g", "h", "i"), selected = character(0)),
        
        radioButtons("Model", "Model", c("gear", "lgwatch", "nexus4", "s3", "s3mini", "Samsung_galaxy", "samsungold"), selected = character(0)),
        radioButtons("Device", "Device", c("gear_1", "gear_2", "lgwatch_1", "lgwatch_2", "nexus4_1", "nexus4_2", "s3_1", "s3_2", "s3mini_1", "s3mini_2", "samsungold_1", "samsungold_2"), selected = character(0)),
        radioButtons("sensor", "sensor", c("Gear_sensor", "Phones_accelerometer", "Phones_gyroscope", "Watch_accelerometer", "Watch_gyroscope"), selected = character(0)),
        
        actionButton("submitbutton", "Submit", 
                     class = "btn btn-primary")
    ),
    
    mainPanel(
        tags$label(h3('Status/Output')), # Status/Output Text Box
        verbatimTextOutput('contents'),
        tableOutput('tabledata') # Prediction results table
        
    )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
    
    # Input Data
    datasetInput <- reactive({	
        
        df <- data.frame(
            Name = c("Arrival_Time", "Creation_time", "x", "y", "z", "User", "Model",
                     "Device", "gt", "sensor"),
            Value = as.character(c(input$Arrival_Time,
                                   input$Creation_time,
                                   input$x,
                                   input$y,
                                   input$z,
                                   input$User,
                                   input$Model,
                                   input$Device,
                                   "walk",
                                   input$sensor,
                                   
            )),
            stringsAsFactors = FALSE)
        
        # Taking transpose of the input data and adding column names to it
        input <- as.data.frame(t(df[,2]))
        names(input) <- df[,1]
        
        # Creating a cpot of input data for testing
        test <- input
        
        # Setting character columns to factor
        test$User <- factor(test$User, levels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"))
        test$Model <- factor(test$Model, levels = c("gear", "lgwatch", "nexus4", "s3", "s3mini", "Samsung_galaxy", "samsungold"))
        test$Device <- factor(test$Device, levels = c("gear_1", "gear_2", "lgwatch_1", "lgwatch_2", "nexus4_1", "nexus4_2", "s3_1", "s3_2", "s3mini_1", "s3mini_2", "samsungold_1", "samsungold_2"))
        test$gt <- factor(test$gt, levels = c("bike", "sit", "stairsup", "stairsdown", "stand", "still", "walk"))
        test$sensor <- factor(test$sensor, levels = c("Gear_sensor", "Phones_accelerometer", "Phones_gyroscope", "Watch_accelerometer", "Watch_gyroscope"))
        
        # Setting integer value for numeric columns
        for (i in c(1:5))
        {
            test[,i] <- (test[,i])
        }
        
        # Taking log transform for continious variable
        #for (i in c("Arrival_Time", "Creation_time"))
        #{
          #  test[,i] <- log(test[,i] + 1)
        #}
        
        # Predicting the value for the test data
        output <- predict(c50.shiny, test)
        
        
        output
})
    
    # Status/Output Text Box
    output$contents <- renderPrint({
        if (input$submitbutton>0) { 
            isolate("Calculation complete.") 
        } else {
            return("Server is ready for calculation.")
        }
    })
    
    # Prediction results table
    output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
            isolate(datasetInput()) 
        } 
    })
    
    }

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
