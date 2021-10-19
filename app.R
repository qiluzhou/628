
library(shiny)
library(scales)
library(ggpubr)
library(shinyalert)
library(htmltools)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel(h1("Body Fat Calculator")),

    # Sidebar with a numeric input for personal information 
      sidebarPanel(
          
            h2("Personal Information"),
            
            h5(textOutput("note")),
            
            radioButtons("gender",
                         "gender",
                         choices = c("male","female")
            ),
            
            numericInput("weight",
                         h3("Weight"),
                         value = 60
                         ),
            
            radioButtons("unit.wt",
                         "Unit",
                         choices = c("kg","lb")
                         ),
            
 #           numericInput("gender",
 #                        h3("gender"),
 #                        value = 170
 #                        ),
            
 #           radioButtons("unit.ht",
 #                        "Unit",
 #                        choices = c("cm","inch")
 #                        ),

            
            numericInput("abdomen",
                         h3("Circumference of Abdomen"),
                         value = 70
                         ),
            
            radioButtons("unit.abd",
                         "Unit",
                         choices = c("cm","inch")
                         ),
            
            submitButton()
            
        ),

        # Show a plot of the body fat ratio!!!!!!!!!!!!
        mainPanel(
          
                  plotOutput("bodyfat.plot"),
                  htmlOutput("bodyfat.ui"),
                  textOutput("info"),
                  textOutput("contact1"),textOutput("contact2")
        )
    
)


# Define server 
server <- function(input, output) {

    output$bodyfat.plot = renderPlot({
      
      # Correct Unit
      
      if (input$unit.wt == "kg") weight = input$weight else weight = input$weight*0.45359237
      
  #    if (input$unit.ht == "inch") height = input$height else height = input$height*0.393701
      
      if (input$unit.abd == "cm") abdomen = input$abdomen else abdomen = input$abdomen*0.393701
      
      
      # Calculate Body Fat
      
      if (input$gender == "male"){
      f=-64.4-0.008*weight+1.14*abdomen-0.003*weight*abdomen
      }else {
        f=-9999
      }
      # Body Fat Level
      bodyfat.level = function(bodyfat){
      
      str1 <- "Your body fat level is 'Essential Fat', we advice you keep a balanced diet and gain more weight to maintain a healthy body build."
      str2 <- "Your body fat level is 'Athletes', go on and keep your figure!"
      str3 <- "Your body fat level is 'Fitness', go on and keep your figure!"
      str4 <- "Your body fat level is 'Acceptable', exersice more to keep fit!"
      str5 <- "Your body fat level is 'Obesity', we advice you to keep diet and keep regular exercise to stay in health!"

      if (bodyfat <= 2) return(NA) 
      if (2 < bodyfat & bodyfat <= 5) return(str1)
      if (5 < bodyfat & bodyfat <= 13) return(str2)
      if (13 < bodyfat & bodyfat <= 17) return(str3)
      if (17 < bodyfat & bodyfat <= 24) return(str4)
      if (bodyfat > 24 & bodyfat <= 50) return(str5)
      if (bodyfat > 50 & bodyfat <9999) return(NULL)
      if (bodyfat == 9999) return("Sorry, we only design this for male becuase we only have male data to train our model. Please use another body fat calculator")
      
      }
      
      # Plot
      #if(!is.null(bodyfat.level(f)) && !is.na(bodyfat.level(f))){
        data = data.frame(value0 = c(f,100-f),Tissue = c("Fat","Other Tissue"))
        ggdonutchart(data,x = "value0",
                     label = "Tissue",
                     color = "white",
                     fill = "Tissue",
                     pallette = c("#FC4E07","#00AFBB")
        )
      #}
    })
      
      output$bodyfat.ui = renderUI({
        
        # Correct Unit
        
        if (input$unit.wt == "kg") weight = input$weight else weight = input$weight*0.45359237
        
 #       if (input$unit.ht == "inch") height = input$height else height = input$height*0.393701
        
        if (input$unit.abd == "cm") abdomen = input$abdomen else abdomen = input$abdomen*0.393701
        
        
        # Calculate Body Fat
        
        if (input$gender == "male"){
        f=-64.4-0.008*weight+1.14*abdomen-0.003*weight*abdomen
        }else {
          f=9999
        }
        # Body Fat Level
        bodyfat.level = function(bodyfat){
          
          str1 <- "Your body fat level is 'Essential Fat', we advice you keep a balanced diet and gain more weight to maintain a healthy body build."
          str2 <- "Your body fat level is 'Athletes', go on and keep your figure!"
          str3 <- "Your body fat level is 'Fitness', go on and keep your figure!"
          str4 <- "Your body fat level is 'Acceptable', exersice more to keep fit!"
          str5 <- "Your body fat level is 'Obesity', we advice you to keep diet and keep regular exercise to stay in health!"
       
          if (bodyfat <= 2) return(NA) 
          if (2 < bodyfat & bodyfat <= 5) return(str1)
          if (5 < bodyfat & bodyfat <= 13) return(str2)
          if (13 < bodyfat & bodyfat <= 17) return(str3)
          if (17 < bodyfat & bodyfat <= 24) return(str4)
          if (bodyfat > 24 & bodyfat <= 50) return(str5)
          if (bodyfat > 50 & bodyfat <9999) return(NULL)
          if (bodyfat == 9999) return("Sorry, we only design this for male becuase we only have male data to train our model. Please use another body fat calculator")
        }
        
        # Output Text
        str0 <- paste0("Your percentage of body fat is: ",round(f,1),"%.")
        
        if(is.null(bodyfat.level(f))){
          HTML(paste(h4("Your percentage of body fat is"), h4(round(f,1)),h4("%"),h4("Oops!! Your body fat is abnormally high. Please check your input.")))
        } else{
          if(is.na(bodyfat.level(f))){
            HTML(paste(h4("Your percentage of body fat is"), h4(round(f,1)),h4("%"),h4("wOops!! Your body fat is extremely low. Please check your input.")))
          } else{
            HTML(paste(h4(str0),h4(bodyfat.level(f))) )
          }
        }
        
      }
      )
      
      # Other Information
      output$info = renderText({ "Contact us"})
      output$contact1 = renderText({ "E-mail: tjia22@wisc.edu"})
      output$contact2 = renderText({ "Tel: (+1)608-867-9053"})
      output$note = renderText({"Note: This body fat calculator is only effective for male since we conduct this study based on a dataset collected among male."})
  
}


# Run the application 
shinyApp(ui = ui, server = server)
