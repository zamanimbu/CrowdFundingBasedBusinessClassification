library(shiny)
library(shinythemes)
shinyUI(navbarPage( title = "Crowd funding Based Business Problem Solution",
                    theme = shinytheme("sandstone"),
  tabPanel("About"),
  tabPanel("Data Analysis",
        sidebarLayout(
        sidebarPanel(
          sliderInput("split","Test Data (%)",min=70,max=90,value=80),
          selectInput("algo","Select Algorithm",c("Decision Tree","ANN","Naive Bayes"),"Decision Tree")
        ),
        mainPanel(
          verbatimTextOutput("tableForDC")
        )
      )),
  tabPanel("Predict Business Status", 
           sidebarLayout(
             sidebarPanel(
               uiOutput("inputUi")
             ),
             mainPanel(
               verbatimTextOutput("result")
             )
           )),
  tabPanel("Statistical Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("stat","Select Option",choices = c("Projects by Category","Projects by Subcategory",
                                                                           "Amount Pledged by Category","Failue Vs Success Ratio"))
                          ),
                          mainPanel(
                            plotOutput("stat")
                          )
                        ))
          
))
        


# 
# 
# shinyUI(fluidPage(
#   titlePanel("Data set description"),
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("split","Test Data (%)",min=70,max=90,value=80)
#       
#     ),
#     mainPanel(
#       verbatimTextOutput("table")
#     )
#   )
# ))