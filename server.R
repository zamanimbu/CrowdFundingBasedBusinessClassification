library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(gplots)
library(rworldmap)
library(knitr)
rawdata <- read.csv('startup.csv')
dataset <- rawdata
ksdata <- rawdata
cat <- as.character(dataset$main_category)
scat <- as.character(dataset$category)
cntry <- as.character(dataset$country)
dataset <- rawdata
smp_size <- floor(.80 * nrow(dataset))
set.seed(123)
training_ind_DC <-sample(seq_len(nrow(dataset)),size = smp_size)
training_set_DC <- dataset[training_ind_DC,]
test_set_DC <- dataset[-training_ind_DC,]

library(C50)
classifierDC = C5.0(formula = state ~ .,
                    data = training_set_DC)
shinyServer(function(input,output){
  
  output$inputUi <- renderUI({
    tagList(
    selectInput("Category","Select Category",choices = c("",cat),selected = NULL),
    selectInput("SubCategory","Select Sub Category",choices = c("",scat),selected = NULL),
    selectInput("Country","Select Country",choices = c("",cntry),selected = NULL),
    textInput("goal","Goal Amount for project"),
    textInput("pledge","Pledged amount for project"),
    textInput("backers","Number of Backer of your project"),
    submitButton("Apply")
    )
  })
  

  
  output$result <- renderPrint({
    
   
      testcase <-data.frame(category=input$SubCategory,main_category= input$Category,
                            backers=as.numeric(input$backers),country=input$Country,
                            pledged=as.numeric(input$pledge),goal=as.numeric(input$goal))
    
    if(is.null(input$SubCategory)  && is.null(input$Category) && is.null(input$Country)){
      return(NULL)
    }
    
    else{
      y_pred_DC = predict(classifierDC, newdata = testcase, type = "class")
      
      return(paste("Business status is", y_pred_DC))
    }
    
   
  })

  
  output$stat <- renderPlot({
    if(input$stat == 'Projects by Category'){
      cat.freq <- ksdata %>%
        group_by(main_category) %>%
        summarize(count=n()) %>%
        arrange(desc(count))
      
      cat.freq$main_category <- factor(cat.freq$main_category, levels=cat.freq$main_category)
      
        pCat <- ggplot(cat.freq, aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
        ggtitle("Projects by Category") + xlab("Project Category") + ylab("Frequency") + 
        geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
        theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
              axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
        scale_fill_gradient(low="skyblue1", high="royalblue4")
        
       return(pCat)
    }
    
    if(input$stat == 'Projects by Subcategory')
    {
      subcat.freq <- ksdata %>%
        group_by(category) %>%
        summarize(count=n()) %>%
        arrange(desc(count))
      
      subcat.freq$category <- factor(subcat.freq$category, levels=subcat.freq$category)
      
      pScat <- ggplot(head(subcat.freq, 10), aes(category, count, fill=count)) + geom_bar(stat="identity") + 
        ggtitle("Projects by Subcategory") + xlab("Project Subcategory") + ylab("Frequency") + 
        geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
        theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
              axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
        scale_fill_gradient(low="skyblue1", high="royalblue4")
      
      return(pScat)
    }
    
    if(input$stat=='Failue Vs Success Ratio'){
      state.pct <- ksdata %>%
        filter(state %in% c("successful", "failed")) %>%
        group_by(main_category, state) %>%
        summarize(count=n()) %>%
        mutate(pct=count/sum(count)) %>%
        arrange(desc(state), pct)
      
      state.pct$main_category <- factor(state.pct$main_category, 
                                        levels=state.pct$main_category[1:(nrow(state.pct)/2)])
      
      pSFR <- ggplot(state.pct, aes(main_category, pct, fill=state)) + geom_bar(stat="identity") + 
        ggtitle("Success vs. Failure Rate by Project Category") + 
        xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
        scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                            labels=c("Success", "Failure")) + 
        geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
                  colour="white", size=5) + theme_economist() + 
        theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
              axis.text.x=element_text(size=12), legend.position="bottom", 
              legend.title=element_text(size=12, face="bold")) + coord_flip()
      
      return(pSFR)
    }
    
    if(input$stat == 'Amount Pledged by Category'){
      pledged.tot <- ksdata %>%
        group_by(main_category) %>%
        summarize(total=sum(pledged)) %>%
        arrange(desc(total))
      
      pledged.tot$main_category <- factor(pledged.tot$main_category, levels=pledged.tot$main_category)
      
     pAPBC <- ggplot(pledged.tot, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
        ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
        ylab("Amount Pledged (USD millions)") + 
        geom_text(aes(label=paste0("$", round(total/1000000,1))), vjust=-0.5) + theme_economist() + 
        theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
              axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
        scale_fill_gradient(low="skyblue1", high="royalblue4")
      
      return(pAPBC)
    }
  })
  
  
  
  
  
})