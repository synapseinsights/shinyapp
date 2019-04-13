
#
library(shiny)
library(recommenderlab)
library(shinythemes)
library(tools)

#C:/Users/dasil/Dropbox/tastespace/
load("bin_ratings.RData")
load("IBCF_model.RData")
load("restaurant_names.RData")

Tuktuk<-restaurant_names[[1]]
Mollys<-restaurant_names[[2]]
Lous<-restaurant_names[[3]]
Murphys<-restaurant_names[[4]]
JewelOfIndia<-restaurant_names[[5]]
Orient<-restaurant_names[[6]]
SaltHill<-restaurant_names[[7]]
Sushiya<-restaurant_names[[8]]

ui <-fluidPage(theme = shinytheme("sandstone"),
               
               
               headerPanel('TasteSpace Recommendation Engine'),
               
               sidebarPanel(
                 
                 selectInput('dataset', 'Choose Restaurant', c('SaltHill', 'Murphys','Tuktuk','Lous','JewelOfIndia','Orient','Sushiya','Mollys')),
                 
                 uiOutput("items_out")
                 
               ),
               
               
               mainPanel(
                 
                 tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
                 
                 fluidRow(
                   
                   column(4,tableOutput("selected_app"),
                          
                          tableOutput("selected_app2"),
                          
                          tableOutput("selected_app3")),
                   
                   column(4,tableOutput("selected_app4"),
                          
                          tableOutput("selected_app5"),
                          
                          tableOutput("selected_app6")),
                   
                   tableOutput("selected_app7"),
                   
                   tableOutput("selected_app8")
                   
                 )
                 
               )
               
)  

server <- function(input,output) {
  
  get_recs<-function(input_in){
    
    selections<-input_in
    
    len_selects<-length(selections)
    
    select_inds<-numeric()
    
    for (i in 1:len_selects){
      
      select_inds[i] <-grep(selections[i],colnames(bin_ratings))
      
    }
    
    ones_in<-rep(1,len_selects)
    
    zero_init<-rep(0,ncol(bin_ratings))
    
    zero_init[select_inds]<-ones_in
    
    jin<-rbind(colnames(bin_ratings),zero_init,c(1,rep(0,706)))
    
    colnames(jin)<-colnames(bin_ratings)
    
    jin<-as.data.frame(jin)
    
    jin<-jin[-1,]
    
    jin<-apply(jin,2,function(x) as.numeric(as.character(x)))
    
    jin<-as.matrix(jin)
    
    jin_ratings<-as(jin,"binaryRatingMatrix")
    
    recommendations <- predict(model, jin_ratings, n = 500)
    
    to_tab<-as(recommendations, "list")[[1]]
    
    splt<-strsplit(to_tab, "_")
    
    outdat<-do.call("rbind", splt)
    
    colnames(outdat)<-c("Item", "Restaurant")
    
    outdat<-as.data.frame(outdat)
    
    outdat$Restaurant<-tools::toTitleCase(as.character(outdat$Restaurant))
    
    outdat$Item<-tools::toTitleCase(as.character(outdat$Item))
    
    res_split<-split.data.frame(outdat,outdat$Restaurant)
    
    return(res_split) 
    
  }
  
  output$items_out = renderUI({
    
    mydata <- get(input$dataset)
    
    selectInput('dish', 'Menu Items', as.character(mydata[,1]), multiple = TRUE)
    
  })
  
  
  output$selected_app <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[1]],5)
    
  })
  
  output$selected_app2 <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[2]],5)
    
  })
  
  output$selected_app3 <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[3]],5)
    
  })
  
  output$selected_app4 <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[4]],5)
    
  }) 
  
  output$selected_app5 <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[5]],5)
    
  }) 
  
  output$selected_app6 <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[6]],5)
    
  }) 
  
  output$selected_app7 <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[7]],5)
    
  }) 
  
  output$selected_app8 <- renderTable({
    
    recout<-get_recs(input$dish)
    
    head(recout[[8]],5)
    
  }) 
  
  
}

shinyApp(ui = ui, server = server)



