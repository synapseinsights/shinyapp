

library(shiny)
library(recommenderlab)
library(shinythemes)
library(tools)

load("bin_ratings_clean.RData")
load("IBCF_model_clean.RData")
load("restaurant_names.RData")

Tuktuk<-restaurant_names[[1]]
Mollys<-restaurant_names[[2]]
Lous<-restaurant_names[[3]]
Murphys<-restaurant_names[[4]]
JewelOfIndia<-restaurant_names[[5]]
Orient<-restaurant_names[[6]]
SaltHill<-restaurant_names[[7]]
Sushiya<-restaurant_names[[8]]

ui <-fluidPage(theme = shinytheme("cosmo"),
  
  tags$head(tags$style(HTML("a {color: blue}"))),             
               
  headerPanel('TasteSpace Recommendation Engine'),
               
  sidebarPanel(
    
    h4('Below are 8 different restaurants from which you can "order".'),
    
    h4("Select a dish (or dishes) and find meals recommended for you at other restaurants!"),
    
    h5("Tuk Tuk Thai Cuisine", a("Tuktuk's menu", href="http://www.tuktukthaicuisine.com/index.php/tuk-tuk-menu")),
    
    h5("Mollys Restaurant and Bar", a("Mollys' menu", href="https://static1.squarespace.com/static/53346523e4b07ce0259fd9d9/t/5bbd0e5f0d9297a50e7b677c/1539116640312/Mollys-Menu-0918KTXT+White.pdf")),
                 
    h5("Lous Restaurant and Bakery", a("Lous' menu", href="https://www.lousrestaurant.com/pages/menu")),
    
    h5("Murphys Pub", a("Murphys' menu", href="http://www.murphysonthegreen.com/dinner-menu/")),
    
    h5("Jewel of India", a("Jewel's menu", href="http://www.jewelofindiahanover.com/menu.html")),
    
    h5("Orient Chinese and Japanese Restaurant", a("Orient's menu", href="http://www.hanoverorient.com/menu.html")),
    
    h5("Salt Hill Pub", a("Salt Hill's menu", href="https://www.salthillpub.com/locations/hanover/hanover-menu/")),
    
    h5("Sushiya Japanese and Korean Cuisine", a("Sushiya's menu", href="https://www.hanoversushiya.com/")),
    
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
      
      select_inds[i] <-grep(selections[i],colnames(bin_ratings_clean))
      
    }
    
    select_inds<-sort(select_inds)
    
    ones_in<-rep(1,len_selects)
    
    zero_init<-rep(0,ncol(bin_ratings_clean))
    
    zero_init[select_inds]<-ones_in
    
    newdat<-rbind(colnames(bin_ratings_clean),zero_init,c(1,rep(0,ncol(bin_ratings_clean) -1)))
    
    colnames(newdat)<-colnames(bin_ratings_clean)
    
    newdat<-as.data.frame(newdat)
    
    newdat<-newdat[-1,]
    
    newdat<-apply(newdat,2,function(x) as.numeric(as.character(x)))
    
    newdat<-as.matrix(newdat)
    
    newdat_ratings<-as(newdat,"binaryRatingMatrix")
    
    recommendations <- predict(model, newdat_ratings, n = 500)
    
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

#rsconnect::deployApp('C:/Users/dasil/Dropbox/tastespace/Tastespace', account = "tastespace")


