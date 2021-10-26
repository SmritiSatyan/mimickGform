#install.packages("plotly")
library(shiny)
library(gdata)
library(dplyr)
library(ggplot2)
library(plotly)

mydata1 = read.csv("C:\\Users\\BPO18\\Documents\\Book1.csv")

ui <- fluidPage(
  titlePanel("Hello User"),
  
   fluidRow(
    
    column(3, wellPanel(
      selectInput("input_type", "Input type",
                  c("Distributor_wise", "Outlet_type_wise", "Product name", 
                    "Product code wise", "Salesman",  "State")
      )
    )
 ),
    
    column(3, wellPanel(
      
      uiOutput("radiobuttonChoiceOutput"),
      plotOutput("distributorOutput"),
      plotOutput("outlettypeOutput"),
      plotOutput("productnameeOutput"),
      plotOutput("productCodeOutput"),
      plotOutput("salesmanOutput"),
      plotOutput("stateOutput")
    )),
    
    column(3,
           tags$p("Input type:"),
           verbatimTextOutput("input_type_text"),
    
           tags$p("Plot type:"),
           plotOutput("distributorOutput"),
           plotOutput("outlettypeOutput"),
           plotOutput("productnameeOutput"),
           plotOutput("productCodeOutput"),
           plotOutput("salesmanOutput"),
           plotOutput("stateOutput")
     
    )
  )
)

Server <- function(input, output) {
  
  output$radiobuttonChoiceOutput <- renderUI({
  
      if (is.null(input$input_type))
      return()
    
    switch(input$input_type,
           selectInput("salesmanInput", label="select the salesmen", 
                       choices=unique(mydata1$salesman), selected = mydata1$salesman[1] ),
           
           checkboxGroupInput("typeInput", 
                              h3("Product type"), 
                              choices = unique(mydata1$prod_name), selected = mydata1$prod_name[1]),
           
           checkboxGroupInput("prodcodeInput", 
                              h3("Product code wise"), 
                              choices = unique(mydata1$prod_code),
                              selected = mydata1$prod_code[1]),
           
           checkboxGroupInput("stateInput", 
                              h3("States"), 
                              choices = unique(mydata1$state),
                              selected = mydata1$state[1]),
           
           Distributor_wise = checkboxGroupInput("distributorInput", 
                                                 h3("Distributor-wise"), 
                                                 choices = unique(mydata1$distributor_name),
                                                 selected = mydata1$distributor_name[1]),
           
           Outlet_type_wise =  checkboxGroupInput("outlettypeInput", 
                                                  h3("Outlet type wise"), 
                                                  choices = unique(mydata1$outlet_type),
                                                  selected = mydata1$outlet_type[1])
           
           
    
    )
  })
 output$distributorOutput <-renderPlot({
   if(input$input_type == 'distributorInput')
   {
    return()
       filtered <-
         mydata1 %>%
         dplyr :: filter(distributor_name == input$distributorInput)
       
       ggplot(filtered, aes(total_sales)) +
         geom_histogram(fill=I("red"),
                        col=I("green"))
     }
   })
 output$results1 <- renderPlot({
   filtered <-
     mydata1 %>%
     filter(prod_code == input$prodcodeInput
     )
   ggplot(filtered, aes(total_sales)) +
     geom_histogram(fill=I("gold"),
                    col=I("violet") )
 })
 output$salesplot <- renderPlot({
   
   filtered <-
     mydata1 %>%
     
     filter(salesman == input$salesmanInput
     )
   
   ggplot(filtered, aes(total_sales)) +
     geom_histogram(fill=I("orange"), 
                    col=I("blue") )
 })
 
 output$salesplot1 <- renderPlot({
   
   filtered <- 
     mydata1 %>%
     
     filter(
       prod_name == input$typeInput
     )
   
   ggplot(filtered, aes(total_sales)) +
     geom_histogram(fill=I("blue"),
                    col=I("yellow") )
 })
 
 output$salesplot2 <- renderPlot({
   
   filtered <-
     mydata1 %>%
     
     filter( 
       state == input$stateInput
     )
   
   ggplot(filtered, aes(total_sales)) +
     geom_histogram(fill=I("red"),
                    col=I("green") )
 })
 
 output$outlettypeOutput <- renderPlot({
   if(input$rb == "outlettypeInput"){
     return()
   
   filtered <-
     mydata1 %>%
     dplyr :: filter(outlet_type == input$outlettypeInput
     )
   ggplot(filtered, aes(total_sales)) +
     geom_histogram(fill=I("blue"),
                    col=I("yellow"))}
 }, height = 400, width = 800)

 
  output$input_type_text <- renderText({
    input$input_type
  })
}
shinyApp(ui = ui, server = Server)
