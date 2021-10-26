#install.packages("shiny")
library(shinyjs)
library(shiny)
labelMandatory <- function(label) {
  tagList(
    label,
    span("* (Required)", class = "mandatory_star")
  )
}


appCSS <- ".mandatory_star { color: red; }"

fieldsMandatory <- c("name", "qual", "lang")
#ValidationfieldsMandatory <- c("name", "qual", "lang")

fieldsAll <- c("name", "qual", "gender", "lang")
responsesDir <- file.path("C:\\Users\\BPO18\\responses")
epochTime <- function() {
  as.integer(Sys.time())
}

shinyApp(
  
  ui = fluidPage(
    
     shinyjs::useShinyjs(),
     shinyjs::inlineCSS(appCSS),
    
    titlePanel("Shiny form"),
    
    div(
      id = "form",
      
      conditionalPanel(
              condition = "input[name] != '   '  && input[qual] != '   ' && input[lang] != '  '" ),
      
      textInput("name", labelMandatory("Name"), ""),
      
      textInput("qual", labelMandatory("Qualifications"), ""),
      
      textInput("age", "Current age"),
      
      radioButtons("gender", "Gender",c("Male", "Female", "Other")),
      
      selectInput("lang", labelMandatory("Programming languages familiar with"),
                  c("",  "C", "C++", "Python", "JAVA", "R")),
       
      
      actionButton("submit", "Submit", class = "btn-primary")
    )
  ),

  server = function(input, output, session) {
   
   # a<-is.alpha(input) 
     observe({
    mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ' '  &&input[[x]] == "a"
               },
               logical(1))
      
      mandatoryFilled <- all(mandatoryFilled)
      
      formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = epochTime())
        data <- t(data)
        data
      })
    
      humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
      
      saveData <- function(data) {
        fileName <- sprintf("%s_%s.csv",
                            humanTime(),
                            digest::digest(data))
        
        write.csv(x = data, file = file.path(responsesDir, fileName),
                  row.names = FALSE, quote = TRUE)
      }
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
        saveData(formData())
      })
      
      
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
  }
)