library(shiny)
load("NBA.Rdata")

#Shiny App

shinyApp(ui =fluidPage(
  titlePanel("Predicting Expected Wins for NBA Teams"),
    sidebarPanel(
      h4("Predictions"),
    
      #Create select input for all of the seasons
      selectInput("season", label = h3("NBA Season"),
                  choices = sort(NBA.df$Season),
                  selected = "2003"),
    
      selectInput("method", label = h3("Expectation Methods"),
                  choices = list("Morey's Method","Hollinger's Method")),
    
      #Set up the cities and other weather variables as output, in order to use renderUI in the server
      uiOutput("teams")
    ),
  
    #Create main panel for eventual plots
    mainPanel(
      h4("Exp  vs Obs:"),
      plotOutput("comp"),
      textOutput("messages"),
      br(),
    
      h4("Error Rates:"),
      plotOutput("plot"),
      br()
    )
  ),



  server = function(input, output) {
  
    #Used renderUI to make the selectInput drop down menu for cities, so that it relies on the input country
    output$teams <- renderUI({
      selectInput("teams", "NBA Teams",
                  choices = NBA.df$Team[NBA.df$Season == input$season])
    })
  }
)






