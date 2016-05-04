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
    
      uiOutput("teams"),
      
      selectInput("method", label = h3("Expectation Methods"),
                  choices = list("Morey's Method","Hollinger's Method", "Both Methods"))
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
    
    output$comp <- renderPlot({
      if(input$method == "Morey's Method"){
        barplot(NBA.df$Wins[NBA.df$Season == input$season && NBA.df$Team == output$teams],
                NBA.df$Exp_W1[NBA.df$Season == input$season && NBA.df$Team == output$teams])
      }
      if(input$method == "Hollinger's Method"){
        barplot(NBA.df$Wins[NBA.df$Season == input$season && NBA.df$Team == output$teams],
                NBA.df$Exp_W2[NBA.df$Season == input$season && NBA.df$Team == output$teams])
      }
      if(input$method == "Both Methods"){
        barplot(NBA.df$Wins[NBA.df$Season == input$season && NBA.df$Team == output$teams],
                NBA.df$Exp_W1[NBA.df$Season == input$season && NBA.df$Team == output$teams],
                NBA.df$Exp_W2[NBA.df$Season == input$season && NBA.df$Team == output$teams])
      }
    })
    
    output$plot <- renderPlot({
      if(input$method == "Morey's Method"){
        barplot(NBA.df$err1[NBA.df$Season == input$season])
      }
      if(input$method == "Hollinger's Method"){
        barplot(NBA.df$err2[NBA.df$Season == input$season])
      }
      if(input$method == "Both Methods"){
        barplot(NBA.df$err1[NBA.df$Season == input$season])
        barplot(NBA.df$err2[NBA.df$Season == input$season])
      }
    })
  }
)






