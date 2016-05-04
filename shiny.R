library(shiny)
load("NBA.Rdata")
View(NBA.df)

#Shiny App

shinyApp(ui =fluidPage(
  titlePanel("Predicting Expected Wins for NBA Teams"),
    sidebarPanel(
      h4("Information"),
    
      #Create select input for all of the seasons
      selectInput("season", "NBA Season:",
                  choices = sort(NBA.df$Season),
                  selected = "2015-2016"),
      hr(),
      uiOutput("team_options"),
      hr(),
      selectInput("method", "Expectation Methods:",
                  choices = c("Morey's Method","Hollinger's Method", "Both Methods"), 
                  selected = "Morey's Method")
    ),
  
    #Create main panel for eventual plots
    mainPanel(
      h4("How did the team do?"),
      plotOutput("comp"),
      br(),
      textOutput("messages"),
      br(),
      h4("Error Rates:"),
      plotOutput("err"),
      br()
    )
  ),



  server = function(input, output, session) {
  
    #Used renderUI to make the selectInput drop down menu for cities, so that it relies on the input country
    output$team_options = renderUI({
      selectInput("teams", "NBA Teams",
                  choices = sorted(NBA.df$Team[NBA.df$Season == input$season]),
                  )
    })
    
    graph_choice = reactive({
      if(input$method == "Morey's Method"){
        return(c(NBA.df$Wins[NBA.df$Season == input$season & NBA.df$Team == input$teams],
                 NBA.df$Exp_W1[NBA.df$Season == input$season & NBA.df$Team == input$teams]))
      },
      if(input$method == "Hollinger's Method"){
        return(c(NBA.df$Wins[NBA.df$Season == input$season && NBA.df$Team == input$teams],
                  NBA.df$Exp_W2[NBA.df$Season == input$season && NBA.df$Team == input$teams]))
      },
      if(input$method == "Both Methods"){
        return(c(NBA.df$Wins[NBA.df$Season == input$season && NBA.df$Team == input$teams],
                  NBA.df$Exp_W1[NBA.df$Season == input$season && NBA.df$Team == input$teams],
                  NBA.df$Exp_W2[NBA.df$Season == input$season && NBA.df$Team == input$teams]))
      }
      
      
    })
    
    output$comp = renderPlot({
      barplot(graph_choice(), width = 1, xlim = c(0, length(graph_choice()+1)), ylim = c(0, 82))
    })
    
    output$messages = renderText(paste(graph_choice(), input$method))
    
    output$err = renderPlot({
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






