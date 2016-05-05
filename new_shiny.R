library(shiny)
load("NBA.Rdata")

shinyApp(
  
  ui = fluidPage(
    titlePanel(
      "NBA Records"
    ),
    sidebarPanel(
      #choose season you want to look at
      selectInput("year", "Season:", sort(unique(NBA.df$Season), decreasing = TRUE), selected = "2015-2016"),
      hr(),
      #which team based on the season
      uiOutput("choose_team"), #dependent on country chosen
      hr(),
      #option of algorithm you look at, or choose both
      selectInput("method", "Method:", c("Morey's Method", "Hollinger's Method", "Both Methods")) #dependent on city chosen
      
      
    ),
    mainPanel(
      h3("How'd your team do?"),
      #plot the comparison of observed vs expected wins
      plotOutput("comparison"),
      br(),
      #output a message of how the team did based on expectations
      textOutput("mess"),
      br(),
      h4("Average Error of Algorithm"),
      #plot to show avg amount of games the algorithm was off by for a team in that season
      plotOutput("err")
    )
    
    
  ),
  
  server = function(input, output, session) {
    #can only choose a team that was from chosen season
    output$choose_team = renderUI({
      selectInput("team", "Teams:", sort(NBA.df$Team[NBA.df$Season == input$year]))
    })
    
    #function returning vector of observed wins and expected wins based on season, team, and method
    get_method = reactive({
      if(input$method == "Morey's Method"){
        return(c(NBA.df$Wins[NBA.df$Season == input$year & NBA.df$Team == input$team],
                 NBA.df$Exp_W1[NBA.df$Season == input$year & NBA.df$Team == input$team]))
      }
      if(input$method == "Hollinger's Method"){
        return(c(NBA.df$Wins[NBA.df$Season == input$year & NBA.df$Team == input$team],
                 NBA.df$Exp_W2[NBA.df$Season == input$year & NBA.df$Team == input$team]))
      }
      if(input$method == "Both Methods"){
        return(c(NBA.df$Wins[NBA.df$Season == input$year & NBA.df$Team == input$team],
                 NBA.df$Exp_W1[NBA.df$Season == input$year & NBA.df$Team == input$team],
                 NBA.df$Exp_W2[NBA.df$Season == input$year & NBA.df$Team == input$team]))
      }
    })
    #function to get bar labels for graph
    titles = reactive({
      if(input$method == "Both Methods"){
        return(c("Actual Wins", "Morey's Expected", "Hollinger's Expected"))
      }
      else{
        return(c("Actual Wins", "Expected"))
      }
    })
    
    #first plot, putting observed wins next to expected
    #two bars if one method chosen, 3 if both methods chosen
    output$comparison = renderPlot({
      barplot(get_method(), names.arg = titles(), ylab = "Number of Wins")
    })
    
    #output a message
    #needs to be updated
    output$mess = renderText({
      get_method()
    })
    
    #vector of colors(all black), will make chosen season blue to highlight it in bottom graph
    colors = reactive({
      temp = rep("black", length(mor_err))
      yrs = sort(unique(NBA.df$Season))
      ind = which(yrs == input$year)
      temp[ind] = "blue"
      return(temp)
    })
    
    #plot the avg error of number of games off for chosen algorithm by season
    output$err = renderPlot({
      if(input$method == "Morey's Method"){
        barplot(mor_err, names.arg = sort(unique(NBA.df$Season)), las = 2,  ylab = "Avg. Games Wrong", main = "Morey's Method", col = colors())
      }
      if(input$method == "Hollinger's Method"){
        barplot(hol_err, names.arg = sort(unique(NBA.df$Season)), las = 2, ylab = "Avg. Games Wrong", main = "Hollinger's Method", col = colors())
      }
      if(input$method == "Both Methods"){
        par(mfrow = c(1,2))
        barplot(mor_err, names.arg = sort(unique(NBA.df$Season)), las = 2, ylab = "Avg. Games Wrong", main = "Morey's Method", col = colors())
        barplot(hol_err, names.arg = sort(unique(NBA.df$Season)), las = 2, ylab = "Avg. Games Wrong", main = "Hollinger's Method", col = colors())
      }
    })
    
    
    
  },
  
  
  options = list(height = 500)
)