library(shiny)
load("NBA.Rdata")

shinyApp(
  
  ui = fluidPage(
    titlePanel(
      "NBA Records"
    ),
    sidebarPanel(
      selectInput("year", "Season:", sort(unique(NBA.df$Season), decreasing = TRUE), selected = "2015-2016"),
      hr(),
      uiOutput("choose_team"), #dependent on country chosen
      hr(),
      selectInput("method", "Method:", c("Morey's Method", "Hollinger's Method", "Both Methods")) #dependent on city chosen
      
      
    ),
    mainPanel(
      h4("How'd your team do?"),
      plotOutput("comparison"),
      br(),
      textOutput("mess"),
      br(),
      plotOutput("err")
    )
    
    
  ),
  
  server = function(input, output, session) {
    #can only choose a city from the chosen country
    output$choose_team = renderUI({
      selectInput("team", "Teams:", sort(NBA.df$Team[NBA.df$Season == input$year]))
    })
    
    #update forecast function
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
    
    titles = reactive({
      if(input$method == "Both Methods"){
        return(c("Actual Wins", "Morey's Expected", "Hollinger's Expected"))
      }
      else{
        return(c("Actual Wins", "Expected"))
      }
    })
    
    output$comparison = renderPlot({
      barplot(get_method(), names.arg = titles(), ylab = "Number of Wins")
    })
    
    output$mess = renderText({
      get_method()
    })
    
    colors = reactive({
      temp = rep("black", length(mor_err))
      yrs = sort(unique(NBA.df$Season))
      ind = which(yrs == input$year)
      temp[ind] = "blue"
      return(temp)
    })
    
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