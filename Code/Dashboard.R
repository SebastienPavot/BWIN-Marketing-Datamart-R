#We first import the packages we need
library(shiny)
#install.packages("shinydashboard", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
library(shinydashboard)
library(ggplot2)

rm(list = ls())

#Then we load the file we need to print the dashboard

load(file = 'C:/Users/alopezzeron/Desktop/Fall_2019/Business_Analytics_Tools_OS/Group_Assignment/Group-Assignment/datamart_final.Rdata')

countrynamesort = unique(datamart_final$CountryName[datamart_final$IsInActualSportGambling ==1])
countrynamesort = sort(countrynamesort)

countrynamesortWebsite = unique(datamart_final$CountryName)
countrynamesortWebsite = sort(countrynamesort)


#We initiate UI which will be the UI of the dashboard

ui <- dashboardPage(
  #We define a title
  dashboardHeader(title = "BWIN Dashboard"),
  #And we put in a sidebar differents menus link to each to one specific graphic
  dashboardSidebar(
    sidebarMenu(
    menuItem("Age distribution", tabName = "Age", icon = icon("fas fa-birthday-cake")),
    menuItem("Poker monthly statistic", tabName = "Poker", icon = icon("fas fa-heart")),
    menuItem('Bets versus Gaming revenue',tabName='Sports',icon = icon("far fa-money-bill-alt")),
    menuItem('Bets per Website',tabName='Website',icon = icon("fab fa-tablet-alt")),
    menuItem('BWIN Datamart',tabName='Data',icon = icon("fal fa-table"))
    )
  ),
  #Now in the body, we will define the value inside each item menu
  dashboardBody(
    tabItems(
    # First tab content containing age distribution
    tabItem(tabName = "Age",
            h2 ("Age distribution per Country"),
            fluidRow(
              box(plotOutput("CountryAgePlot")),
              
              box(
                  selectInput("Country", "Region:", 
                              choices=countrynamesort)
              )
          )
      ),

# Second tab content containing Poker statistics
      tabItem(tabName = "Poker",
        h2("Monthly statistics"),
        fluidRow(
          box(plotOutput('PokerStats')),
          box(selectInput("Stats", "Stats type:", 
                          choices= c('Buy','Sell','NbTrans')),
              
              # br() element to introduce extra vertical spacing
              br(),
              
              # Input: Slider for the month
              selectInput("Month", "Values for the month:", 
                          choices= c('February','March','April','June','July','August','Sept','Oct')))
        )
    ),

###Third tab content for Bets versus game revenue
      tabItem(tabName = "Sports",
        h2("Bets versus Game revenue"),
        fluidRow(
          box(plotOutput('plot')),
          box(sliderInput(inputId = "TotalBets", label = "No of Bets",
                           min = 0, max = 500,
                           value = c(1, 50)),
               selectInput("country", "Country",
                           choices = c("All", unique(datamart_final$CountryName))))
        )
      ),
#Fourth tab content for bets per website
    tabItem(tabName = 'Website',
            h2('Bets per Website'),
            fluidRow(
              box(plotOutput('WebsitePlot')),
              box(selectInput("Application", "Name of the website:", 
                              choices=unique(datamart_final$`Application Description`)),
                  hr(),
                  helpText("Data from BWIN 2005"))
               )
            ),


#Fifth tab content for the datatable
    tabItem(tabName = "Data",
            h2('Datamart'),
            #We use div to make it scrollable horizontally
            div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
            
            )
    )
  )
)

#Now we will define the function to make the graphs interactive
server = function(input, output) {
  
  #Function for website graphic:
  
  output$WebsitePlot <- renderPlot({
    
    # Render a barplot
    hist(datamart_final[datamart_final$`Application Description` == input$Application, "TotalNumberofBets"],
         main=input$TotalNumberofBets,
         ylab="Number of people",
         xlab="Number of bets",
         breaks = 1000,
         xlim = c(0,2000))
  })
  
  #This function is used to filter data for the bets versus game revenue regarding slider and country 
  filtered_data <- reactive({
    data <- datamart_final
    data <- subset(
      data,
      TotalBets >= input$TotalBets[1] & TotalBets <= input$TotalBets[2]
    )
    if (input$country != "All") {
      data <- subset(
        data,
        input$country == datamart_final$CountryName
      )
    }
    data
  })
  
  output$plot <- renderPlot({
    # To subset the data regarding the filter
    data <- datamart_final
    data <- subset(
      data,
      TotalBets >= input$TotalBets[1] & TotalBets <= input$TotalBets[2]
    )
    if (input$country != "All") {
      data <- subset(
        data,
        input$country == datamart_final$CountryName
      )
    }
    ggplot(data, aes(OverallGains, TotalBets)) +
      geom_point() +
      scale_x_log10()
  })
  
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  
  
  # Fill in the spot we created for a plot
  output$CountryAgePlot <- renderPlot({
    
    # Render a histogram regarding country and if it's in the flag column
    hist(datamart_final[datamart_final$CountryName == input$Country & datamart_final$IsInActualSportGambling == 1, "AGE"], 
         main= paste("Histogram of age distribution in ",input$Country),
         ylab="Number of users",
         xlab="Year",
         breaks = 15)
  })
  output$PokerStats <- renderPlot({
    #Test is equal to paste of Month slider & Stats slider value to select the column name
    test = paste(input$Month,'_',input$Stats,sep="")
    
    # Render a histogram for poker
    hist(datamart_final[datamart_final$IsInPokerFinal == 1, test], 
         main= paste("Histogram of ", input$Stats, "in ", input$Month),
         ylab="Count of operations",
         xlab= paste("Value of ", as.character(input$Stats)),
         breaks = 100,
         xlim = c(1,50))
  })
}
#We launch the shiny application to have our dashboard
shinyApp(ui, server)