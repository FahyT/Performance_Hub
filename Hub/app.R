#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)

recommendation <- read.csv('https://raw.githubusercontent.com/amrrs/sample_revenue_dashboard_shiny/master/recommendation.csv',stringsAsFactors = F,header=T)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Performance Hub")
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search..."),
  sidebarMenu(
    menuItem("Key KPIs", tabName = "keykpis", icon = icon("dashboard")),
    menuItem("Commitments", tabName = "commitments", icon = icon("chart-line"),
             menuSubItem('SO1',tabName = 'so1',icon = icon('line-chart')),
             menuSubItem('SO2',tabName = 'so2',icon = icon('line-chart')),
             menuSubItem('SO3',tabName = 'so3',icon = icon('line-chart'))),
    menuItem("Transformation", tabName = "transformation", icon = icon("city")),
    menuItem("Volumetrics", tabName = "volumetrics", icon = icon("chart-bar")),
    menuItem("Customer Experience", tabName = "custexp", icon = icon("grin")),
    menuItem("Risk", icon = icon("send",lib='glyphicon'))
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Revenue per Account"
    ,status = "warning"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd", height = "300px")
  )
  ,box(
    title = "Revenue per Product"
    ,status = "danger"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegion", height = "300px")
  ) 
)

body <- dashboardBody(frow1, frow2)

ui <- dashboardPage(header, sidebar, body, title = "PAGE TITLE HERE", skin='blue')

server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  total.revenue <- sum(recommendation$Revenue)
  sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Top Account:',sales.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")  
  })
  
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")  
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Product:',prof.prod$Product)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })
  #creating the plotOutput content
  output$revenuebyPrd <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Product, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Product") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      labs(fill = "Region")
  })
  output$revenuebyRegion <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Account, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Account") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
       labs(fill = "Region")
  })
}

shinyApp(ui, server)


