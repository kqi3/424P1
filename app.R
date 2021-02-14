#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(usmap)
library(tidyverse)

load(".RData")


# Define UI for application that draws a histogram
ui <- navbarPage(title= "Project 1- Power and the Passion",
    tabPanel(title="US total",
      plotOutput("norm"),
      actionButton("renorm","Resample")
  ),
    navbarMenu(title="Total Energy",
       #First chart:stacked bar chart showing the amount of each energy source per year from 1990 - 2019
       tabPanel(title="Energy Source Amount Chart",
       radioButtons("EnergySource", "Select Energy Source type:",
                   c("Coal" = "Coal","Geothermal" = "Geothermal",
                    "Hydroelectric" = "Hydro","Natural Gas"="Natural Gas"
                    )),            
       sliderInput(inputId = "num",label="choose a number", value=25, min=1,max=100),
       textInput(inputId = "title",label="Write a title",value="histogram of random Normal Values"),
       plotOutput("stacked_amount"),
       actionButton("reunif","Resample1")
     ),
     #second chart: stacked bar chart showing percent of the total production for each energy source per year from 1990 - 2019
      tabPanel(title="Percent of Energy Prodction",
        plotOutput("stacked_percent"),
        actionButton("reunif","Resample2")
      ),
     #Third chart: line chart showing the amount of each energy source per year from 1990 - 2019
      tabPanel(title="line amount of each energy",
              plotOutput("line_amount"),
              actionButton("reunif","Resample2")
      ),
    # Fourth chart:line chart showing the percent of the total production for each energy source per year from 1990 - 2019
      tabPanel(title="line percent of total each energy",
                       plotOutput("line_percent"),
                       actionButton("reunif","Resample2")
      ),
    # Fifth table:the table showing amount and the percent of the total production for each energy source per year from 1990 - 2019
    tabPanel(title="Amount and Percent table",
             checkboxGroupInput("energysources", "Choose energy source to show:",
                                c("Coal" = "Coal",
                                  "Geothermal" = "Geothermal",
                                  "Hydro" = "Hydro",
                                  "Natural Gas"="Natural Gas",
                                  "Nuclear"="Nuclear",
                                  "Petroleum"="Petroleum",
                                  "Solar"="Solar",
                                  "Wind"="Wind",
                                  "Wood"="Wood")),
             column(6,
                    tableOutput('amount_percent')
             )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
 #stacked bar chart showing amount of each energy source per year from 1990-2019
  output$stacked_amount <- renderPlot({
    options(repr.plot.width=24, repr.plot.height=12)
    ggplot(noUS_totalEnergy, aes(x=YEAR, y=MWH,fill=ES)) +
      labs(title="                 The total amount of each energy source per year", 
           x="The year from 1990 to 2019", y = "The energy produced unit: MegaWatthours")+
      geom_bar(position="stack", stat="identity")+theme(
        # Change legend Postion to left
        legend.position="left",
        # Change legend background color
        legend.background = element_rect(fill = "darkgray"),
        legend.key = element_rect(fill = "lightblue", color = NA),
        # Change legend key size and key width
        legend.key.size = unit(1.2, "cm"),
        legend.key.width = unit(0.6,"cm"),
        plot.title = element_text(size = 18),
        legend.title = element_text(color = "blue", size = 12),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(color = "purple",size =12))
  })
  #stacked bar chart showing the percent of tha total producnt from 1990-2019
  output$stacked_percent <- renderPlot({
    options(repr.plot.width=24, repr.plot.height=14)
   
    #data_percent variable has grouped by year and mutated with pertage amount      
    
    ggplot(data_percent, aes(x=YEAR, y=amount_percent,fill=ES)) +
      geom_bar(position="fill", stat="identity")+
      labs(title="                 The percent of total production for each energy source per year", 
           x="The year from 1990 to 2019", y = "The percent each energy source Unit: %")+
      geom_bar(position="stack", stat="identity")+theme(
        # Change legend Postion to left
        legend.position="left",
        # Change legend background color
        legend.background = element_rect(fill = "darkgray"),
        legend.key = element_rect(fill = "white", color = NA),
        # Change legend key size and key width
        legend.key.size = unit(1.2, "cm"),
        legend.key.width = unit(0.6,"cm"),
        plot.title = element_text(size = 18),
        legend.title = element_text(color = "blue", size = 12),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(color = "purple",size =12))
  })
  #line chart showing the amount of each energy source per year from 1990 to 2019
  output$line_amount <- renderPlot({
    options(repr.plot.width=24, repr.plot.height=14)
    ggplot(noUS_totalEnergy, mapping=aes(x=YEAR,y=MWH/1000000,group=ES,color=ES))+
    labs(title="     The amount of each energy source per year", x="The year from 1990 to 2019", y = "Amount Produced Unit: Million MegaWattHours ")+
    coord_cartesian(ylim=c(0,2000))+geom_line(size=1)+theme(legend.position = "left")+
      theme(
        # Change legend Postion to left
        legend.position="left",
        # Change legend background color
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = NA),
        # Change legend key size and key width
        legend.key.size = unit(1.2, "cm"),
        legend.key.width = unit(0.6,"cm"),
        plot.title = element_text(size = 18),
        legend.title = element_text(color = "blue", size = 12),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(color = "purple",size =12))
    
  })
  #line chart showing the percent of the total production for each energy source per year from 1990 - 2019
  output$line_percent <- renderPlot({
    options(repr.plot.width=24, repr.plot.height=14)
    
    ggplot(data_percent, mapping=aes(x=YEAR,y=amount_percent,group=ES,color=ES))+
      labs(title="     The percent of the total production for each energy source per year", x="The year from 1990 to 2019", y = "Percent Unit: % ")+
      coord_cartesian(ylim=c(0,60))+geom_line(size=1)+theme(legend.position = "left")+
      theme(
        # Change legend Postion to left
        legend.position="left",
        # Change legend background color
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "lightblue", color = NA),
        # Change legend key size and key width
        legend.key.size = unit(1.2, "cm"),
        legend.key.width = unit(0.6,"cm"),
        plot.title = element_text(size = 18),
        legend.title = element_text(color = "blue", size = 12),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(color = "purple",size =12))
        
    
  })
  output$amount_percent <- renderTable({
    data_percent[,c("YEAR","STATE","TP","ES","MWH","amount_percent",input$variable),drop = FALSE]
  }, rownames = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
