#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tibble)
library(readxl)
library(markdown)
temp<-read.csv("./2022HL100_lotteryentrants_final.csv", stringsAsFactors = FALSE) #LOAD THE DATA
df<-as_tibble(temp)


df$fullname<-paste(df$First_Name, df$Last_Name, sep=" ", collapse = NULL)
head(df)

#df$USA<-ifelse(df$Country=="USA",1,0)

#NUMBER OF MEN AND WOMEN APPLICANTS
n_men_app=nrow(men<-df[which(df$Gender=="M"),])
n_women_app=nrow(women<-df[which(df$Gender=="F"),])

# Define UI for application that draws a histogram
ui <- fluidPage(

  htmltools::includeMarkdown("./markdown/implementingtheodds.md"),
  # Copy the line below to make a number input box into the UI.
  fluidRow(
    column(6, sliderInput("exp", label = h5("Exponent Base"), min=2, max=5, value = 2)),
    #column(6, sliderInput("relative", label = h5("Relative Worth"), min=0, max=1, value = 0.5)),
    #column(6, sliderInput("peak", label = h5("Optimal Finishes"), min=1, max=4, value = 3)),
    column(6, sliderInput("mult", label = h5("Multiplier Base"), min=1, max=10, value = 2)),
    column(6, sliderInput("Nm", label = h5("Number of Men"), min=50, max=301, value = 62)),
    column(6, sliderInput("Nw", label = h5("Number of Women"), min=50, max=108, value =66))
  ),
  
  htmltools::includeMarkdown("./markdown/howmanytickets.md"),
  fluidRow(
    column(6, sliderInput("apps", label = h5("Previous Applications"), min=0, max=6, value = 0)),
    column(6, sliderInput("finishes", label = h5("Previous Finishes"), min=0, max=4, value = 0)),
    column(6, sliderInput("volunteer", label = h5("Volunteer Shifts"), min=0, max=30, value = 0)),
    column(6, sliderInput("trailwork", label = h5("Extra Trailwork"), min=0, max=10, value = 0))
  ),
  
  fluidRow("Your tickets in the lottery:", textOutput("tickets")),
  
  fluidRow("These are the odds. Women first, then men:", tableOutput("women_odds"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$tickets <- renderText({
    #Do THE VARIABLE TRANSFORMATIONS FOR PEOPLE's ENTERED VALUES
    k_sim <- ifelse(input$finishes==0 , 0,
                    ifelse(input$finishes==1,  0.5,
                           ifelse(input$finishes==2, 1, 
                                  ifelse(input$finishes==3, 1.5,
                                         ifelse(input$finishes>=4, 0.5, 0)))))
    
    #Shifts max out at 30 (10 each race)
    v_sim<-pmin(input$volunteer, 30)
    t_sim<-pmin(input$trailwork, 10)
    
    #Tickets=2^(n+k+1)+2ln(v+t+1) where n, k, v, and t are defined as follows:
    input$exp^(k_sim+input$apps+1) + input$mult*log(v_sim+t_sim+1)
  })
  
  output$women_odds <- renderPrint({
    
    ######################################################################
    
    shit<-reactive({
    shit<-data.frame(matrix(ncol=8, nrow=1))
    colnames(shit)<-c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    shit$a<-input$finishes
    shit$b<-2
    shit$c<-3
  
})  
output$women_odds<-renderTable(shit)

        })
}

# Run the application 
shinyApp(ui = ui, server = server)
