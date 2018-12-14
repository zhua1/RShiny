rm(list=ls())
options(encoding = 'UTF-8')
library(plotly)
library(shiny)
library(shinyBS)
#library(ROracle)
library(shinydashboard)

filePath <- 'C:/Users/zhua1/OneDrive/MikaTest.csv'

f <- read.csv(filePath)

# if ("ï.." %in% names(f)) {
#   names(f) <- gsub("ï..", "", names(f))
# }

take_data <- f$apple[1]
take_pear <- f$pear[1]
take_peach <- f$peach[1]
take_banana <- f$banana[1]
take_watermelon <- f$watermelon[1]

sidebar <- dashboardSidebar(
  fluidPage(
    fluidRow(div(style = "height:10px; width:100%; background-color:#B7B700;")),
    HTML('<br>'),
    fluidRow('Apple Counts'),
    fluidRow(
      column(5, actionButton("clickOff", "-1 Click")),
      column(5, actionButton("click", "+1 Click"))
      ),
    fluidRow(div(style = "height:10px; width:100%; background-color:#B7B700;")),
    HTML('<br>'),
    fluidRow('Pear Counts'),
    fluidRow(
      column(5, actionButton("clickOff_pear", "-1 Click")),
      column(5, actionButton("click_pear", "+1 Click"))
      ),
    fluidRow(div(style = "height:10px; width:100%; background-color:#B7B700;")),
    HTML('<br>'),
    fluidRow('Peach Counts'),
    fluidRow(
      column(5, actionButton("clickOff_peach", "-1 Click")),
      column(5, actionButton("click_peach", "+1 Click"))
      ),
    fluidRow(div(style = "height:10px; width:100%; background-color:#B7B700;")),
    HTML('<br>'),
    fluidRow('Banana Counts'),
    fluidRow(
      column(5, actionButton("clickOff_banana", "-1 Click")),
      column(5, actionButton("click_banana", "+1 Click"))
      ),
    fluidRow(div(style = "height:10px; width:100%; background-color:#B7B700;")),
    HTML('<br>'),
    fluidRow('Watermelon Counts'),
    fluidRow(
      column(5, actionButton("clickOff_watermelon", "-1 Click")),
      column(5, actionButton("click_watermelon", "+1 Click"))
    ),
    fluidRow(div(style = "height:10px; width:100%; background-color:#B7B700;"))
  )
)

body <- dashboardBody(
  tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
  bsModal("ThankYou", "Message", "",tags$p(tags$h1("Data Saved!", style = "color:red", align = "center")), size = "small"),
  tags$head(
    tags$style(
      HTML('
           h3 {
           font-weight: bold;
           }
           ')
      )
      ),
  fluidRow(
    box(
      width = 12,
      title = "Click Tracker",
      "Increase or decrease clicks by 1",
      # Make the box red
      status = 'danger'
    )
  ),
  fluidRow(
           infoBoxOutput("click_box", width=2),
           infoBoxOutput("click_box2", width=2),
           infoBoxOutput("click_box3", width=2),
           infoBoxOutput("click_box4", width=2),
           infoBoxOutput("click_box5", width=2)
    ),
  
  fluidRow(
    column(width = 2,
           actionButton("do", "Save")
    )
  ),
  fluidRow(
    mainPanel(
      HTML('<br>'),
      textOutput("selected_var")
    )
  ),
  fluidRow(
    HTML('<br>'),
    column(width = 10,
      plotOutput("plot", width="100%")
    )
  )
)

ui <- dashboardPage(
  skin = "green",
  header = dashboardHeader(),
  sidebar = sidebar,
  body = body)

firstUp <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

server <- function(input, output, session) {
    output$plot <- renderPlot({
      barplot(height = as.numeric(f[1,]), width = 6, space = .5, names.arg = firstUp(names(f)), ylab = 'Counts', col=c('darkred', 'greenyellow', 'pink', 'yellow', 'green'))
    })
    # input$click - input$clickOff
    output$click_box <- renderInfoBox({
      infoBox('Apple', value=take_data+input$click-input$clickOff)
    })
    output$click_box2 <- renderInfoBox({
      infoBox('Pear', value=take_pear+input$click_pear-input$clickOff_pear)
    })
    output$click_box3 <- renderInfoBox({
      infoBox('Peach', value=take_peach+input$click_peach-input$clickOff_peach)
    })
    output$click_box4 <- renderInfoBox({
      infoBox('Banana', value=take_banana+input$click_banana-input$clickOff_banana)
    })
    output$click_box5 <- renderInfoBox({
      infoBox('Watermelon', value=take_watermelon+input$click_watermelon-input$clickOff_watermelon)
    })
    observeEvent(input$do, {
      apple <- take_data+input$click-input$clickOff
      pear <- take_pear+input$click_pear-input$clickOff_pear
      peach <- take_peach+input$click_peach-input$clickOff_peach
      banana <- take_banana+input$click_banana-input$clickOff_banana
      watermelon <- take_watermelon+input$click_watermelon-input$clickOff_watermelon
      activate_modal <- "$('#ThankYou').modal('show')"
      session$sendCustomMessage(type = 'jsCode', list(value = activate_modal))
      output$selected_var <- renderText({ 
        paste("You have saved", apple, "for apple,", pear, "for pear,", peach, "for peach,", banana, "for banana,", watermelon, "for watermelon.")
      })
      f$apple[1] <- apple
      f$pear[1] <- pear
      f$peach[1] <- peach
      f$banana[1] <- banana
      f$watermelon[1] <- watermelon
      output$plot <- renderPlot({
        barplot(height = as.numeric(f[1,]), width = 6, space = .5, names.arg = firstUp(names(f)), ylab = 'counts', col=c('darkred', 'greenyellow', 'pink', 'yellow', 'green'))
      })
      write.csv(f, filePath, row.names=F)
    })
  }

shinyApp(ui, server)