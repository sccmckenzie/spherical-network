library(shiny)
library(htmltools)
library(tidyverse)
library(reactable)
library(widyr)
library(threejs)

source("sphereFun.R")
source("code.R")

ui <- tagList(
  tags$head(includeScript("window-size.js")),
  includeCSS("css.css"),
  fluidPage(
    verbatimTextOutput("height"),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   HTML("<a href = \"https://sccm.io\">sccm.io</a>"),
                   h2(style = "margin-top: 5px", "Select a Cocktail!"),
                   br(),
                   reactableOutput("drink_list"),
                   br(),
                   reactableOutput("ingredients"),
                   br(),
                   "Ingredients with higher intercorrelation are clustered in center."
                   ),
      mainPanel(width = 9, uiOutput("ui_sphere")),
      position = "right",
      fluid = FALSE
    )
  )
)

server <- function(input, output, session) {
  set.seed(93)
  drinks <- cocktails %>% 
    distinct(drink) %>% 
    arrange(sample(n()))
  
  output$drink_list <- renderReactable({
    reactable(drinks,
              defaultColDef = colDef(name = "Search below", filterable = TRUE),
              highlight = TRUE,
              sortable = FALSE,
              showPageInfo = FALSE,
              defaultPageSize = 5,
              selection = "single",
              onClick = "select"
    )})
  
  drink <- reactive({
    state <- getReactableState("drink_list", name = "selected")
    
    if (is.null(state)) return(NULL) else drinks[state, ][[1]]
  })
  
  output$ingredients <- renderReactable({
    req(drink())
    
    reactable(filter(cocktails, drink == drink()) %>% 
                select(`#` = ingredient_number, ingredient, measure),
              sortable = FALSE,
              showPageInfo = FALSE,
              pagination = FALSE,
              rowStyle = function(index) {
                  if (filter(cocktails, drink == drink())[index, "grp"] == 1) {
                    list(background = "#79D7F7")
                  }
              },
              columns = list(
                `#` = colDef(width = 20)
              )
  )})
  
  output$sphere <- renderScatterplotThree({
    net <- generate_net(drink())
    
    graphjs(net, layout = sphereFun(V(net)$grp), edge.color = "#DB565D")
  })
  
  output$ui_sphere <- renderUI(
    scatterplotThreeOutput("sphere", height = input$height)
  )
}

shinyApp(ui, server)