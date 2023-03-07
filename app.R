
# Libraries --------------------------------------------------------------------


# install.packages("devtools")
# require(devtools)
# install_version("bslib", version = "0.3.1", repos = "http://cran.us.r-project.org")

if (!require("pacman")) install.packages("pacman")
# 
# pacman::p_load(plyr, tidyverse, ggplot2, leaflet, plotly, 
#                shiny, shinyjs, shinythemes, shinyWidgets, shinyBS, shinybusy,
#                shinydashboard, shinycssloaders, scales)
# 
# 
# library(civis)
# library(bslib)

pacman::p_load(shiny, shinyWidgets, leaflet, sf, tidyverse, civis, leafpop, ggplot2, ggthemes, shinycssloaders)

# library(shiny)
# library(shinyWidgets)
# library(leaflet)
# library(sf)
# library(tidyverse)
# library(civis)
# library(leafpop)
# library(ggplot2)
# library(ggthemes)
# library(shinycssloaders)

# Source ------------------------------------------------------------------------



source("app_global.R")

# App ----------------------------------------------------------------------------

# shinyApp(ui = ui, server = server)

ui <- fluidPage(
  
  tags$head(
    tags$style(type = "text/css", "html, body {width:100%;height:100%;}"),
  ),
  
  
  fluidRow(
    column(width = 12,
      tags$h2("SMC Eligibility")# ,
      # tags$h5("visualizations provide etc, etc, etc.....")
    )
  ),
  

  fluidRow(style = "margin-top:30px;",
    column(width = 3,
      pickerInput(
        inputId = "in_adm0", label = ("Select Country"), 
        choices = cc_list, selected = c("Senegal", "Mali", "Guinea"),
        options = list('actions-box' = T),
        multiple = T
      )
    ),
    column(width = 4,
      radioGroupButtons(
        inputId = "in_thresh", label = ("Cutoff"),
        choices = c("50%", "55%", "60%", "65%", "70%"),
        selected = "60%"
      )
    ),
    column(width = 3, style = "margin-top:20px;",
      actionButton(
        inputId = "update", label = ("Update plots"),
        class = "btn-primary"
      )
    )
  ),
  
  fluidRow(style = "margin-top:40px;",
    column(width = 6,
      withSpinner(leafletOutput("map", height = "67vh"))
    ),
    column(width = 6,
       tabsetPanel(
         tabPanel(style = "overflow-y:scroll;", # max-height: 67vh",
           "Eligible Areas",
           withSpinner(plotOutput("plot_eligible"))
           # plotOutput("plot_eligible")
         ),
         tabPanel(style = "overflow-y:scroll;", # max-height: 67vh",
           "Ineligible Areas",
           withSpinner(plotOutput("plot_ineligible"))
         )
       )
    )
  )
  
  
)



server <- function(input, output, session){
  
  
  xmain <- eventReactive(input$update, {
    get_data_main(ctry = input$in_adm0, cutoff = input$in_thresh)
    }, ignoreNULL = F)
  
  
  
  output$map <- renderLeaflet({
    
    sdat <- xmain()
    
    leg <- paste0("SMC Eligibility<br>(", sdat$cutoff, " cutoff)")
    
    leaflet(sdat$s2in) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "black", weight = 0.4, fillColor = ~ clr_smc(smc_in),
                  popup = ~popup_txt) %>%
      addLegend("bottomright", pal = clr_smc, values = ~ smc_in, title = leg)
  })
  
  
  ncol_eligible <- eventReactive(input$update, {
    xdat <- xmain() 
    xdat <- xdat$xin %>% filter(smc_in == "Eligible")
    
    narea <- length(unique(xdat$area))
    
    if(length(narea) == 0){
      ncolfacet <- 3
    } else {
      ncolfacet <- ceiling(narea / 4)
    }
    
    (ncolfacet)
    
  }, ignoreNULL = F)
  
  
  
  ncol_ineligible <- eventReactive(input$update, {
    xdat <- xmain() 
    xdat <- xdat$xin %>% filter(smc_in == "Not Eligible")
    
    narea <- length(unique(xdat$area))
    
    if(length(narea) == 0){
      ncolfacet <- 2
    } else {
      ncolfacet <- ceiling(narea / 4)
    }
    
    (ncolfacet)
    
  }, ignoreNULL = F)
  
  
  
  output$plot_eligible <- renderPlot({
    
    xdat <- xmain() 
    xdat <- xdat$xin %>% filter(smc_in == "Eligible")
    
    narea <- length(unique(xdat$area))
    
    if(length(narea) == 0) {
      gout <- ggplot() + theme_void() + 
        geom_text(aes(0, 0, label = "Data unavailable"))
    } else {
      ncolfacet <- ceiling(narea / 4)
      gout <- ggplot(xdat, aes(x = date, y = confirmed_cases)) +
        geom_line() + geom_point() +
        facet_wrap(~ area, scales = "free", ncol = 4) +
        theme_minimal() +
        xlab("") + ylab("Confirmed Cases") +
        scale_x_date(date_labels = "%b-%Y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    gout
    
    
    
  }, height = function(){150*ncol_eligible()})
  
  
  
  
  output$plot_ineligible <- renderPlot({
    
    xdat <- xmain() 
    xdat <- xdat$xin %>% filter(smc_in == "Not Eligible")
    
    narea <- length(unique(xdat$area))
    
    if(length(narea) == 0) {
      gout <- ggplot() + theme_void() + 
        geom_text(aes(0, 0, label = "Data unavailable"))
    } else {
      ncolfacet <- ceiling(narea / 4)
      gout <- ggplot(xdat, aes(x = date, y = confirmed_cases)) +
        geom_line() + geom_point() +
        facet_wrap(~ area, scales = "free", ncol = 4) +
        theme_minimal() +
        xlab("") + ylab("Confirmed Cases") +
        scale_x_date(date_labels = "%b-%Y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    gout
    
    
    
  }, height = function(){150*ncol_ineligible()})
  
  
}
shinyApp(ui = ui, server = server)