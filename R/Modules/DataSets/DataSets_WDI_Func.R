
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.




#' WorldEconomic_Dash_Func 
#'
#' 
#'
#' @param Shinyinputs c(input, output, session)
#' @return plots
#' @export

WorldEconomic_Dash_Func <- function(input, output, session){
  
 library("WDI")

  # ------------------------------------------------------------------------ # 
  # Data (later read from DB)
  # ------------------------------------------------------------------------ # 

  # ------
  if(file.exists(paste0(pathwd_dash, "/Data/WDI_new_cache.rds")) && 
     file.exists(paste0(pathwd_dash, "/Data/WDI_Valid_Indicators.rds")) ){
    
    new_cache <- readRDS(file = paste0(pathwd_dash, "/Data/WDI_new_cache.rds"))
    Valid_Indicators <- readRDS(file = paste0(pathwd_dash, "/Data/WDI_Valid_Indicators.rds"))
  }
  
  WDI_indicators = data.frame( new_cache[["series"]] )
  # WDI_indicators = data.frame( WDIsearch(cache = new_cache) )
  WDI_countries = data.frame( new_cache[["country"]] )
  
  
  Countries <- list(
    Europe = c('Germany', 'United Kingdom'),
    America = c('United States', 'Canada'),
    Asia = c('China',  'Iran, Islamic Rep.', 'Turkey', 'United Arab Emirates')
  )
  
  Default_Countries <- c('Germany', 'United States', 'United Kingdom', 'Iran, Islamic Rep.',
                'China', 'United Arab Emirates')

  
  Default_Indicators <- c('NY.GDP.PCAP.KD' , 'NE.TRD.GNFS.ZS', 'NY.GDP.DEFL.KD.ZG', 
                          'NY.GDP.PCAP.CD')
  
  # Valid_Indicators$Indics[sample(1:nrow(Valid_Indicators), 4)]
  
  # ------------------------------------------------------------------------ # 
  # UI
  # ------------------------------------------------------------------------ # 

  myGauge <- function(id, label, value){
    tagList(
      tags$label(
        `for` = id,
        label
      ),
      tags$meter(
        id = id,
        value = value,
        min = "0",
        max = "1"
      )
    )
  }
  
  output$DataSets_WDI_UI <- renderUI({
    
  tagList(
    
  fluidRow(
    column(3, # offset = 1,
           selectInput(inputId = "WD_Indicators",
                         label = "Economic Indicators", 
                         #multiple = TRUE,
                        #selectize = FALSE,
                         choices = Default_Indicators, # Valid_Indicators$Indics,
                         selected = Default_Indicators)
           ),
    column(3, 
           selectInput(inputId = "WD_Countries",
                       label = "Countries", 
                       #multiple = TRUE,
                       #selectize = FALSE,
                       choices = Default_Countries, # Countries, 
                       selected = Default_Countries[1])
    ),
    column(3, style = 'padding: 2px',
           dateRangeInput("WD_DateRange", "Date range:",
                          start = "2010-01-01",
                          end   = "2025-12-31"),
           tags$head(
             tags$style("
              .input-daterange input {
                min-height: 35px;
              }
            ") )
    )
    ),
 fluidRow(
   col_12(br())
   ),

 fluidRow(
    column(6, 
           plotlyOutput("WD_Pltly_1", width = "100%", height = "350px"),
           plotlyOutput("WD_Pltly_3", width = "100%", height = "350px")
    ),
    column(6, 
           plotlyOutput("WD_Pltly_2", width = "100%", height = "350px"),
           plotlyOutput("WD_Pltly_4", width = "100%", height = "350px")
    ) 
    )
  )
  })
    
  # ------------------------------------------------------------------------ # 
  # Calculations 
  # ------------------------------------------------------------------------ #
  

  
  
PlotWDI_Function <- function(indicator = 'NY.GDP.PCAP.CD',
                               country = "Germany",
                               start = 1990,
                               end = 2030){
    

  }

  
  
  
  # ------------------------------------------------------------------------ # 
  # Server
  # ------------------------------------------------------------------------ # 

  output$WD_Pltly_1 <- renderPlotly({
    
    PlotWDI_Function(indicator = Default_Indicators[1], # input$WD_Indicators[1], 
                       country = input$WD_Countries,
                       start = 1990,
                       end = 2030 )
    })
  
  ##
  output$WD_Pltly_2 <- renderPlotly({
    
    PlotWDI_Function(indicator = Default_Indicators[2], # input$WD_Indicators[2], 
                     country = input$WD_Countries,
                     start = 1990,
                     end = 2030 )
    })
  
  ##
  output$WD_Pltly_3 <- renderPlotly({
    
    PlotWDI_Function(indicator = Default_Indicators[3], # input$WD_Indicators[3], 
                       country = input$WD_Countries,
                       start = 1990,
                       end = 2030 )
    })
  
  ##
  output$WD_Pltly_4 <- renderPlotly({
      
      PlotWDI_Function(indicator = Default_Indicators[4], # input$WD_Indicators[4], 
                       country = input$WD_Countries,
                       start = 1990,
                       end = 2030 )
    })
    


  output$ExGBox1 <- output$ExGBox2 <- output$ExGBox3 <- output$ExGBox4 <- renderValueBox({
    
    shinydashboard::valueBox(40,  subtitle = "xx",
      icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
      color = "green"
    )
  })
  

 
}

