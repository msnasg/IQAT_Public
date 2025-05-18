

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


Fund_Screening_ui_func <- function(input, output, session){
  
  
  criteria_specs <- data.frame(matrix(NA, nrow = 4, ncol = 0))
  criteria_specs$factor <- c("Return on Capital", 
                             "EarningsYield", "P/E", "Return on Assets")
  criteria_specs$ui <- c("num", "num", "num", "num") # char, radio
  criteria_specs$limit <- c("range", "range", "range", "range") # Min, Max
  criteria_specs$scale <- c("", "", "", "")
  criteria_specs$value <- c(0.2, 0.05, 5, 0.2)
  criteria_specs$min <- c( 0, 0, 0, 0)
  criteria_specs$max <- c(NA, NA, NA, NA)
  criteria_specs <<- criteria_specs
  
  output$id_Fundamental_Screening_ui <- renderUI({
    fluidRow(
      style = "font-size:12px;",
     
        bs4Card(
          id = NULL,
        title = "Market Screening",
        # footer = "Table ....", 
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        collapsed = FALSE,
        closable = FALSE,
        maximizable = TRUE,
        label = NULL,
        width = 12,
        # height = "800px",
        sidebar = boxSidebar(
          tagList(
          ),
          id = "id_Fund_Screening_main_bs4Card_sidebar",
          width = 60,
          background = "#f9f9fa",
          startOpen = FALSE,
          icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
          easyClose = TRUE
        ),
        fluidRow(
         # 
          bs4Card(
            id = "id_Fund_Screening_Param_card",
            title = "Parameters",
            # footer = "Help",
            status = "primary",
            solidHeader = FALSE,
            headerBorder = TRUE,
            collapsible = FALSE,
            collapsed = FALSE,
            closable = FALSE,
            label = NULL,
            width = 3,
            height = "750px",
            fluidPage(style = "margin-left:5px; margin-right:5px;",
                     col_12( style = "margin-top:5px;",
                       selectInput(inputId = "Fund_Scr_region_country_id",
                                   label = "Region / Country (s)", 
                                   choices = unique(all_tickers$Country), 
                                   selected = "USA",
                                   multiple = TRUE
                       )
                     ),
                     # -------------- #
                     col_12(
                       selectInput(inputId = "Fund_Scr_market_exchange_id",
                                  label = "Market / Exchange (s)", 
                                  choices = unique(all_tickers$Exchange), 
                                  selected = c("NASDAQ", "NYSE"),
                                  multiple = TRUE
                                  )
                       ),
                     # -------------- #
                     col_12(
                       selectInput(inputId = "Fund_Scr_sectors_id",
                                   label = "Sectors", 
                                   choices = unique(all_tickers$Sector), 
                                   selected = unique(all_tickers$Sector),
                                   # options = list(`actions-box` = TRUE,
                                   #                `background-color` = "#ffffff"),
                                   selectize = F,
                                   multiple = TRUE)
                       
                     ),
                     # -------------- # # If filter or own select
                     col_12(
                       checkboxInput(inputId = "Fund_Scr_SelectiveSymbols_id", 
                                     label = "Selective Portfolio", value = FALSE)
                     ),
                     col_12(
                       conditionalPanel("input.Fund_Scr_SelectiveSymbols_id",
                                        selectInput(inputId = "Fund_Scr_symbols_id",
                                                    label = "Symbols", 
                                                    choices = unique(all_tickers$ticker), 
                                                    selected = unique(all_tickers$ticker)[1:4],
                                                    multiple = TRUE
                                                    )
                                        ),
                       conditionalPanel("!input.Fund_Scr_SelectiveSymbols_id",
                                        div(class = "container", 
                                            div(class="row",
                                            div(class = "col",
                                            numericInput(inputId = "Fund_Scr_MarketCap_id",
                                                         label = "Market Cap (mil)", 
                                                         value = 50, 
                                                         min = NA, max = NA,
                                                         step = 1)
                                          ),
                                        # -------------- #
                                        div(class = "col",
                                          numericInput(inputId = "Fund_Scr_NumSymb_id",
                                                       label = "# Tickers in Portfolio",
                                                       value = 10,
                                                       min = 1, max = 50,
                                                       step = 1)
                                        )
                                            ) # class row
                                        ) # main div
                                        ) # conditionalPanel !
                       ),
                     # -------------- # # Time range
                     col_12(
                       radioButtons(
                         inputId = "Fund_Scr_PeriodRange_id", 
                         label   = "Period (Months)",
                         choices = list("1M" = 1, 
                                        "3M" = 3, 
                                        "6M" = 6, 
                                        "12M" = 12, 
                                        "YTD" = 100, "Custom" = 200), 
                         selected = 3, inline = T )
                     ),
                     # -------------- #
                     col_12(
                       conditionalPanel("input.Fund_Scr_PeriodRange_id == 200",
                                        dateRangeInput(
                                          inputId = "Fund_Scr_CustomPeriodRange_id",
                                          label = "Range",
                                          start = Sys.Date() - 20,
                                          end = Sys.Date(),
                                          min = NULL,
                                          max = Sys.Date(),
                                          format = "yyyy-mm-dd",
                                          startview = "month",
                                          weekstart = 0,
                                          language = "en",
                                          separator = " to ",
                                          width = NULL,
                                          autoclose = TRUE
                                        )
                                        )
                     ),
                     # -------------- # # Benchmark
                     col_12(
                       selectInput(
                         inputId = "Fund_Scr_BenchmarkSymbol_id", 
                         label = "Benchmark",
                         choices = c("SP500", "Nasdaq100", "DAX", "None"),
                         selected = "SP500")
                       ),
                     # -------------- #
                     col_12(align= "center",
                       actionButton(inputId = "Fund_Scr_FilterStrategy_btn_id", 
                                    label = "Strategy / Filter", 
                                    icon = shiny::icon(configs$icons$chess, verify_fa = FALSE)
                                    )
                     ),
                     col_12(
                       br(),br(), br()
                     )
                     # -------------- #
                     ) # main fluidRow all
            ),
          
         # 
         bs4Card(
           id = "id_Fund_Screening_ResultView_card",
           title = "Portfolio",
           # footer = "Help",
           status = "primary",
           solidHeader = FALSE,
           headerBorder = TRUE,
           collapsible = FALSE,
           collapsed = FALSE,
           closable = FALSE,
           label = NULL,
           width = 9,
           height = "750px",
           fluidRow(
             col_12(
               bs4Dash::tabsetPanel(
                 id = "Fund_Scr_mainbody_tabcard",
                 # type = "pills", 
                 type = "tabs",
                 selected = "Portfolio",
                 
                 shiny::tabPanel(
                   title = "Portfolio",
                   icon = icon("filter", verify_fa = FALSE),
                   uiOutput("Fund_Scr_mainbody_portfolio_outui_id")
                 ),
                 
                 # shiny::tabPanel(
                 #   title = "Stats",
                 #   icon = icon("check", verify_fa = FALSE),
                 #   uiOutput("Fund_Scr_mainbody_stats_outui_id")
                 # ),
                 shiny::tabPanel(
                   title = "Risk/Return",
                   icon = icon(configs$icons$chart_line, verify_fa = FALSE),
                   uiOutput("Fund_Scr_mainbody_PortfolioAnalysis_outui_id")
                 ),
                 shiny::tabPanel(
                   title = "Backtesting",
                   icon = icon("infinity", verify_fa = FALSE),
                   uiOutput("Fund_Scr_mainbody_backtest_outui_id")
                 ),
                 shiny::tabPanel(
                   title = "...",
                   icon = icon("layer-group", verify_fa = FALSE),
                   #uiOutput(ns("outui_ts_4_id"))
                 )
               ) # tabsetPanel
           ) # col 12
             ) # fluidRow
           ) # bs4Card view
         
        ) # tagList
      ) # bs4Card 
   
  ) # fluidRow
  })
  
} # Fund_Screening_ui_func
