
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


#' @import shiny
app_server <- function(input, output, session) {
  
  # ns <- session$ns
  enc <- getOption("Encoding", "UTF-8")
  

  # -------------------------------------------------------------------------- #
  ## login
  # -------------------------------------------------------------------------- #
  result_auth <<- secure_server(check_credentials = check_credentials(Authorization_Table))
  
  output$userID <- renderUI({
    div(
      shiny::icon(configs$icons$user_check, verify_fa = FALSE),
      paste0(reactiveValuesToList(result_auth)$name)
    )
  })
  
  AdminRole <<- reactive({
    reactiveValuesToList(result_auth)$admin
  })
  # observe({ cat(AdminRole()) })
  
  # -------------------------------------------------------------------------- #
  # directory path in main header
  # -------------------------------------------------------------------------- #
  output$id_DirPath <- renderUI({
    
    dirname = base::switch(input$current_maintab,
                           "maintab_id_Home" = "Home",
                           "id_Markets_Screener" = "Markets / Screener",
                           "id_Markets_EconomicCalendar" = "Markets / Economic Calendar",
                           "id_Markets_Indices" = "Markets / Indices",
                           "id_Markets_Stocks" = "Markets / Stocks",
                           "id_Markets_Forex" = "Markets / Forex",
                           "id_Markets_Crypto" = "Markets / Crypto Currencies",
                           "id_Markets_Commodities" = "Markets / Commodities",
                           "id_Markets_Bonds" = "Markets / Bonds",
                           "id_Markets_Funds" = "Markets / Funds",
                           "id_Markets_WorldEconomics" = "Markets / World Economics",
                           "id_Markets_Calculators" = "Markets / Calculators",
                           "id_DataSets_FRED_tab" = "Data Sets / Federal Reserve Economic Data (FRED)",
                           "id_DataSets_WDI_tab" = "Data Sets / World Development Indicators (WDI)",
                           "id_Fundamental_Screening" = "Fundamental / Screening",
                           "id_Analytics_TA_Chart" = "Technical Analysis / Chart",
                           "id_Analytics_TA_Tools" = "Technical Analysis / Patterns",
                           "id_Analytics_TA_Tools2" = "Technical Analysis / Signals",
                           "id_Analytics_Social" = "Sentimental Analysis / Social Media",
                           "id_Analytics_News" = "Sentimental Analysis / News",
                           "id_AssetPricing_Valuation" = "Asset Pricing / Valuation",
                           "id_TSModels_Regression" = "Time Series Models / Regression",
                           "id_TSModels_ARMA" = "Time Series Models / AR & MA",
                           "id_TSModels_VARVECM" = "Time Series Models / VAR & VECM",
                           "id_TSModels_Stochastics" = "Time Series Models / Stochastic Models",
                           "id_TradingModels_PairFinder" = "Trading Models / Pair Trade Finder",
                           "id_TradingModels_Pair" = "Trading Models / Pair Trading",
                           "id_TradingModels_Algo" = "Trading Models / Algorithmic Trading",
                           "id_TradingModels_MLDL" = "Trading Models / Machine & Deep Learning",
                           # Risk
                           "id_InterestRate_Risk" = "Risk Management / Interest Rate Risk",
                           "id_Risk_Market" = "Risk Management / Market Risk",
                           "id_Risk_Credit" = "Risk Management / Credit Risk", 
                           # "maintab_id_Portfolio" = "Portfolio Management",
                           "id_Portfolio_MPT" = "Portfolio Management / Modern Portfolio Theory (MPT)",
                           "id_Portfolio_OLPS" = "Portfolio Management / Online Portfolio Selection",
                           "id_Portfolio_Hedging" = "Portfolio Management / Hedging",
                           # Trading System
                           "id_TradingSys_MoneyMang" = "Trading System / Money Management",
                           "id_TradingSys_Volume" = "Trading System / Trade Volume",
                           "id_TradingSys_Strategy" = "Trading System / Strategy",
                           "id_TradingSys_BackTest" = "Trading System / Back & Forward Testing",
                           "maintab_id_Forex" = "Forex",
                           "maintab_id_Cryptos" = "Cryptocurrency",
                           # Personal Finance
                           "id_PersonalFinance_Retirement" = "Personal Finance / Retirement Planning",
                           "id_PersonalFinance_RealEstate" = "Personal Finance / Real Estate"
                           ) 
    

    div(style = "margin-top: 4px; color: #fff; font-weight: bold; font-size: 5px;",
      h4(paste0(dirname))
    )
  })
  
  
  
  # -------------------------------------------------------------------------- #
  # Home tab & About
  # -------------------------------------------------------------------------- #

  # output$HomeTab_ui <- renderUI({
  #   HomeTab_ui_server_func(input, output, session)
  # })
  
  callModule(mod_welcome_server, "welcome_ui_1")
  callModule(mod_about_server, "about_ui_1")
  
  # -------------------------------------------------------------------------- #
  # Mrkets main tab
  # -------------------------------------------------------------------------- #
  
  output$MarketNews_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "MarketNews")
  })
  
  output$Screener_Dash_UI <- renderUI({
    Screener_Dash_Func(input, output, session)
  })
  
  output$Top5_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "Top5")
  })
  
  output$SymbolInfo_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "SymbolInfo")
  })
  
  output$MarketNews_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "MarketNews")
  })
  
  output$ChartOnly_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "ChartOnly")
  })
  
  output$Fundamental_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "Fundamental")
  })
  
  output$SymbolNews_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "SymbolNews")
  })
  
  output$CompanyProfile_Stocks_Dash_UI <- renderUI({
    Stocks_Dash_Func(input, output, session, type = "CompanyProfile")
  })
  
  output$Calculators_Dash_UI <- renderUI({
    Calculators_Dash_Func(input, output, session, type = "")
  })
  
  # -------------------------------------------------------------------------- #
  ## Fundamental main Tab
  # -------------------------------------------------------------------------- #
  
  output$id_Fundamental_Screening_ui <- renderUI({
    Fund_Screening_ui_func(input, output, session)
  })

  Fund_Screening_server_func(input, output, session)

  # -------------------------------------------------------------------------- #
  ## DataSets 
  # -------------------------------------------------------------------------- #
  
  output$DataSets_FRED_UI <- renderUI({
    # WorldEconomic_Dash_Func(input, output, session)
  })
  
  output$DataSets_WDI_UI <- renderUI({
    WorldEconomic_Dash_Func(input, output, session)
  })
  
  # -------------------------------------------------------------------------- #
  ## Analytics_Sentiment_TwitterExplorer_UI
  # -------------------------------------------------------------------------- #
  
  observeEvent(input$id_Analytics_Sentiment_Twitter_btn,{
    output$Analytics_Sentiment_UI <- renderUI({
      Func_Twitter(input, output, session)
    })
  })
  
  # -------------------------------------------------------------------------- #
  ## Analytics_TA_Tools_UI
  # -------------------------------------------------------------------------- #
  
  observeEvent(input$id_Analytics_TA_Tools_Patterns_btn,{
    
    output$Analytics_TA_Tools_UI <- renderUI({
        Analytics_Technical_Tools(input, output, session)
    })
  }) # observeEvent input$id_Analytics_TA_Tools_Patterns_btn
  
  # -------------------------------------------------------------------------- #
  ## Time Series main tab  / Regression
  # -------------------------------------------------------------------------- #
  ts_regress_ui("id_ts_reg")
  ts_regress_server("id_ts_reg")
  
  # -------------------------------------------------------------------------- #
  ## Pair Finder main tab
  # -------------------------------------------------------------------------- #
  PairFind_Excel <<- reactiveValues(Tbl = NULL)
  
  options(shiny.maxRequestSize=30*1024^2)
  
  output$id_TradingModels_PairFinder_DataTab_DTable <- DT::renderDT({
    
    inFile <- input$id_TradingModels_PairFinder_Excel
    
    if(is.null(inFile)){return ()  }else{
      
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      
      PairFind_Excel$Tbl <<- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      PairFinder_Excel_Smry <<- PairFinder_Excel_Smry_Func(df = PairFind_Excel$Tbl)
        
      # update Symbols in Main params
      Symbs = unique(PairFinder_Excel_Smry[,"Symbol"])
      
      updateSelectInput(session, "id_TradingModels_PairFinder_DataTab_Symbols_selectInput",
                        choices = Symbs,
                        selected = Symbs[1]
      )
      
      Func_DT_Table(df = PairFinder_Excel_Smry, 
                     pageLength_Set = 20,
                     scrollY = '35vmax', 
                     type = "B",
                     info = TRUE, 
                     filter = "none",
                     fontSize = '12px')
      } # if else
    }) # output$id_TradingModels_PairFinder_DataTab_DTable <- DT::renderDT


  # -------------------------------------------------------------------------- #
  # Main Run btn
  # -------------------------------------------------------------------------- #

  
  observeEvent(input$id_TradingModels_PairFinder_Run_btn, {
    
    if(!is.null(PairFind_Excel$Tbl) && nrow(PairFinder_Excel_Smry) >= 1){
      
      showNotification(ui = "Imputation takes a while.", action = NULL, duration = 20, closeButton = TRUE,
                       id = NULL, type = "message", # c("default", "message", "warning", "error")
                       )

      tmpdf <<- PairFind_Excel$Tbl
      
      MissingImputed_list <<- Imputation_MissingValues_Func(df = PairFind_Excel$Tbl, 
                                                            ret_method = input$id_TradingModels_PairFinder_Return_selectInput,
                                                            type = input$id_TradingModels_PairFinder_MissingDataModel_selectInput)
      
      shiny::updateTabsetPanel(
        session, 
        inputId = "id_TradingModels_PairFinder_tabsetPanel", 
        selected = "Stats"
      )
      
      # Run Stats Tab
      if(exists("MissingImputed_list") && length(MissingImputed_list) >= 1){
       
        PairFinder_Stats_Func(input, output, session,
                              L = MissingImputed_list,
                              Symb = input$id_TradingModels_PairFinder_DataTab_Symbols_selectInput)
      } # if
      
    }else{
      
      Func_ModalDialog_Msg(input, output, session, 
                           typebox = "modal",
                           title = "Error", 
                           ui = "First upload excel input file.",
                           type = "default" )
    }
  }) # observeEvent input$id_TradingModels_PairFinder_Run_btn

  # -------------------------------------------------------------------------- #

  # Stats UI and Func
  observeEvent(input$id_TradingModels_PairFinder_DataTab_Symbols_selectInput, {
    
    Symb <- input$id_TradingModels_PairFinder_DataTab_Symbols_selectInput
    # Run Stats tab
    if(exists("MissingImputed_list") && length(MissingImputed_list) >= 1 && 
       !is.null(Symb) && Symb != "NA"){
      
      PairFinder_Stats_Func(input, output, session,
                            L = MissingImputed_list,
                            Symb = input$id_TradingModels_PairFinder_DataTab_Symbols_selectInput)
    }
      
  }) # observe
  
  

  # -------------------------------------------------------------------------- #
  # Pair Trading update symbols based on Market 1, 2
  # -------------------------------------------------------------------------- #
  
  observeEvent(input$id_TradingModels_Pair_Market_selectInput_1, { 
    
    all_symbols <- unique(configs_xls$symbol[which(configs_xls$market == 
                                                     input$id_TradingModels_Pair_Market_selectInput_1)])
    
    updateSelectInput(session, "id_TradingModels_Pair_Symbol_selectInput_1",
                      choices = all_symbols,
                      selected = all_symbols[1] )
  })
  
  observeEvent(input$id_TradingModels_Pair_Market_selectInput_2, { 
    
    all_symbols <- unique(configs_xls$symbol[which(configs_xls$market == 
                                                     input$id_TradingModels_Pair_Market_selectInput_2)])
    
    updateSelectInput(session, "id_TradingModels_Pair_Symbol_selectInput_2",
                      choices = all_symbols,
                      selected = all_symbols[2] )
  })
  
  # -------------------------------------------------------------------------- #
  # Pair Trading Result UI
  # -------------------------------------------------------------------------- #
  observeEvent(input$id_TradingModels_Pair_run_btn,{

    output$id_TradingModels_Pair_UI <- renderUI({
      
      Pair_Trading_Basic_Model_Func(input, output, session)
   
    })
  }) # observeEvent input$id_TradingModels_Pair_run_btn


  #--------------------------------------------------------------------------- #
  # Contact
  #--------------------------------------------------------------------------- #
  observeEvent(input$ContactUs, {
    showModal(modalDialog(
      title = "Contact Us!",
      div(style = "display:flex;",
          div(style = "margin-left:25px; flex-flow: row wrap;",
              div(style = "margin-left:1px; margin-top:1px;",
                  HTML(paste(
                    "<ul><li> ", icon(configs$icons$people_group, verify_fa = FALSE),
                    tags$b("Developed by: "), "Mohsen Asgari", " </li></ul>"
                  ))
              ),

              div(style = "margin-left:1px; margin-top:10px;",
                  HTML(paste(
                    "<ul><li> ", icon(configs$icons$atsign, verify_fa = FALSE),
                    tags$a(
                      href = "mailto: ....", 
                      "...",
                      class = "externallink",
                      style = "color: blue; text-decoration: none"
                    ),
                    " </li></ul>"
                  ))
              ),
              div(style = "margin-left:1px; margin-top:10px;",
                  HTML(paste(
                    "<ul><li> ", icon(configs$icons$headset, verify_fa = FALSE),
                    tags$a("+49 ...."), " </li></ul>"
                  ))
              )
          )
      ),
      easyClose = TRUE,
      footer = "Hannover / Germany"
    ))
  })
} # app_server
