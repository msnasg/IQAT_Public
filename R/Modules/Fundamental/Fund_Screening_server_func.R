
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


Fund_Screening_server_func <- function(input, output, session){
  
  
  output$Fund_Scr_CriteriaSetting_id_uiout <- renderUI({
    
    specs <- filtering_factors[which(filtering_factors$factor %in% input$Fund_Scr_filteringfactors_id ),]
    
    # print(specs)
    
   tmp <<- lapply(1:nrow(specs), function(x){
      if(specs$ui[x] == "numeric" && specs$limit[x] == "range"){
         col_4(
          numericInput(inputId = paste0("num_range_", gsub(" ", "", specs$factor[x]) ),
            label = specs$factor[x],
            value = specs$value[x],  min = specs$min[x],  max = specs$max[x],
            step = 1)
        ) 
      }
    })
   
   # ---- #
   fluidRow(tmp)
   })
  
  # -------------------------------------------------------------------------- #
  # Fund_Scr_FilterStrategy_btn_id
  # -------------------------------------------------------------------------- #
  observeEvent(input$Fund_Scr_FilterStrategy_btn_id,{
    
    showModal(modalDialog(
      title = "Create your Strategy / Filters",
      size = "xl",
      easyClose = T,
      footer = actionButton(inputId = "Fund_Scr_View_btn_id", 
                            label = "View Portfolio",
                            icon = shiny::icon(configs$icons$list_check, verify_fa = FALSE)
                            ),
      fluidRow(
        # -------------- #
        col_12(
          selectInput(inputId = "Fund_Scr_filteringfactors_id",
                      label = "Filtering Factors", 
                      choices = filtering_factors$factor, 
                      selected = c("MarketCapitalization","ReturnOnAssetsTTM","PEGRatio", "Beta", "ReturnOnAssetsTTM"),
                      selectize = F,
                      multiple = TRUE  )
        ),
        # -------------- # conditionalPanel 
        col_12(
          uiOutput("Fund_Scr_CriteriaSetting_id_uiout")
        ),
        col_12(
          br(),br(),br(),br(),br(),br(),br(),br()
        )
      ) # main fluidRow
    )) # showModal 
    
  }) # observeEvent Fund_Scr_FilterStrategy_btn_id
  
  # -------------------------------------------------------------------------- #
  # renderUI Fund_Scr_mainbody_portfolio_outui_id
  # -------------------------------------------------------------------------- #
  output$Fund_Scr_mainbody_portfolio_outui_id <- renderUI({
    
    fluidRow(style = "margin-top:15px; margin-left:5px; margin-right:5px;",
             col_12(align = "right",
               actionButton(inputId = "Fund_Scr_PortfolioAnalysis_btn_id", 
                            label = "Analysis", 
                            icon = shiny::icon(configs$icons$chart_line, verify_fa = FALSE),
                            style = 'font-size:90%'
               ),
               actionButton(inputId = "Fund_Scr_PortfolioBacktesting_btn_id", 
                            label = "Backtesting", 
                            icon = shiny::icon(configs$icons$chart_pie, verify_fa = FALSE),
                            style = 'font-size:90%'
               )
             ),
             col_12(
               br(),
               DTOutput("Fund_Scr_mainbody_portfolio_dttbl_id")
               # formattableOutput("Fund_Scr_mainbody_portfolio_dttbl_id")
               ),
             col_12(
               br(),br()
               )
             ) # fluidRow
  }) # renderUI Fund_Scr_mainbody_portfolio_outui_id
  
  # -------------------------------------------------------------------------- #
  # Fund_Scr_View_btn_id Show portfolio table in tab portfolio
  # -------------------------------------------------------------------------- #
  Porfolio_Tbl <<- reactiveValues(df_Funda = NULL, PriceL = NULL)
  
  observeEvent(input$Fund_Scr_View_btn_id,{
    
    removeModal()
    
    output$Fund_Scr_mainbody_portfolio_dttbl_id <- renderDT({
      req(input$Fund_Scr_filteringfactors_id)
      req(input$Fund_Scr_View_btn_id)
      
      Porfolio_Tbl$df_Funda <- Query_AlphaVantage_func(SelectiveColumns = input$Fund_Scr_filteringfactors_id)
      
      datatable(Porfolio_Tbl$df_Funda,
                options = list(scrollX = TRUE, scrollX = '100%'), escape = FALSE)
    })
    
  })
  
  # -------------------------------------------------------------------------- #
  # Fund_Scr_mainbody_PortfolioAnalysis_outui_id 
  # -------------------------------------------------------------------------- #
  output$Fund_Scr_mainbody_PortfolioAnalysis_outui_id <- renderUI({
    fluidRow(
      style = "margin-top:10px; margin-left:10px;margin-right:10px;",
     col_12(align = "right",
           actionButton(inputId = "Fund_Scr_PortfolioAnalysisTab_Chart_btn_id", 
                        label = "Price Chart", 
                        icon = shiny::icon(configs$icons$chart_line, verify_fa = FALSE),
                        style = 'font-size:90%'),
           
           actionButton(inputId = "Fund_Scr_PortfolioAnalysisTab_Returns_btn_id", 
                        label = "Returns", 
                        icon = shiny::icon(configs$icons$chart_area, verify_fa = FALSE),
                        style = 'font-size:90%'),
           actionButton(inputId = "Fund_Scr_PortfolioAnalysisTab_Risk_btn_id", 
                        label = "Risk", 
                        icon = shiny::icon(configs$icons$chart_area, verify_fa = FALSE),
                        style = 'font-size:90%'),
           actionButton(inputId = "Fund_Scr_PortfolioAnalysisTab_MarketLine_btn_id", 
                        label = "Market Line", 
                        icon = shiny::icon(configs$icons$chart_line, verify_fa = FALSE),
                        style = 'font-size:90%'),
           br()
     ),
     col_12(
      uiOutput("Fund_Scr_PortfolioAnalysis_Tabs_outui_id")
     )
    )
  }) # renderUI Fund_Scr_mainbody_PortfolioAnalysis_outui_id
  

  observeEvent(input$Fund_Scr_PortfolioAnalysisTab_Chart_btn_id, {
    
    output$Fund_Scr_PortfolioAnalysis_Tabs_outui_id <- renderUI({
      plotOutput("Fund_Scr_PortfolioAnalysis_priceChart_plt_id", height = "600px")
    })
    
    if(!is.null(Porfolio_Tbl$PriceL) && length(Porfolio_Tbl$PriceL) >= 1){
      
      df <- Porfolio_Tbl$PriceL$Prices_df
      # df <- tmpPrices_L$Prices_df
      df <- df[which(df$Symbol %in% input$Fund_Scr_symbols_id ), ]
      
      output$Fund_Scr_PortfolioAnalysis_priceChart_plt_id <- renderPlot({
        req(input$Fund_Scr_PortfolioAnalysisTab_Chart_btn_id)
        
        df %>% ggplot(aes(x = Date, y = Adjusted, color = Symbol)) +
          geom_line() + facet_wrap(~Symbol, scales = 'free_y') +
          theme_classic() +
          labs(x = 'Date',
               y = "Adjusted Price", title = "Price Chart") +
          scale_x_date(date_breaks = "month",
                       date_labels = "%b\n%y")
        
      })
    }
  })
  
  # -------------------------------------------------------------------------- #
  # PortfolioAnalysisTab Returns
  # -------------------------------------------------------------------------- #
  observeEvent(input$Fund_Scr_PortfolioAnalysisTab_Returns_btn_id, {
    
    output$Fund_Scr_PortfolioAnalysis_Tabs_outui_id <- renderUI({
      plotOutput("Fund_Scr_PortfolioAnalysis_RRChart_plt_id", height = "600px")
    })
    
    if(!is.null(Porfolio_Tbl$PriceL) && length(Porfolio_Tbl$PriceL) >= 1){
      df <- Porfolio_Tbl$PriceL$Return_df
      df <- df[which(df$Symbol %in% input$Fund_Scr_symbols_id ), ]
      
      output$Fund_Scr_PortfolioAnalysis_RRChart_plt_id <- renderPlot({
        req(input$Fund_Scr_PortfolioAnalysisTab_Returns_btn_id)
        ggplot(df, aes(Date, Return, color = Symbol)) + geom_path(stat = "identity") +
          facet_grid(Symbol ~ .) + theme_minimal() + labs(x = "Date", y = "Returns")
        })
    } # if
  })
  
  # -------------------------------------------------------------------------- #
  # PortfolioAnalysisTab MarketLine
  # -------------------------------------------------------------------------- #
  observeEvent(input$Fund_Scr_PortfolioAnalysisTab_MarketLine_btn_id,{

    output$Fund_Scr_PortfolioAnalysis_Tabs_outui_id <- renderUI({
      plotOutput("Fund_Scr_PortfolioAnalysis_MarketLineChart_plt_id", height = "600px")
    })
    
    tmpL <- Funda_Charts_func(dfL = Porfolio_Tbl, charttype = "ReturnBeta")
    
    if(!is.null(tmpL) && length(tmpL) >= 1){
      
      output$Fund_Scr_PortfolioAnalysis_MarketLineChart_plt_id <- renderPlot({
        req(input$Fund_Scr_PortfolioAnalysisTab_MarketLine_btn_id)
        tmpL$gplt
      })
    } # if not NULL
    
  }) # Fund_Scr_PortfolioAnalysisTab_MarketLine_btn_id
  # -------------------------------------------------------------------------- #
  # Fund_Scr_PortfolioAnalysis_btn_id 
  # -------------------------------------------------------------------------- #
  observeEvent(input$Fund_Scr_PortfolioAnalysis_btn_id,{
  
    if(!is.null(Porfolio_Tbl$df_Funda) && nrow(Porfolio_Tbl$df_Funda) >= 1){
      tmpsymbols = Porfolio_Tbl$df_Funda$Symbol
      
      Porfolio_Tbl$PriceL <- GetTSdata_YahooFinance_func(symbols = tmpsymbols)
      tmpPrices_L <<- Porfolio_Tbl$PriceL
      updateTabsetPanel(session, "Fund_Scr_mainbody_tabcard", selected = "Risk/Return")
      
      # here to show outputs
      
    } # if 
    
  }) # Fund_Scr_PortfolioAnalysis_btn_id
  
  
  # -------------------------------------------------------------------------- #
  # Fund_Scr_PortfolioBacktesting_btn_id 
  # -------------------------------------------------------------------------- #
  observeEvent(input$Fund_Scr_PortfolioBacktesting_btn_id,{
    
    updateTabsetPanel(session, "Fund_Scr_mainbody_tabcard", selected = "Backtesting")
    
  }) # Fund_Scr_PortfolioBacktesting_btn_id
  
} # Fund_Screening_server_func