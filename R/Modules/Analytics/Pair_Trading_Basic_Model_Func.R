
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


#' Pair_Trading_Basic_Model_Func
#' 
#' 
#' @param Shinyinputs c(input, output, session)
#' @return 
#' @export
#' @examples
#'  
Pair_Trading_Basic_Model_Func <- function(input, output, session, ...){
  # --- params

  # -------------------------------------------------------------------------- #
  # UI
  # -------------------------------------------------------------------------- #
  output$id_TradingModels_Pair_UI <- renderUI({
    
    fluidRow(
      column(6,
             shinycustomloader::withLoader(plotOutput('Pair_Symb1_plt', height = "600px", width = "100%"))
      ),
      column(6,
             shinycustomloader::withLoader(plotOutput('Pair_Symb2_plt', height = "600px", width = "100%"))
      ),
      column(6,
             column(12, br(), br()),
             shinycustomloader::withLoader(plotOutput('Pair_Box_Corr_plt', height = "300px", width = "100%"))
      ),
      column(6,
             shinycustomloader::withLoader(plotOutput('Pair_pltRCum_plt', height = "300px", width = "100%"))
      ),
      column(6,
             shinycustomloader::withLoader(plotOutput('Pair_violin_plt', height = "400px", width = "100%"))
      ),
      column(6,
             shinycustomloader::withLoader(plotOutput('Pair_density_plt', height = "400px", width = "100%"))
      ),
      column(6,
             shinycustomloader::withLoader(plotOutput('Pair_CumRet_plt', height = "400px", width = "100%"))
      ),
      column(6, 
             shinycustomloader::withLoader(plotOutput('Pair_Trade_plt', height = "400px", width = "100%"))
      ),
      column(6,
             shinycustomloader::withLoader(plotOutput('Pair_Spread_plt', height = "400px", width = "100%"))
      ),
      column(6,
             shinycustomloader::withLoader(DTOutput('Pair_TotalTable'))
      )
    ) # fluidRow
    
  }) 
  
  # -------------------------------------------------------------------------- #
  # Server
  # -------------------------------------------------------------------------- #
  
  
  quantstrat_Func <- eventReactive(input$id_TradingModels_Pair_run_btn,{
    
    PairTradingStrategy_quantstrat_Func(Symb_1 = input$id_TradingModels_Pair_Symbol_selectInput_1, 
                                   Symb_2 = input$id_TradingModels_Pair_Symbol_selectInput_2, 
                                   TF = input$id_TradingModels_Pair_Timeframe, 
                                   Windw = input$id_TradingModels_Pair_Window, 
                                   fromdate = input$id_TradingModels_Pair_Date_from,
                                   InitCap = input$id_TradingModels_Pair_capital,
                                   MaxPos = input$id_TradingModels_Pair_MaxPos,
                                   Model = input$id_TradingModels_Pair_Model_selectInput,
                                   Currency = "USD") 
    }) 
  
  # PairFinder_Func <- eventReactive(input$id_TradingModels_Pair_run_btn,{
  #   PairFinder2(dat = RefTbl, 
  #               Sy1 = input$Stock_PairBT_A, 
  #               Sy2 = input$Stock_PairBT_B, 
  #               TF = input$Timeframe_PairBT, 
  #               Windw = input$Window_PairBT, 
  #               ExclID=ExclID)
  # })
  
  
  quantstrat_Func_res <<- quantstrat_Func()
  # -------------------------------------------------------------------------- #
  # Output Render
  # -------------------------------------------------------------------------- #
  
  # -------------------------------------------- #
  output$Pair_TotalTable <- renderDT({
    Func_DT_Table (quantstrat_Func_res$TotalTable, 
                   pageLength_Set = 20,
                   scrollY = '15vmax', 
                   type = "B",
                   info = F, 
                   filter = "none",
                   fontSize = "10px")
  }) # Pair_TotalTable
  
  # -------------------------------------------- #
  output$Pair_Box_Corr_plt <- renderPlot({
    
    gridExtra::grid.arrange(quantstrat_Func_res$boxplt, 
                 quantstrat_Func_res$corrplt , 
                 ncol = 2, 
                 top = NULL, 
                 heights = c(3,2))
  }) # Pair_Box_Corr_plt
  
  # -------------------------------------------- #
  output$Pair_Symb1_plt <- renderPlot({
    chart.Posn(Portfolio = quantstrat_Func_res$portfolio1.st, 
               Symbol = input$id_TradingModels_Pair_Symbol_selectInput_1)
  }) # Pair_Symb1_plt
  
  # -------------------------------------------- #
  output$Pair_Symb2_plt <- renderPlot({
    chart.Posn(Portfolio = quantstrat_Func_res$portfolio1.st, 
               Symbol = input$id_TradingModels_Pair_Symbol_selectInput_2)
  }) # Pair_Symb2_plt
  
  # -------------------------------------------- #
  output$Pair_pltRCum_plt <- renderPlot({
    quantstrat_Func_res$pltRCum
  }) # Pair_pltRCum_plt
 
  # -------------------------------------------- #
  output$Pair_violin_plt <- renderPlot({
    quantstrat_Func_res$violinplt
  }) # Pair_violin_plt
  
  # -------------------------------------------- #
  output$Pair_density_plt <- renderPlot({
    quantstrat_Func_res$densityplt
  }) # Pair_density_plt
  
  # -------------------------------------------- #
  output$Pair_CumRet_plt <- renderPlot({
    quantstrat_Func_res$P_Cum_Ret
  }) # Pair_CumRet_plt
  
  # -------------------------------------------- #
  output$Pair_Trade_plt <- renderPlot({
    quantstrat_Func_res$Tradeplt
  }) # Pair_Trade_plt
  
  # -------------------------------------------- #
  output$Pair_Spread_plt <- renderPlot({
    quantstrat_Func_res$Spreadplt
  }) # Pair_Spread_plt
  

  # -------------------------------------------------------------------------- #
  # End
  # -------------------------------------------------------------------------- #
  
} # Pair_Trading_Basic_Model_Func
