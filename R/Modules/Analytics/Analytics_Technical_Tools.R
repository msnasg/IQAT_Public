
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.




#' Analytics_Technical_Tools
#' 
#' 
#' @param Shinyinputs c(input, output, session)
#' @return 
#' @export
#' @examples 
Analytics_Technical_Tools <- function(input, output, session, 
                                      ...){
  # --- params

  all_markets <- unique(configs_xls$market)

  # --- UI
  
  output$Analytics_TA_Tools_UI <- renderUI({
   
     fluidRow(
       tagList(
    col_2(style = paste0("font-size:",configs$font_size$general, "px;"),
          bs4Card(
            id = "id_Analytics_TA_Tools_Param_card",
            title = "Parameters",
            # footer = "Help",
            status = "primary",
            solidHeader = FALSE,
            headerBorder = TRUE,
            collapsible = FALSE,
            collapsed = FALSE,
            closable = FALSE,
            label = NULL,
            width = 12,
            height = "800px",
            # ------------------ #
            tagList(
              br(),
              col_12(
                selectInput("id_Analytics_TA_Tools_Patterns_Market_selectInput",
                          label = "Market",
                          choices = all_markets,
                          selected = all_markets[1])
              ),
              col_12(
                selectInput("id_Analytics_TA_Tools_Patterns_Symbol_selectInput",
                          label = "Symbol",
                          choices = NULL, # reactive later
                          selected = NULL)
              ),
              col_12(
                selectInput("id_Analytics_TA_Tools_Patterns_Timeframe", 
                                  label = "Time Frame", choices = c("D","W","M","M3","M6","Y"),
                            selected = "D"  )
              ),
              col_12(
                selectInput("id_Analytics_TA_Tools_Patterns_Window", 
                            label = "Window", choices = c("All", seq(as.numeric(format(Sys.Date(), "%Y")), 2010)), 
                            selected = format(Sys.Date(), "%Y") )
              ),
              col_12(
                dateInput("id_Analytics_TA_Tools_Patterns_Date_from", "From Date", 
                          value = Sys.Date()- 500),
                  tags$style(
                    paste0(".form-control {font-size: ", configs$font_size$general, "px !important;}")
                  )
              ),
              col_12(
                numericInput("id_Analytics_TA_Tools_Patterns_Last_Candles",
                             label = "Last Candles", value = 100, min = 20, max = NA, step = 1 )
              ),
              br(),
              col_12(align="center",
                actionButton(inputId = "id_Analytics_TA_Tools_Patterns_run_btn", 
                             label = "Run", width = "120px",
                             icon = shiny::icon(configs$icons$searchengin, verify_fa = FALSE)),
                        tippy_this("id_Analytics_TA_Tools_Patterns_run_btn", "Run")
                )
              # -------
            ) # tagList
          ) # bs4Card
    ), # col_2
  # -------------------------------------------------------------------- #
  col_10(style = paste0("font-size:", configs$font_size$general, "px;"),
      bs4Card(
        id = "id_Analytics_TA_Tools_Patterns_Results_card",
        title = "Technical Patterns",
        #footer = "Help",
        status = "primary",
        maximizable = TRUE,
        solidHeader = FALSE,
        headerBorder = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        closable = FALSE,
        label = NULL,
        width = 12,
        height = "800px",
        sidebar = boxSidebar(
          tagList(),
          id = "id_Analytics_TA_Tools_Patterns_Results_sidebar",
          width = 60,
          background = "#f9f9fa",
          startOpen = FALSE,
          icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
          easyClose = TRUE
        ),
        # ----- #
        bs4Dash::tabBox(
          id = "id_Analytics_TA_Tools_Patterns_Results_tabBox",
          title = NULL,
          width = 12,
          height = "100%",
          collapsible = FALSE,
          maximizable = TRUE,
          selected = "Summary",
          status = "primary",
          solidHeader = FALSE,
          type = "pills", # "tabs",
          side = "left",
          tabPanel("Summary",
                   col_12(
                     br(),
                     DTOutput("id_Analytics_TA_Tools_Patterns_Results_DTtbl", 
                              width = "100%")
                   )
          ),
          tabPanel("Channel",
                   plotOutput("id_Analytics_TA_Tools_Patterns_Results_plotOutput_Channel", 
                                width = "100%", height = "800px")
          ),
          tabPanel("Pattern",
                   col_12(
                     br(),
                    h5(textOutput("id_Analytics_TA_Tools_Patterns_Results_plotOutput_Pattern_Smrytxt"))
                   ),
                   col_12(
                   plotOutput("id_Analytics_TA_Tools_Patterns_Results_plotOutput_Pattern_1", 
                                width = "100%", height = "400px"),
                   plotOutput("id_Analytics_TA_Tools_Patterns_Results_plotOutput_Pattern_2", 
                              width = "100%", height = "400px")
                   )
          ),
          tabPanel("Res_Sup",
                   plotOutput("id_Analytics_TA_Tools_Patterns_Results_plotOutput_2", 
                              width = "100%", height = "600px")
          ),
          tabPanel("Ending",
                   plotlyOutput("id_Analytics_TA_Tools_Patterns_Results_plotOutput_1", 
                                width = "100%", height = "600px")
          )
        ) # tabBox
        
      ) # bs4Card
      ) # col_10
  ) # tagList
  ) # fluidrow
  
  }) # ui
  
  # -------------------------------------------------------------------------- #
  # --- SERVER
  # -------------------------------------------------------------------------- #
  # update symbols based on Market
  observeEvent(input$id_Analytics_TA_Tools_Patterns_Market_selectInput,{ 
    
    all_symbols <- unique(configs_xls$symbol[which(configs_xls$market == 
                                                     input$id_Analytics_TA_Tools_Patterns_Market_selectInput)])
    
    updateSelectInput(session, "id_Analytics_TA_Tools_Patterns_Symbol_selectInput",
                      choices = all_symbols,
                      selected = all_symbols[1] )
  })
  # -----------
  # -----------
  
  df_symbol <<- reactiveValues(df = NULL)
  list_symbol <<- reactiveValues(list = NULL)
  
  observeEvent(input$id_Analytics_TA_Tools_Patterns_run_btn ,{
    cat("A_")
    
    df_symbol$df <- Func_GetData(input$id_Analytics_TA_Tools_Patterns_Symbol_selectInput,
                                                   from = input$id_Analytics_TA_Tools_Patterns_Date_from)
    
    if(!is.null(df_symbol$df) && nrow(df_symbol$df) >= 1){
      colnames(df_symbol$df) <-  gsub(paste0(input$id_Analytics_TA_Tools_Patterns_Symbol_selectInput,
                                   "."), "", colnames(df_symbol$df),
                                   ignore.case = TRUE)
      
      df_OHLC <<- df_symbol$df
      
      list_symbol$list <- suppressWarnings({ Func_Market_Filters (dat = df_symbol$df,
                                  # Symbol = "EURUSD=X",
                                  Symbol = input$id_Analytics_TA_Tools_Patterns_Symbol_selectInput,
                                  Filter = c("Triangle", "MACD_Divergence"),
                                  TF = "D",
                                  Windw = input$id_Analytics_TA_Tools_Patterns_Window,
                                  LastPr0 = nrow(df_symbol$df),
                                  zgzg = 20) })
      
      
      tmp_list <<- list_symbol$list
      
      
    } else{
      df_symbol$df <- NULL
    }
  
    
  })
  
  # ---------------- #
  output$id_Analytics_TA_Tools_Patterns_Results_DTtbl <- renderDT({
    
    if(!is.null(df_symbol$df) ){ # && nrow(list_symbol$list$Table >= 1)
      
      tmp_df <<- Func_Get_patterns(ticker = input$id_Analytics_TA_Tools_Patterns_Symbol_selectInput,
                               from  = input$id_Analytics_TA_Tools_Patterns_Date_from,
                               lastcandles = input$id_Analytics_TA_Tools_Patterns_Last_Candles,
                               session)
      
      if(nrow(tmp_df) >= 1){
        Func_DT_Table (tmp_df, # list_symbol$list$Table,
                       pageLength_Set = 15,
                       scrollY = '35vmax',
                       type = "B",
                       info = TRUE,
                       filter = "none",
                       fontSize = "12px")
      }else{
        sendSweetAlert(
          session = session,
          title = "Window Error",
          text = "The date and the number of last candles do not match.",
          type = "error"
        )
      }
      
    }
  }) # DT table
  # -------------------------- #
  output$id_Analytics_TA_Tools_Patterns_Results_plotOutput_1 <- renderPlotly({
    
    if(!is.null(df_symbol$df) ){ # && !is.null(list_symbol$list)
    
     ggplotly(list_symbol$list$Plots[[1]])
      
    }
  }) # plot
  
  output$id_Analytics_TA_Tools_Patterns_Results_plotOutput_2 <- renderPlot({
    cat("10")
    if(!is.null(df_symbol$df)){
      #list_symbol$list$Plots[2]
      x <- df_symbol$df
      sups <- find.pivots(x, type = "FIB")
      #summary(sups)
      #sups <- find.pivots(x, type = "SR", strength = 5)
      #summary(sups)
      imppts <- find.imppoints(x,2)
      
      quantmod::chart_Series(x)
      
      if(length(sups$lines)>= 1){
        sapply(1:length(sups$lines), function(i){
          print(quantmod::add_TA(sups$lines[[i]],on = 1, lty = 2))
        })
        print(points(as.numeric(imppts$maxima$pos), as.numeric(imppts$maxima$value),
                     bg = "green", pch = 24,cex = 1.25))
        print(points(as.numeric(imppts$minima$pos), as.numeric(imppts$minima$value),
                     bg = "red", pch = 25, cex = 1.25))
      } # if
    }
  }) # plot
  
  # --------------------------- # Channel
  # "id_Analytics_TA_Tools_Patterns_Results_plotOutput_Channel"
  output$id_Analytics_TA_Tools_Patterns_Results_plotOutput_Channel <- renderPlot({
  
    
    if(!is.null(df_symbol$df)){
      x <- df_symbol$df
      
      x <- xts::last(x, input$id_Analytics_TA_Tools_Patterns_Last_Candles)
      
      tchannel <- find.trend.channel(x) 
      
      if(!is.na(tchannel$name)){
        # cat("5")
        print(quantmod::chart_Series(x))
        print(quantmod::add_TA(tchannel$xlines$maxlines[[1]], on=1, lty = 3, col = "brown"))
        print(quantmod::add_TA(tchannel$xlines$minlines[[1]], on=1, lty = 3, col = "brown"))
       
      } # if
    } # if
    }) #  output plot Channel
  
  # --------------------------- # Pattern
# "id_Analytics_TA_Tools_Patterns_Results_plotOutput_Pattern_1"
  output$id_Analytics_TA_Tools_Patterns_Results_plotOutput_Pattern_1 <- renderPlot({
    cat("3")
    if(!is.null(df_symbol$df)){
      # x = df_OHLC; x <- xts::last(x, 100)
      x <- df_symbol$df
      
      x <- xts::last(x, input$id_Analytics_TA_Tools_Patterns_Last_Candles)
      tpattern <- find.pattern(x)
      
      output$id_Analytics_TA_Tools_Patterns_Results_plotOutput_Pattern_Smrytxt <- renderText({
        paste("Pattern: ", tpattern$matches[[1]]$name) #, 
                  #  " (Duration: ", tpattern$matches[[1]]$duration, " Days)")
      })
      if(!is.na(tpattern$matches)){
        quantmod::chart_Series(x)
        quantmod::add_TA(tpattern$matches[[1]]$data,on=1,
                        col = adjustcolor("red",alpha.f = 0.5), lwd=5)
      } # if
    } # if
  }) #  output plot Pattern_1
  
  output$id_Analytics_TA_Tools_Patterns_Results_plotOutput_Pattern_2 <- renderPlot({
    cat("1")
     if(!is.null(df_symbol$df)){
  #     # ohlc = df_OHLC
       ohlc = df_symbol$df
       # quantmod::chart_Series(ohlc)
      
      Auto_TechPattern_Func_SIT(ohlc ,
                                ticker = input$id_Analytics_TA_Tools_Patterns_Symbol_selectInput,
                                TF = "D",
                                lastP = input$id_Analytics_TA_Tools_Patterns_Last_Candles)
     } # if
  }) #  output plot Pattern_2
  
  
  
} # Analytics_Technical_Tools



