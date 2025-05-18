

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


#------------------------------------------------------------------------------# 
## .... Shiny App (ui.R)
#------------------------------------------------------------------------------#
# Version: 0.0
# Date: 2022
# By: Mohsen Asgari
# Email: ....

#------------------------------------------------------------------------------#

app_ui <- shinymanager::secure_app(
  
  tags_top = tags$div(
    tags$head(tags$style(css_ui)),
    tags$h3("IQAT Tools", style = "align:center; color:#006729;") 
    ),
  
  tags_bottom = tags$div(
    tags$p("For any question, please contact",
           tags$a( href = "mail", target ="_top", "Admin")
    )
  ),
  
  language = "en", # "de"
  fab_position = "none", # "bottom-right",
  
  tagList(
    
  
    
     # golem_add_external_resources(),
     golem::activate_js(),
     useShinyjs(), # Include shinyjs
     
     add_busy_spinner(spin = "semipolar", position = c("bottom-left"), 
                      color = configs$colors$main_nvar),
     
     # autoWaiter(color = "white"),
     
     
     introjsUI(),   # Required to enable introjs scripts  introBox()
    
    bs4DashPage(
      title = "IQAT",
      
      # ---------------------------------------------------------------------- #
      # navigation bar
      header = bs4DashNavbar(
        
        fixed = FALSE,  # Whether to fix the navbar to the top. FALSE by default
        
        sidebarIcon = shiny::icon(configs$icons$bars, verify_fa = FALSE),
        
        controlbarIcon = shiny::icon(configs$icons$gear, verify_fa = FALSE),
        
        compact = TRUE, # Whether items should be compacted. FALSE by default.
        # -------------------------------------------------------------------- #
        title = tagList(# style = "text-align:center;",
          # tags$a(img(src = base64enc::dataURI(file = paste0(pathwd_dash, "/www/logo.jpg"), mime = "image/png"), 
          #                 height = 35, width = 55), style = "margin-top: -15px; margin-left: 1px;"),
          tags$h4("IQAT", style = paste0("color: ", configs$colors$main_nvar, "; 
          text-align: center;"))
          ),
        
        # -------------------------------------------------------------------- #
        # Right UI
        rightUi = tagList(
          dropdownMenu(
            icon = shiny::icon(configs$icons$user, verify_fa = FALSE),
            badgeStatus = "danger",
            type = "messages",
            headerText = "My Profile",
            messageItem(
              inputId = "Myprofile",
              icon = shiny::icon(configs$icons$user, verify_fa = FALSE),
              message = "1", #paste0(Authorization_Table[Authorization_Table$user == data.frame(Sys.info())["login",], "id"],
                          #    " (", Authorization_Table[Authorization_Table$user == data.frame(Sys.info())["login",], "role"], ")"),
              from = "2", #paste0(Authorization_Table[Authorization_Table$user == data.frame(Sys.info())["login",], "name"])  ,
              image = base64enc::dataURI(file = paste0(pathwd_dash, "/www/team_1.jpg"), mime = "image/png"),
              time = "today",
              color = "lime"
            )
          ),
          dropdownMenu(
            badgeStatus = "info",
            type = "notifications",
            icon = shiny::icon(configs$icons$bell, verify_fa = FALSE),
            notificationItem(
              inputId = "Info_Notif_1",
              icon = shiny::icon(configs$icons$exclamation,  verify_fa = FALSE),
              text = uiOutput("Info_Notif_text1") , # "Error!",
              status = "danger"
            ), 
            notificationItem(
              inputId = "Info_Notif_2",
              icon = shiny::icon(configs$icons$exclamation,  verify_fa = FALSE),
              text = uiOutput("Info_Notif_text2") , # "Error!",
              status = "danger"
            )
          ),
          dropdownMenu(
            badgeStatus = "info",
            type = "tasks",
            icon = shiny::icon(configs$icons$list_check, verify_fa = FALSE),
            taskItem(
              inputId = "triggerAction3",
              text = "My progress",
              color = "orange",
              value = 10
            )
          )
        ),
        # left UI
        leftUi = tagList(
          uiOutput("id_DirPath")
        ),
        
        skin = "light", # "dark",
        # to get all status :  getAdminLTEColors()
        status = "primary" 
      ),
     
      # right sidebar / controlbar
     
     controlbar =  dashboardControlbar(
       id = "id_controlbar",
       width = 250,
       collapsed = TRUE,
       overlay = TRUE,
       skin = "light",
       
       controlbarMenu(
         id = "menu",
         controlbarItem(
           icon = shiny::icon(configs$icons$gear, verify_fa = FALSE),
           paste0("Setting"),
           paste0("Welcome to Setting")
         ),
         controlbarItem(
           icon = shiny::icon(configs$icons$circle_info, verify_fa = FALSE),
           paste0("Help"),
           paste0("Welcome to Help")
         )
       )
     ),
     
     # ----------------------------------------------------------------------- #
     # left sidebar
     # ----------------------------------------------------------------------- # 
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "white",
        collapsed = FALSE, # TRUE,
        title = "Reporting",

        # ------------------------------------------------------------------ #
        bs4SidebarMenu(
          id = "current_maintab",
          
          div(style = "margin-top:30px",
           
          # ------------------------------------------------------------------ #
          # Tab Home    
          bs4SidebarMenuItem(
            "Home",
            tabName = "maintab_id_Home",
            icon = shiny::icon(configs$icons$home, verify_fa = FALSE, style = "color: #000") ,
           selected = F
          ),
          
          # ------------------------------------------------------------------ #
          # Tab Markets 
          bs4SidebarMenuItem(
            "Data Analytics",
            startExpanded = FALSE,
            tabName = "maintab_id_DataAnalytics", 
            icon = shiny::icon(configs$icons$upload, verify_fa = FALSE, style = "color: #000"),
            menuSubItem("Upload", tabName = "id_DataAnalytics_Upload", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE)
          ),
          #tags$hr(style="border-color: #eeeeef;"),
          # ------------------------------------------------------------------ #
          # Tab Markets 
          bs4SidebarMenuItem(
            "Market Views",
            startExpanded = FALSE,
            tabName = "maintab_id_Markets", 
            icon = shiny::icon(configs$icons$money_bill_trend_up, verify_fa = FALSE, style = "color: #000"),
            # Screener
            menuSubItem("Screener", tabName = "id_Markets_Screener", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Economic Calendar
            menuSubItem("Economic Calendar", tabName = "id_Markets_EconomicCalendar", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Indices
            menuSubItem("Indices", tabName = "id_Markets_Indices", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Stocks
            menuSubItem("Stocks", tabName = "id_Markets_Stocks", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Forex
            menuSubItem("Forex", tabName = "id_Markets_Forex", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Crypto
            menuSubItem("Crypto Currencies", tabName = "id_Markets_Crypto", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Commodities
            menuSubItem("Commodities", tabName = "id_Markets_Commodities", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Bonds
            menuSubItem("Bonds", tabName = "id_Markets_Bonds", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Funds
            menuSubItem("Funds", tabName = "id_Markets_Funds", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Calculators
            menuSubItem("Calculators", tabName = "id_Markets_Calculators", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE)
          ),
          #tags$hr(style="border-color: #eeeeef;"),
          
          
          # ------------------------------------------------------------------ #
          # Tab Macroeconomics 
          bs4SidebarMenuItem(
            "Macro Economics",
            startExpanded = FALSE,
            tabName = "maintab_id_DataSets",  ## must change
            icon = shiny::icon(configs$icons$database, verify_fa = FALSE, style = "color: #000"),
            
            # Federal Reserve Economic Data (FRED) Tools
            menuSubItem("FRED", tabName = "id_DataSets_FRED_tab", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # World Development Indicators Tools
            menuSubItem("WDI", tabName = "id_DataSets_WDI_tab", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = F)
          ),
          
          # ------------------------------------------------------------------ #
          # Tab Fundamental 
          bs4SidebarMenuItem(
            "Fundamental",
            startExpanded = FALSE,
            tabName = "maintab_id_Fundamental", 
            icon = shiny::icon(configs$icons$bars, verify_fa = FALSE, style = "color: #000"),
            # Fundamental Screening
            menuSubItem("Screening", tabName = "id_Fundamental_Screening", 
                        icon = shiny::icon(configs$icons$list, verify_fa = FALSE), 
                        selected = T  )
          ),
          # ------------------------------------------------------------------ #
          # Tab Technical 
          bs4SidebarMenuItem(
            "Technical",
            # p("Analytics", style = paste0("font-size:",configs$font_size$tab_titr, "px; ")),
            startExpanded = FALSE,
            tabName = "maintab_id_Analytics", 
            icon = shiny::icon(configs$icons$chart_line, verify_fa = FALSE, style = "color: #000"),
            
            # Technical Chart
            menuSubItem("Chart", tabName = "id_Analytics_TA_Chart", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Technical Tools
            menuSubItem("Patterns", tabName = "id_Analytics_TA_Tools", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            menuSubItem("Signal", tabName = "id_Analytics_TA_Tools2", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE)
          ),
          #tags$hr(style="border-color: #eeeeef;"),
          # ------------------------------------------------------------------ #
          # Tab Sentimental 
          bs4SidebarMenuItem(
            "Sentimental",
            startExpanded = FALSE,
            tabName = "maintab_id_Sentimental", 
            icon = shiny::icon(configs$icons$puzzle_piece, verify_fa = FALSE, style = "color: #000"),
            # Social Media
            menuSubItem("Social Media", tabName = "id_Analytics_Social", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # News
            menuSubItem("News", tabName = "id_Analytics_News", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE)
          ),
          # ------------------------------------------------------------------ #
          # Tab Fundamental (Asset Pricing)
          bs4SidebarMenuItem(
            "Asset Pricing",
            startExpanded = FALSE,
            tabName = "maintab_id_AssetPricing", 
            icon = shiny::icon(configs$icons$chart_pie, verify_fa = FALSE, style = "color: #000"),
            # Valuation
            menuSubItem("Valuation", tabName = "id_AssetPricing_Valuation", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE)
          ),
          # ------------------------------------------------------------------ #
          # Tab Time Series Models
          bs4SidebarMenuItem(
            "Time Series",
            tabName = "maintab_id_TSModels", 
            icon = shiny::icon(configs$icons$arrowtrendup, verify_fa = FALSE, style = "color: #000"),
            # Regression
            menuSubItem("Regression", tabName = "id_TSModels_Regression", 
                        icon = shiny::icon(configs$icons$chart_line, verify_fa = FALSE),  
                        selected = FALSE),
            # AR / MA
            menuSubItem("AR / MA", tabName = "id_TSModels_ARMA", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # VAR / VECM
            menuSubItem("VAR / VECM", tabName = "id_TSModels_VARVECM", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Stochastics
            menuSubItem("Stochastics", tabName = "id_TSModels_Stochastics", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE)
          ),
          # ------------------------------------------------------------------ #
          # Tab Trading Models
          bs4SidebarMenuItem(
            "Trading Models",
            tabName = "maintab_id_TradingModels", 
            icon = shiny::icon(configs$icons$chess, verify_fa = FALSE, style = "color: #000"),
            # Pair Finder 
            menuSubItem("Pair Finder", tabName = "id_TradingModels_PairFinder", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Pair Trading
            menuSubItem("Pair Trading", tabName = "id_TradingModels_Pair", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Algo Trading
            menuSubItem("Algo Trading", tabName = "id_TradingModels_Algo", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # ML / DL Trading
            menuSubItem("ML / DL", tabName = "id_TradingModels_MLDL", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE)
          ),
          # ------------------------------------------------------------------ #
          # Tab Risk Management
          bs4SidebarMenuItem(
            "Risk",
            tabName = "maintab_id_Risk", 
            icon = shiny::icon(configs$icons$yin_yang, verify_fa = FALSE, style = "color: #000"),
            # Risk Metrics
            menuSubItem("Interest Rate Risk", tabName = "id_InterestRate_Risk", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Market Risk
            menuSubItem("Market Risk", tabName = "id_Risk_Market", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Credit Risk
            menuSubItem("Credit Risk", tabName = "id_Risk_Credit", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE)
          ),
          #tags$hr(style="border-color: #eeeeef;"),
          # ------------------------------------------------------------------ #
          # Tab Portfolio
          bs4SidebarMenuItem(
            "Portfolio",
            tabName = "maintab_id_Portfolio", 
            icon = shiny::icon(configs$icons$piggy_bank, verify_fa = FALSE, style = "color: #000"),
            # MPT
            menuSubItem("MPT", tabName = "id_Portfolio_MPT", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # OLPS
            menuSubItem("OLPS", tabName = "id_Portfolio_OLPS", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Hedging
            menuSubItem("Hedging", tabName = "id_Portfolio_Hedging", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE)
          ),
          #tags$hr(style="border-color: #eeeeef;"),
          # ------------------------------------------------------------------ #
          # Tab Trading System
          bs4SidebarMenuItem(
            "Trading System",
            tabName = "maintab_id_TradingSys", 
            icon = shiny::icon(configs$icons$sack_dollar, verify_fa = FALSE, style = "color: #000"),
            # Money Management
            menuSubItem("Money Management", tabName = "id_TradingSys_MoneyMang", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE),
            # Trade volume (Position Sizing)
            menuSubItem("Position Sizing", tabName = "id_TradingSys_Volume", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Strategy
            menuSubItem("Strategy", tabName = "id_TradingSys_Strategy", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Back Test
            menuSubItem("Backtest", tabName = "id_TradingSys_BackTest", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE)
            
          ),
          # ------------------------------------------------------------------ #
          # Tab Forex
          bs4SidebarMenuItem(
            "Forex",
            tabName = "maintab_id_Forex", 
            icon = shiny::icon(configs$icons$dollar, verify_fa = FALSE, style = "color: #000")
          ),
          # ------------------------------------------------------------------ #
          # Tab Cryptos
          bs4SidebarMenuItem(
            "Cryptocurrency",
            tabName = "maintab_id_Cryptos", 
            icon = shiny::icon(configs$icons$bitcoin, verify_fa = FALSE, style = "color: #000")
          ),
          # ------------------------------------------------------------------ #
          # Tab Personal Finance
          bs4SidebarMenuItem(
            "Personal Finance",)),
            tabName = "maintab_id_PersonalFinance", 
            icon = shiny::icon(configs$icons$people_roof, verify_fa = FALSE, style = "color: #000"),
            # Retirement
            menuSubItem("Retirement", tabName = "id_PersonalFinance_Retirement", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE), 
                        selected = FALSE),
            # Real Estate
            menuSubItem("Real Estate", tabName = "id_PersonalFinance_RealEstate", 
                        icon = shiny::icon(configs$icons$check, verify_fa = FALSE),  
                        selected = FALSE)
          ),
          # ------------------------------------------------------------------ #
          # Tab Personal Finance
          bs4SidebarMenuItem(
            "About",
            tabName = "maintab_id_About", 
            icon = icon(configs$icons$user_tie, verify_fa = FALSE, style = "color: #000")
          )
        ) # div
        ) # bs4SidebarMenu
      ), # bs4DashSidebar
      
     # ----------------------------------------------------------------------- #
     # main body
     # ----------------------------------------------------------------------- #
      body = bs4DashBody(
        
        bs4TabItems(
          # welcome ui ----
          bs4TabItem(
            tabName = "maintab_id_Home",
            mod_welcome_ui("welcome_ui_1")
          ),
          
          # ------------------------------------------------------------------ #
          # Markets
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "id_Markets_Screener",
            fluidRow(
              # style = "margin-top: 10px;",
              bs4Card(
                id = "id_Markets_Screener_bs4Card",
                title = div(class = "row",
                  div(
                    h5("Market Screener")
                  ),
                  div(style = "margin-left:30px;",
                    selectInput("Mrkt_Screner_Exchange_Pikr",
                                label = NULL, # "Exchange",
                                choices = c("USA", "Germany",
                                            "UK"),
                                selected = "USA",
                                width = "140px")
                  )
                  ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Markets_Screener_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                  # ---
                  column(8, # style= "padding-top: 5px",
                         uiOutput("Screener_Dash_UI")
                  ),
                  # ---
                  column(4,
                         uiOutput("MarketNews_Stocks_Dash_UI")
                  )
                ) # fluidRow
              ) # bs4Card 
              
            ) # fluidrow
            
          
          ),
          # ------------------------ #
          # Markets_EconomicCalendar
          bs4TabItem(
            tabName = "id_Markets_EconomicCalendar",
            fluidRow(
              bs4Card(
                id = "id_Markets_EconomicCalendar_bs4Card",
                title = div(class = "row",
                            div(
                              h5("Economic Calendar")
                            )
                            ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Markets_EconomicCalendar_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                  # ---
                  column(12, 
                         htmlTemplate(filename = paste0(pathwd_dash, 
                                                        "/R/Modules/HTML/TView_EconomicCalendarWidget.Rhtml"))
                  ) # col 12
              ) # fluidRow
            ) # bs4Card 
            ) # main fluidRow
          ),
          
          # ------------------------------------------------------------------ #
          # Markets_Indices
          bs4TabItem(
            tabName = "id_Markets_Indices",
            fluidRow(
              bs4Card(
                id = "id_Markets_Indices_bs4Card",
                title = div(class = "row",
                            div(
                              h5("Market Indices")
                            )
                ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Markets_Indices_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                  # ---
                  column(12, 
                         htmlTemplate(filename = paste0(pathwd_dash, 
                                                        "/R/Modules/HTML/TView_TickerWidget.Rhtml")),
                         htmlTemplate(filename = paste0(pathwd_dash, 
                                                        "/R/Modules/HTML/TView_IndicesWidget.Rhtml"))
                  ) # col 12
                ) # fluidRow
              ) # bs4Card 
            ) # main fluidRow
          ),
          
          # ------------------------------------------------------------------ #
          # Markets_Stocks
          bs4TabItem(
            tabName = "id_Markets_Stocks",
            fluidRow(
              bs4Card(
                id = "id_Markets_Stocks_bs4Card",
                title = div(class = "row",
                            div(
                              h5("Stock Market View")
                            ),
                            div(style = "margin-left:30px;",
                              selectInput("Stock_Dash_Exchange_Pikr",
                                          label = NULL,
                                          choices = c("US", "NASDAQ",
                                                      "NYSE", "AMEX", "OTC", "LSE", "BER", "DUS"),
                                          selected = "US",
                                          width = "140px")
                            ),
                            div(style = "margin-left:10px;",
                              selectInput("Stock_Dash_Symbol_Pikr",
                                          label = NULL, 
                                          choices = c("NASDAQ:AAPL", "NASDAQ:TSLA"),
                                          selected = "NASDAQ:AAPL",
                                          width = "200px")
                            )
                ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Markets_Stocks_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                  column(3,
                         uiOutput("Top5_Stocks_Dash_UI")
                  ),
                  
                  column(5,
                         column(12,
                                uiOutput("SymbolInfo_Stocks_Dash_UI") 
                         ),
                         column(12, style = "margin-top:-20px;",
                                  uiOutput("ChartOnly_Stocks_Dash_UI")
                                  )
                         ),
                  column(2,
                         uiOutput("Fundamental_Stocks_Dash_UI")
                  ),
                  column(2,
                         uiOutput("SymbolNews_Stocks_Dash_UI")
                  ),
                  column(12,
                         uiOutput("CompanyProfile_Stocks_Dash_UI")
                         )
                ) # fluidRow
              ) # bs4Card 
            ) # main fluidRow
          ),
          # ------------------------------------------------------------------ #
          # Markets_Forex
          bs4TabItem(
            tabName = "id_Markets_Forex",
            fluidRow(
              bs4Card(
                id = "id_Markets_Forex_bs4Card",
                title = div(class = "row",
                            div(
                              h5("Forex Market tools")
                            )
                            ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Markets_Forex_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                 
                  column(6, # style= "margin-left:-10px; margin-right:-10px;",
                         column(12, 
                                htmlTemplate(filename = paste0(pathwd_dash, 
                                                               "/R/Modules/HTML/TView_ForexCrossRatesWidget.Rhtml"))
                         ),
                         column(12, style = "margin-top:-20px;",
                                htmlTemplate(filename = paste0(pathwd_dash, 
                                                               "/R/Modules/HTML/TView_ForexHeatMapWidget.Rhtml"))
                         ),
                         column(12, style = "margin-top:-20px;",
                                htmlTemplate(filename = paste0(pathwd_dash, 
                                                               "/R/Modules/HTML/TView_ForexWidget.Rhtml"))
                         )
                  ), # 5
                  column(6,
                         htmlTemplate(filename = paste0(pathwd_dash, 
                                                        "/R/Modules/HTML/TView_TechnicalAnalysis_Widget_Forex.Rhtml"))
                  )
                ) # fluidRow
              ) # bs4Card 
            ) # main fluidRow
          ),
          # ------------------------------------------------------------------ #
          # Markets_Crypto
          bs4TabItem(
            tabName = "id_Markets_Crypto",
            fluidRow(
              bs4Card(
                id = "id_Markets_Crypto_bs4Card",
                title = div(class = "row",
                            div(
                              h5("Crypto Market")
                            )
                ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Markets_Crypto_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                  column(6,
                         htmlTemplate(filename = paste0(pathwd_dash, 
                                                         "/R/Modules/HTML/TView_Cryptos_OverView_Widget.Rhtml"))
                  ),
                  column(6,
                         htmlTemplate(filename = paste0(pathwd_dash, 
                                                         "/R/Modules/HTML/TView_Cryptos_Performance_Widget.Rhtml"))
                      )
                ) # fluidRow
              ) # bs4Card 
            ) # main fluidRow
          ),
          
          # ------------------------------------------------------------------ #
          # Markets_Commodities
          bs4TabItem(
            tabName = "id_Markets_Commodities",
            fluidRow(
              bs4Card(
                id = "id_Markets_Commodities_bs4Card",
                title = div(class = "row",
                            div(
                              h5("Commodities")
                            )
                ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Markets_Commodities_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                  column(12,
                         htmlTemplate(filename = paste0(pathwd_dash, 
                                                        "/R/Modules/HTML/TView_FuturesWidget.Rhtml"))
                  )
                ) # fluidRow
              ) # bs4Card 
            ) # main fluidRow
          ),
          # ------------------------------------------------------------------ #
          # Markets_Bonds
          bs4TabItem(
            tabName = "id_Markets_Bonds",
            mainPanel(width = 12, style = "margin-left:0.5%; margin-right:0.5%",
            ) # mainPanel
          ),
          
          # ------------------------------------------------------------------ #
          # Markets_Funds
          bs4TabItem(
            tabName = "id_Markets_Funds",
            mainPanel(width = 12, style = "margin-left:0.5%; margin-right:0.5%",
            ) # mainPanel
          ),
          

          # ------------------------------------------------------------------ #
          # Markets_Calculators
          bs4TabItem(
            tabName = "id_Markets_Calculators",
            mainPanel(width = 12, style = "margin-left:0.5%; margin-right:0.5%",
                      fluidRow(
                        uiOutput("Calculators_Dash_UI")
                      )
            ) # mainPanel
          ),
          
          # ------------------------------------------------------------------ #
          # DataSets FRED
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "id_DataSets_FRED_tab",
            fluidRow(style = "font-size:12px;",
              col_12(
                bs4Card(
                  id = "id_DataSets_FRED_main_bs4Card",
                  title = "FRED",
                  # footer = "Table ....", 
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  collapsed = FALSE,
                  closable = FALSE,
                  label = NULL,
                  width = 12,
                  sidebar = boxSidebar(
                    tagList(
                    ),
                    id = "id_DataSets_FRED_main_bs4Card_sidebar",
                    width = 60,
                    background = "#f9f9fa",
                    startOpen = FALSE,
                    icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                    easyClose = TRUE
                  ),
                  tagList(
                    uiOutput("DataSets_FRED_UI")
                  ) # tagList
                ) # bs4Card 
              ) # col_12
            ) # fluidRow
          ),
          
          # ------------------------------------------------------------------ #
          # DataSets WDI
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "id_DataSets_WDI_tab",
            fluidRow(style = "font-size:12px;",
              col_12(
                bs4Card(
                  id = "id_DataSets_WDI_main_bs4Card",
                  title = "WDI",
                  # footer = "Table ....", 
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  collapsed = FALSE,
                  closable = FALSE,
                  label = NULL,
                  width = 12,
                  # height = "800px",
                  sidebar = boxSidebar(
                    tagList(
                    ),
                    id = "id_DataSets_WDI_main_bs4Card_sidebar",
                    width = 60,
                    background = "#f9f9fa",
                    startOpen = FALSE,
                    icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                    easyClose = TRUE
                  ),
                  tagList(
                    uiOutput("DataSets_WDI_UI")
                  ) # tagList
                ) # bs4Card 
              ) # col_12
            ) # fluidRow
          ),
          
          # ------------------------------------------------------------------ #
          # Fundamental
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "id_Fundamental_Screening",
            fluidRow(
              col_12( 
                uiOutput("id_Fundamental_Screening_ui")
                )
              )
            ),
          # ------------------------------------------------------------------ #
          # Analytics
          # ------------------------------------------------------------------ #
          # id_Analytics_TA_Chart
          bs4TabItem(
            tabName = "id_Analytics_TA_Chart",
            mainPanel(width = 12, 
                      htmlTemplate(filename = paste0(pathwd_dash, 
                                                     "/R/Modules/HTML/TView_TechnicalAnalysis_Widget.Rhtml"))
                      
            )
          ),
          # ------------------------------------------------------------------ #
          # id_Analytics_TA_Tools
          bs4TabItem(
            tabName = "id_Analytics_TA_Tools",
            fluidRow(style = "font-size:12px;",
              col_12(
                bs4Card(
                  id = "id_Analytics_TA_Tools_main_bs4Card",
                  title = tagList(
                    actionButton("id_Analytics_TA_Tools_Patterns_btn",
                                 label = "Find Patterns",
                                 icon = shiny::icon(configs$icons$chart_line, 
                                                    verify_fa = FALSE ),
                                 style = 'font-size: 12px; background: #fff; width: 120px; height: 30px;
                                 vertical-align: top;'
                    ),
                    actionButton("id_Analytics_TA_Tools_Filters_btn",
                                 label = "Filters",
                                 icon = shiny::icon(configs$icons$chart_line, 
                                                    verify_fa = FALSE ),
                                 style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px; height: 30px;
                                 vertical-align: top;'
                    ),
                    actionButton("id_Analytics_TA_Tools_Backtest_btn",
                                 label = "Back Testing",
                                 icon = shiny::icon(configs$icons$chart_line, 
                                                    verify_fa = FALSE ),
                                 
                                 style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px; height: 30px;
                                 vertical-align: top;'
                    ),
                    actionButton("id_Analytics_TA_Tools_4_btn",
                                 label = "...",
                                 icon = shiny::icon(configs$icons$chart_line, 
                                                    verify_fa = FALSE ),
                                 style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px; height: 30px;
                                 vertical-align: top;'
                    ),
                    actionButton("id_Analytics_TA_Tools_5_btn",
                                 label = "...",
                                 icon = shiny::icon(configs$icons$chart_line, 
                                                    verify_fa = FALSE ),
                                 style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px; height: 30px;
                                 vertical-align: top;'
                    ),
                    actionButton("id_Analytics_TA_Tools_6_btn",
                                 label = "...",
                                 icon = shiny::icon(configs$icons$chart_line, 
                                                    verify_fa = FALSE ),
                                 style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px; height: 30px;
                                 vertical-align: top;'
                    )
                  ),
                  # footer = "Table ....", 
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  collapsed = FALSE,
                  closable = FALSE,
                  label = NULL,
                  width = 12,
                  sidebar = boxSidebar(
                    tagList(
                    ),
                    id = "id_Analytics_TA_Tools_main_bs4Card_sidebar",
                    width = 60,
                    background = "#f9f9fa",
                    startOpen = FALSE,
                    icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                    easyClose = TRUE
                  ),
                  tagList(
                    uiOutput("Analytics_TA_Tools_UI")
                  ) # tagList
                ) # bs4Card 
              ) # col_12
            ) # fluidRow
          ),
          # ------------------------------------------------------------------ #
          # id_Analytics_Sentiment
          bs4TabItem(
            tabName = "id_Analytics_Social" , # "id_Analytics_Sentiment",
            
            fluidRow(style = "font-size:12px;",
              
              col_12(# style = "margin-top: 10px;",
                            bs4Card(
                                  id = "id_Analytics_Sentiment_main_bs4Card",
                                  title = tagList(
                                    actionButton("id_Analytics_Sentiment_Twitter_btn",
                                                 label = "Twitter",
                                                 icon = shiny::icon(configs$icons$twitter, 
                                                                    verify_fa = FALSE ),
                                                 style = 'font-size: 12px; background: #fff; width: 120px;'
                                    ),
                                    actionButton("id_Analytics_Sentiment_Facebook_btn",
                                                 label = "Facebook",
                                                 icon = shiny::icon(configs$icons$facebook, 
                                                                    verify_fa = FALSE ),
                                                  
                                                 style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px;'
                                    ),
                                    actionButton("id_Analytics_Sentiment_Google_btn",
                                                 label = "Google",
                                                 icon = shiny::icon(configs$icons$google, 
                                                                    verify_fa = FALSE ),
                                                 style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px;'
                                    ),
                                    # actionButton("id_Analytics_Sentiment_News_btn",
                                    #              label = "News",
                                    #              icon = shiny::icon(configs$icons$newspaper, 
                                    #                                 verify_fa = FALSE ),
                                    #              style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px;'
                                    # ),
                                    actionButton("id_Analytics_Sentiment_NLP_btn",
                                                 label = "NLP",
                                                 icon = shiny::icon(configs$icons$industry, 
                                                                    verify_fa = FALSE ),
                                                 style = 'margin-left:5px; font-size: 12px; 
                                                 background: #fff; width: 120px;'
                                    )# ,
                                    # actionButton("id_Analytics_Sentiment_RSS_btn",
                                    #              label = "RSS",
                                    #              icon = shiny::icon(configs$icons$rss, 
                                    #                                 verify_fa = FALSE ),
                                    #              style = 'margin-left:5px; font-size: 12px; background: #fff; width: 120px;'
                                    # )
                                  ),
                                  # footer = "Table ....", 
                                  status = "primary",
                                  solidHeader = FALSE,
                                  collapsible = FALSE,
                                  collapsed = FALSE,
                                  closable = FALSE,
                                  label = NULL,
                                  width = 12,
                                  sidebar = boxSidebar(
                                    tagList(
                                    ),
                                    id = "NULL133",
                                    width = 60,
                                    background = "#f9f9fa",
                                    startOpen = FALSE,
                                    icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                                    easyClose = TRUE
                                  ),
                                  tagList(
                                    uiOutput("Analytics_Sentiment_UI")
                                  ) # tagList
                                ) # bs4Card 
                            ) # col_12
                      ) # fluidRow
          ),
          # ------------------------------------------------------------------ #
          # Time Series Models Regression Tab
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "id_TSModels_Regression", 
            ts_regress_ui("id_ts_reg")
          ),
          
          # ------------------------------------------------------------------ #
          # Trading Models Pair Finder
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "id_TradingModels_PairFinder", 
            fluidRow(style = "font-size:12px;",
              col_2(
              bs4Card(
                id = "id_TradingModels_PairFinder_Param_card",
                title = "Main Parameters",
                # footer = "Help",
                status = "primary",
                solidHeader = FALSE,
                headerBorder = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                height = "900px",
                # ------------------ #
                fluidRow(
                  col_12(
                    radioButtons("id_TradingModels_PairFinder_DataSource_rdiobtn",
                                 label = "Data Source", 
                                 inline = TRUE,
                                 choices = c("Excel", "DB", "Web")
                                 ),
                    conditionalPanel(
                      condition = "input.id_TradingModels_PairFinder_DataSource_rdiobtn == 'Excel'",
                      fileInput("id_TradingModels_PairFinder_Excel", "Choose .xlsx",
                                multiple = FALSE,
                                accept = ".xlsx"
                                )
                    ), # Excel
                    conditionalPanel(
                      condition = "input.id_TradingModels_PairFinder_DataSource_rdiobtn == 'DB'",
                    ), # DB
                    conditionalPanel(
                      condition = "input.id_TradingModels_PairFinder_DataSource_rdiobtn == 'Web'",
                    ) # Web
                  ),
                  col_12(
                    selectInput("id_TradingModels_PairFinder_Return_selectInput",
                                label = "Return Method",
                                choices = c("Log", "Discrete", "Difference"),
                                selected = "Log"
                    )
                  ),
                  col_12(
                    selectInput("id_TradingModels_PairFinder_MissingDataModel_selectInput",
                                label = "Imputation Algorithm",
                                choices = c("tsrobprep",
                                            "Aggregated values",
                                            "Most recent value",
                                            "Next observation backward",
                                            "Linear interpolation"),
                                selected = "Next observation backward"
                                )
                    ),
                  col_12(
                    selectInput("id_TradingModels_PairFinder_ClusteringModel_selectInput",
                                label = "Clustering Model",
                                choices = c("OPTICS", "DBSCAN", "PCA", "K-mean")
                    )
                  ),
                  col_12(
                    selectInput("id_TradingModels_PairFinder_DataTab_Symbols_selectInput",
                                label = "Symbol",
                                choices = NA,
                                selected = NULL )
                  ),
                  col_12(),
                  col_12(),
                  col_12(align = "center",
                    actionButton("id_TradingModels_PairFinder_Run_btn", 
                                 label = "Refresh", 
                                 icon = shiny::icon(configs$icons$refresh, verify_fa = FALSE),
                                 style = "color:#000; background-color:#fff; border-radius:5px;"
                    ) 
                  )
                ) #  fluidRow
              )# bs4Card
              ), # col 2
              
              # -------------------------------------------------------------- #
              col_10(
              bs4Card(
                id = "id_TradingModels_PairFinder_ModelResults_card",
                title = "Model Results",
                # footer = "Help",
                status = "primary",
                solidHeader = FALSE,
                headerBorder = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                maximizable = TRUE,
                label = NULL,
                width = 12,
                # height = "900px",
                # ------------------ #
               fluidRow(
                  col_12(style = "margin-top:-10px;",
                 bs4Dash::tabsetPanel(
                   id = "id_TradingModels_PairFinder_tabsetPanel",
                   type = "tabs", # "pills",
                   vertical = F,
                   side = "left",
                   selected	= "Data",
                   tabPanel(title = "Data", 
                            # value = title,
                            icon = shiny::icon("magnifying-glass-chart", verify_fa = FALSE ),
                            
                            col_12(style = "margin-top:10px",
                              DTOutput("id_TradingModels_PairFinder_DataTab_DTable")
                            )
                            ),
                   # --------------------------------------------------------- #
                   tabPanel(title = "Stats",
                            icon = shiny::icon(configs$icons$chart_pie, verify_fa = FALSE ),
                            col_12(style = "margin-top:10px",
                                   # textOutput("id_TradingModels_PairFinder_Stats_MsgText_UI") ,
                                   uiOutput("id_TradingModels_PairFinder_Stats_UI")
                            )
                   ),
                   # --------------------------------------------------------- #
                   tabPanel("Clustring", icon = shiny::icon(configs$icons$bars, verify_fa = FALSE ),
                            h6("eeeeeeeeeee")),
                   # --------------------------------------------------------- #
                   tabPanel("Cointegration" , icon = shiny::icon("code-merge", verify_fa = FALSE ),
                            h6("1111111111111")),
                   # --------------------------------------------------------- #
                   tabPanel("Mean Reversion" , icon = shiny::icon("chart-line", verify_fa = FALSE ),
                            h6("2222")),
                   # --------------------------------------------------------- #
                   tabPanel("Prediction" , icon = shiny::icon("hamsa", verify_fa = FALSE ),
                            h6("3333")),
                   # --------------------------------------------------------- #
                   tabPanel("Backtest" , icon = shiny::icon(configs$icons$list_check, verify_fa = FALSE ),
                            h6("4444")),
                   # --------------------------------------------------------- #
                   tabPanel("Pair Trade" , icon = shiny::icon(configs$icons$chess, verify_fa = FALSE ),
                            h6("555"))
                 ) # tabsetPanel
                  ) # col 12
                ) #  fluidRow
              ) # bs4Card
              ) # col 10
            ) # fluidRow
          ), # bs4TabItem # wellpanel
          
          # ------------------------------------------------------------------ #
          # Trading Models Pair Trading
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "id_TradingModels_Pair",
            fluidRow(style = "font-size:12px;",
              tagList(
                col_2(style = paste0("font-size:",configs$font_size$general, "px;"),
                      bs4Card(
                        id = "id_TradingModels_Pair_Param_card",
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
                        height = "900px",
                        # sidebar = boxSidebar(
                        #   tagList(
                        #   ),
                        #   id = "id_TradingModels_Pair_Param_sidebar",
                        #   width = 60,
                        #   background = "#f9f9fa",
                        #   startOpen = FALSE,
                        #   icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                        #   easyClose = TRUE
                        # ),
                        # ------------------ #
                        tagList(
                          br(),
                          
                          fluidRow(
                            col_6(
                              selectInput("id_TradingModels_Pair_Market_selectInput_1",
                                          label = "Market (I)",
                                          choices = all_markets,
                                          selected = all_markets[1])
                            ),
                            col_6(
                              selectInput("id_TradingModels_Pair_Symbol_selectInput_1",
                                          label = "Symbol (I)",
                                          choices = NULL, # reactive later
                                          selected = NULL)
                            )
                          ),
                         
                          fluidRow(
                            col_6(
                              selectInput("id_TradingModels_Pair_Market_selectInput_2",
                                          label = "Market (II)",
                                          choices = all_markets,
                                          selected = all_markets[1])
                            ),
                            col_6(
                              selectInput("id_TradingModels_Pair_Symbol_selectInput_2",
                                          label = "Symbol (II)",
                                          choices = NULL, # reactive later
                                          selected = NULL)
                            )
                          ),
                          fluidRow(
                          col_12(
                            selectInput("id_TradingModels_Pair_Model_selectInput",
                                        label = "Model",
                                        choices = c("Basic", "2"), # reactive later
                                        selected = "Basic")
                          ),
                          col_6(
                            numericInput("id_TradingModels_Pair_capital", "Initial Capital" , 
                                         value = 1000, min = 1, max = NA, step = 1) 
                          ),
                          col_6(
                            numericInput("id_TradingModels_Pair_MaxPos", "Max Positions" , 
                                         value = 1000, min = 1, max = NA, step = 1) 
                          )
                          
                          # Currency
                          ),
                          fluidRow(
                          col_6(
                            selectInput("id_TradingModels_Pair_Timeframe", 
                                        label = "Time Frame", 
                                        choices = c("D","W","M","M3","M6","Y"),
                                        selected = "D" )
                          ),
                          col_6(
                            selectInput("id_TradingModels_Pair_Window", 
                                        label = "Window", 
                                        choices = c("All", seq(as.numeric(format(Sys.Date(), "%Y")), 2010)), 
                                        selected = format(Sys.Date(), "%Y") )
                          ),
                          col_6(
                            dateInput("id_TradingModels_Pair_Date_from", "From Date", 
                                      value = Sys.Date()- 500),
                            tags$style(
                              paste0(".form-control {font-size: ", configs$font_size$general, "px !important;}")
                            )
                          ),
                          col_6(
                            numericInput("id_TradingModels_Pair_Last_Candles",
                                         label = "Last Candles", 
                                         value = 100, min = 20, max = NA, step = 1 )
                          )
                          ),
                          br(),
                          col_12(align="center",
                                 actionButton(inputId = "id_TradingModels_Pair_run_btn", 
                                              label = "Run", width = "120px",
                                              icon = shiny::icon(configs$icons$searchengin, verify_fa = FALSE)),
                                 tippy_this("id_TradingModels_Pair_run_btn", "Run")
                          )
                          # -------
                        ) # tagList
                      ) # bs4Card
                ), # col_2
                # -------------------------------------------------------------------- #
                col_10(style = paste0("font-size:", configs$font_size$general, "px;"),
                       bs4Card(
                         id = "id_TradingModels_Pair_Results_card",
                         title = "Pair Trading",
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
                         height = "900px",
                         sidebar = boxSidebar(
                           tagList(),
                           id = "id_TradingModels_Pair_Results_sidebar",
                           width = 50,
                           background = "#f9f9fa",
                           startOpen = FALSE,
                           icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                           easyClose = TRUE
                         ),
                         # ----- #
                         tagList(
                           uiOutput("id_TradingModels_Pair_UI")
                         ) # tagList
                       ) # bs4Card
                ) # col_10
              ) # tagList
            ) # fluidrow
          ),
          
          # ------------------------------------------------------------------ #
          # Risk Main Tab
          # ------------------------------------------------------------------ #
          
          # id_InterestRate_Risk
          bs4TabItem(
            tabName = "id_InterestRate_Risk",
            fluidRow(
            tagList(
              col_12(style = paste0("font-size:",configs$font_size$general, "px;"),
                    bs4Card(
                      id = "id_InterestRate_Risk_main_card",
                      title = "Interest Rate Models",
                      # footer = "Help",
                      status = "primary",
                      solidHeader = FALSE,
                      headerBorder = TRUE,
                      collapsible = FALSE,
                      collapsed = FALSE,
                      closable = FALSE,
                      label = NULL,
                      width = 12,
                      height = "900px",
                      sidebar = boxSidebar(
                        tagList(
                        ),
                        id = "id_InterestRate_Risk_sidebar",
                        width = 40,
                        background = "#f9f9fa",
                        startOpen = FALSE,
                        icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                        easyClose = TRUE
                      ),
                      # ------------------ #
                      tagList(
                        br(),
                        fluidRow(  
                          col_12(
                            p("Interest Rate Models ")
                            ) # col_12
                          ),
                        br(),
                        
                      ) # tagList
                    ) # bs4Card
              ) # col_12
            ) # tagList
          ) # fluid
          ),
          
          # ------------------------------------------------------------------ #
          # Forex
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "maintab_id_Forex",
            fluidRow(
              bs4Card(
                id = "id_Forex_bs4Card",
                title = div(class = "row",
                            div(
                              h5("Forex Analysis Tools")
                            )
                ),
                # footer = "Table ....", 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                collapsed = FALSE,
                closable = FALSE,
                label = NULL,
                width = 12,
                sidebar = boxSidebar(
                  tagList(
                  ),
                  id = "id_Forex_bs4Card_sidebar",
                  width = 50,
                  background = "#f9f9fa",
                  startOpen = FALSE,
                  icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
                  easyClose = TRUE
                ),
                fluidRow(
                  
                  p("Forex")
                 
                ) # fluidRow
              ) # bs4Card 
            ) # main fluidRow
          ),
          
          # ------------------------------------------------------------------ #
          # Crypto
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "maintab_id_Cryptos",
            fluidRow(
              p("Cryptos")
            )
          ),
          
          # ------------------------------------------------------------------ #
          # Crypto
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "maintab_id_AI_ML",
          ),
          
          # ------------------------------------------------------------------ #
          # About
          # ------------------------------------------------------------------ #
          bs4TabItem(
            tabName = "maintab_id_About",
            mod_about_ui("about_ui_1")
          )
        ) # bs4TabItems
      ), # body
      
     # ----------------------------------------------------------------------- #
     # footer
     # ----------------------------------------------------------------------- #
  footer = tags$footer(class = 'main-footer', 
                       fluidRow(
            column(4, style = 'margin-left:5px;', 
                   actionLink("btn_indicator_updates", 
                              label = "IQAT Tools | Developed by: Mohsen Asgari | Version: 0.1",
                              style = "color: #fff;")
                   ),
            column(3,
                   actionLink("ContactUs", label = "Contact Us!", 
                              icon = icon(configs$icons$atsign, verify_fa = FALSE), style = "color: #fff;")
                   ),
            column(2,
                   tags$a(
                     href = "https://www.google.com", "Privacy & cookies",
                     class = "externallink", style = "color: #fff; text-decoration: none"
                   )
            ),
            column(1, 
                     HTML(paste0( format(Sys.time(), "%d - %b - %Y") )) # (%a) 
                   
                   )
            ),
  style = "
       position:fixed;
       /*text-align:center; */
       left: 0;
       bottom: 0;
       width:100%;
       z-index:1000;
       height:30px; /*  Height of the footer */
       color: white ;
       padding: 5px;
       font-size: 12px;
       /*  font-weight: bold;  */
       background-color: #808080; "  ) # footer
# ----------------------------------------------------------------------- #
) # bs4DashPage









