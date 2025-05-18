


# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.




#' Twitter sentiment Analytics and data mining
#' @param Shinyinputs c(input, output, session)
#' @return 
#' @export
#' @examples
#'
Func_Twitter_Analytics <- function(input, output, session, 
                                   df = tweets$df, 
                                   type = "WordLevel",
                                   ...){
  # ------------------------------------------------------------------------- #
# df = twdf
  # ------------------------------------------------------------------------- #
  if(nrow(df) >= 1){
    
    if(type == "WordLevel"){
      words_data <- df %>% dplyr::select(text)  %>% 
        unnest_tokens(word, text) %>% dplyr::count(word, sort = TRUE)
      
      words_data <- words_data %>% dplyr::filter(!word %in% c('https', 't.co', 'he\'s', 'i\'m', 'it\'s'))
      
      words_data2 <- words_data %>% anti_join(stop_words) %>% dplyr::count(word, sort = TRUE)
      
      words_data2 %>%  dplyr::inner_join(get_sentiments("bing")) %>% dplyr::count(sentiment, sort = TRUE)
      
      profanity_list <- unique(tolower(lexicon::profanity_alvarez))
      
      plt = words_data %>% dplyr::filter(!word %in% c('https', 't.co', 'he\'s', 'i\'m', 'it\'s', profanity_list)) %>%
        dplyr::inner_join(get_sentiments("bing")) %>%
        dplyr::count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("red", "blue"),
                         max.words = 50)
    }
    
    # ---
    if(type == "NegPos"){
    # library(sentimentr)
    tweet_sentences_data <- sentimentr::sentiment(sentimentr::get_sentences(df$text)) %>% 
      dplyr::group_by(element_id) %>% 
      dplyr::summarize(meanSentiment = mean(sentiment))
    
    slices <- c(sum(tweet_sentences_data$meanSentiment < 0), sum(tweet_sentences_data$meanSentiment == 0),
                sum(tweet_sentences_data$meanSentiment > 0))
    labels <- c("Negative Tweets: ", "Neutral Tweets: ", "Positive Tweets: ")
    pct <- round(slices/sum(slices)*100)
    labels <- paste(labels, pct, "%", sep = "") #customize labeling
    #add in appropriate colors for positive, neutral, negative
    plt = pie(slices, labels = labels, col = c('red', 'yellow', 'green'), 
        main="Tweet Sentiment Percentages")
    
    }
    
    if(type == "UserLevel"){
      #selecting top 50 tweets by favorites
      user_sentiment <- df %>% dplyr::select(user_id, text, favorite_count) %>% arrange(desc(favorite_count)) %>% slice(1:50)
      out <- sentiment_by(get_sentences(user_sentiment$text), 
                          list(user_sentiment$user_id))
       plt = plot(out)
    }
    
    return(plt)
    
  } # if df 
  
  
} # Func_Twitter_Analytics



# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #

#' Twitter sentiment Analysis (Search Engine)
#' @param Shinyinputs c(input, output, session)
#' @return 
#' @export
#' @examples
#'
Func_Twitter <- function(input, output, session, ...){
  
  # ------------------------------------------------------------------------- #
  # --- parameters
  
  file <- "chirp.yml"
  
  if( !file.exists(paste0(pathwd_dash, "/www/yml/", file)) ){
    cat(
      crayon::red(cli::symbol$cross), "No", file, "in working directory\n"
    )
    return(NULL)
  }
  
  settings <<- yaml::read_yaml(paste0(pathwd_dash, "/www/yml/", file))
  
  
  # --------------------------- #
  if(length(settings$credentials) == 0 || length(unlist(settings$credentials)) == 0){
    
    rtweet_token <- tryCatch(rtweet::get_token(), error = function(e) NULL)
    
    if(!is.null(rtweet_token)){
      cat(
        crayon::yellow(cli::symbol$warning), "No credentials in", file, 
        ". Using stored credentials found on machine.\n"
      )
    } else {
      cat(
        crayon::red(cli::symbol$cross), "No credentials in", file, "\n"
      )
      return(NULL)  
    }
    
  } else {
    rtweet_token <- tryCatch(
      rtweet::create_token(
        app = "chirp",
        consumer_key = settings$credentials$consumer_key,
        consumer_secret = settings$credentials$consumer_secret,
        access_token = settings$credentials$access_token,
        access_secret = settings$credentials$access_secret
      ),
      error = function(e) e
    )
    
    if(inherits(rtweet_token, "error")){
      cat(
        crayon::red(cli::symbol$cross), "Invalid credentials in", file, "\n"
      )
      return(NULL)
    }
  } # if else
  
  # --------------------------- #  
  if(!"theme" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No theme set in _chirp.yml, setting to",
      crayon::underline("paper.\n")
    )
    theme <- "paper"
  } else {
    theme <- settings[["style"]][["theme"]]
  }
  # --------------------------- #   
  
  if(!"sliders" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No sliders color in _chirp.yml, defaulting to white\n"
    )
    slider_color <- "white"
  } else {
    slider_color <- settings[["style"]][["sliders"]]
  }
  # --------------------------- #
  
  if(!"font" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No font set in _chirp.yml, defaulting to",
      crayon::underline("Raleway.\n")
    )
    
    font <- "Raleway"
    font_family <- "'Raleway', sans-serif"
  } else {
    font <- settings[["style"]][["font"]]
    font_family <- settings[["style"]][["font_family"]]
  }
  # --------------------------- #
  
  if(!"continuous" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No continuous palette set in _chirp.yml, setting to default.\n"
    )
    
    palette <- c("#4b2991", "#872ca2", "#c0369d", "#ea4f88", "#fa7876", "#f6a97a", "#edd9a3")
  } else {
    palette <- settings[["style"]][["continuous"]]
  }
  # --------------------------- #
  
  if(!"vr_background" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No vr_background set in _chirp.yml, defaulting to #052960.\n"
    )
    
    vr_background <- "#052960"
  } else {
    vr_background <- settings[["style"]][["vr_background"]]
  }
  # --------------------------- #
  
  if(!"sentiment_palette" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No sentiment_palette set in _chirp.yml.\n"
    )
    
    sentiment_palette <- c("red", "green", "yellow")
  } else {
    sentiment_palette <- settings[["style"]][["sentiment_palette"]]
  }
  # --------------------------- #
  
  ## # Maximum number of tweets one can fetch
  if(!"max_tweets" %in% names(settings[["options"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No max_tweets specified in _chirp.yml, defaulting to 17,000.\n"
    )
    
    max_tweets <- 17000
  } else {
    max_tweets <- settings[["options"]][["max_tweets"]]
  }
  # --------------------------- #
  
  ## # Minimum number of tweets one can fetch
  if(!"min_tweets" %in% names(settings[["options"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No min_tweets specified in _chirp.yml, defaulting to 500.\n"
    )
    
    min_tweets <- 100
  } else {
    min_tweets <- settings[["options"]][["min_tweets"]]
  }
  # --------------------------- #
  
  if(min_tweets > max_tweets){
    cat(
      crayon::red(cli::symbol$cross), "min_tweets is greater than max_tweets.\n"
    )
    return(NULL)
  }
  # --------------------------- #
  
  if(!"discrete" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No discrete palette set in _chirp.yml, setting to default discrete palette.\n"
    )
    
    discrete <- c("#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C", "#DAA51B",
                  "#2F8AC4", "#764E9F", "#ED645A", "#CC3A8E", "#A5AA99")
  } else {
    discrete <- settings[["style"]][["discrete"]]
  }
  # --------------------------- #
  
  if(!"edges_color" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No edges_color set in _chirp.yml, setting to default.\n"
    )
    
    edge_color <- "#bababa"
  } else {
    edge_color <- settings[["style"]][["edges_color"]]
  }
  # --------------------------- #
  
  font_name <- gsub("[[:space:]]", "+", font)
  
  inverse <- settings$style$inverse %||% FALSE
  
  # --------------------------- #
  # head
  head <- tagList(
    tags$link(
      href = paste0("https://fonts.googleapis.com/css?family=", font_name),
      rel = "stylesheet"
    ),
    tags$style(
      paste0("*{font-family: '", font, "', sans-serif;}")
    ),
    tags$link(
      href = "chirp-assets/pushbar.css",
      rel="stylesheet",
      type="text/css"
    ),
    tags$link(
      href = "chirp-assets/custom.css",
      rel = "stylesheet",
      type = "text/css"
    ),
    tags$script(
      src = "chirp-assets/pushbar.js"
    ),
    tags$link(
      href = "chirp-assets/please-wait.css",
      rel = "stylesheet",
      type = "text/css"
    ),
    tags$script(
      src = "chirp-assets/please-wait.min.js"
    ),
    tags$script(
      src = "https://unpkg.com/micromodal/dist/micromodal.min.js"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.7.2/css/all.css", 
      integrity = "sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr",
      crossorigin = "anonymous"
    ),
    tags$script(
      src = "chirp-assets/custom.js"
    ),
    tags$link(
      rel="shortcut icon",
      href = "https://chirp.sh/img/chirp_favicon.png"
    ),
    tags$style(
      paste0(".pushbar{background-color:", slider_color, ";}")
    )
  )
  
  # add google analytics if present
  if("ganalytics" %in% names(settings$tracking)){
    
    ga_id <- settings$tracking$ganalytics %||% ""
    
    ga_tag <- tagList(
      tags$script(
        async = NA,
        src = paste0("https://www.googletagmanager.com/gtag/js?id={{", ga_id, "}}")
      ),
      tags$script(
        paste0(
          "window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', '{{", ga_id, "}}');"
        )
      )
    )
    
    head <- tagAppendChild(head, ga_tag)
  }
  
  particles_json <- jsonlite::fromJSON(
    #system.file(paste0(pathwd_dash, "/www/js/particles.json") , package = "chirp")
    readr::read_file(paste0(pathwd_dash, "/www/js/particles.json"))
  )
  
  options(
    sentiment_palette = sentiment_palette,
    chirp_discrete = discrete,
    chirp_palette = palette,
    chirp_edge_color = edge_color,
    chirp_font_family = font_family,
    rtweet_token = rtweet_token,
    vr_background = vr_background,
    min_tweets = min_tweets,
    max_tweets = max_tweets,
    search_query = ""
  )
  
  # ------------------------------------------------------------------------- #
  # --- UI
    
 # output$Analytics_Sentiment_TwitterExplorer_UI <- renderUI({
    
    output$Analytics_Sentiment_UI <- renderUI({ 
    
    
      fluidRow(
        col_2(style = "font-size: 12px;",
          bs4Card(
            id = "id_Analytics_Sentiment_Twitter_Param",
            title = "Parameters",
            # footer = "Help", 
            status = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            collapsed = FALSE,
            closable = FALSE,
            label = NULL,
            width = 12,
            height = "820px",
            sidebar = boxSidebar(
              tagList(
              ),
              id = "id_Analytics_Sentiment_Twitter_Param_sidebar",
              width = 60,
              background = "#f9f9fa",
              startOpen = FALSE,
              icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
              easyClose = TRUE
            ),
            # ------------------ #
            tagList(
            col_12(
              textInput("q", 
                        label = "Search",
                        placeholder = "  Enter search query here. "),
              
              tags$style(HTML("#q.form-control{color: blue;  
                              border: 0.5px solid #c6c7c8; border-radius:5px}")),
              tippy_this("q", "Your search query")
            ),
            # ---
            col_12(
              div(style = "display:flex;",
                  div(style = "width:49%;",
                      selectInput(
                        "network",
                        "Network Type",
                        choices = c(
                          "Retweets" = "retweet_screen_name",
                          "Hashtags" = "hashtags",
                          "Conversations" = "mentions_screen_name"
                        ),
                        selected = "Retweets"
                      ),
                      tippy_this("network", "Type of network to draw")
                      ),
                  
                  div(style = "width:49%; margin-left:5px;",
                      selectInput(
                        "type",
                        "Type",
                        choices = c(
                          "Recent" = "recent",
                          "Mixed" = "mixed",
                          "Popular" = "popular"
                        ),
                        selected = "recent"
                      ),
                      tippy_this("type", "Type of tweets to fetch")
                      )
                  )
            ), # 12
            
            # ---
            col_12(
              div(style = "display:flex;",
                  div(style = "width:49%;",
                        selectInput("size",
                          "Nodes Size",
                          choices = c(
                            "# tweets" = "n_tweets",
                            "In-degree" = "in_degree",
                            "Out-degree" = "out_degree",
                            "Closeness" = "closeness",
                            "Pagerank" = "pagerank",
                            "Authority" = "authority",
                            "Eigen" = "eigen"
                          ),
                          selected = "n_tweets"
                        ),
                        tippy_this("size", "Variable to size nodes")
                  ),
                  div(style = "width:49%; margin-left:5px;",
                        selectInput(
                          "colour",
                          "Nodes Colour",
                          choices = c(
                            "Cluster" = "group",
                            "# tweets" = "n_tweets",
                            "Components" = "components",
                            "In-degree" = "in_degree",
                            "Out-degree" = "out_degree",
                            "Closeness" = "closeness",
                            "Pagerank" = "pagerank",
                            "Authority" = "authority",
                            "Eigen" = "eigen",
                            "Type" = "type"
                          ),
                          selected = "Cluster"
                        ),
                        tippy_this("colour", "Variable to colour nodes")
                  )
              )
            ),
            # ---
            col_12(
              sliderInput(
                "Twit_number_tweets",
                label = "Number of tweets",
                min = min_tweets,
                max = max_tweets,
                value = min_tweets,
                step = 100
              ),
              tippy_this("Twit_number_tweets", "Number of tweets to fetch")
            ),
            # ---
            col_12(
              div(style = "display:flex;",
                  div(style = "width:69%;",
                        conditionalPanel(
                          "input.network != 'retweet_screen_name'",
                          checkboxInput(
                            "comentions",
                            "Co-mentions",
                            width = "90%"
                          )
                        ),
                        conditionalPanel(
                          "input.network == 'retweet_screen_name'",
                          checkboxInput(
                            "quoted",
                            "Include quoted",
                            width = "90%",
                            value = TRUE
                          )
                        )
                  ),
                  div(style = "width:29%; margin-left:5px;",
                      conditionalPanel(
                            "input.network != 'retweet_screen_name'",
                            checkboxInput(
                              "include_retweets",
                              "RTs",
                              value = TRUE
                            )
                          )
                  )
              )
            ), 
            # ---
            col_12(
              checkboxInput(
                "include_rts",
                "Include retweets",
                TRUE
                ),
              tippy_this("include_rts", "Whether to include retweets")
            ),
            # ---
            col_12(
              div(style = "display:flex;",
                  div(style = "width:39%;",
                      actionButton("Twit_options_btn", "", icon = shiny::icon("plus", verify_fa = FALSE)) ,
                      tippy_this("Twit_options_btn", "More options")
                  ),
                  div(style = "width:39%;",
                      actionButton(
                        "submit", 
                        "Search", 
                        icon = shiny::icon("search", verify_fa = FALSE), 
                        width = "120px", 
                        class = "btn btn-primary"
                        )
                  )
              )
            ),
            # ---
            
            col_12(
              uiOutput("node_search_ui")
            ),
            # ---
            col_12(
              div(
                id = "Twit_options_tools",
                style = "display:none;",
                
                
                fluidRow(
                  column(12, h5("More Options") ),
                  
                  # ----
                  column(12,
                         selectInput(
                           "edges_colour",
                           "Edges Colour",
                           choices = c(
                             "None" = "none",
                             "Sentiment" = "sentiment",
                             "# tweets" = "size"
                           ),
                           selected = "None",
                           width = "100%"
                         )
                  ),
                  # -----------
                  # column(12, h5("FILTER")   ),
                  # ---
                  column(12, 
                         checkboxInput(
                           "delete_nodes",
                           "Delete Nodes", 
                           value = FALSE
                         ),
                         tippy_this("delete_nodes", "Tick and click on nodes to delete them")
                  ),
                  
                  # ----
                  column(12,
                         sliderInput(
                           "node_size",
                           "Filter node by size",
                           width = "97%",
                           min = 3,
                           max = 17,
                           value = c(3, 17)
                         )
                  ),
                  
                  # --- 
                  column(6, checkboxInput("append", "Append")
                  ),
                  column(12,
                         textInput("longitude", "Longitude", value = "", width = "100%")
                  ),
                  column(12,
                         textInput("latitude", "Latitude", value = "", width = "100%")
                  ),
                  column(6,
                         textInput("radius", "Radius", value = "", width = "100%")
                  ),
                  column(6, 
                         selectInput("metric", "Metric", choices = c("Kilometer" = "km", "Miles" = "mi"))
                  )
                ) # fluidrow
              ) # option btn div 
            ) # 12
            # -------
            ) # tagList
           
          ) # bs4Card
        ), # col_3
        # -------------------------------------------------------------------- #
        col_10(style = "font-size: 12px;",
           
          bs4Card(
            id = "id_Analytics_Sentiment_Twitter_Results",
            title = "Analytics",
            #footer = "Help", 
            status = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            collapsed = FALSE,
            closable = FALSE,
            label = NULL,
            width = 12,
            height = "820px",
            sidebar = boxSidebar(
              tagList(
              ),
              id = "id_Analytics_Sentiment_Twitter_Results_sidebar",
              width = 60,
              background = "#f9f9fa",
              startOpen = FALSE,
              icon = shiny::icon(configs$icons$cogs, verify_fa = FALSE),
              easyClose = TRUE
            ),
        
            fluidRow(
              col_12(style = "margin-top:-5px;",
            bs4Dash::tabsetPanel( # tabBox
              # id = "id_Analytics_Sentiment_Twitter_Results_tabBox",
              #title = NULL,
              # width = 12,
              #collapsible = FALSE,
              #maximizable = TRUE,
              selected = "Graph",
              #status = "primary",
              #solidHeader = FALSE,
              type = "tabs", # "tabs",
              side = "left",
              vertical = F,
              tabPanel("Graph", 
                           col_12(
                             div(style = "display:flex;",
                                 div(style = "width:59%;",
                                     shinycustomloader::withLoader(
                                       sigmajs::sigmajsOutput("graph", height = "600px") #,
                                       # type = "html" ,   loader = "loader9"
                                     )
                                 ),
                                 div(style = "width:39%;",
                                     shinyjqui::jqui_draggable(
                                       htmlOutput(
                                         "display", # style = "position:absolute;z-index:99; left:20px; top:20px;"
                                       )
                                     )
                                 )
                             )
                           ) 
                       ),
              tabPanel("Stats",
                           col_12(
                             shinycustomloader::withLoader(uiOutput("trend_text")),
                             shinycustomloader::withLoader(plotOutput('trendline', height = "300px", width = "100%"))
                           ),
                           col_12(
                             fluidRow(
                               column(3, uiOutput("n_nodes")),
                               column(3, uiOutput("n_edges")),
                               column(3, uiOutput("n_tweets"))
                             )
                           ),
                           col_12(
                             fluidRow(
                               column(6,
                                      shinycustomloader::withLoader(plotOutput("Tweet_Analytics_plot_1",
                                                                               height = "350px", width = "100%"))
                               ),
                               column(6,
                                      shinycustomloader::withLoader(plotOutput("Tweet_Analytics_plot_2",
                                                                               height = "350px", width = "100%"))
                               )
                             )
                           )
                       ),
              tabPanel("Table", 
                       col_12(
                         shinycustomloader::withLoader(DTOutput('Tweet_DTtbl'))
                         )
                    )
            ) # tabsetPanel
              ) # col 12
            ) # fluidRow
            
            #     col_12(
            #       # fluidRow(
            #       # uiOutput("selected_headline"),
            #       # uiOutput("selected_source"),
            #       # fluidRow(
            #       #   column(6, uiOutput("source_indegree")),
            #       #   column(6, uiOutput("source_outdegree"))
            #       #   ),
            #       # fluidRow(
            #       #   column(6, uiOutput("source_pagerank")),
            #       #   column(6, uiOutput("source_eigen"))
            #       #   ),
            #       # uiOutput("arrow_down"),
            #       # uiOutput("selected_target"),
            #       # fluidRow(
            #       #   column(6, uiOutput("target_indegree")),
            #       #   column(6, uiOutput("target_outdegree"))
            #       #   ),
            #       # fluidRow(
            #       #   column(6, uiOutput("target_pagerank")),
            #       #   column(6, uiOutput("target_eigen"))
            #       #   )
            #       # )
            #     )
           
          ) # bs4Card
        
        )
      ) # fluidRow all 
      
   
    #   fluidRow(
    # div(style = "display:flex;",
    #     
    #     # --- Side bar
    #     
    #     div(style = "width:20%; flex-wrap: wrap; margin-left:0px; margin-right:5px",
    #      
    #         wellPanel(# style = "border:1px solid #eee;",  
    #                   style =  "border:1px solid #eee; ", # height: 900px
    #         div(class = "row",
    #           h5("Twitter Sentiment Analytics", 
    #           style = "font-weight: bold;
    #           color: #0096FF;  text-align: center;
    #           margin-top : 1px; ") 
    #            ),
    #         
    #         div(class = "row", style = "margin-top: 10px;",
    #           textInput("q", 
    #                     label = NULL,
    #                     # h5("Set Parameters"),
    #                     width = "97%",
    #                     placeholder = "  Enter search query here. "),
    #     
    #           tags$style(HTML("#q.form-control{color: blue;  
    #                           border: 0.5px solid #c6c7c8; border-radius:5px}")),
    #           tippy_this("q", "Your search query"),
    #         ), 
    #         # ---
    #         div(class = "row", style = "margin-top: 10px;",
    #             selectInput(
    #                    "network",
    #                    "Network Type",
    #                    choices = c(
    #                      "Retweets" = "retweet_screen_name",
    #                      "Hashtags" = "hashtags",
    #                      "Conversations" = "mentions_screen_name"
    #                    ),
    #                    selected = "Retweets",
    #                    width = "97%"
    #                  ),
    #                  tippy_this("network", "Type of network to draw")
    #             ),
    #         # ----
    #         div(class = "row", style = "margin-top: 10px;", 
    #           selectInput(
    #             "type",
    #             "Type",
    #             choices = c(
    #               "Recent" = "recent",
    #               "Mixed" = "mixed",
    #               "Popular" = "popular"
    #             ),
    #             selected = "recent",
    #             width = "97%"
    #           ),
    #           tippy_this("type", "Type of tweets to fetch")
    #         ),
    #         # --- 
    #         div(class = "row", style = "margin-top: 10px;",
    #         column(6,
    #                selectInput(
    #                  "size",
    #                  "Nodes Size",
    #                  choices = c(
    #                    "# tweets" = "n_tweets",
    #                    "In-degree" = "in_degree",
    #                    "Out-degree" = "out_degree",
    #                    "Closeness" = "closeness",
    #                    "Pagerank" = "pagerank",
    #                    "Authority" = "authority",
    #                    "Eigen" = "eigen"
    #                  ),
    #                  width = "97%"
    #                ),
    #                tippy_this("size", "Variable to size nodes")
    #         ),
    #         # ---
    #         column(6,
    #                selectInput(
    #                  "colour",
    #                  "Nodes Colour",
    #                  choices = c(
    #                    "Cluster" = "group",
    #                    "# tweets" = "n_tweets",
    #                    "Components" = "components",
    #                    "In-degree" = "in_degree",
    #                    "Out-degree" = "out_degree",
    #                    "Closeness" = "closeness",
    #                    "Pagerank" = "pagerank",
    #                    "Authority" = "authority",
    #                    "Eigen" = "eigen",
    #                    "Type" = "type"
    #                  ),
    #                  selected = "Cluster",
    #                  width = "97%"
    #                ),
    #                tippy_this("colour", "Variable to colour nodes")
    #         )
    #         ),
    #         # ----
    #         div(class = "row", style = "margin-top: 10px;",
    #             sliderInput(
    #               "Twit_number_tweets",
    #               label = "Number of tweets",
    #               min = min_tweets,
    #               max = max_tweets,
    #               value = min_tweets,
    #               step = 100,
    #               width = "97%"
    #             ),
    #             tippy_this("Twit_number_tweets", "Number of tweets to fetch")
    #         ),
    #         # ----
    #         div(class = "row", style = "margin-top: 10px;",
    #             column(8,
    #                conditionalPanel(
    #                  # "input['networks-network'] != 'retweet_screen_name'",
    #                  "input.network != 'retweet_screen_name'",
    #                  checkboxInput(
    #                    "comentions",
    #                    "Co-mentions",
    #                    width = "97%"
    #                  )
    #                ),
    #                conditionalPanel(
    #                  # "input['networks-network'] == 'retweet_screen_name'",
    #                  "input.network == 'retweet_screen_name'",
    #                  checkboxInput(
    #                    "quoted",
    #                    "Include quoted",
    #                    width = "97%",
    #                    value = TRUE
    #                  )
    #                )
    #                ),
    #             column(4,
    #             conditionalPanel(
    #               # "input['networks-network'] != 'retweet_screen_name'",
    #               "input.network != 'retweet_screen_name'",
    #               checkboxInput(
    #                 "include_retweets",
    #                 "RTs",
    #                 value = TRUE
    #               )
    #             )
    #             )
    #         ),
    #         # --- 
    #         div(class = "row", style = "margin-top: -5px;",
    #             column(12,
    #               checkboxInput(
    #                 "include_rts",
    #                 "Include retweets",
    #                 TRUE,
    #                 width = "97%"
    #               ),
    #               tippy_this("include_rts", "Whether to include retweets")
    #         )
    #         ),
    #         
    #         # ---
    #         div(class = "row", style = "margin-top: 10px;",
    #           div(class = "column", style = "float: left; width: 20%;", 
    #           actionButton("Twit_options_btn", "", icon = shiny::icon("plus", verify_fa = FALSE)) ,
    #           tippy_this("Twit_options_btn", "More options")
    #           ),
    #           # ---
    #           div(class = "column", style = "float: left; width: 20%;",
    #               shinyjs::hidden(
    #                 actionButton(
    #                   "hide_tweet",
    #                   "",
    #                   icon = shiny::icon("times", verify_fa = FALSE),
    #                   class = "btn-danger"
    #                 )
    #               )
    #           ),
    #           # --
    #           div(class = "column", style = "float: right; width: 60%;", 
    #             actionButton(
    #               "submit", 
    #               "Search", 
    #               icon = shiny::icon("search", verify_fa = FALSE), 
    #               width = "97%", 
    #               class = "btn btn-primary"
    #             )
    #           )
    #         ),
    #         
    #         div(class = "row", style = "margin-top: 10px;",
    #             column(12, uiOutput("node_search_ui"))
    #         ),
    #         # -------
    #         div(
    #           id = "Twit_options_tools",
    #           style = "display:none;",
    #           
    #           
    #       fluidRow(
    #         column(12, h5("More Options") ),
    #      
    #         # ----
    #         column(12,
    #                selectInput(
    #                  "edges_colour",
    #                 "Edges Colour",
    #                 choices = c(
    #                   "None" = "none",
    #                   "Sentiment" = "sentiment",
    #                   "# tweets" = "size"
    #                 ),
    #                 selected = "None",
    #                 width = "100%"
    #               )
    #               ),
    #         # -----------
    #         # column(12, h5("FILTER")   ),
    #         # ---
    #         column(12, 
    #                checkboxInput(
    #                     "delete_nodes",
    #                     "Delete Nodes", 
    #                     value = FALSE
    #                     ),
    #                   tippy_this("delete_nodes", "Tick and click on nodes to delete them")
    #                 ),
    #       
    #       # ----
    #       column(12,
    #               sliderInput(
    #                 "node_size",
    #                 "Filter node by size",
    #                 width = "97%",
    #                 min = 3,
    #                 max = 17,
    #                 value = c(3, 17)
    #               )
    #              ),
    #       
    #       # --- 
    #         column(6, checkboxInput("append", "Append")
    #                ),
    #         column(12,
    #           textInput("longitude", "Longitude", value = "", width = "100%")
    #         ),
    #         column(12,
    #           textInput("latitude", "Latitude", value = "", width = "100%")
    #         ),
    #         column(6,
    #           textInput("radius", "Radius", value = "", width = "100%")
    #         ),
    #         column(6, style = "margin-top: 10px;",
    #           selectInput("metric", "Metric", choices = c("Kilometer" = "km", "Miles" = "mi"))
    #         )
    #       ) # fluidrow
    #     ) # option btn div
    #     )  # wellPanel  side
    # ), # side div
    # 
    # 
    # # mainbar
    # 
    # div(style = "width:80%;",
    #     
    #     wellPanel(style =  "border:1px solid #eee; margin-left:5px; ", #  height: 1000px;
    #       
    #       div(id = "Analytics_Sentiment_Twitter_tabsetPanel",
    #         style = "display: none; margin-top:-10px;",
    #         tabsetPanel(
    #           tabPanel("Nodes", 
    #                    mainPanel(
    #                    div(class = "row",  style = "display:flex; margin-left:10px; margin-top:10px;",
    #                        div(class = "column", style = "float: left; width: 90%;", 
    #                            
    #                            # wellPanel( style = "border:1px solid #eee;", 
    #                              shinycustomloader::withLoader(
    #                                sigmajs::sigmajsOutput("graph", height = "600px"),
    #                                type = "html" ,
    #                                loader = "loader9"
    #                              )
    #                            #)
    #                        ),
    #                         div(class = "column", style = "float: right; width: 10%;", 
    #                        
    #                            #wellPanel(style = "border:1px solid #eee;", 
    #                              shinyjqui::jqui_draggable(
    #                                htmlOutput(
    #                                  "display", # style = "position:absolute;z-index:99; left:20px; top:20px;"
    #                                )
    #                              )
    #                            #)
    #                        )
    #                    )
    #                    ) # mainPanel
    #           ),
    #           # ---
    #           tabPanel("Stats", 
    #                    br(),
    #                    fluidRow(
    #                      uiOutput("trend_text"),
    #                      plotOutput('trendline', height = "300px", width = "100%")
    #                      # reactrendOutput("trendline", width = "100%")
    #                      
    #                    ),
    #                    fluidRow(
    #                      column(3, uiOutput("n_nodes")),
    #                      column(3, uiOutput("n_edges")),
    #                      column(3, uiOutput("n_tweets"))
    #                             ),
    #                    fluidRow(
    #                      column(6,
    #                             plotOutput("Tweet_Analytics_plot_1")
    #                      ),
    #                      column(6,
    #                             plotOutput("Tweet_Analytics_plot_2")
    #                      )
    #                    )
    #                    # fluidRow(
    #                    # uiOutput("selected_headline"),
    #                    # uiOutput("selected_source"),
    #                    # fluidRow(
    #                    #   column(6, uiOutput("source_indegree")),
    #                    #   column(6, uiOutput("source_outdegree"))
    #                    #   ),
    #                    # fluidRow(
    #                    #   column(6, uiOutput("source_pagerank")),
    #                    #   column(6, uiOutput("source_eigen"))
    #                    #   ),
    #                    # uiOutput("arrow_down"),
    #                    # uiOutput("selected_target"),
    #                    # fluidRow(
    #                    #   column(6, uiOutput("target_indegree")),
    #                    #   column(6, uiOutput("target_outdegree"))
    #                    #   ),
    #                    # fluidRow(
    #                    #   column(6, uiOutput("target_pagerank")),
    #                    #   column(6, uiOutput("target_eigen"))
    #                    #   )
    #                    # )
    #                    ),
    #           # ----
    #           tabPanel("Table", 
    #                    br(),
    #                    DTOutput('Tweet_DTtbl')
    #                    ),
    #           # ----
    #           tabPanel("Analytics", 
    #                   
    #           )
    #           )
    #         
    #     )
    #     
    #     # div(style = "height: 80%;",
    #     # shinycustomloader::withLoader(
    #     #   sigmajs::sigmajsOutput("graph", height = "80vh"),
    #     #   type = "html" ,
    #     #   loader = "loader9"
    #     # )
    #     # ),
    #     # div(style = "height: 10%;",
    #     # shinyjqui::jqui_draggable(
    #     #         htmlOutput(
    #     #           "display"  # , # style = "position:absolute;z-index:99;left:20px;top:70px;"
    #     #         )
    #     #         )
    #     # )
    #     
    #     
    #     ) # wellPanel
    # ) # main div
    # ) # page div
    #   )
  }) # end UI
  

  # -------------------------------------------------------------------------- #
  # --- Server

  shinyjs::hide("id_Analytics_Sentiment_Twitter_Results")
  shinyjs::hide("trendline")
  shinyjs::hide("trend_text")
  shinyjs::hide("n_nodes")
  shinyjs::hide("n_edges")
  shinyjs::hide("n_tweets")
  shinyjs::hide("Tweet_Analytics_plot_1")
  shinyjs::hide("Tweet_Analytics_plot_2")

  
  # shinyjs::hide("id_Analytics_Sentiment_Twitter_Results_tabBox")
  
  shinyjs::hide("Twit_options_tools")
  
  observeEvent(input$Twit_options_btn, {
    shinyjs::toggle("Twit_options_tools")
  })
  
  
  # -------------------------- #
  # graph
  graph_test <- reactive({
    
    # cat("reactive graph \n")
    
    if(!is.null(tweets$df)){
      
      tw <- tweets$df %>%
        dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets)) %>% .compute_sentiment()
      
      # tw <- tweets$df %>% 
      #   dplyr::filter(retweeted %in% c(FALSE, input$include_retweets)) %>% 
      #   .compute_sentiment()
      
      # cat(paste0(dim(tw)))
      
      # edges <- tw %>% gt_co_edges(!!sym(input$network)) # "hashtags"
      
      if(isTRUE(input$comentions) && input$network %in% c("hashtags", "mentions_screen_name")){
        edges <- tw %>% gt_co_edges(!!sym(input$network))
      } else{
        edges <- tw %>%
          gt_edges(screen_name, !!sym(input$network), sentiment) %>%
          gt_preproc_edges(.preproc)
        
        if(isTRUE(input$quoted) && input$network == "retweet_screen_name"){
          edges <- edges %>%
          gt_edges_bind(screen_name, quoted_screen_name)
        } # retweet_screen_name
      } # if mentions_screen_name
      
      
      graph <- edges %>%
        gt_nodes() %>%
        gt_collect()
      
       
      graph <- tbl_graph(
        nodes = graph$nodes,
        edges = graph$edges
      ) %>%
        activate(nodes) %>%
        dplyr::mutate(
          name = nodes,
          id = name,
          label = name,
          n_tweets = n,
          out_degree = centrality_degree(mode = "out"),
          in_degree = centrality_degree(mode = "in"),
          authority = centrality_authority(),
          pagerank = centrality_pagerank(),
          closeness = centrality_closeness(),
          eigen = centrality_eigen(),
          components = group_components(type = "weak"),
          group = group_walktrap()
        ) %>%
        igraph::as_data_frame("both")
      
      
      edges <- graph$edges %>%
        dplyr::mutate(
          id = 1:n(),
          source = from,
          target = to,
          size = n,
          type = "arrow"
        ) %>%
        dplyr::select(-one_of("to", "from"))

      nodes <- graph$vertices %>%
        dplyr::mutate(
          group = as.factor(group),
          components = as.factor(components)
        ) %>%
        dplyr::select(-one_of("n", "nodes"))

      # session$sendCustomMessage("unload", "") # stop loading

      graph_test2 <<- list(
        nodes = nodes,
        edges = edges
      )
      return(list(nodes = nodes,edges = edges ) )
      
    } # if
    
  }) #  graph_test <- reactive({
  
   # -----
  tweets <<- reactiveValues(df = NULL) # reactiveVal(df = NULL)
  
  
  # --- btn submit query search
  observeEvent(input$submit, {
    
    if(AdminRole()){
      
    #  cat("Admin \n")
    # input = list(); input$q = "bitcoin"; input$Twit_number_tweets = 100;  input$include_rts = TRUE; input$type = "recent"
      
    if(input$q != ""){
      
      geocode <- NULL
      
      shinyjs::show("id_Analytics_Sentiment_Twitter_Results")
      shinyjs::show("trendline")
      shinyjs::show("trend_text")
      shinyjs::show("n_nodes")
      shinyjs::show("n_edges")
      shinyjs::show("n_tweets")
      shinyjs::show("Tweet_Analytics_plot_1")
      shinyjs::show("Tweet_Analytics_plot_2")
      
      if(input$longitude != "" && input$latitude != "" && input$radius != ""){
        geocode <- paste(input$longitude, input$latitude, paste0(input$radius, input$metric), sep = ",")
      }
      
      # Query from Twitter
      tweets$df <- rtweet::search_tweets(
        input$q,
        n = input$Twit_number_tweets,
        type = input$type ,
        include_rts = input$include_rts,
        geocode = geocode ,
       token = .get_token()
      )
      
      #colnames(tweets)
      #x = users_data(tweets)
      
      twdf <<- tweets$df
      
      if(isTRUE(input$append)){
          tw <- rtweet::search_tweets(
            input$q,
            n = input$Twit_number_tweets,
            type = input$type,
            include_rts = input$include_rts,
            geocode = geocode,
            token = .get_token()
          )
          tweets$df <<- rbind.data.frame(tweets$df, tw) 
      } else{ 
        tweets$df <<- tweets$df 
        } # else if isTRUE
    
    # --------------------------------------- #
    output$graph <- sigmajs::renderSigmajs({
      # cat("output$graph \n")
      g <<- graph_test()
      
      # DT table in tabset
      if(nrow(g$nodes) >= 1){
        
      output$Tweet_DTtbl <- renderDT({
        Func_DT_Table (g$nodes, 
                       pageLength_Set = 25,
                       scrollY = '35vmax', 
                       type = "B",
                       info = TRUE, 
                       filter = "none",
                       fontSize = "12px")
      })
      } # if >= 1
      
      nodes <- g$nodes
      nodes <- .color_nodes(nodes, x = "group") # toPaletteFunc
      nodes <- .size_nodes(nodes, "n_tweets")
      edges <- g$edges

      sigmajs::sigmajs(type = "webgl") %>%
        sigmajs::sg_nodes(nodes, id, label, size, color, group) %>%
        sigmajs::sg_edges(edges, id, source, target, type, size) %>%
        sigmajs::sg_force(slowDown = 4) %>%
        sigmajs::sg_kill() %>%
        sigmajs::sg_drag_nodes() %>%
        sigmajs::sg_force_stop(2500) %>%
        sigmajs::sg_layout() %>%
        sigmajs::sg_settings(
          minArrowSize = 1,
          batchEdgesDrawing = TRUE,
          edgeColor = "default",
          defaultEdgeColor = .get_edge_color(),
          font = .get_font(),
          labelThreshold = 9999
        ) %>% sg_events("clickNode")
      
    })
    } else{ # if q
        sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = "Select a query",
          type = "warning"
        )
    } # if q
    
    
    output$node_search_ui <- renderUI({
      ch <- graph_test()$nodes %>% pull(label)

      div(style = "margin-top: 15px;",
        selectizeInput(
          "node_searched",
          "Search for a node",
          multiple = FALSE,
          choices = ch,
          width = "100%"
        )
      )
    })
    
    } else{ # Admin
      sendSweetAlert(
        session = session,
        title = "Warning !!!",
        text = "This service is only for Admin users",
        type = "warning"
      )

    } # Admin
    
  }) # observeEvent(input$submit
  

  # ---------------------------------------- #
  nodes <- data.frame()
  
  nodes_clicked <- reactive({ # # for stats
    # cat("nodes_clicked \n")
 
    if(!is.null(input$graph_click_nodes)){
      nodes <<- rbind.data.frame(input$graph_click_nodes, nodes) %>% 
        slice(1:2)}
    return(nodes)
  }) # for stats
  
  # display if chick on node
  
  observeEvent(input$graph_click_node, {
    
    test_graph_click_node <<- input$graph_click_node
    
    # cat("graph_click_node observeEvent \n")
    
    cat(input$graph_click_node$label); cat("\n")
    
    node_clicked <<- input$graph_click_node$label
    # ns <<- session$ns
    
    if(isTRUE(input$delete_nodes)){  # 
      sigmajs::sigmajsProxy("graph") %>%
      sigmajs::sg_drop_node_p(id = input$graph_click_node$id)
    } else {
      sigmajs::sigmajsProxy("graph") %>% 
        sigmajs::sg_filter_neighbours_p(node = input$graph_click_node$id, "neighbours-filter")
      shinyjs::show("display")
      # shinyjs::show("hide_tweet")
    }
      
  
  output$display <- renderText({
    
    input$graph_click_node
    
    cat("renderText display \n")
    # cat( input$graph_click_node)
    
    user <<- input$graph_click_node$label
    user <- gsub("#", "", user)
    
    # print(user)
    # user = "ki"
    
    tw <- ""
    # tq <<- tweets$df
    # tweets$df <- tq
    
    if(!is.null(input$graph_click_node$label) & !isTRUE(input$delete_nodes) ){ 
      
      # cat("!is.null(input$graph_click_node$label) \n")
      #input$network = "hashtags"
      
      cat("is_retweet \n")
      
      tw <- tweets$df %>%
        dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets)) %>%   # input$include_retweets # TRUE
        dplyr::select(
          status_id,
          screen_name,
          retweet_count,
          v2 = !!sym(input$network) # "hashtags"
        ) %>%
        tidyr::separate_rows(v2) %>%
        dplyr::mutate(
          screen_name = tolower(screen_name),
          v2 = tolower(v2)
        ) 
      
      src <- tw %>%
        dplyr::filter(screen_name == user) %>%
        dplyr::arrange(-retweet_count)

      if(nrow(src) >= 1){
        tw <- src %>%
          dplyr::slice(1) %>%
        .get_tweet()
      }else{
        tw <- tw %>%
        dplyr::filter(v2 == user) %>%
          dplyr::arrange(-retweet_count) %>%
          dplyr::slice(1) %>%
        .get_tweet()
      }
      #View(src)
      # View(tw)
    }
    
    if(inherits(tw, "error")){
      tw <- ""
      shinyjs::hide("display")
    }
    
    # ----
      showModal(modalDialog(
        title = "Tweet!",
        fluidRow(
          column(10, offset = 2, HTML(tw)   )
        ),
        easyClose = TRUE,  size = "m", fade = TRUE,
        footer = paste0("User: ", input$graph_click_node$label)
      )) 
    
    return(tw)
    }) # output$display <- renderText({
  
  }) # observeEvent(input$graph_click_node, {

  # ----------------------------------------------- #
  
  trend <- reactive({
    
    if(!is.null(tweets$df)){

    .get_trend <- function(x = "%Y-%m-%d"){
     # tweets() %>%
        tweets$df %>%
        dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets)) %>% 
        dplyr::mutate(
          created_at = format(created_at, x)
        ) %>%
        dplyr::count(created_at) %>%
        dplyr::pull(n) %>%
        list(
          trend = .,
          format = x
        )
    }
    
    trend <- .get_trend()
    
    if(length(trend$trend) < 4)
      trend <- .get_trend("%Y-%m-%d %H")
    
    if(length(trend$trend) < 3)
      trend <- .get_trend("%Y-%m-%d %H:%M")
    
    if(length(trend$trend) < 2)
      trend <- .get_trend("%Y-%m-%d %H:%M:%S")
    
    
    return(trend)
    }
  })
  
  
  output$trend_text <- renderUI({
    if(!is.null(tweets$df)){
      p(strong("Tweets"), .get_time_scale(trend()$format))
    }
    
  })
  
  # trendline 
  # there is no package called ‘reactrend’
  output$trendline <- renderReactrend ({ #
    if(!is.null(tweets$df)){
      # cat("trendline \n")
  
      trend()$trend %>%
        reactrend(
          draw = TRUE,
          gradient = .get_pal(),
          smooth = TRUE,
          stroke_width = 2,
          line_cap = "round"
        )
    }
  })
  
  output$trendline <- renderPlot({
    if(!is.null(tweets$df)){
      test_trend <<- trend()
  
      dff = data.frame(x = 1:length(test_trend$trend), y = test_trend$trend)
      ggplot(data=dff, aes(x=x, y=y)) +
        geom_line( color = "blue")+ scale_color_brewer(palette="Dark2") +
        geom_point( color = "blue") + scale_color_grey() + theme_minimal()
    }
  })
  
  # ----------------------------------------------- #
  output$n_nodes <- renderUI({
    if(!is.null(tweets$df)){
    p(
      strong("Nodes:"),
      prettyNum(
        # nrow(graph()$nodes),
        nrow(graph_test()$nodes),
        big.mark = ","
      )
    )
    }
  })
  
  output$n_edges <- renderUI({
    if(!is.null(tweets$df)){
    p(
      strong("Edges:"),
      prettyNum(
       # nrow(graph()$edges),
        nrow(graph_test()$nodes),
        big.mark = ","
      )
    )
    }
  })
  
  output$n_tweets <- renderUI({
    if(!is.null(tweets$df)){
    p(
      strong("Tweets:"),
      # prettyNum(
      #   nrow(tweets() %>% dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets))),
      #   big.mark = ","
      # )
      prettyNum(
        nrow(tweets$df %>% dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets))),
        big.mark = ","
      )
    )
    }
  })
  
  output$target_pagerank <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 2)
      
      if(!length(sel)){return("")}
      
      span(
        strong("Pagerank"),
        graph_test()$nodes %>% 
          dplyr::filter(label == sel) %>% 
          dplyr::pull(pagerank) %>% 
          round(.3)
      )
    }
    # graph_test2$nodes %>% 
    #   dplyr::filter(label == "90percenters") %>% 
    #   pull(pagerank) %>% 
    #   round(.3)
  })
  
  output$target_eigen <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 2)
      
      if(!length(sel)){return("")}
      
      span(
        strong("Eigen"),
        graph_test()$nodes %>% 
          dplyr::filter(label == sel) %>% 
          dplyr::pull(eigen) %>% 
          round(.3)
      )
    }
  })
  
  output$selected_headline <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 1)
      
      if(!is.null(sel)){
        h5(
          "SELECTED NODES"
        )
      }
    }
  })
  
  output$selected_source <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 1)
      
      print(paste0("sel: ", sel))
      
      if(is.null(sel)){
        p(
          "Select nodes to see their network metrics",
          class = "text-warning"
        )}else{
        h5(
          tags$a(
            .get_random_icon(),
            href = paste0("https://twitter.com/", sel),
            target = "_blank"
          ),
          sel
        )
        }
    }
    
  })
  
  output$arrow_down <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 2)
      
      if(!length(sel)){ ""} else{
        shiny::icon("chevron-down", class = "fa-lg center_arrow")
      }
    }
  })
  
  output$selected_target <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 2)
      
      if(!length(sel)){
        span("")
      }else{
        h5(
          tags$a(
            .get_random_icon(),
            href = paste0("https://twitter.com/", sel),
            target = "_blank"
          ),
          sel
        )
      }
    }
  })
  
  output$source_indegree <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 1)
      
      if(is.null(sel)){  return("") }
      
      span(
        strong("In-degree"),
        graph_test()$nodes %>% 
          dplyr::filter(label == sel) %>% 
          dplyr::pull(in_degree) %>% 
          round(.3)
      )
    }
  })
  
  output$source_outdegree <- renderUI({
    if(!is.null(tweets$df)){
      sel <<- .slice_node(nodes_clicked(), 1)
      
      if(is.null(sel)){  return("") }
      
      span(
        strong("Out-degree"),
        graph_test()$nodes %>% 
          dplyr::filter(label == sel) %>% 
          dplyr::pull(out_degree) %>% 
          round(.3)
      )
    }
  })
  
  # output$source_pagerank <- renderUI({
  #   if(!is.null(tweets$df)){
  #     sel <<- .slice_node(nodes_clicked(), 1)
  #     
  #     if(is.null(sel)){  return("") }
  #     
  #     span(
  #       strong("Pagerank"),
  #       graph_test()$nodes %>% 
  #         dplyr::filter(label == sel) %>% 
  #         pull(pagerank) %>% 
  #         round(.3)
  #     )
  #   }
  # })
  
  # output$source_eigen <- renderUI({
  #   if(!is.null(tweets$df)){
  #     sel <<- .slice_node(nodes_clicked(), 1)
  #     
  #     if(is.null(sel)){  return("") }
  #     
  #     span(
  #       strong("Eigen"),
  #       graph_test()$nodes %>% 
  #         dplyr::filter(label == sel) %>% 
  #         pull(eigen) %>% 
  #         round(.3)
  #     )
  #   }
  # })
  
  # output$target_indegree <- renderUI({
  #   if(!is.null(tweets$df)){
  #     sel <<- .slice_node(nodes_clicked(), 2)
  #     
  #     if(!length(sel)){  return("") }
  #     
  #     span(
  #       strong("In-degree"),
  #       graph_test()$nodes %>% 
  #         dplyr::filter(label == sel) %>% 
  #         pull(in_degree) %>% 
  #         round(.3)
  #     )
  #   }
  # })
  
  # output$target_outdegree <- renderUI({
  #   if(!is.null(tweets$df)){
  #     sel <<- .slice_node(nodes_clicked(), 2)
  #     
  #     if(!length(sel)){  return("") }
  #     
  #     span(
  #       strong("Out-degree"),
  #       graph_test()$nodes %>% 
  #         dplyr::filter(label == sel) %>% 
  #         pull(out_degree) %>% 
  #         round(.3)
  #     )
  #   }
  # })
  
  # ----------------------------------------------- #
  # observeEvent(input$graph_click_stage, {
  #   
  #   cat("graph_click_stage \n")
  #   
  #   shinyjs::hide("display")
  #   shinyjs::hide("hide_tweet")
  #   
  #   # ns <- session$ns
  #   
  #   sigmajs::sigmajsProxy("graph") %>% 
  #     sigmajs::sg_filter_undo_p("neighbours-filter")
  # })
  
  # ----------------------------------------------- #
  
  notification <- NULL
  
  observeEvent(input$delete_nodes, {
    if(isTRUE(input$delete_nodes)){
      notification <<- showNotification(
        "Click a node to delete it.",
        duration = NULL,
        type = "error",
        closeButton = FALSE
      )
    } else {
      if (!is.null(notification)) removeNotification(notification)
      notification <<- NULL
    }
  })
  # ----------------------------------------------- #
  
  output$Tweet_Analytics_plot_1 <- renderPlot({
    if(!is.null(tweets$df)){
    Func_Twitter_Analytics(input, output, session, 
                           df = tweets$df,
                           type = "UserLevel") # WordLevel
    }
  })
  
  output$Tweet_Analytics_plot_2 <- renderPlot({
    if(!is.null(tweets$df)){
    Func_Twitter_Analytics(input, output, session, 
                           df = tweets$df,
                           type = "NegPos")
    }
  })
} # Func_Twitter_Analysis


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#' Twitter sentiment Analysis and data mining
#'
#' @param Shinyinputs c(input, output, session)
#' @return 
#' @export
#' @examples
#'
# Func_Twitter_Analysis <- function(input, output, session, ...){
#   
#   library(tidytext)
#   library(rtweet)
#   library(igraph)
#   library(shinycustomloader)
#   library(shinyjqui)
#   library(sigmajs)
#   library(particlesjs) # remotes::install_github("dreamRs/particlesjs")
#   library(tippy)
#   library(reactR)
#   library(graphTweets)
#   library(tidygraph)
#   # Make sure to install any packages listed below that you don't already have on your system:
#   library("reactable")
#   library("glue")
#   library("stringr")
#   library("httpuv")
#   library("dplyr")
#   library("purrr")
#   
#   # -- UI
#   output$Analytics_Sentiment_TwitterExplorer_UI <- renderUI({
#     
#     chirp()
#   })
#   # ---
#   
# 
#   
#   # create token named "twitter_token"
#   # twitter_token <- create_token(
#   #   app = "IQAT",
#   #   consumer_key = "KGRb326eLosOeoXwANkbjjqRp",
#   #   consumer_secret = "DWSS8XvM1UHDjxy5W65vNtvTJ0nXiM4ZyIeESggdyDndhrYOxQ",
#   #   access_token = "1740657270-IYlXaLf4cQL4jKfiDB2R2XgdmKobLHli45bFvuS",
#   #   access_secret = "d5ler1l1NVk0Eho4cPQly3meTIivVvqiYXNcFlt571Ofx"
#   #   )
#   
#   # consumer_key = "KGRb326eLosOeoXwANkbjjqRp"
#   # Bearer Token
#   # AAAAAAAAAAAAAAAAAAAAANGHewEAAAAAvO7%2F9U8DzQFTBiENiPs2l8KEx5c%3D8B89Rwd6qJRQG0iXSfYfEAPKFFPwugoYJAWsHE1jvZ5U6oFUQr
#   
#   # Access Token
#   # "1740657270-IYlXaLf4cQL4jKfiDB2R2XgdmKobLHli45bFvuS"
#   
#   # Access Token Secret
#   # "d5ler1l1NVk0Eho4cPQly3meTIivVvqiYXNcFlt571Ofx"
#   
#   # Configure variables: number of tweets to download and hashtag search query
#   # num_tweets_to_download <- 500
#   # hashtag_to_search <- "#forex"
#   # q = "forex"
#   
#    # df_tweets <- search_tweets(q = "forex",  
#    #                            type = "recent",
#    #                            include_rts = TRUE,
#    #                            geocode = NULL, 
#    #                            n = num_tweets_to_download # , 
#    #                            # token = twitter_token
#    #                            )
#   
# 
# 
#   # Code to actually search for tweets
#   
#   # rtweet_token <- tryCatch(
#   #   rtweet::create_token(
#   #     app = "IQAT",
#   #     consumer_key = settings$credentials$consumer_key,
#   #     consumer_secret = settings$credentials$consumer_secret,
#   #     access_token = settings$credentials$access_token,
#   #     access_secret = settings$credentials$access_secret
#   #   ),
#   #   error = function(e) e
#   # )
#   
#   # tweet_df <- search_tweets(hashtag_to_search, n = num_tweets_to_download, include_rts = FALSE,
#   #                           token = rtweet_token)
#   # 
#   # tweets <<- rtweet::search_tweets(
#   #   hashtag_to_search,
#   #   n = 100,
#   #   type = "recent", # input$type = "recent"
#   #   include_rts = T,
#   #   geocode = NULL # ,
#   #  #  token = .get_token()
#   # )
# 
#   
#} # Func_Twitter_Analysis


# --- All Functions

#' Launch
#'
#' Launch dashboard.
#'
#' export
# chirp <- function(){
#   
#   # file <- "_chirp.yml"
#   file <- "chirp.yml"
#   
#   if( !file.exists(paste0(pathwd_dash, "/www/yml/", file)) ){
#     cat(
#       crayon::red(cli::symbol$cross), "No", file, "in working directory\n"
#     )
#     return(NULL)
#   }
#   
#   settings <- yaml::read_yaml(paste0(pathwd_dash, "/www/yml/", file))
#   
#   if(length(settings$credentials) == 0 || length(unlist(settings$credentials)) == 0){
#     
#     rtweet_token <- tryCatch(rtweet::get_token(), error = function(e) NULL)
#     
#     if(!is.null(rtweet_token)){
#       cat(
#         crayon::yellow(cli::symbol$warning), "No credentials in", file, 
#         ". Using stored credentials found on machine.\n"
#       )
#     } else {
#       cat(
#         crayon::red(cli::symbol$cross), "No credentials in", file, "\n"
#       )
#       return(NULL)  
#     }
#     
#   } else {
#     rtweet_token <<- tryCatch(
#       rtweet::create_token(
#         app = "chirp",
#         consumer_key = settings$credentials$consumer_key,
#         consumer_secret = settings$credentials$consumer_secret,
#         access_token = settings$credentials$access_token,
#         access_secret = settings$credentials$access_secret
#       ),
#       error = function(e) e
#     )
#     
#     if(inherits(rtweet_token, "error")){
#       cat(
#         crayon::red(cli::symbol$cross), "Invalid credentials in", file, "\n"
#       )
#       return(NULL)
#     }
#   }
#   
#   # theme: paper  # from: https://rstudio.github.io/shinythemes
#   if(!"theme" %in% names(settings[["style"]])){ 
#     cat(
#       crayon::yellow(cli::symbol$warning), "No theme set in _chirp.yml, setting to",
#       crayon::underline("paper.\n")
#     )
#     theme <- "paper"
#   } else {
#     theme <- settings[["style"]][["theme"]]
#   }
#   
#   #  sliders: 'rgb(255, 255, 255)'   # background color of sliders
#   if(!"sliders" %in% names(settings[["style"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No sliders color in _chirp.yml, defaulting to white\n"
#     )
#     slider_color <- "white"
#   } else {
#     slider_color <- settings[["style"]][["sliders"]]
#   }
#   
#   # # google font
#   if(!"font" %in% names(settings[["style"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No font set in _chirp.yml, defaulting to",
#       crayon::underline("Raleway.\n")
#     )
#     
#     font <- "Raleway"
#     font_family <- "'Raleway', sans-serif"
#   } else {
#     font <- settings[["style"]][["font"]]
#     font_family <- settings[["style"]][["font_family"]]
#   }
#   
#   ## # network color palettes for continuous variable
#   if(!"continuous" %in% names(settings[["style"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No continuous palette set in _chirp.yml, setting to default.\n"
#     )
#     
#     palette <- c("#4b2991", "#872ca2", "#c0369d", "#ea4f88", "#fa7876", "#f6a97a", "#edd9a3")
#   } else {
#     palette <- settings[["style"]][["continuous"]]
#   }
#   
#   ## # # VR background color
#   if(!"vr_background" %in% names(settings[["style"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No vr_background set in _chirp.yml, defaulting to #052960.\n"
#     )
#     
#     vr_background <- "#052960"
#   } else {
#     vr_background <- settings[["style"]][["vr_background"]]
#   }
#   
#   ## # Palette for sentiment edges
#   if(!"sentiment_palette" %in% names(settings[["style"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No sentiment_palette set in _chirp.yml.\n"
#     )
#     
#     sentiment_palette <- c("red", "green")
#   } else {
#     sentiment_palette <- settings[["style"]][["sentiment_palette"]]
#   }
#   
#   ## # Maximum number of tweets one can fetch
#   if(!"max_tweets" %in% names(settings[["options"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No max_tweets specified in _chirp.yml, defaulting to 17,000.\n"
#     )
#     
#     max_tweets <- 17000
#   } else {
#     max_tweets <- settings[["options"]][["max_tweets"]]
#   }
#   
#   ## # Minimum number of tweets one can fetch
#   if(!"min_tweets" %in% names(settings[["options"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No min_tweets specified in _chirp.yml, defaulting to 500.\n"
#     )
#     
#     min_tweets <- 500
#   } else {
#     min_tweets <- settings[["options"]][["min_tweets"]]
#   }
#   
#   # check
#   if(min_tweets > max_tweets){
#     cat(
#       crayon::red(cli::symbol$cross), "min_tweets is greater than max_tweets.\n"
#     )
#     return(NULL)
#   }
#   
#   ## # discrete palette 
#   if(!"discrete" %in% names(settings[["style"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No discrete palette set in _chirp.yml, setting to default discrete palette.\n"
#     )
#     
#     discrete <- c("#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C", "#DAA51B",
#                   "#2F8AC4", "#764E9F", "#ED645A", "#CC3A8E", "#A5AA99")
#   } else {
#     discrete <- settings[["style"]][["discrete"]]
#   }
#   
#   ##  edges_color: 'rgba(196, 196, 196, .6)'  # Color of edges
#   if(!"edges_color" %in% names(settings[["style"]])){
#     cat(
#       crayon::yellow(cli::symbol$warning), "No edges_color set in _chirp.yml, setting to default.\n"
#     )
#     
#     edge_color <- "#bababa"
#   } else {
#     edge_color <- settings[["style"]][["edges_color"]]
#   }
#   
#   ## font_family: "'Raleway', sans-serif"    # font family
#   font_name <- gsub("[[:space:]]", "+", font)
#   
#   inverse <- settings$style$inverse %||% FALSE
#   
#   # head
#   head <- tagList(
#     tags$link(
#       href = paste0("https://fonts.googleapis.com/css?family=", font_name),
#       rel = "stylesheet"
#     ),
#     tags$style(
#       paste0("*{font-family: '", font, "', sans-serif;}")
#     ),
#     tags$link(
#       href = paste0(pathwd_dash, "/www/assets/pushbar.css"),  # "chirp-assets/pushbar.css",
#       rel="stylesheet",
#       type="text/css"
#     ),
#     tags$link(
#       href = paste0(pathwd_dash, "/www/assets/custom.css"), # "chirp-assets/custom.css",
#       rel = "stylesheet",
#       type = "text/css"
#     ),
#     tags$script(
#       src = paste0(pathwd_dash, "/www/assets/pushbar.js"), # "chirp-assets/pushbar.js"
#     ),
#     tags$link(
#       href = paste0(pathwd_dash, "/www/assets/please-wait.css"), # "chirp-assets/please-wait.css",
#       rel = "stylesheet",
#       type = "text/css"
#     ),
#     tags$script(
#       src = paste0(pathwd_dash, "/www/assets/please-wait.min.js"), # "chirp-assets/please-wait.min.js"
#     ),
#     tags$script(
#       src = "https://unpkg.com/micromodal/dist/micromodal.min.js"
#     ),
#     tags$link(
#       rel = "stylesheet",
#       href = "https://use.fontawesome.com/releases/v5.7.2/css/all.css", 
#       integrity = "sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr",
#       crossorigin = "anonymous"
#     ),
#     tags$script(
#       src =  paste0(pathwd_dash, "/www/assets/custom.js"),  # "chirp-assets/custom.js"
#     ),
#     tags$link(
#       rel = "shortcut icon",
#       href = "https://chirp.sh/img/chirp_favicon.png"
#     ),
#     tags$style(
#       paste0(".pushbar{background-color:", slider_color, ";}")
#     )
#   )
#   
#   # add google analytics if present
#   if("ganalytics" %in% names(settings$tracking)){
#     
#     ga_id <- settings$tracking$ganalytics %||% ""
#     
#     ga_tag <- tagList(
#       tags$script(
#         async = NA,
#         src = paste0("https://www.googletagmanager.com/gtag/js?id={{", ga_id, "}}")
#       ),
#       tags$script(
#         paste0(
#           "window.dataLayer = window.dataLayer || [];
#           function gtag(){dataLayer.push(arguments);}
#           gtag('js', new Date());
#           gtag('config', '{{", ga_id, "}}');"
#         )
#       )
#     )
#     
#     head <- tagAppendChild(head, ga_tag)
#   }
#   
#   particles_json <- jsonlite::fromJSON(
#     # system.file("assets/particles.json", package = "chirp") 
#     paste0(pathwd_dash, "/www/assets/particles.json")
#   )
#   
#   options(
#     sentiment_palette = sentiment_palette,
#     chirp_discrete = discrete,
#     chirp_palette = palette,
#     chirp_edge_color = edge_color,
#     chirp_font_family = font_family,
#     rtweet_token = rtweet_token,
#     vr_background = vr_background,
#     min_tweets = min_tweets,
#     max_tweets = max_tweets,
#     search_query = ""
#   )
#   
#   ui <- navbarPage(
#     title = "Twitter Network Explorer",
#     fluid = TRUE,
#     inverse = inverse,
#     # windowTitle = "Twitter Network Explorer",
#     header = head,
#     position = "fixed-top",
#     theme = shinythemes::shinytheme(theme),
#     id = "Sent_twitter_tabs",
#     tabPanel(
#       "HOME",
#       shinyjs::useShinyjs(),
#       div(
#         # class = "container",
#         style = "min-height:50vh;",
#         div(
#           style = "width: 100%; height: 100px; ", # position: relative; z-index:-9;
#           div(
#             id = "particles-target",
#             style = "position: absolute; top: 0; bottom: 0; right: 0; left: 0;"
#           )# ,
#           # div(
#           #   style = "padding-top:60px;",
#           #   # h1("\u007c\u0074\u0283\u0259\u02d0\u0070\u007c", class = "center"),
#           #   # h3("Twitter Network Explorer.", class = "center")
#           # )
#         ),
#         particlesjs::particles(particles_json, target_id = "particles-target", element_id = "particles"),
#         tabsetPanel(
#           type = "tabs",
#           tabPanel(
#             "SEARCH",
#             fluidRow(
#               column(
#                 1, 
#                 br(), 
#                 actionButton("opts", "", icon = shiny::icon("plus")) ,
#                 tippy_this("opts", "More options")
#               ),
#               column(
#                 9, 
#                 textInput("q", "", width = "100%", placeholder = "Enter your search query here."),
#                 tippy_this("q", "Your search query")
#               ),
#               column(
#                 2,
#                 br(),
#                 actionButton(
#                   "submit", 
#                   "Search", 
#                   icon = shiny::icon("search"), 
#                   width = "100%", 
#                   class = "btn btn-primary"
#                 )
#               )
#             ),
#             div(
#               id = "options",
#               style = "display:none;",
#               h3("Options"),
#               fluidRow(
#                 column(
#                   4,
#                   sliderInput(
#                     "n",
#                     label = "Number of tweets",
#                     min = min_tweets,
#                     max = max_tweets,
#                     value = min_tweets,
#                     step = 100,
#                     width = "100%"
#                   ),
#                   tippy_this("n", "Number of tweets to fetch")
#                 ),
#                 column(
#                   4, 
#                   selectInput(
#                     "type",
#                     "Type",
#                     choices = c(
#                       "Recent" = "recent",
#                       "Mixed" = "mixed",
#                       "Popular" = "popular"
#                     ),
#                     selected = "recent",
#                     width = "100%"
#                   ),
#                   tippy_this("type", "Type of tweets to fetch")
#                 ),
#                 column(
#                   4, 
#                   checkboxInput(
#                     "include_rts",
#                     "Include retweets",
#                     TRUE,
#                     width = "100%"
#                   ),
#                   tippy_this("include_rts", "Whether to include retweets")
#                 )
#               ),
#               fluidRow(
#                 column(
#                   3, textInput("longitude", "Longitude", value = "", width = "100%")
#                 ),
#                 column(
#                   3, textInput("latitude", "Latitude", value = "", width = "100%")
#                 ),
#                 column(
#                   4, textInput("radius", "Radius", value = "", width = "100%")
#                 ),
#                 column(
#                   2, selectInput("metric", "Metric", choices = c("Kilometer" = "km", "Miles" = "mi"))
#                 )
#               )
#             )
#           ) #,
#           # tabPanel(
#           #   "LOAD",
#           #   fileInput(
#           #     "file",
#           #     label = "Choose one or more previously downloaded Chirp file (.RData)",
#           #     accept = c(".RData", ".rdata"),
#           #     placeholder = " No file selected",
#           #     multiple = TRUE,
#           #     width = "80%"
#           #   )
#           # )
#         ),
#         br(),
#         br() # ,
#         # div(
#         #   style = "position:fixed;bottom:0px;right:43%;",
#         #   p(
#         #     class = "center",
#         #     "Visit", 
#         #     a(
#         #       "chrip.sh",
#         #       href = "https://chirp.sh",
#         #       target = "_blank"
#         #     ),
#         #     "for more information."
#         #   )
#         # )
#       )
#     ),
#     tabPanel(
#       "NETWORKS",
#       networks_ui("networks")
#     )
#   )
#   
#   server <- function(input, output, session){
#     
#     shinyjs::hide("options")
#     # shinyjs::hide("pushbarSearchNode")
#     # shinyjs::hide("pushbarBottom")
#     # shinyjs::hide("pushbarTop")
#     
#     observeEvent(input$opts, {
#       shinyjs::toggle("options")
#     })
#     observeEvent(input$search, {
#       shinyjs::toggle("pushbarTop")
#     })
#     
#     observeEvent(input$searchNode, {
#       shinyjs::toggle("pushbarSearchNode")
#     })
#     
#     observeEvent(input$stats, {
#       shinyjs::toggle("pushbarBottom")
#     })
#     
#   
#     
#     
#     observeEvent(input$submit, {
#       
#       geocode <- NULL
#       
#       if(input$longitude != "" && input$latitude != "" && input$radius != "")
#         geocode <- paste(input$longitude, input$latitude, paste0(input$radius, input$metric), sep = ",")
#       
#       # lim <- .check_rate_limit()
#       lim <- data.frame(remaining = 1, reset_at = Sys.time() - 2) 
#      
#       
#       if(lim$remaining == 0){
#         shinyjs::disable("submit")
#         shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("submit"))
#         time <- difftime(Sys.time(), lim$reset_at, units = "mins")
#         time <- ceiling(time)
#         showModal(
#           modalDialog(
#             title = "Rate limit hit!",
#             "You have hit the rate limit, wait until",
#             time 
#             , "to make another search.",
#             easyClose = TRUE,
#             footer = NULL
#           )
#         )
#       } 
#       
#       session$sendCustomMessage(
#         "load", 
#         paste("Fetching", prettyNum(input$n, big.mark = ","), "tweets")
#       )
#       
#       if(lim$remaining != 0){
#         # tweets <<- rtweet::search_tweets(
#         #   input$q,
#         #   n = input$n,
#         #   type = input$type,
#         #   include_rts = input$include_rts,
#         #   geocode = geocode,
#         #   token = .get_token()
#         # )
#         tweets <<- rtweet::search_tweets(
#           input$q,
#           n = input$n,
#           type = input$type,
#           include_rts = input$include_rts,
#           geocode = geocode #,
#           # token = .get_token()
#         )
#         
#         
#         
#         options(search_query = .clean_input(input$q))
#         
#         updateTabsetPanel(session = session, inputId = "Sent_twitter_tabs", selected = "NETWORKS")
#         callModule(networks, "networks", dat = tweets)
#       }
#       
#     })
#     
#     observeEvent(input$file, {
#       
#       file <- input$file
#       
#       if (!is.null(file)){
#         session$sendCustomMessage(
#           "load", 
#           "Loading file..."
#         )
#         
#         tweets <- file$datapath %>% 
#           purrr::map_df(function(x){
#             get(load(x))
#           })
#         
#         showTab(inputId = "Sent_twitter_tabs", target = "NETWORKS")
#         updateTabsetPanel(session = session, inputId = "Sent_twitter_tabs", selected = "NETWORKS")
#         callModule(networks, "networks", dat = tweets)
#       }
#       
#     })
#     
#   }
#   
#   shinyApp(ui, server)
#   
#}


# networks_ui <- function(id){
#   
#   ns <- NS(id)
#   
#   tagList(
#     tags$a(
#       shiny::icon("pencil-ruler", class = "fa-lg"),
#       onclick = "pushbar.open('save_pushbar');",
#       class = "btn btn-primary",
#       `data-pushbar-target` = "save_pushbar",
#       id = "optsBtn"
#     ),
#     tags$a(
#       shiny::icon("database", class = "fa-lg"),
#       onclick = "pushbar.open('search_pushbar');",
#       class = "btn btn-primary",
#       `data-pushbar-target` = "search_pushbar",
#       id = "search"
#     ),
#     tags$a(
#       shiny::icon("searchengin", class = "fa-lg"),
#       onclick = "pushbar.open('search_node_pushbar');",
#       class = "btn btn-primary",
#       `data-pushbar-target` = "search_node_pushbar",
#       id = "searchNode"
#     ),
#     shinyjs::hidden(
#       actionButton(
#         ns("hide_tweet"),
#         "",
#         icon = shiny::icon("times", verify_fa = FALSE),
#         class = "btn-danger"
#       )
#     ),
#     conditionalPanel(
#       "input['networks-network'] != 'hashtags'",
#       tags$a(
#         shiny::icon("layer-group", class = "fa-lg"),
#         onclick = "pushbar.open('legend_pushbar');",
#         class = "btn btn-primary",
#         `data-pushbar-target` = "legend_pushbar",
#         id = "legendBottom"
#       )
#     ),
#     div(
#       id = "pushbarSearchNode",
#       `data-pushbar-id` = "search_node_pushbar",
#       class = "pushbar from_left",
#       h4("SEARCH"),
#       fluidRow(
#         column(1, uiOutput(ns("node_search_ui"))), # 9
#         column(
#           3,
#           br(),
#           actionButton(
#             ns("search_node"),
#             "",
#             icon = shiny::icon("search-plus"),
#             width = "100%",
#             class = "btn-primary"
#           )
#         )
#       ),
#       radioButtons(
#         ns("zoom"),
#         "Zoom level",
#         choices = c(
#           "High" = "high",
#           "Medium" = "medium",
#           "Low" = "low"
#         ),
#         inline = TRUE,
#         width = "100%",
#         selected = "medium"
#       ),
#       tags$a(
#         id = "closeSearchNode",
#         shiny::icon("times", verify_fa = FALSE), onclick = "pushbar.close();", class = "btn btn-danger"
#       )
#     ),
#     actionButton(
#       "stats",
#       "",
#       shiny::icon("brain", class = "fa-lg"),
#       class = "btn-primary",
#       onclick = "pushbar.open('stats_pushbar');",
#     ),
#     div(
#       id = "pushbarBottom",
#       `data-pushbar-id` = "stats_pushbar",
#       class = "pushbar from_right",
#       column(2, # new 
#              
#       h4("STATS"),
#       uiOutput(ns("trend_text")),
#       # reactrend::reactrendOutput(ns("trendline"), width = "100%"),
#       reactrendOutput(ns("trendline"), width = "100%"),
#       fluidRow(
#         column(6, uiOutput(ns("n_nodes"))),
#         column(6, uiOutput(ns("n_edges")))
#       ),
#       fluidRow(
#         column(6, uiOutput(ns("n_tweets")))
#       ),
#       uiOutput(ns("selected_headline")),
#       uiOutput(ns("selected_source")),
#       fluidRow(
#         column(6, uiOutput(ns("source_indegree"))),
#         column(6, uiOutput(ns("source_outdegree")))
#       ),
#       fluidRow(
#         column(6, uiOutput(ns("source_pagerank"))),
#         column(6, uiOutput(ns("source_eigen")))
#       ),
#       uiOutput(ns("arrow_down")),
#       uiOutput(ns("selected_target")),
#       fluidRow(
#         column(6, uiOutput(ns("target_indegree"))),
#         column(6, uiOutput(ns("target_outdegree")))
#       ),
#       fluidRow(
#         column(6, uiOutput(ns("target_pagerank"))),
#         column(6, uiOutput(ns("target_eigen")))
#       ),
#       tags$a(
#         id = "closeStats",
#         shiny::icon("times", verify_fa = FALSE), onclick = "pushbar.close();", class = "btn btn-danger"
#       )
#       ) # new 
#     ),
#     div(
#       id = "pushbarTop",
#       `data-pushbar-id` = "search_pushbar",
#       class = "pushbar from_left",
#       h4("DATA"),
#       tabsetPanel(
#         type = "tabs",
#         tabPanel(
#           "SEARCH ",
#           textInput(
#             ns("q"),
#             "",
#             width = "100%",
#             placeholder = "Query"
#           ),
#           tippy_this(ns("q"), "Your search query"),
#           fluidRow(
#             column(
#               4,
#               actionButton(
#                 ns("addOpts"), 
#                 "",
#                 icon = shiny::icon("plus")
#               )
#             ),
#             column(
#               8,
#               actionButton(
#                 ns("submit"),
#                 "Search",
#                 icon = shiny::icon("search"),
#                 width = "100%",
#                 class = "btn btn-primary"
#               )
#             )
#           ),
#           br(),
#           div(
#             id = ns("searchOptions"),
#             style = "display:none;",
#             sliderInput(
#               ns("n"),
#               label = "Number of tweets",
#               min = .get_tweet_range("min"),
#               max = .get_tweet_range("max"),
#               value = .get_tweet_range("min"),
#               step = 100,
#               width = "100%"
#             ),
#             tippy_this(ns("n"), "Number of tweets to fetch"),
#             selectInput(
#               ns("type"),
#               "Type",
#               choices = c(
#                 "Recent" = "recent",
#                 "Mixed" = "mixed",
#                 "Popular" = "popular"
#               ),
#               selected = "recent",
#               width = "100%"
#             ),
#             tippy_this(ns("type"), "Type of tweets to fetch"),
#             fluidRow(
#               column(
#                 7,
#                 checkboxInput(
#                   ns("include_rts"),
#                   "Include retweets",
#                   TRUE,
#                   width = "100%"
#                 )
#               ),
#               column(5, checkboxInput(ns("append"), "Append"))
#             ),
#             tippy_this(ns("include_rts"), "Whether to include retweets"),
#             textInput(ns("longitude"), "Longitude", value = "", width = "100%"),
#             textInput(ns("latitude"), "Latitude", value = "", width = "100%"),
#             fluidRow(
#               column(6,textInput(ns("radius"), "Radius", value = "", width = "100%")),
#               column(6, selectInput(ns("metric"), "Metric", choices = c("Kilometer" = "km", "Miles" = "mi")))
#             )
#           )
#         ),
#         tabPanel(
#           "LOAD",
#           fileInput(
#             ns("file"),
#             label = "Choose one or more previously downloaded Chirp file(s) (.RData)",
#             accept = c(".RData", ".rdata"),
#             placeholder = " No file selected",
#             width = "100%",
#             multiple = TRUE
#           ),
#           checkboxInput(ns("append_file"), "Append")
#         )
#       ),
#       # a(
#       #   "chrip.sh",
#       #   id = "leftLink",
#       #   href = "https://chirp.sh",
#       #   target = "_blank"
#       # ),
#       tags$a(
#         id = "closeSearch",
#         shiny::icon("times", verify_fa = FALSE), onclick = "pushbar.close();", class = "btn btn-danger"
#       )
#     ),
#     shinyjs::useShinyjs(),
#     div(
#       `data-pushbar-id` = "legend_pushbar",
#       class = "pushbar from_bottom",
#       fluidRow(
#         column(12, uiOutput(ns("legend"), class = "center"))
#       ),
#       tags$a(
#         style = "right:20px;bottom:20px;position:absolute;",
#         shiny::icon("times", verify_fa = FALSE), onclick = "pushbar.close();", class = "btn btn-danger"
#       )
#     ),
#     div(
#       id = "pushbarLeft",
#       `data-pushbar-id` = "save_pushbar",
#       class = "pushbar from_right",
#       h4("OPTIONS"),
#       br(),
#       selectInput(
#         ns("network"),
#         "NETWORK TYPE",
#         choices = c(
#           "Retweets" = "retweet_screen_name",
#           "Hashtags" = "hashtags",
#           "Conversations" = "mentions_screen_name"
#         ),
#         selected = "Retweets",
#         width = "100%"
#       ),
#       tippy_this(ns("network"), "Type of network to draw"),
#       conditionalPanel(
#         "input['networks-network'] != 'retweet_screen_name'",
#         checkboxInput(
#           ns("comentions"),
#           "Co-mentions",
#           width = "100%"
#         )
#       ),
#       conditionalPanel(
#         "input['networks-network'] == 'retweet_screen_name'",
#         checkboxInput(
#           ns("quoted"),
#           "Include quoted",
#           width = "100%",
#           value = TRUE
#         )
#       ),
#       fluidRow(
#         column(
#           6, 
#           selectInput(
#             ns("size"), 
#             "NODES SIZE", 
#             choices = c(
#               "# tweets" = "n_tweets",
#               "In-degree" = "in_degree",
#               "Out-degree" = "out_degree",
#               "Closeness" = "closeness",
#               "Pagerank" = "pagerank",
#               "Authority" = "authority",
#               "Eigen" = "eigen"
#             ),
#             width = "100%"
#           ),
#           tippy_this(ns("size"), "Variable to size nodes")
#         ),
#         column(
#           6, 
#           selectInput(
#             ns("colour"), 
#             "NODES COLOUR", 
#             choices = c(
#               "Cluster" = "group",
#               "# tweets" = "n_tweets",
#               "Components" = "components", 
#               "In-degree" = "in_degree",
#               "Out-degree" = "out_degree",
#               "Closeness" = "closeness",
#               "Pagerank" = "pagerank",
#               "Authority" = "authority",
#               "Eigen" = "eigen",
#               "Type" = "type"
#             ),
#             selected = "Cluster",
#             width = "100%"
#           ),
#           tippy_this(ns("colour"), "Variable to colour nodes")
#         )
#       ),
#       selectInput(
#         ns("edges_colour"),
#         "EDGES COLOUR",
#         choices = c(
#           "None" = "none",
#           "Sentiment" = "sentiment",
#           "# tweets" = "size"
#         ),
#         selected = "None",
#         width = "100%"
#       ),
#       h5("FILTER"),
#       fluidRow(
#         column(
#           8,
#           checkboxInput(
#             ns("delete_nodes"), 
#             "DELETE NODES", value = FALSE
#           ),
#           tippy_this(ns("delete_nodes"), "Tick and click on nodes to delete them")
#         ),
#         column(
#           4,
#           conditionalPanel(
#             "input['networks-network'] != 'retweet_screen_name'",
#             checkboxInput(
#               ns("include_retweets"), 
#               "RTs",
#               value = TRUE
#             )
#           )
#         )
#       ),
#       sliderInput(
#         ns("node_size"),
#         "Filter node by size",
#         width = "100%",
#         min = 3,
#         max = 17,
#         value = c(3, 17)
#       ),
#       h5("LAYOUT"),
#       fluidRow(
#         # column(
#         #   6, 
#         #   actionButton(
#         #     ns("start_layout"), 
#         #     "START", 
#         #     icon = shiny::icon("play"),
#         #     width = "100%"
#         #   )
#         # ),
#         # column(
#         #   6, 
#         #   actionButton(
#         #     ns("kill_layout"), "
#         #     STOP", 
#         #     icon = shiny::icon("stop"),
#         #     width = "100%"
#         #   )
#         # )
#       ),
#       br(),
#       # actionButton(
#       #   ns("noverlap"), 
#       #   "NO OVERLAP", 
#       #   icon = shiny::icon("magnet"),
#       #   width = "100%"
#       # ),
#       # h5("EXPORT"),
#       fluidRow(
#         # column(
#         #   6, 
#         #   actionButton(
#         #     ns("save_img"), 
#         #     "SAVE IMAGE", 
#         #     icon = shiny::icon("image"),
#         #     width = "100%"
#         #   )
#         # ),
#         
#         # column(
#         #   6, 
#         #   actionButton(
#         #     ns("save_svg"), 
#         #     "SAVE SVG",
#         #     icon = shiny::icon("html5"),
#         #     width = "100%"
#         #   )
#         # )
#       ),
#       br() # ,
#       # downloadButton(ns("downloadData"), "DOWNLOAD DATA", style = "width:100%;"),
#       # tags$a(
#       #   id = "closeOpts",
#       #   shiny::icon("times", verify_fa = FALSE), onclick = "pushbar.close();", class = "btn btn-danger"
#       # )
#     ),
#     # actionButton(
#     #   ns("vr"),
#     #   "",
#     #   icon = shiny::icon("vr-cardboard", class = "fa-lg"),
#     #   class = "btn btn-primary"
#     # ),
#     
#     shinyjqui::jqui_draggable(
#       htmlOutput(
#         ns("display"), style = "position:absolute;z-index:99;left:20px;top:70px;"
#       )
#     ),
#     shinycustomloader::withLoader(
#       sigmajs::sigmajsOutput(ns("graph"), height = "110vh"),
#       type = "html" ,
#      loader = "loader9"
#     )
#     # , uiOutput(ns("aforce"))
#   )
#   
# }

# ------------
# networks <- function(input, output, session, dat){
#   
#   tweets <- reactiveVal(dat)
#   
#   # shinyjs::hide("aforce")
#   
#   
#   observeEvent(input$submit, {
#     geocode <- NULL
#     
#     if(input$longitude != "" && input$latitude != "" && input$radius != "")
#       geocode <- paste(input$longitude, input$latitude, paste0(input$radius, input$metric), sep = ",")
#     
#     if(input$q != ""){
#       
#       session$sendCustomMessage(
#         "load", 
#         paste("Fetching", prettyNum(input$n, big.mark = ","), "tweets")
#       )
#       
#       # lim <- .check_rate_limit()
#       lim <- data.frame(remaining = 1, reset_at = Sys.time() - 2) 
#       
#       options(search_query = .clean_input(input$q))
#       
#       if(lim$remaining == 0){
#         shinyjs::disable("submit")
#         shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("submit"))
#         time <- difftime(Sys.time(), lim$reset_at, units = "mins")
#         time <- ceiling(time)
#         showModal(
#           modalDialog(
#             title = "Rate limit hit!",
#             "You have hit the rate limit, wait until",
#             time 
#             , "to make another search.",
#             easyClose = TRUE,
#             footer = NULL
#           )
#         )
#       } else {
#         tw <- rtweet::search_tweets(
#           input$q,
#           n = input$n,
#           type = input$type,
#           include_rts = input$include_rts,
#           geocode = geocode,
#           token = .get_token()
#         )
#         if(isTRUE(input$append))
#           rbind.data.frame(tweets(), tw) %>% 
#           tweets()
#         else
#           tweets(tw)
#       }
#       
#       session$sendCustomMessage("unload", "") # stop loading
#     }
#     
#   })
#   
#   observeEvent(input$file, {
#     
#     file <- input$file
#     
#     s <- ""
#     if(length(file$datapath))
#       s <- "s"
#     
#     session$sendCustomMessage(
#       "load", 
#       paste0("Loading file", s, "...")
#     )
#     tw <- file$datapath %>% 
#       purrr::map_df(function(x){
#         get(load(x))
#       })
#     if(isTRUE(input$append_file))
#       rbind.data.frame(tweets(), tw) %>% 
#       tweets()
#     else
#       tweets(tw)
#     session$sendCustomMessage("unload", "") # stop loading
#   })
#   
#   shinyjs::hide("save_el")
#   
#   observeEvent(input$save_opts, {
#     shinyjs::toggle("save_el")
#   })
#   
#   # observeEvent(input$save_img, {
#   #   ns <- session$ns
#   #   sigmajs::sigmajsProxy(ns("graph")) %>%
#   #     sigmajs::sg_export_img_p(file = "chirp.png")
#   # })
#   
#   # observeEvent(input$save_svg, {
#   #   ns <- session$ns
#   #   sigmajs::sigmajsProxy(ns("graph")) %>%
#   #     sigmajs::sg_export_svg_p(file = "chirp.svg")
#   # })
#   
#   graph <- reactive({
#     
#     tw <- tweets() %>% 
#       dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets)) %>% 
#       .compute_sentiment()
#     
#     if(isTRUE(input$comentions) && input$network %in% c("hashtags", "mentions_screen_name"))
#       edges <- tw %>% gt_co_edges(!!sym(input$network))
#     else
#       edges <- tw %>% 
#         gt_edges(screen_name, !!sym(input$network), sentiment) %>% 
#         gt_preproc_edges(.preproc)
#     
#     if(isTRUE(input$quoted) && input$network == "retweet_screen_name")
#       edges <- edges %>% 
#         gt_edges_bind(screen_name, quoted_screen_name) 
#     
#     graph <- edges %>%
#       gt_nodes() %>%
#       gt_collect()
#     
#     graph <- tbl_graph(
#       nodes = graph$nodes, 
#       edges = graph$edges
#     ) %>% 
#       activate(nodes) %>% 
#       dplyr::mutate(
#         name = nodes,
#         id = name,
#         label = name,
#         n_tweets = n,
#         out_degree = centrality_degree(mode = "out"),
#         in_degree = centrality_degree(mode = "in"),
#         authority = centrality_authority(),
#         pagerank = centrality_pagerank(),
#         closeness = centrality_closeness(),
#         eigen = centrality_eigen(),
#         components = group_components(type = "weak"),
#         group = group_walktrap()
#       ) %>% 
#       igraph::as_data_frame("both")
#     
#     edges <- graph$edges %>% 
#       dplyr::mutate(
#         id = 1:n(),
#         source = from,
#         target = to,
#         size = n,
#         type = "arrow"
#       ) %>% 
#       dplyr::select(-one_of("to", "from"))
#     
#     nodes <- graph$vertices %>% 
#       dplyr::mutate(
#         group = as.factor(group),
#         components = as.factor(components)
#       ) %>% 
#       dplyr::select(-one_of("n", "nodes"))
#     
#     session$sendCustomMessage("unload", "") # stop loading
#     
#     list(
#       nodes = nodes,
#       edges = edges
#     )
#     
#   })
#   
#   output$legend <- renderUI({
#     
#     nodes <- .color_nodes(graph()$nodes, "group") %>% 
#       dplyr::select(label, group, color)
#     
#     if(input$network == "hashtags"){
#       return("")
#     }
#     
#     leg <- tweets() %>% 
#       select_("hashtags", "screen_name", "v2" = input$network) %>% 
#       dplyr::mutate(
#         screen_name = tolower(screen_name),
#         v2 = tolower(v2)
#       ) %>% 
#       dplyr::left_join(nodes, by = c("screen_name" = "label")) %>% 
#       dplyr::left_join(nodes, by = c("v2" = "label"), suffix = c("_source", "_target")) %>% 
#       dplyr::mutate(
#         group_source = case_when(
#           is.na(group_source) ~ group_target,
#           TRUE ~ group_source,
#         ),
#         color_source = case_when(
#           is.na(color_source) ~ color_target,
#           TRUE ~ color_source,
#         ),
#         grp = case_when(
#           group_source == group_target ~ group_source,
#           TRUE ~ group_source
#         ),
#         color = case_when(
#           color_source == color_target ~ color_source,
#           TRUE ~ color_source
#         )
#       )  %>% 
#       dplyr::filter(!is.na(grp)) %>% 
#       tidyr::unnest(hashtags) %>% 
#       dplyr::mutate(hashtgas = tolower(hashtags)) %>% 
#       group_by(grp, color) %>% 
#       dplyr::count(hashtags, sort = TRUE) %>%  
#       dplyr::filter(hashtags != .get_search_query()) %>% 
#       dplyr::filter(!is.na(hashtags)) %>% 
#       slice(1) %>% 
#       ungroup() %>% 
#       dplyr::mutate(grp = as.integer(grp)) %>% 
#       arrange(grp) %>% 
#       slice(1:10) 
#     
#     ch <- as.character(unlist(leg$grp))
#     ch <- c("all", ch)
#     names(ch) <- c("All nodes", paste0("#", as.character(unlist(leg$hashtags))))
#     
#     ns <- session$ns
#     tgs <- radioButtons(
#       ns("legendOut"),
#       "FILTER CLUSTERS",
#       choices = ch,
#       inline = TRUE,
#       width = "100%"
#     )
#     
#     tgs
#     
#   })
#   
#   observeEvent(input$legendOut, {
#     ns <- session$ns
#     if(input$legendOut != "all")
#       sigmajs::sigmajsProxy(ns("graph")) %>% 
#       sigmajs::sg_filter_undo_p("legend-filter") %>% 
#       sigmajs::sg_filter_eq_p(input$legendOut, "group", name = "legend-filter")
#     else if(input$legendOut == "all")
#       sigmajs::sigmajsProxy(ns("graph")) %>% 
#       sigmajs::sg_filter_undo_p("legend-filter") 
#   })
#   
#   
#   
#   
#   output$graph <- sigmajs::renderSigmajs({
#     
#     g <- graph()
#     
#     nodes <- g$nodes
#     nodes <- .color_nodes(nodes, "group")
#     nodes <- .size_nodes(nodes, "n_tweets")
#     edges <- g$edges
#     
#     sigmajs::sigmajs(type = "webgl") %>%
#       sigmajs::sg_nodes(nodes, id, label, size, color, group) %>%
#       sigmajs::sg_edges(edges, id, source, target, type, size) %>%
#       sigmajs::sg_force(slowDown = 4) %>%
#       sigmajs::sg_kill() %>%
#       sigmajs::sg_drag_nodes() %>%
#       sigmajs::sg_force_stop(2500) %>%
#       sigmajs::sg_layout() %>% 
#       sigmajs::sg_settings(
#         minArrowSize = 1,
#         batchEdgesDrawing = TRUE,
#         edgeColor = "default",
#         defaultEdgeColor = .get_edge_color(),
#         font = .get_font(),
#         labelThreshold = 9999
#       )
#     
#   })
#   
#   
#   
#   observeEvent(input$edges_colour, {
#     
#     ns <- session$ns
#     
#     edges <- isolate(graph()$edges)
#     
#     df <- .color_edges(edges, input$edges_colour)
#     
#     sigmajs::sigmajsProxy(ns("graph")) %>% 
#       sigmajs::sg_change_edges_p(df, color, "color")
#   })
#   
#   observeEvent(input$colour, {
#     ns <- session$ns
#     
#     nodes <- isolate(graph()$nodes)
#     
#     df = .color_nodes(nodes, input$colour)
#     
#     sigmajs::sigmajsProxy(ns("graph")) %>% 
#       sigmajs::sg_change_nodes_p(df, color, "color")
#   })
#   
#   observeEvent(input$size, {
#     ns <- session$ns
#     
#     nodes <- isolate(graph()$nodes)
#     
#     df = .size_nodes(nodes, input$size)
#     
#     sigmajs::sigmajsProxy(ns("graph")) %>% 
#       sigmajs::sg_change_nodes_p(df, size, "size")
#   })
#   
# 
#   
#   trend <- reactive({
#     
#     .get_trend <- function(x = "%Y-%m-%d"){
#       tweets() %>%
#         dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets)) %>% 
#         dplyr::mutate(
#           created_at = format(created_at, x)
#         ) %>%
#         dplyr::count(created_at) %>%
#         pull(n) %>%
#         list(
#           trend = .,
#           format = x
#         )
#     }
#     
#     trend <- .get_trend()
#     
#     if(length(trend$trend) < 4)
#       trend <- .get_trend("%Y-%m-%d %H")
#     
#     if(length(trend$trend) < 3)
#       trend <- .get_trend("%Y-%m-%d %H:%M")
#     
#     if(length(trend$trend) < 2)
#       trend <- .get_trend("%Y-%m-%d %H:%M:%S")
#     
#     return(trend)
#   })
#   
#   
#   
#   output$trend_text <- renderUI({
#     p(strong("Tweets"), .get_time_scale(trend()$format))
#   })
#   
#   # output$trendline <- reactrend::renderReactrend({
#   output$trendline <- renderReactrend({
#     trend()$trend %>%
#       # reactrend::reactrend(
#       reactrend(
#         draw = TRUE,
#         gradient = .get_pal(),
#         smooth = TRUE,
#         stroke_width = 2,
#         line_cap = "round"
#       )
#   })
#   
#   output$n_nodes <- renderUI({
#     p(
#       strong("Nodes:"),
#       prettyNum(
#         nrow(graph()$nodes),
#         big.mark = ","
#       )
#     )
#   })
#   
#   output$n_edges <- renderUI({
#     p(
#       strong("Edges:"),
#       prettyNum(
#         nrow(graph()$edges),
#         big.mark = ","
#       )
#     )
#   })
#   
#   output$n_tweets <- renderUI({
#     p(
#       strong("Tweets:"),
#       prettyNum(
#         nrow(tweets() %>% dplyr::filter(is_retweet %in% c(FALSE, input$include_retweets))),
#         big.mark = ","
#       )
#     )
#   })
#   
#   observeEvent(input$graph_click_node, {
#     
#     node_clicked <<- input$graph_click_node$label
#     ns <<- session$ns
#     
#     if(isTRUE(input$delete_nodes))
#       sigmajs::sigmajsProxy(ns("graph")) %>%
#       sigmajs::sg_drop_node_p(id = input$graph_click_node$id)
#     else {
#       sigmajs::sigmajsProxy(ns("graph")) %>% 
#         sigmajs::sg_filter_neighbours_p(node = input$graph_click_node$id, "neighbours-filter")
#       shinyjs::show("display")
#       shinyjs::show("hide_tweet")
#     }
#     
#   })
#   
#   observeEvent(input$start_layout, {
#     ns <- session$ns
#     
#     sigmajs::sigmajsProxy(ns("graph")) %>%
#       sigmajs::sg_force_start_p()
#     
#   })
#   
#   observeEvent(input$kill_layout, {
#     ns <- session$ns
#     
#     sigmajs::sigmajsProxy(ns("graph")) %>%
#       sigmajs::sg_force_kill_p()
#     
#   })
#   
#   observeEvent(input$noverlap, {
#     ns <- session$ns
#     
#     sigmajs::sigmajsProxy(ns("graph")) %>%
#       sigmajs::sg_noverlap_p(nodeMargin = .05)
#     
#   })
#   
#   notification <- NULL
#   observeEvent(input$delete_nodes, {
#     if(isTRUE(input$delete_nodes)){
#       notification <<- showNotification(
#         "Click a node to delete it.",
#         duration = NULL,
#         type = "error",
#         closeButton = FALSE
#       )
#     } else {
#       if (!is.null(notification)) removeNotification(notification)
#       notification <<- NULL
#     }
#   })
#   
#   shinyjs::hide("searchOptions")
#   
#   observeEvent(input$addOpts, {
#     ns <- session$ns
#     shinyjs::toggle("searchOptions")
#   })
#   
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste('chirp-', Sys.Date(), '.RData', sep='')
#     },
#     content = function(file) {
#       tw <- tweets() 
#       save(tw, file = file)
#     }
#   )
#   
#   
#   
#   # aforce <- eventReactive(input$vr, {
#   #   
#   #   vr <- ""
#   #   
#   #   if(input$vr %% 2 == 1){
#   #     session$sendCustomMessage(
#   #       "load", 
#   #       "Get your headset!"
#   #     )
#   #     
#   #     g <- graph()
#   #     
#   #     nodes <- g$nodes
#   #     nodes <- .color_nodes(nodes, "group")
#   #     nodes <- .size_nodes(nodes, "n_tweets")
#   #     
#   #     # aforce::aForce$
#   #     
#   #     vr <- aForce$
#   #       new(n_label = "label")$ # initialise
#   #       nodes(nodes, id, size, color, label)$ # add nodes
#   #       links(graph()$edges, source, target)$ # add edges
#   #       build( # build
#   #         aframer::a_camera(
#   #           `wasd-controls` = "fly: true; acceleration: 600",
#   #           aframer::a_cursor(opacity = 0.5)
#   #         ),
#   #         aframer::a_sky(color=getOption("vr_background"))
#   #       )$ 
#   #       embed(width="100%", height = "80vh")
#   #     
#   #     session$sendCustomMessage(
#   #       "unload", 
#   #       ""
#   #     )
#   #   } 
#   #   
#   #   return(vr)
#   # })
#   
#   # output$aforce <- renderUI({
#   #   aforce()
#   # })
#   
#   # observeEvent(input$vr, {
#   #   shinyjs::toggle("aforce")
#   # })
#   
#}

#' Initialise
#'
#' Create a \code{_chirp.yml} file in the current working directory.
#'
#' @param edit Whether to open the file for edit.
#'
#' @import tippy
#' @import shiny
#' @import dplyr
#' @import tidygraph
#' @import graphTweets
#' @importFrom utils URLencode
#'
#' @export
# build_nest <- function(edit = interactive()){
#   
#   config <- "chirp.yml"
#   # config <- paste0(pathwd_dash, "/www/yml/", config) # "_chirp.yml"
#   
#   if(file.exists(paste0(pathwd_dash, "/www/yml/", config))){
#     cat(
#       crayon::red(cli::symbol$cross), " The configuration file,", config, " already exists.\n",
#       sep = ""
#     )
#     
#   } else {
#     file.copy(
#       # system.file("templates/chirp.yml", package = "chirp"),
#       system.file(paste0(pathwd_dash, "/www/yml/", config)),
#       config
#     )
#     
#     cat(
#       crayon::green(cli::symbol$tick), "Copied", config, "\n"
#     )
#   }
#   
#   # if(edit) fileEdit(config)
# }

# fileEdit <- function(file) {
#   fileEditFunc <- eval(parse(text = "file.edit"), envir = globalenv())
#   fileEditFunc(file)
# }

globalVariables(
  c(
    "mentions_screen_name",
    "quoted_screen_name",
    "degree(igraph)",
    "retweet_count",
    "rtweet_token",
    "screen_name",
    "created_at",
    "is_retweet",
    "components",
    "out_degree",
    "remaining",
    "status_id",
    "in_degree",
    "sentiment",
    "reset_at",
    "pagerank",
    "hashtags",
    "negative",
    "positive",
    "target",
    "tweets",
    "weight",
    "degree",
    "group",
    "color",
    "label",
    "name",
    "size",
    "type",
    "from",
    "text",
    "word",
    "grp",
    "v2",
    "to",
    "."
  )
)

"%||%" <- function(x, y) {
  if (length(x) > 0 || !is.null(x)) x else y
}

.get_pal <- function(){
  getOption("chirp_palette")
}

.get_discrete <- function(){
  getOption("chirp_discrete")
}

.get_search_query <- function(){
  getOption("search_query")
}

.get_edge_color <- function(){
  getOption("chirp_edge_color")
}

.get_font <- function(){
  getOption("chirp_font_family")
}

.get_token <- function(){
  getOption("rtweet_token")
}

.get_sentiment_pal <- function(){
  getOption("sentiment_palette")
}

.get_tweet_range <- function(v){
  switch(
    v,
    max = getOption("max_tweets"),
    min = getOption("min_tweets")
  )
}

.get_tweet <- function(data){
  
  url <- URLencode(
    paste0(
      "https://twitter.com/", data$screen_name[1], "/status/", data$status_id[1]
    )
  )
  
  response <- httr::GET(
    url = paste0(
      "https://publish.twitter.com/oembed?url=", url,
      "&maxwidth=300&omit_script=false&link_color="
    )
  )
  content <- httr::content(response)
  content$html[1]
}

.rescale <- function(x, t){
  x <- as.numeric(x)
  x <-  (x - min(x)) / (max(x) - min(x))
  x <- x * t
  return(x)
}

.get_time_scale <- function(x){
  if(x == "%Y-%m-%d") return("daily")
  if(x == "%Y-%m-%d %H") return("hourly")
  if(x == "%Y-%m-%d %H:%M") return("minute by minute")
  if(x == "%Y-%m-%d %H:%M:%S") return("second by second")
}

.color_nodes <- function(nodes, x){
  var = pull(nodes, x)
  library(scales)
  if(inherits(var, "factor") || inherits(var, "character")){
    var_unique <- unique(var)
    colors <- scales::col_factor(
      .get_discrete(), 
      var_unique
    )(var)
  } else {
    colors <- scales::col_numeric(
      .get_pal(), domain = NULL
    )(var)
  }
  
  nodes$color <- colors
  
  return(nodes)
}

.compute_sentiment <- function(df = tweets$df){
  cat(".compute_sentiment \n")
  
  lexicon <- tidytext::get_sentiments("bing")  
  
  df %>% 
    dplyr::select(status_id, text) %>% 
    tidytext::unnest_tokens(word, text) %>% 
    dplyr::inner_join(lexicon, by = "word") %>% 
    dplyr::count(status_id, sentiment) %>%
    tidyr::spread(sentiment, n, fill = 0) %>% 
    dplyr::mutate(sentiment = positive - negative) %>% 
    dplyr::left_join(df, ., by = "status_id") %>% 
    dplyr::mutate(
      sentiment = ifelse(is.na(sentiment), 0, sentiment),
      sentiment = case_when(
        sentiment > 0 ~ 1,
        sentiment < 0 ~ -1,
        TRUE ~ 0
      )
    )
}

.color_edges <- function(edges, x){
  
  if(x != "sentiment")
    palette <- .get_pal()
  else 
    palette <- .get_sentiment_pal()
  
  if(x != "none"){
    var <- edges %>% 
      pull(x)
    
    colors <- scales::col_numeric(
      palette, domain = c(-1, 1)
    )(var)
  } else {
    colors <- rep(.get_edge_color(), nrow(edges))
  }
  
  data.frame(color = colors)
  
}

.size_nodes <- function(nodes, x){
  var = pull(nodes, x)
  var <- scales::rescale(var, to = c(3, 17))
  nodes$size <- var
  return(nodes)
}

.check_rate_limit <- function(){
  rtweet::rate_limit(.get_token(), "search_tweets") %>% 
    dplyr::select(remaining, reset_at)
}

.zoom <- function(x){
  switch(
    x,
    low = .1,
    medium = .05,
    high = .01
  )
}

.get_random_icon <- function(){
  x <- c(
    "user-tie",
    "user-secret",
    "user-ninja",
    "user-md",
    "user-graduate",
    "user-astronaut"
  )
  icn <- sample(x, 1)
  shiny::icon(icn, class = "text-primary")
}

.clean_input <- function(x){
  x <- tolower(x)
  gsub("#", "", x)
}

.slice_node <- function(x, i){
  
  if(is.null(x))
    return(NULL)
  
  x %>% 
    slice(i) %>% 
    pull(label)
}

.preproc <- function(df){
  df %>% 
    dplyr::group_by(source, target) %>% 
    dplyr::summarise(
      n = sum(n),
      sentiment = sum(sentiment)
    ) %>% 
    dplyr::ungroup()
}


#' Trend
#'
#' Create a trend line.
#' 
#' @param data A vector of numerical values.
#' @param draw Whether to use a drawing animation.
#' @param draw_duration Duration of draw animation in milliseconds.
#' @param draw_easing Easing to use for draw animation.
#' @param stroke Color of trend line.
#' @param stroke_width Width of stroke.
#' @param stroke_opacity Opacity of stroke.
#' @param smooth Whether to smooth trend line.
#' @param line_cap Shape of line edges.
#' @param radius Controls curve when \code{smooth} is set top \code{TRUE}.
#' @param dash A vector of dash length.
#' @param dash_offset Controls where dash starts.
#' @param gradient Vector of colors to use as gradient.
#' @param svg_width,svg_height Width of svg.
#' @param padding Padding for large \code{stroke_width}.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId A valid CSS element id.
#' 
#' @examples 
#' reactrend(runif(100))
#' 
#' reactrend(
#'   runif(20), 
#'   gradient = c('#00c6ff', '#F0F', '#FF0'),
#'   smooth = TRUE,
#'   draw = TRUE,
#'   dash = c(1,7,3)
#' )
#'
#' @import htmlwidgets
#'
#' @export
reactrend <- function(data, draw = FALSE, draw_duration = 3000, draw_easing = "ease-in", 
                      stroke = "#000", stroke_width = 1, stroke_opacity = 1, smooth = FALSE,
                      line_cap = c("butt", "round", "square"), radius = 10, padding = 8,
                      dash = NULL, dash_offset = 10, gradient = NULL, svg_width = NULL,
                      svg_height = NULL, width = NULL, height = "25%", elementId = NULL) {
  
  if(missing(data))
    stop("missing data", call. = FALSE)
  
  # describe a React component to send to the browser for rendering.
  component <- reactR::reactMarkup(
    htmltools::tag(
      "Trend", 
      list(
        data = data,
        stroke = stroke,
        strokeWidth = stroke_width,
        strokeOpacity = stroke_opacity,
        smooth = smooth,
        strokeLinecap = match.arg(line_cap),
        strokeDasharray = dash,
        strokeDashoffset = dash_offset,
        gradient = gradient,
        autoDraw = draw,
        autoDrawDuration = draw_duration,
        autoDrawEasing = draw_easing,
        width = svg_width,
        height = svg_height,
        padding = padding
      )
    )
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'reactrend',
    component,
    width = width,
    height = height,
    package = 'reactrend',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 0
    )
  )
}

#' Shiny bindings for reactrend
#'
#' Output and render functions for using reactrend within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a reactrend
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param style CSS.
#' @param id,class Valid CSS id and class.
#' @param ... Any other arguments to pass to the HTML element.
#'
#' @name reactrend-shiny
#'
#' @export
reactrendOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'reactrend', width, height, package = 'reactrend')
}

#' @rdname reactrend-shiny
#' @export
renderReactrend <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, reactrendOutput, env, quoted = TRUE)
}

#' Called by HTMLWidgets to produce the widget's root element.
#' @rdname reactrend-shiny
reactrend_html <- function(id, style, class, ...) {
  htmltools::tagList(
    # Necessary for RStudio viewer version < 1.2
    reactR::html_dependency_corejs(),
    reactR::html_dependency_react(),
    reactR::html_dependency_reacttools(),
    htmltools::tags$div(id = id, class = class)
  )
}


#' Check
#' 
#' Check configuration file, \code{_chirp.yml}.
#' 
#' @export
check_nest <- function(){
  
  file <- "_chirp.yml"
  
  if(!file.exists(file)){
    cat(
      crayon::red(cli::symbol$cross), "No", file, "in working directory\n"
    )
    return(NULL)
  }
  
  config <- yaml::read_yaml(file)
  
  if(length(config$credentials) == 0 || length(unlist(config$credentials)) == 0){
    
    rtweet_token <- tryCatch(rtweet::get_token(), error = function(e) NULL)
    
    if(!is.null(rtweet_token)){
      cat(
        crayon::yellow(cli::symbol$warning), "No credentials in", file, 
        ". Using stored credentials found on machine.\n"
      )
    } else {
      cat(
        crayon::red(cli::symbol$cross), "No credentials in", file, "\n"
      )
      return(NULL)  
    }
    
  } else {
    rtweet_token <- tryCatch(
      rtweet::create_token(
        app = "chirp",
        consumer_key = config$credentials$consumer_key,
        consumer_secret = config$credentials$consumer_secret,
        access_token = config$credentials$access_token,
        access_secret = config$credentials$access_secret
      ),
      error = function(e) e
    )
    
    if(inherits(rtweet_token, "error")){
      cat(
        crayon::red(cli::symbol$cross), "Invalid credentials in", file, "\n"
      )
      return(NULL)
    }
  }
  
  cat(
    crayon::green(cli::symbol$tick), file, "looks valid", "\n"
  )
  
}

