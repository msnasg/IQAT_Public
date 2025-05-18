


# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


# ---------------------------------------------------------------------------- #
# ui: Home Tab
# ---------------------------------------------------------------------------- #
#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_welcome_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      col_12(
        column(12, br()),
        bs4Card(
          id = "Home_maincard_id",
          title = tagList(
            h4("Welcome to Integrated Quantitative Analytical Tools (IQAT)!",
              style = paste0(
                "text-align: justify; font-weight: bold; color: ", configs$colors$title_text,
                "; margin-left: 20px;"
              )
            )
            # #0000FF;
          ),
          status = "info",
          solidHeader = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          closable = FALSE,
          label = NULL,
          width = 12,
          height = "100%",
          fluidRow(
            column(
              12,
              p("No one gets into trading thinking it will be easy.
                 I certainly didn't when I started trading, more than a decade ago.
                 What surprised me was how much external factors, emotions, bias,
                 a Silicon Valley CEO's tweets, ..., tended to play into my analyses,
                 keeping me from finding a consistent strategy.
                 Like many others, I have spent way too much time manually crunching charts,
                 only to start from scratch the next day. Not only is this time-consuming,
                 it often interferes with the end goal: consistently profitable trades.

                 In 2017, after several years trading, while studying at the university of Milan,
                 I was fed up with these inefficiencies and the substandard results all available tools produced.
                 I set out to build a new type of quantitative analysis platform,
                 one that would give me the tools to build better plans while helping to
                 mitigate profit-killing blunders by making analysis more consistent.
                 Today, this platform has evolved into the most comprehensive analysis software
                 on the market and I continue to innovate.",
                style = "text-align: justify; font-size:14px;"
              )
            ),
            column(
              12, # align = "center",
              h6("See how IQAT software can help you make smarter,
                        more efficient investment decisions: ", # , style = "font-weight:bold; "
                style = "text-align: justify; font-size:14px;"
              )
            ),
            column(12, br()),
            column(
              4,
              userBlock(
                image = base64enc::dataURI(
                  file = paste0(pathwd_dash, "/www/icon_target.jpg"), # icon_gears.jpg
                  mime = "image/png"
                ),
                title = h4("Automate your grunt work",
                  style = "text-align: justify;
                                          font-weight: bold; font-size:14px; color: #000;"
                ),
                subtitle = "Customize and automate the manual technical analysis you would otherwise do by hand - your way, your rules."
              )
            ),
            column(
              4,
              userBlock(
                image = base64enc::dataURI(
                  file = paste0(pathwd_dash, "/www/icon_target.jpg"), # icon_speed.jpg
                  mime = "image/png"
                ),
                title = h4("Speed up your analysis",
                  style = "text-align: justify;
                                          font-weight: bold; font-size:14px; color: #000;"
                ),
                subtitle = "Comprehensive, fully automated technical analysis on any chart, on-demand, in under 60 seconds."
              )
            ),
            column(
              4,
              userBlock(
                image = base64enc::dataURI(
                  file = paste0(pathwd_dash, "/www/icon_target.jpg"),
                  mime = "image/png"
                ),
                title = h4("Improve your accuracy",
                  style = "text-align: justify;
                                          font-weight: bold; font-size:14px;color: #000;"
                ),
                subtitle = "Analysis powered by algorithms, formulas and math, not only tweets,
                           'expert opinions' or 'gut feelings.'"
              )
            ),
            column(12, br()),
            column(
              4,
              userBlock(
                image = base64enc::dataURI(
                  file = paste0(pathwd_dash, "/www/icon_target.jpg"), # icon_alerts.jpg
                  mime = "image/png"
                ),
                title = h4("Reduce costly analysis mistakes",
                  style = "text-align: justify;
                                          font-weight: bold; font-size:14px;color: #000;"
                ),
                subtitle = "Remove bias, emotion, social influence, FOMO, and other costly analysis mistakes from your routine."
              )
            ),
            column(
              4,
              userBlock(
                image = base64enc::dataURI(
                  file = paste0(pathwd_dash, "/www/icon_target.jpg"), # icon_charts.jpg
                  mime = "image/png"
                ),
                title = h4("Find winning chart setups",
                  style = "text-align: justify;
                                          font-weight: bold; font-size:14px;color: #000;"
                ),
                subtitle = "Market Scanner makes it easier than ever for you to quickly and accurately find actionable chart patterns."
              )
            ),
            column(
              4,
              userBlock(
                image = base64enc::dataURI(
                  file = paste0(pathwd_dash, "/www/icon_target.jpg"), # icon_calendar.jpg
                  mime = "image/png"
                ),
                title = h4("Time your trades with precision",
                  style = "text-align: justify;
                                          font-weight: bold; font-size:14px;color: #000;"
                ),
                subtitle = "Use algorithms to watch your chart setups for you in real-time to make your trade timing more precise."
              )
            ),
            column(12, br())
          ) # fluidRow
        ), # bs4Card


        fluidRow(column(12, style = "margin-top:5px", br())),
        fluidRow(
          # 1
          col_4(
            bs4Card(
              id = "Home_card1_id",
              title = h4("Quick & Easy to Use",
                style = "text-align: justify;
                     font-size:16px; font-weight: bold; color: #0000FF;"
              ),
              status = "info",
              solidHeader = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              label = NULL,
              width = 12,
              height = "300px",
              tagList(
                p("IQAT is for all investors. If you use Yahoo Finance, MSN Money, your brokerage company's
              web site or any other financial sites for investment research, you owe it to yourself to
              check out our App. Every feature we provide is built around demand from real investors like you.
              IQAT is a powerful investment research and analysis tool that goes well beyond what investors
              are used to from existing equity research sites. As an advanced web platform, IQAT uses many
              standard desktop software user interface conventions. This makes IQAT powerful and functional,
              while still being quick to learn and simple to use. IQAT is also fully mobile, working seamlessly
              on desktops, laptops, tablets and phones.",
                  style = "text-align: justify; font-size:14px;"
                )
              ) # tagList
            ) # bs4Card
          ),
          # 2
          col_4(
            bs4Card(
              id = "Home_card2_id",
              title = h4("Powerfull Screening with Essential Features",
                style = "text-align: justify; font-size:16px;font-weight: bold; color: #0000FF;"
              ),
              status = "info",
              solidHeader = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              label = NULL,
              width = 12,
              height = "300px",
              tagList(
                p("Our screening capabilities are unmatched at any price point.
              Many users discover IQAT first for its powerful screeners that can find and rank
              financial instruments. Take a deep dive on any stock with one click.
              Everything you would want to know about the financial markets is incorporated into
              the app and reports. The information is displayed in a clear and well-structured way,
              making it effortless to do comprehensive research on a market.",
                  style = "text-align: justify; font-size:14px;"
                )
              ) # tagList
            ) # bs4Card
          ),
          # 3
          col_4(
            bs4Card(
              id = "Home_card3_id",
              title = h4("ideal for Comparison",
                style = "text-align:justify; font-size:16px; font-weight: bold; color: #0000FF;"
              ),
              status = "info",
              solidHeader = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              label = NULL,
              width = 12,
              height = "300px",
              tagList(
                p("Comparison is our specialty. No other site evaluates competing investment options like IQAT.
              We compute fair values and margin of safety, warn about trends that deserve your attention,
              and score companies for quality, growth, value and sentiment. View performance metrics that are
              accurate to the minute. Our charts go beyond the industry standard to let you compare screeners,
              portfolios and any benchmark. You can even chart fundamental metrics. A Picture is Worth 1000 Words.",
                  style = "text-align:justify; font-size:14px;"
                )
              ) # tagList
            ) # bs4Card
          )
        )
        # ------------ #
        # fluidRow(
        #   # 1
        #   col_4(
        #     bs4Card(
        #       id = "Home_card4_id",
        #       title = h4("Bespoke Experiences at Scale", style = "font-weight: bold; color: #006729;"),
        #       status = "info",
        #       solidHeader = FALSE,
        #       collapsible = FALSE,
        #       collapsed = FALSE,
        #       closable = FALSE,
        #       label = NULL,
        #       width = 12,
        #       height = "150px",
        #       tagList(
        #         p("Alternate between solid and variable text,
        #           graphs and images, based on the respective answers of each individual parameters.
        #           Our tool allows you to design your very own tailor-made reports! ")
        #       ) # tagList
        #     ) # bs4Card
        #   ),
        #   # 2
        #   col_4(
        #     bs4Card(
        #       id = "Home_card5_id",
        #       title = h4("Smart Dashboards", style = "font-weight: bold; color: #006729;"),
        #       status = "info",
        #       solidHeader = FALSE,
        #       collapsible = FALSE,
        #       collapsed = FALSE,
        #       closable = FALSE,
        #       label = NULL,
        #       width = 12,
        #       height = "150px",
        #       tagList(
        #         p("Combine multiple views of data to get richer insight. Best practices of data visualization are baked right in.
        #           Our tool allow you to display data the way you want: your metrics, dimensions, filters, segments, graph-type ...
        #           it's all in your hands!")
        #       ) # tagList
        #     ) # bs4Card
        #   ),
        #   # 3
        #   col_4(
        #     bs4Card(
        #       id = "Home_card6_id",
        #       title = h4("As simple as ABC", style = "font-weight: bold; color: #006729;"),
        #       status = "info",
        #       solidHeader = FALSE,
        #       collapsible = FALSE,
        #       collapsed = FALSE,
        #       closable = FALSE,
        #       label = NULL,
        #       width = 12,
        #       height = "150px",
        #       tagList(
        #         p("Easy to use, flexible and reliable! Our tool fits seamlessly with your existing IT applications,
        #           and does not require any prior reporting know-how.")
        #       ) # tagList
        #     ) # bs4Card
        #   )
        # )
        # ----------- #
      ) # ,
      # ------------------ # pic
      # col_5(
      #   tags$a(img(src = base64enc::dataURI(file = paste0(pathwd_dash, "/www/Home_Pic.jpg"), mime = "image/png"),
      #              height = "950px", width = "100%"), style = "margin-top: 15px; margin-left: 1px;")
      # )
    ),

    # fluidRow
    # ---------
    #  fluidRow(
    #   tablerDashBody(
    #   tablerProfileCard(
    #     width = 12,
    #     title = "Reporting",
    #     subtitle = "text ...",
    #     background = base64enc::dataURI(file = paste0(pathwd_dash, "/www/Dash_Image.jpg"), mime = "image/png"),
    #     src = base64enc::dataURI(file = paste0(pathwd_dash, "/www/logorep.jpg"), mime = "image/png"),
    #     tablerSocialLinks(
    #       tablerSocialLink(
    #         name = "facebook",
    #         href = "https://www.facebook.com",
    #         icon = "facebook"
    #       ),
    #       tablerSocialLink(
    #         name = "twitter",
    #         href = "https://www.twitter.com",
    #         icon = "twitter"
    #       )
    #     )
    #   )
    #   )
    # ) # fluidRow
    # ----
  )
}

#' welcome Server Function
#'
#' @noRd
mod_welcome_server <- function(input, output, session) {
  ns <- session$ns
}



#' About UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_about_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      h3("My Story")
    ),
    fluidRow(
      col_12(
        p("My name is Mohsen Asgari. I'm an independent financial data analysts and beside software developer.",
          style = "text-align: justify; font-size: 20px;"
        ),
        p("I've been in Financial markets as an analyst and trader for over 10 years and creating software professionally about 5 years.
      I am passionate about making useful tools for real people and companies.
      Several years ago I went looking for software to help me with my
      investments, but couldn't find anything for the average investor like myself.
      Everything that I found was either very expensive, not easy to use, or designed only for
      professionals. So I decided to build my own, making it simple and affordable,
      so that others could benefit too. There you go, IQAT was born.
      The solutions have come a long way since its initial release.
      It continues to grow, with new features added regularly.",
          style = "text-align: justify; font-size: 20px;"
        )
      )
    )
  )
} # mod_about_ui


#' About Server Function
#'
#' @noRd
mod_about_server <- function(input, output, session) {
  ns <- session$ns
}
