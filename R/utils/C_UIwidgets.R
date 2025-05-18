

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


# ---------------------------------------------------------------------------- #
# Class
# ---------------------------------------------------------------------------- #
C_UIwidgets <- methods::setRefClass(Class = "C_UIidgets", 
                                    fields = list(
                                      id = "character",
                                      params = "list",
                                      type = "list"
                                    ))
# ---------------------------------------------------------------------------- #
# Methods / Functions
# ---------------------------------------------------------------------------- #
C_UIwidgets$methods(
  
  # -------------------------------------------------------------------------- # 
  wdg_fileInput = function(.self, inputId, label, multiple = FALSE,
                           accept = NULL,
                           buttonLabel = "Browse...", 
                           placeholder = "No file selected"){
    shiny::fileInput(inputId = NS(namespace = .self$id, id = inputId),
                     label = label, 
                     accept = accept,
                     buttonLabel = buttonLabel, 
                     placeholder = placeholder,
                     multiple = multiple)
  },
  
  # -------------------------------------------------------------------------- # 
  
  # -------------------------------------------------------------------------- # 
  wdg_selectInput = function(.self, session = NULL, inputId, label = NULL, choices, 
                             selected, 
                             width = "100%",
                             height = "30px", 
                             fontsize = "11px",
                             type = "ui"){
    if(type == "ui"){
      return(
        #div(
        
        selectInput(inputId = NS(namespace = .self$id, id = inputId),
                    label = label,
                    choices = choices,
                    selected = selected,
                    width = width)
        
      )
    }
    if(type == "update"){
      if(!is.null(label)){
        return(shiny::updateSelectInput(session = session, 
                                        label = label,
                                        inputId = inputId,
                                        choices = choices,
                                        selected = selected))
      }else{
        return(shiny::updateSelectInput(session = session, 
                                        inputId = inputId,
                                        choices = choices,
                                        selected = selected))
      }
      
    }
  },
  
  # -------------------------------------------------------------------------- #
  wdg_DTOutput = function(.self, outputId){
  
    DT::dataTableOutput(outputId = NS(namespace = .self$id, id = outputId), width = "100%", 
                        height = "auto", fill = T)
  },
  
  # -------------------------------------------------------------------------- #
  wdg_plotOutput = function(.self, outputId, height = "400px"){
    shiny::plotOutput(outputId = NS(namespace = .self$id, id = outputId),
                      width = "100%", height = height )
  },
  
  # -------------------------------------------------------------------------- #
  wdg_selectizeInput = function(.self, session = NULL, output = NULL, inputId, label = NULL, 
                                choices = NULL, selected = NULL, multiple = TRUE,
                                placeholder = NULL, type = NULL){
    if(type == "ui"){

      return(
        shiny::selectizeInput(
          inputId = NS(namespace = .self$id, id = inputId),
          label = label, 
          choices = choices,
          selected = selected,
          multiple = multiple,
          options = list(
            placeholder = placeholder,
            plugins = list("remove_button", "drag_drop") )
        )
      )
    }
    if(type == "update"){
      return(shiny::updateSelectizeInput(session = session, 
                                         inputId = inputId,
                                         choices = choices,
                                         selected = selected))
    }
  },
  
  # -------------------------------------------------------------------------- #
  wdg_numericInput = function(.self, inputId = NULL, label = NULL, 
                              value = 0, min = NA, max = NA, step = NA){
    shiny::numericInput(
      inputId = NS(namespace = .self$id, id = inputId), 
      label = label, 
      value = value, 
      min = min, max = max, step = step )
  },
  
  # -------------------------------------------------------------------------- #
  wdg_textAreaInput = function(.self, inputId = NULL, label = NULL,
                               value = "",  height = 50,
                               cols = NULL, rows = NULL, placeholder = NULL,
                               font = 12, width = NULL){
    fluidRow(
      tags$style(paste0("#", NS(namespace = .self$id, id = inputId), " {font-size:", font,"px; height:", height,"px;}")),
      shiny::textAreaInput(inputId = NS(namespace = .self$id, id = inputId), 
                           label = p(label, style = "font-weight:bold;margin-bottom:-18px;"), 
                           value = value, width = width, height = height,
                           cols = cols, rows = rows, 
                           placeholder = placeholder, resize = NULL)
    )
  },
  
  # -------------------------------------------------------------------------- #
  wdg_textInput = function(.self, inputId = NULL, label = NULL,
                           value = "", width = NULL, placeholder = NULL,
                           font = 12,
                           height = 33){
    fluidRow(
      tags$style(paste0("#", NS(namespace = .self$id, id = inputId), " {font-size:", font,"px; height:", height,"px;}")),
      shiny::textInput(inputId = NS(namespace = .self$id, id = inputId), 
                       label = label, 
                       value = value, 
                       width = NULL, 
                       placeholder = placeholder)
    )
  },
  # -------------------------------------------------------------------------- #
  wdg_textarea = function(.self, inputId = NULL, label = NULL,
                          value = "", width = NULL, height = NULL,
                          cols = NULL, rows = NULL, placeholder = NULL){
    
    fluidRow(
      tags$style(type = "text/css", "textarea {width:100%; font-size: 12px;
                                                    border: 1px solid #eeeeee;}") ,
      tags$b(label),
      tags$textarea(id = NS(namespace = .self$id, id = inputId),
                    value = value,
                    rows = rows, placeholder = placeholder )
    )
  },
  
  # -------------------------------------------------------------------------- #
  wdg_checkboxGroupInput = function(.self, inputId = NULL, label = NULL, 
                                    choices = NULL, 
                                    selected = NULL, 
                                    inline = FALSE,
                                    width = NULL){
    setid = NS(namespace = .self$id, id = inputId)
    return(
      shiny::checkboxGroupInput(inputId = setid,
                                label = label, 
                                choices = choices, 
                                selected = selected, 
                                inline = inline,
                                width = width)
    )
    
  },
  
  # -------------------------------------------------------------------------- #
  wdg_checkboxInput = function(.self, inputId = NULL, label = NULL, value = FALSE){
    shiny::checkboxInput(inputId = NS(namespace = .self$id, id = inputId),
                         label = label, value = value, width = NULL)
    
  },
  
  # -------------------------------------------------------------------------- #
  wdg_radioButtons = function(.self, inputId = NULL, label = NULL,
                              choices = NULL, selected = NULL, inline = FALSE){
    shiny::radioButtons(inputId = NS(namespace = .self$id, id = inputId),
                        label = label, choices = choices, selected = selected,
                        inline = inline)
  }, 
  # -------------------------------------------------------------------------- #
  wdg_actionButton = function(.self, inputId = NULL, label = NULL, 
                              icon = icon("refresh", verify_fa = FALSE), 
                              width = "100%",
                              align = "center", style = ""){
    column(12, align = align,
           shiny::actionButton(inputId = NS(namespace = .self$id, id = inputId), 
                               label = label,
                               width = width, 
                               icon = icon , # class = "btn-success",
                               style = paste0("color: #000; background-color: 
                        white; border-color: #006729;", style) )
    )
    # set style auto
  },
  # -------------------------------------------------------------------------- #
  show = function(){
    
    cat("class: ", classLabel(class(.self)), " is initiated. \n")
    if(length(params) >= 1){ cat("Length of params list:", length(params), "\n")}
    if(exists("id") && length(id) >= 1){ cat("id: ", id, "\n")}
    # cat("All Shiny Widget Methods: ", grep("wdg", C_UIwidgets$methods(), value = T) )
    
  }
)

# initiate 
# print(C_UIwidgets())
# obj_C_UIwidgets <- C_UIwidgets(id = "fff")

# ---------------------------------------------------------------------------- #
col_12 <- function(...){
  shiny::column(12, ...)
}

col_11 <- function(...){
  shiny::column(11, ...)
}

col_10 <- function(...){
  shiny::column(10, ...)
}

col_9 <- function(...){
  shiny::column(9, ...)
}


col_8 <- function(...){
  shiny::column(8, ...)
}


col_7 <- function(...){
  shiny::column(7, ...)
}


col_6 <- function(...){
  shiny::column(6, ...)
}


col_5 <- function(...){
  shiny::column(5, ...)
}

col_4 <- function(...){
  shiny::column(4, ...)
}


col_3 <- function(...){
  shiny::column(3, ...)
}


col_2 <- function(...){
  shiny::column(2, ...)
}


col_1 <- function(...){
  shiny::column(1, ...)
}


# ---------------------------------------------------------------------------- #
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

# ---------------------------------------------------------------------------- #
enurl <- function(url, text){
  tags$a(href = url, text)
}
# ---------------------------------------------------------------------------- #
list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(lapply(list, tags$li))
  } else {
    res <- lapply(list, tags$li)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }
  
}

# ---------------------------------------------------------------------------- #
list_to_p <- function(list, class = NULL){
  if (is.null(class)){
    tagList(lapply(list, tags$p))
  } else {
    res <- lapply(list, tags$p)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }
  
}

# ---------------------------------------------------------------------------- #
named_to_li <- function(list, class = NULL){
  if(is.null(class)){
    res <- mapply(
      function(x, y){
        tags$li(HTML(glue::glue("<b>{y}:</b> {x}")))
      },
      list, names(list), SIMPLIFY = FALSE)
    #res <- lapply(res, HTML)
    tagList(res)
  } else {
    res <- mapply(
      function(x, y){
        tags$li(HTML(glue("<b>{y}:</b> {x}")))
      },
      list, names(list), SIMPLIFY = FALSE)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }
}




namedlist_func <<- function(...) { 
  l <- setNames( list(...) , as.character( match.call()[-1]) ) 
  l
}

named.list <<- function(...){
  l <- setNames( list(...), as.character( match.call()[-1]) )
  l
}

month_name_func <- function(n){
  
  month.names = c("January",
                  "February",
                  "March",
                  "April",
                  "May",
                  "June",
                  "July",
                  "August",
                  "September",
                  "October",
                  "November",
                  "December")
  
  month.name <- month.names[n]
  return(month.name)
  
}

#' Turn an R list into an HTML list
#'
#' @param list An R list
#'
#' @return an HTML list
#' @export
#'
#' @importFrom htmltools tags tagAppendAttributes tagList
#'
#' @rdname lists
#'
#' @examples
#' list_to_li(c("a","b"))

list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(lapply(list, tags$li))
  } else {
    res <- lapply(list, tags$li)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }
  
}

#' @export
#' @rdname lists
#' @importFrom htmltools tags tagAppendAttributes tagList

list_to_p <- function(list, class = NULL){
  if (is.null(class)){
    tagList(lapply(list, tags$p))
  } else {
    res <- lapply(list, tags$p)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }
  
}

#' @export
#' @rdname lists
#' @importFrom glue glue
#' @importFrom htmltools tags tagAppendAttributes tagList


named_to_li <- function(list, class = NULL){
  if(is.null(class)){
    res <- mapply(
      function(x, y){
        tags$li(HTML(glue("<b>{y}:</b> {x}")))
      },
      list, names(list), SIMPLIFY = FALSE)
    #res <- lapply(res, HTML)
    tagList(res)
  } else {
    res <- mapply(
      function(x, y){
        tags$li(HTML(glue("<b>{y}:</b> {x}")))
      },
      list, names(list), SIMPLIFY = FALSE)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @export
#'
#' @examples
#' \dontrun{
#' a <- tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
#' }
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[ attrs[i] ]] <- NULL
  }
  tag
}

#' Hide or display a tag
#'
#' @param tag the tag
#'
#' @return a tag
#' @export
#
#' @importFrom htmltools tagList
#'
#' @examples
#' \dontrun{
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' }
#' @rdname display

undisplay <- function(tag) {
  # if not already hidden
  if (!is.null(tag$attribs$style) && !grepl("display:\\s+none", tag$attribs$style)) {
    tag$attribs$style <- paste("display: none;", tag$attribs$style)
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @rdname display
#' @importFrom htmltools tagList
#'
#' @examples
#'\dontrun{
#' ## Show
#' a <- shiny::tags$p(src = "plop", "pouet")
#' a_hidden <- undisplay(a)
#' display(a_hidden)
#' # do not change not hidden tags
#' b_show <- shiny::actionButton("go_filter", "go")
#' display(b_show)
#' # Keep other attributes
#' b_show$attribs$style <- 'display: none; background: red'
#' display(b_show)
#' }
#' @export

display <- function(tag) {
  if (!is.null(tag$attribs$style) && grepl("display:\\s+none", tag$attribs$style)) {
    tag$attribs$style <- gsub("(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*", "", tag$attribs$style)
  }
  tag
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @export
#'
#' @examples
#' with_red_star("Enter your name here")
#'
#' @importFrom htmltools tags HTML
#'
with_red_star <- function(text) {
  htmltools::tags$span(
    HTML(
      paste0(
        text,
        htmltools::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @export
#'
#' @examples
#' rep_br(5)
#'
#' @importFrom htmltools HTML

rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @export
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")

enurl <- function(url, text){
  tags$a(href = url, text)
}


#' Columns 12, 6 and 4
#' 
#' Most shiny columns are 12, 6 or 4 of width. 
#' These are convenient wrappers around 
#' `column(12, ...)`, `column(6, ...)` and `column(4, ...)`.
#' 
#' @export
#' @rdname columns
col_12 <- function(...){
  shiny::column(12, ...)
}

#' @export
#' @rdname columns
col_11 <- function(...){
  shiny::column(11, ...)
}
#' @export
#' @rdname columns
col_10 <- function(...){
  shiny::column(10, ...)
}
#' @export
#' @rdname columns
col_9 <- function(...){
  shiny::column(9, ...)
}

#' @export
#' @rdname columns
col_8 <- function(...){
  shiny::column(8, ...)
}

#' @export
#' @rdname columns
col_7 <- function(...){
  shiny::column(7, ...)
}

#' @export
#' @rdname columns
col_6 <- function(...){
  shiny::column(6, ...)
}

#' @export
#' @rdname columns
col_5 <- function(...){
  shiny::column(5, ...)
}

#' @export
#' @rdname columns
col_4 <- function(...){
  shiny::column(4, ...)
}

#' @export
#' @rdname columns
col_3 <- function(...){
  shiny::column(3, ...)
}

#' @export
#' @rdname columns
col_2 <- function(...){
  shiny::column(2, ...)
}

#' @export
#' @rdname columns
col_1 <- function(...){
  shiny::column(1, ...)
}