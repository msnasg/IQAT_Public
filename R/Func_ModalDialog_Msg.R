

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


#' create Message box in App
#'
#' @param type default, ...
#' @param title title of box
#' @param footer footer of box
#' @param size size of box
#' @param btn button at box
#' @return box
#' @export
#' @examples
#' Func_ModalDialog_Msg(input, output, session, typebox = "modal",
#'                      title = "Warning", ui = "First Select Excel file Tree.",
#'                      type = "default" ) 
#'                      
#' Func_ModalDialog_Msg(input, output, session, typebox = "sweet",
#'                      title = "Done", ui = "First Select Excel file Tree.",
#'                      type = "success" ) 
#'                      
Func_ModalDialog_Msg <- function(input, output, session, 
                                 typebox = "modal",
                                 type = "default",
                                 title = "",
                                 footer = modalButton("Close (Esc)"),
                                 size = "m",
                                 ui = NULL,
                                 btn = c("Cancel", "Ok")) {
  
  if(typebox == "modal"){
    
    showModal(modalDialog(
      title = title,
      fluidRow(
        hr(),
        column(12, wellPanel(
          style = "background-color: #fff; border-color: #F5F5F5; border-radius: 10px",
          ui
        ))
      ),
      easyClose = TRUE, size = size, fade = TRUE,
      footer =  footer ))
  } # default
  
  if(typebox == "sweet"){
    sendSweetAlert(
      session = session,
      title = title,
      text = ui,
      type = type
    )
  }
  
}
