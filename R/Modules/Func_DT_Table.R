

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


Func_DT_Table <- function(df, pageLength_Set = 20,
                          scrollY = '30vmax', type = "",
                          info = TRUE, 
                          filter = "bottom",
                          fontSize = '12px'){
  

  editable_Set = F

  if(type == "A"){
    
    datatable(df, selection = "none",  
              rownames = F , 
              options = list(paging = FALSE, 
                             initComplete = options_col_fontsize,
                             info = info,
                             autoWidth = F, dom = 'tip') ) %>% formatStyle(c(1:(ncol(df))), border = '1px solid #ddd') %>% formatStyle(
                as.character(colnames(df)), target = 'row', backgroundColor = "white") %>% 
      formatStyle(1, 'border-left' = '1px solid #C6C7C8') %>% formatStyle(ncol(df), 'border-right' = '1px solid #C6C7C8') %>%
      formatStyle(columns = colnames(df), fontSize = fontSize)
              
  }else if(type == "B"){
    
    datatable(df, selection = "none", filter = filter, 
              rownames = F , 
              options = list(paging = TRUE, 
                             info = info,
                             initComplete = options_col_fontsize,
                             pageLength = pageLength_Set, 
                             lengthMenu = list(c(5, 10, 15, 20, 25, 30, 50, 100, 1000, -1),
                                               c('5', '10', '15', '20', '25', '30','50', '100', '1000', 'All')),
                             scrollX = T, 
                             scrollY = scrollY, 
                             scrollCollapse = T,
                             autoWidth = F, 
                             dom = 'Blfrtip') 
              ) %>% formatStyle(c(1:(ncol(df))), border = '1px solid #ddd') %>% formatStyle(
                as.character(colnames(df)), target = 'row', backgroundColor = "white") %>% 
      formatStyle(1, 'border-left' = '1px solid #C6C7C8') %>% formatStyle(ncol(df), 'border-right' = '1px solid #C6C7C8')  %>% 
      formatStyle(columns = colnames(df), fontSize = fontSize)
  
  } else if(type != "A" && type != "B"){
  datatable(df,
            editable = editable_Set, 
            extensions = 'Buttons', 
            filter = "bottom", 
            selection = "none",
            options = list(paging = TRUE, pageLength = pageLength_Set, 
                                    lengthMenu = list(c(5, 10, 15, 20, 25, 30, 50, 100, 1000, -1),
                                                      c('5', '10', '15','20', '25', '30','50', '100', '1000', 'All')), 
                           # order = list(list(colOrder, 'desc')), 
                                    # columnDefs = list(list(className = 'dt-center', visible = FALSE, targets = column2hide)),
                           dom = 'Blfrtip',
                           info = info,
                           autoWidth = T,
                           scrollX = T, 
                           scrollY = scrollY, 
                           scrollCollapse = T,
                           buttons = c("colvis", 'copy', 'excel')
                           ), 
            fillContainer = TRUE,
            caption = "", 
            rownames = F) %>% formatStyle(c(1:(ncol(df))), border = '1px solid #ddd') %>% formatStyle(
        as.character(colnames(df)), target = 'row', backgroundColor = "white") %>% 
        formatStyle(1, 'border-left' = '1px solid #C6C7C8') %>% formatStyle(ncol(df), 'border-right' = '1px solid #C6C7C8')
  }
  
}
