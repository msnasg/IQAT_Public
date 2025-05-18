
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.



if(FALSE){

  
library(fredr) 
library(purrr)


CategoriesList <- lapply(1:120, function(x){
  tryCatch({
    fredr_category(category_id = x)
  }, error = function(e){return(NULL)})
  })

CategoriesList <- CategoriesList[!sapply(CategoriesList,is.null)]

library(data.table)
x = rbindlist(CategoriesList, fill = TRUE)


fredr_category(category_id = 15)
fredr_category_children(category_id = 15L)

fredr_category_series(
  category_id = 15L, # Housing
  limit = 100L,
  search_text = "usa",
  order_by = "popularity",
  sort_order = "desc"

) 
  
fredr_category_related_tags(
  category_id = 15L,
  tag_names = "daily",
  #exclude_tag_names = "rate",
  order_by = "name"
)
  
popular_search_series <-  data.frame(fredr_series_search_text(
  search_text = "interest rate",
  order_by = "popularity",
  sort_order = "desc",
  limit = 1000
))

x = fredr_series_search_tags(
  series_search_text = "unemployment",
  limit = 100L
)

x = fredr_series_search_related_tags(
  series_search_text = "gnp",
  tag_names = "usa",
  limit = 30L
)

x = fredr_series(series_id = "DEXUSEU")
x = fredr_series_categories(series_id = "UNRATE")

updatelist = fredr_series_updates(limit = 10L)

updatelist = fredr_series_updates(
  start_time = Sys.time() - 60 * 60 * 24,
  end_time = Sys.time(),
  filter_value = "macro",
  limit = 10L
)

i = 1
id = updatelist$id[i]
tit = updatelist$title[i]
observation_start = as.Date("2020-01-01")
observation_end = as.Date("2024-01-01")


id = "DEXUSEU"

id %>%
  fredr(
    observation_start = observation_start,
    observation_end = observation_end
  ) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  ggtitle(tit) + 
  labs(x = "Observation Date", y = "Rate", color = "Series")


ids = c("DEXUSEU", "DEXCAUS","T10Y2Y", "T10Y3M")[1:2]
# ids = updatelist$id[1:2]

tit = unname(sapply(ids, function(x){
  paste0(x, ": ", updatelist[which(updatelist$id == x),"title"])}))

map_dfr(ids, fredr) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  ggtitle(tit[1]) +
  labs(x = "Observation Date", y = "Rate", color = "Series", subtitle = tit[2])
  # + scale_color_discrete(name = "Seies", label = tit)


} 
