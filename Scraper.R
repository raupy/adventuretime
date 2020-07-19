library(httr)
library(jsonlite)
library(stringr)
library(data.table)

## --------- functions for scraping --------------------- 



get_names = function(){
  url = "https://adventuretime.fandom.com/wiki/Category:Characters"
  from = "?from="
  list = get_names_for_url(url)
  for(i in 1:6){
    print(list[length(list)])
    next_url = paste(c(url, from, list[length(list)]), collapse = "")
    print(next_url)
    l = get_names_for_url(next_url)
    print(length(l))
    k = c(list, l)
    list = k
  }
  list
}

get_names_for_url = function(url){
  chars = GET(url)
  raw = rawToChar(chars$content)
  chars = str_sub(raw, str_locate(raw, "All items")[1], str_length(raw))
  s = str_split(chars, fixed("<a href=\"/wiki/"))
  chars = s[[1]][c(T,F)]
  n = lapply(chars[-1], subname, pattern = "\"", nr = 1)
  n = delete_betises(n, ":")
  n = delete_betises(n, fixed("Local_Sitemap"))
}


delete_betises = function(n, pat){
  x = which(str_locate(n, pat) != "")
  if(length(x) > 0) n = n[-x]
  else n
}



## now the functions for the data table

create_and_save_table = function(){
  table = get_table(read_names())
  write.csv(table, file = "adventuretime.csv")
}

read_names = function(){
  names <- readRDS("characterNames.rds")
}

get_table = function(names){
  #exclude unwanted pages:
  #Bird_Woman (74) (redirection to Gunter's page)
  #Forest_inhabitant (418) (group of characters)
  #Son_of_Rap_Bear_(character) (999) (stub)
  c = c(74, 418, 999)
  names = names[-c]
  rbindlist(lapply(names, get_row), fill = T)
}

get_url = function(name){
  paste(c("https://adventuretime.fandom.com/api.php?action=query&prop=revisions&titles=", 
          name, "&rvprop=content&format=json"), collapse = "")
}


get_row = function(name){
  nap(0.5)
  t = GET(get_url(name))
  this.raw.content <- rawToChar(t$content)
  this.content <- fromJSON(this.raw.content)
  df = as.data.frame(this.content[[1]])
  text = df[length(df)][[1]]
  text = gsub("<[^>]+>", "",text)
  text = str_replace_all(text, "'", "")
  text = str_replace_all(text, "[\\[\\]]", "")
  text = delete_if_it_exists(text, "DISPLAYTITLE", fixed("\n"))
  text = delete_if_it_exists(text, "Youmay|", fixed("\n"))
  text = delete_if_it_exists(text, "Youmay|", fixed("\n"))
  splits = str_split(text, "\\}\\}")
  x = which(str_locate(splits[[1]], fixed("|")) != "")[1]
  cbind(get_entries(splits[[1]][x], name), get_categories(splits[[1]][length(splits[[1]])]))
}

get_entries = function(cols, c_name){
  cols = str_replace_all(cols, "[ \t\r\n]", "")
  data = str_split(cols, fixed("|"))
  x = which(str_locate(data[[1]], fixed("=")) != "")
  data[[1]][x[x < length(data[[1]]) + 1]]
  list = lapply(data[[1]][x[x < length(data[[1]]) + 1]], get_df)
  n = str_replace_all(c_name, fixed("_"), " ")
  #if("name" %in% names(list) == FALSE || list$name != n)
  #  list[[1]]$name = n
  as.data.frame(list)
}

get_categories = function(cats){
  splits = str_split(cats, "Category:")
  df = data.frame(categories = paste(splits[[1]][2:length(splits[[1]])],collapse=", "))
}

get_df = function(x_eq_y){
  splits = str_split(x_eq_y, "=")
  df = data.frame(splits[[1]][2])
  names(df) <- splits[[1]][1]
  if(names(df) == "species" && str_sub(df$species, str_length(df$species), str_length(df$species)) == "s")
    df$species = str_sub(df$species, 1, str_length(df$species) -1)
  df
}

delete_if_it_exists = function(string, pattern, splitter){
  if(grepl(pattern, string, fixed = T)){
    start = str_locate(string, splitter)[1]
    string = str_sub(string, start + 1, str_length(string))
  }
  string
}

subname = function(name, pattern, nr){
  splits = str_split(name, pattern)
  splits[[1]][nr]
}

nap = function(x)
{
  p1 = proc.time()
  Sys.sleep(x)
  proc.time() - p1 
}
