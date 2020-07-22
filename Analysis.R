library(wordcloud2)
library(tm)
library(stringr)
library(ggplot2)
library(data.table)
library(treemap)
library(d3treeR)

####--------------------- word cloud -----------------------------------------------
word.cloud = function(col, df, sz){
  if(missing(df)){
    df = make.df.for.word.cloud(col)
  }
  if(missing(sz)){
    sz = 1.5
  }
  set.seed(1234) # for reproducibility 
  wordcloud2(data=df, size = sz, backgroundColor = "pink", shape = "pentagon", shuffle = FALSE)
}

get.words.for.cloud = function(file.name, col){
  umfrage8 = read.csv(file.name, header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
  words = umfrage8[which(umfrage8[col] != ""),]
  words = words[col]
  #words[which(words == "Masturbieren"),1] = "Masturbation"
  words
}

make.df.for.word.cloud = function(words_data){
  docs = Corpus(VectorSource(words_data))
  docs = tm_map(docs, removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE)
  docs = tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
}


####----------------------- categories handling -----------------------------------------



sort_vector = function(vec_to_sort, vec_to_adapt){
  vec_to_sort[order(match(vec_to_sort, vec_to_adapt))]
}

sort_entry = function(entry, vec){
  entry = tolower(entry)
  cats = str_split(entry, fixed(", "))
  cats = sort_vector(cats[[1]], vec)
  entry = str_c(cats, collapse = ", ")
  entry
}

sort_categories = function(table, col, vec_to_adapt){
  ref = lapply(table[[col]], sort_entry, vec = vec_to_adapt)
  refc = as.character(ref)
  fac = as.factor(refc)
  table$maincats = fac
  table
}

get_first_split = function(entry){
  cats = str_split(entry, fixed(", "))
  cats[[1]][1]
}

make_cat_table = function(col){
  rbindlist(lapply(col, make_cat_row), fill = T)
}

make_cat_row = function(entry){
  cats = str_split(entry, fixed(", "))
  df = data.frame(cat_main = cats[[1]][1])
  if(length(cats[[1]]) <= 1)
    return(df)
  for(i in 2:length(cats[[1]])){
    text = str_c(c("cat_", as.character(i)), collapse = "")
    df[text] = cats[[1]][i]
  }
  df
}

get_cats_tab = function(){
  df = make.df.for.word.cloud(tab$cats)
  vec = df$word
  test = sort_categories(tab, "cats", vec)
  df_test = data.frame(test$maincats, test$cats)
  cats_tab = make_cat_table(df_test$test.maincats)
  cats_tab = cbind(df_test$test.maincats,cats_tab)
  cats_tab = cats_tab[-c(296, 598, 726, 802, 847, 928),]
}

get_second_cats = function(tab, col, maincat){
  if(missing(col))
    col = "cat_main"
  rows = getRowsWithSpecialValue(tab, col, maincat, TRUE)
  count = sort((summary(as.factor(rows$cat_2))))
  dfs = as.data.frame(count)
  dimnames(dfs)[[1]][which(dimnames(dfs)[[1]] == "NA's")] = str_c(c("only", maincat), collapse = " ")
  dfs
}

tmap_data = function(cat, catstab){
  dfs = get_second_cats(catstab, maincat = cat)
  group <- c(rep(cat,length(dimnames(dfs)[[1]])))
  subgroup <- dimnames(dfs)[[1]]
  value <- dfs$count
  data <- data.frame(group,subgroup,value)
}

make_big_tmap_data = function(tab, col){
  if(missing(col))
    col = "cat_main"
  cats = dimnames(as.data.frame(sort(summary(tab[[col]]))))[[1]]
  c = c(which(cats == ""), which(cats == "na"), which(cats == "species"))
  cats = cats[-c]
  rbindlist(lapply(cats, tmap_data, catstab = tab), fill = T)
}

make_tree_map = function(tab){
  data = make_big_tmap_data(tab)
  p = treemap(data,
              index=c("group","subgroup"),
              vSize="value",
              type="index", 
              fontface.labels=c(2,1), 
              bg.labels=c("transparent"), 
              palette = "Set1", 
              fontsize.title=12, 
              title="Character Categories Treemap"  
  )
  inter <- d3tree2( p ,  rootname = "Character Categories Treemap" )
}



# ---------------------------------- misc ----------------------------------------------------------


getRowsWithSpecialValue = function(table, colname, value, perfectMatch, dontInvertQuery){
  if (missing(perfectMatch)) {
    perfectMatch = FALSE
  }
  if (missing(dontInvertQuery)) {
    dontInvertQuery = TRUE
  }
  if(perfectMatch){
    return(table[which(table[[colname]] == value),])
  }
  else
    return(table[which(grepl(value, table$categories, fixed = T) == dontInvertQuery),])
}

tab = read.csv("at_refactored.csv", encoding = "UTF-8")




#princesses = getRowsWithSpecialValue(table, "categories", "Princess")
#noprincesses = getRowsWithSpecialValue(table, "categories", "Princess", dontInvertQuery = FALSE)
#female = getRowsWithSpecialValue(table, "sex", "Female", perfectMatch = TRUE)
#femaleAndPrincess = getRowsWithSpecialValue(female, "categories", "Princess")

#t = princesses[which(princesses$species == "BreakfastPeople"),]

#t = princesses[which(princesses$actor == "GreyDeLisle"),]

#g = ggplot(df) 
#g + geom_bar(mapping = aes(x = specPrin)) + coord_flip()





#summary(as.data.frame(table$occup))
        

