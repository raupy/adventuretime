library(readr)
library(stringr)

refactor_names = function(table){
  names <- readRDS("characterNames.rds")
  #exclude unwanted pages:
  #Bird_Woman (74) (redirection to Gunter's page)
  #Forest_inhabitant (418) (group of characters)
  #Son_of_Rap_Bear_(character) (999) (stub)
  c = c(74, 418, 999)
  names = names[-c]
  c1 = which(match(table$name, NA) == 1)
  c2 = which(table$name == "{{PAGENAME")
  to_ref = sort(c(c1,c2))
  newNames = names[to_ref]
  table$name[to_ref] = newNames
  table$name = as.character(table$name)
  table
}

join_similar_cols = function(table, remaining_col, col_to_delete){
  c = which(table[col_to_delete] != "")
  values = table[[col_to_delete]][c]
  table[[remaining_col]][c] = values
  table[col_to_delete] = NULL
  table
}

refactor_categories = function(entry){
  entry = str_replace_all(entry, fixed("\n, Characters\n, "), ", ")
  entry = str_replace_all(entry, fixed(" Characters\n, "), ", ")
  entry = str_replace_all(entry, fixed("Characters\n, "), "")
  entry = str_replace_all(entry, fixed("Characters"), "")
  entry = str_replace_all(entry, fixed("Character"), "")
  entry = str_replace_all(entry, fixed("characters"), "")
  entry = str_replace_all(entry, fixed("character"), "")
  entry = str_replace_all(entry, "[\n]", "")
  cats = str_split(entry, fixed(", "))
  cats = lapply(cats[[1]], replace_white_spaces)
  cats = str_sort(cats)
  cats = str_c(cats, collapse = ", ")
  cats
}

replace_white_spaces = function(text){
  splits = str_split(text, " ")
  splits = splits[[1]][which(splits[[1]] != "")]
  splits = paste(splits, collapse = "-")
  splits
}

refactor_table = function(table){
  table = refactor_names(table)
  table = join_similar_cols(table, "lastseen", "Latestappearance")
  table = join_similar_cols(table, "lastseen", "latestappearance")
  table = join_similar_cols(table, "lastseen", "lastintroduced")
  table = join_similar_cols(table, "lastseen", "last")
  table = join_similar_cols(table, "sex", "gender")
  table = join_similar_cols(table, "name", "Name")
  table = join_similar_cols(table, "name", "Rname")
  table = join_similar_cols(table, "occup", "Occupation")
  table = join_similar_cols(table, "occup", "occupation")
  table = join_similar_cols(table, "age", "Age")
  table = join_similar_cols(table, "species", "Species")
  table = join_similar_cols(table, "firstintroduced", "Introducedin")
  table = join_similar_cols(table, "firstintroduced", "first")
  table = join_similar_cols(table, "alias", "aliases")
  del = c("image", "image1", "image2", "image3", "image4", "image5", "Image", "imagewidth", "imgewidth",
          "caption", "caption2", "caption3",
          "extrainfo1", "extrainfo2", "extrainfo3",
          "extra1", "extra2", "extra3",
          "form1", "form2", "form3", "form4", "form5",
          "residein", "location",
          "ruler", "bio", "status", "real", "members", "featured", "leader", "act",
          "origin", "owner", "knowncharacters", "type")
  table = delete_cols(table, del)
  ref = lapply(table$categories, refactor_categories)
  refc = as.character(ref)
  fac = as.factor(refc)
  table$cats = fac
  write_csv(table, path = "at_refactored.csv")
  table
}


delete_cols = function(table, cols){
  table[, cols] = NULL
  table
}


#376 löschen (Finn Sword), also auch origin und owner löschen
#473 löschen (GooSkulls), also auch knowncharacters löschen
#282, 552 löschen, members löschen
#730 löschen, leader löschen

#evtl form1, form2, form3, form4, form5 in neue tabelle

my.read.csv = function(file.name) {
  read.csv(file.name, header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
}

table = my.read.csv("adventuretime.csv")

