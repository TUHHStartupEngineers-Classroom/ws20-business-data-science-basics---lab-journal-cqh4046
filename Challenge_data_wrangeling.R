# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# Counter
library(tictoc)

#Load patent.tsv into memory

#Table of entrys
col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

#reading in the file
patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

class(patent_tbl)
setDT(patent_tbl)

assignee <- fread("assignee.tsv")
class(assignee)
setDT(assignee)

patent_assignee <- fread("patent_assignee.tsv")
class(patent_assignee)
setDT(patent_assignee)

## set cols

assigne_cols <- c("id", "types", "name_first", "name_last", "organization")
assignee_tbl <- assignee_tbl[, ..assigne_cols]
##Question 1
# who has the most patents

combined_data <- merge(x = assignee, y = patent_assignee, 
                       by.x= "id",
                       by.y= "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
temp <- combined_data[!is.na(patent_id), .N, by= organization]
temp[order(-N)] %>% head(n=11)

##Question 2
#who has the most granted patent in 2019
combined_data_2 <- merge(x = assignee, y = patent_assignee, 
                       by.x= "id",
                       by.y= "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
combined_data_tbl <- merge(x = combined_data_2, y = patent_tbl, 
                         by.x= "patent_id",
                         by.y= "id", 
                         all.x = TRUE, 
                         all.y = FALSE)

temp <- combined_data_tbl[!is.na(patent_id) & year(date)==2019, .N, by= organization]
temp[order(-N)] %>% head(n=11)
##Question 3 
#what is the most innovative tech-sector

#read in file
uspc <- fread("uspc.tsv")
class(uspc)
setDT(uspc)

uspc_main_class_id_tbl <-  uspc[,.N, by = mainclass_id]
uspc_main_class_id_tbl[order(-N)]

#combine into the previous files
combined_data_tbl_3 <- merge(x = combined_data_tbl, y = uspc, 
                           by.x= "patent_id",
                           by.y= "patent_id", 
                           all.x = TRUE, 
                           all.y = FALSE)

#try to bring the data down to the answer
answer <- combined_data_tbl_3[!is.na(mainclass_id), .N, by = .(mainclass_id, organization)]
answer <- answer[order(-N)]%>% head(n=11)
answer <- answer[1:10]
answer <- answer[, .N, by = mainclass_id]
answer <- answer[order(-N)]
answer