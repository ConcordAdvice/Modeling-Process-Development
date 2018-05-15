

data_wd <- "W:/Analytics_Group_Files/US_files/Modeling_Process/Modeling_Process_Gamma/Data_Rda_Update"
source_wd <- "W:/Analytics_Group_Files/US_files/Modeling_Process/Modeling_Process_Delta/Source"
project_wd <- "W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1803_New_WaterFall_Design"






# <------------------------------------------------------------------->
# Section One : Data Preparation
# Data Type and Category
# Choose Dependent and Independent
# Out lier and Special Value
# Training, Validation, Testing sampling and split
# <------------------------------------------------------------------->


"Blank_Space"
"Blank_None"
"Outlier_Range"
"Outlier_Type"

-0.01
-0.02
-0.03















# <------------------------------------------------------------------->
# Section Two : Modeling 
# <------------------------------------------------------------------->


a <- 988641200
b <- c(002341566,235341566,012358966,002346766,005341566)
nchar(b)
b[nchar(b)<8]

Target_Or_Not <- nchar(b)<8
Target_Or_Not <- nchar(b)<9



b <- as.character(b)
b[Target_Or_Not] <- paste0("00",b[Target_Or_Not])



last_5_2 <- substr(a,nchar(a)-2,nchar(a)-1)

nchar(b)


last_5_2 <- substr(a,nchar(a)-2,nchar(a)-1)

sample_str_long <- "c01.bank_account_zeros~4;c01.date_of_birth~1956-06-07;c01.date_of_last_activity~2018-04-23T12:30:52Z;c01.date_of_next_payday~2018-05-01"
str <- sample_str_long

FUNCTION_split_vector_str <- function(str){
  sign1 <- "~"
  sign2 <- ";"
  by_var <- strsplit(str,sign2)
  len_by_var <- lengths(by_var)
  var_names <- vector(length = len_by_var)
  var_value <- vector(length = len_by_var)
  for(i in 1:len_by_var){
    by_var_split <- as.character(strsplit(by_var[[1]][i],sign2))
    var_names[i] <- as.character(strsplit(by_var_split,sign1)[[1]][1])
    var_value[i] <- as.character(strsplit(by_var_split,sign1)[[1]][2])
  }
  var_value <- as.data.frame(t(var_value),stringsAsFactors = F)
  names(var_value) <- var_names
  return(var_value)
}

str <- FUNCTION_split_vector_str(sample_str_long)


# <------------------------------------------------------------------->
# This function will generate a time range sheet with 6 new columns
# The one end with "IND" are numeric column, without are characters
# <------------------------------------------------------------------->

