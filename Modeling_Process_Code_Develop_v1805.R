
library(MASS)
library(partykit)
library(klaR)
library(glmnet)
library(Formula)

data_wd <- "W:/Analytics_Group_Files/US_files/Modeling_Process/Modeling_Process_Gamma/Data_Rda_Update"
source_wd <- "W:/Analytics_Group_Files/US_files/Modeling_Process/Modeling_Process_Delta/Source"
project_wd <- "W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1803_New_WaterFall_Design"
process_wd <- "W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1805_Modeling_Process_Development"


source("Modeling_Process_Code_Source_v1805.r")
Data_Independent <- get(load("Data_Independent_Clean_20180125.Rda"))
Data_Dependent <- get(load())

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

# <-------------------------------------------------------------------->
# Section Two : Feature Engineering
# Select the variable derive kits or sets
# Transform them and output the final data set for the modeling
# <-------------------------------------------------------------------->
install.packages("pryr")
library(pryr)
typeof(Age_date_to_days)


setwd(process_wd)
data <- get(load("Data_Independent_Clean_20180125.Rda"))
perf <- get(load("Data_Dependent_20171001_20180125_C.Rda"))
att <- read.csv(file="Variable_Attributes_20180125.csv")

data$MRID
perf$MRID
perf_name <- "FirstPaymentDefault"
data_mx <- merge(data,perf,by="MRID")
df <- data_mx[1:8000,]
df$FirstPaymentDefault <- as.character(df$FirstPaymentDefault)
df <- df[is.na(df$FirstPaymentDefault)==F,]
y <- df$FirstPaymentDefault


Create_Kit <- function(){
  
}

# "d04.hri_1","d04.hri_2","d04.hri_3",
# "d04.hri_4","d04.hri_5","d04.hri_6",
d04.Der_hri_code_XXX.DER <- function(data,tree){
  
  code_mx <- cbind(data$d04.hri_1,data$d04.hri_2,data$d04.hri_3,
                   data$d04.hri_4,data$d04.hri_5,data$d04.hri_6)
  
  data$d04.Der_hri_code_2_3_71_DI_CL_SSN_risk <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("2","3","71","DI","CL","02","03"))))
  data$d04.Der_hri_code_7_8_55_56_46_75_82_Phone_risk <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("7","8","55","56","46","75","82","07","08"))))
  data$d04.Der_hri_code_10_57_Mobile <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("10","57"))))
  data$d04.Der_hri_code_4_11_12_14_16_PO_SD_VA_Address_risk <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("4","11","12","14","16","PO","SD","VA","04"))))
  data$d04.Der_hri_code_19_25_49_SR_ZI_Address_Na <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("19","25","49","SR","ZI"))))
  data$d04.Der_hri_code_19_26_SSN_Na <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("19","26"))))
  data$d04.Der_hri_code_19_27_Phone_Na <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("19","27"))))
  data$d04.Der_hri_code_19_28_Birth_Na <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("19","28"))))
  data$d04.Der_hri_code_19_DV_DL_Na <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("19","DV","DL"))))
  data$d04.Der_hri_code_29_30_31_76_83_DM_MisKey <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("29","30","31","76","83","DM"))))
  data$d04.Der_hri_code_32_OFAC <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("32"))))
  data$d04.Der_hri_code_WL_OFAC_Watch <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("WL"))))
  data$d04.Der_hri_code_38_48_51_52_66_SSN_Na <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("38","48","51","52","66"))))
  data$d04.Der_hri_code_41_DD_DL_DF_risk <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("41","DD","DL","DF"))))
  data$d04.Der_hri_code_49_53_Distant <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("49","53"))))
  data$d04.Der_hri_code_74_Phone_Mismatch <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("74"))))
  data$d04.Der_hri_code_CA_Address <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("CA"))))
  data$d04.Der_hri_code_IS_MS_RS_SSN_Fraud <- as.numeric(apply(code_mx,1,function(x) any(x%in%c("IS","MS","RS"))))
  
  return(data)
}
d04.Tree_hri_code_2_RS_risk.FIT <- function(data_temp,y){
  
  minsp <- dim(data_temp)[1]/5
  minbu <- dim(data_temp)[1]/20
  
  data_temp <- data_temp[c("d04.Der_hri_code_2_3_71_DI_CL_SSN_risk","d04.Der_hri_code_4_11_12_14_16_PO_SD_VA_Address_risk",
                           "d04.Der_hri_code_19_28_Birth_Na","d04.Der_hri_code_41_DD_DL_DF_risk","d04.Der_hri_code_19_26_SSN_Na",
                           "d04.Der_hri_code_38_48_51_52_66_SSN_Na","d04.Der_hri_code_IS_MS_RS_SSN_Fraud")]
  data_temp$y <- as.factor(y)
  fit <- ctree(y~.,data=data_temp,control = ctree_control(minsplit = minsp,minbucket = minbu))
  return(fit)
}
d04.Tree_hri_code_7_CA_phone.FIT <- function(data_temp,y){
  
  minsp <- dim(data_temp)[1]/5
  minbu <- dim(data_temp)[1]/20
  
  data_temp <- data_temp[c("d04.Der_hri_code_7_8_55_56_46_75_82_Phone_risk","d04.Der_hri_code_10_57_Mobile",
                           "d04.Der_hri_code_19_25_49_SR_ZI_Address_Na","d04.Der_hri_code_19_27_Phone_Na",
                           "d04.Der_hri_code_74_Phone_Mismatch","d04.Der_hri_code_CA_Address")]
  data_temp$y <- as.factor(y)
  fit <- ctree(y~.,data=data_temp,control = ctree_control(minsplit = minsp,minbucket = minbu))
  return(fit)
}
d04.Tree_hri_code_2_RS_risk.DER <- function(data,tree){
  
  fit_tree <- tree$d04.Tree_hri_code_2_RS_risk.DER
  
  new <- data[c("d04.Der_hri_code_2_3_71_DI_CL_SSN_risk","d04.Der_hri_code_4_11_12_14_16_PO_SD_VA_Address_risk",
                "d04.Der_hri_code_19_28_Birth_Na","d04.Der_hri_code_41_DD_DL_DF_risk","d04.Der_hri_code_19_26_SSN_Na",
                "d04.Der_hri_code_38_48_51_52_66_SSN_Na","d04.Der_hri_code_IS_MS_RS_SSN_Fraud")]
  
  data$d04.Tree_hri_code_2_RS_risk <- predict(fit_tree,newdata=new,type="node")
  
  # data[c("d04.Der_hri_code_2_3_71_DI_CL_SSN_risk","d04.Der_hri_code_4_11_12_14_16_PO_SD_VA_Address_risk",
  #        "d04.Der_hri_code_19_28_Birth_Na","d04.Der_hri_code_41_DD_DL_DF_risk","d04.Der_hri_code_19_26_SSN_Na",
  #        "d04.Der_hri_code_38_48_51_52_66_SSN_Na","d04.Der_hri_code_IS_MS_RS_SSN_Fraud")] <- NULL
  # 
  return(data)
}
d04.Tree_hri_code_7_CA_phone.DER <- function(data,tree){
  
  fit_tree <- tree$d04.Tree_hri_code_7_CA_phone.DER
  
  new <- data[c("d04.Der_hri_code_7_8_55_56_46_75_82_Phone_risk","d04.Der_hri_code_10_57_Mobile",
                "d04.Der_hri_code_19_25_49_SR_ZI_Address_Na","d04.Der_hri_code_19_27_Phone_Na",
                "d04.Der_hri_code_74_Phone_Mismatch","d04.Der_hri_code_CA_Address")]
  
  data$d04.Tree_hri_code_7_CA_phone <- predict(fit_tree,newdata=new,type="node")
  
  # data[c("d04.Der_hri_code_7_8_55_56_46_75_82_Phone_risk","d04.Der_hri_code_10_57_Mobile",
  #        "d04.Der_hri_code_19_25_49_SR_ZI_Address_Na","d04.Der_hri_code_19_27_Phone_Na",
  # "d04.Der_hri_code_74_Phone_Mismatch","d04.Der_hri_code_CA_Address")] <- NULL
  
  return(data)
}
tree <- 0
df = d04.Der_hri_code_XXX.DER(df,tree)

k1 <- list(raw_var_name = raw_var_name <- c("d04.hri_1","d04.hri_2","d04.hri_3","d04.hri_4","d04.hri_5","d04.hri_6"),
           der_var_name = der_var_name <- c("d04.Tree_hri_code_2_RS_risk","d04.Tree_hri_code_7_CA_phone",
                                            "d04.Der_hri_code_2_3_71_DI_CL_SSN_risk","d04.Der_hri_code_4_11_12_14_16_PO_SD_VA_Address_risk",
                                            "d04.Der_hri_code_19_28_Birth_Na","d04.Der_hri_code_41_DD_DL_DF_risk","d04.Der_hri_code_19_26_SSN_Na",
                                            "d04.Der_hri_code_38_48_51_52_66_SSN_Na","d04.Der_hri_code_IS_MS_RS_SSN_Fraud",
                                            "d04.Der_hri_code_7_8_55_56_46_75_82_Phone_risk","d04.Der_hri_code_10_57_Mobile",
                                            "d04.Der_hri_code_19_25_49_SR_ZI_Address_Na","d04.Der_hri_code_19_27_Phone_Na",
                                            "d04.Der_hri_code_74_Phone_Mismatch","d04.Der_hri_code_CA_Address"))

k2 <- list(d04.Der_hri_code_XXX.DER=d04.Der_hri_code_XXX.DER,
           d04.Tree_hri_code_2_RS_risk.DER=d04.Tree_hri_code_2_RS_risk.DER,
           d04.Tree_hri_code_7_CA_phone.DER=d04.Tree_hri_code_7_CA_phone.DER)

k3 <- list(d04.Der_hri_code_XXX.DER=NULL,
           d04.Tree_hri_code_2_RS_risk.DER=NULL,
           d04.Tree_hri_code_7_CA_phone.DER=NULL)

k4 <- list(process_list <- c("d04.Der_hri_code_XXX.DER","d04.Tree_hri_code_2_RS_risk.DER","d04.Tree_hri_code_7_CA_phone.DER"))
kit_d04_Tree_hri_code_mix <- list(k1=k1,k2=k2,k3=k3,k4=k4)
kit_d04_Tree_hri_code_mix$k3$d04.Tree_hri_code_2_RS_risk.DER <- d04.Tree_hri_code_2_RS_risk.FIT(df,y)
kit_d04_Tree_hri_code_mix$k3$d04.Tree_hri_code_7_CA_phone.DER <- d04.Tree_hri_code_7_CA_phone.FIT(df,y)


# "c06.clear_bureau_lite_reason_codes",
c06.Der_bureau_code_BLXX.DER <- function(data,tree){
  
  # c06.clear_bureau_lite_reason_codes to ten different new variables
  
  code_mx <- strsplit(as.character(data$c06.clear_bureau_lite_reason_codes),",")
  
  data$c06.Der_bureau_code_BL01 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL01")))))
  data$c06.Der_bureau_code_BL02 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL02")))))
  data$c06.Der_bureau_code_BL03 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL03")))))
  data$c06.Der_bureau_code_BL04 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL04")))))
  data$c06.Der_bureau_code_BL05 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL05")))))
  data$c06.Der_bureau_code_BL06 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL06")))))
  data$c06.Der_bureau_code_BL07 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL07")))))
  data$c06.Der_bureau_code_BL08 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL08")))))
  data$c06.Der_bureau_code_BL09 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL09")))))
  data$c06.Der_bureau_code_BL10 <- as.numeric(unlist(lapply(code_mx,function(x) any(unlist(x)%in%c("BL10")))))
  
  return(data)
}
c06.Tree_bureau_code_1_5.FIT <- function(data_temp,y){
  
  minsp <- dim(data_temp)[1]/5
  minbu <- dim(data_temp)[1]/20
  
  data_temp <- data_temp[c("c06.Der_bureau_code_BL01","c06.Der_bureau_code_BL02",
                           "c06.Der_bureau_code_BL03","c06.Der_bureau_code_BL04",
                           "c06.Der_bureau_code_BL05")]
  data_temp$y <- as.factor(y)
  fit <- ctree(y~.,data=data_temp,control = ctree_control(minsplit = minsp,minbucket = minbu))
  return(fit)
}
c06.Tree_bureau_code_6_10.FIT <- function(data_temp,y){
  
  minsp <- dim(data_temp)[1]/5
  minbu <- dim(data_temp)[1]/20
  
  data_temp <- data_temp[c("c06.Der_bureau_code_BL06",
                           "c06.Der_bureau_code_BL07","c06.Der_bureau_code_BL08",
                           "c06.Der_bureau_code_BL09","c06.Der_bureau_code_BL10")]
  data_temp$y <- as.factor(y)
  fit <- ctree(y~.,data=data_temp,control = ctree_control(minsplit = minsp,minbucket = minbu))
  return(fit)
}
c06.Tree_bureau_code_1_5.DER <- function(data,tree){
  
  fit_tree <- tree$c06.Tree_bureau_code_1_5.DER
  
  new <- data[c("c06.Der_bureau_code_BL01","c06.Der_bureau_code_BL02",
                "c06.Der_bureau_code_BL03","c06.Der_bureau_code_BL04",
                "c06.Der_bureau_code_BL05")]
  
  data$c06.Tree_bureau_code_1_5 <- predict(fit_tree,newdata=new,type="node")
  
  # data[c("c06.Der_bureau_code_BL01","c06.Der_bureau_code_BL02",
  #        "c06.Der_bureau_code_BL03","c06.Der_bureau_code_BL04",
  #        "c06.Der_bureau_code_BL05")] <- NULL
  
  return(data)
}
c06.Tree_bureau_code_6_10.DER <- function(data,tree){
  
  fit_tree <- tree$c06.Tree_bureau_code_6_10.DER
  
  new <- data[c("c06.Der_bureau_code_BL06",
                "c06.Der_bureau_code_BL07","c06.Der_bureau_code_BL08",
                "c06.Der_bureau_code_BL09","c06.Der_bureau_code_BL10")]
  
  data$c06.Tree_bureau_code_6_10 <- predict(fit_tree,newdata=new,type="node")
  
  # data[c("c06.Der_bureau_code_BL06",
  #        "c06.Der_bureau_code_BL07","c06.Der_bureau_code_BL08",
  #        "c06.Der_bureau_code_BL09","c06.Der_bureau_code_BL10")] <- NULL
  
  return(data)
}

tree <- 0
df = c06.Der_bureau_code_BLXX.DER(df,tree)

k1 <- list(raw_var_name = raw_var_name <- c("c06.clear_bureau_lite_reason_codes"),
           der_var_name = der_var_name <- c("c06.Tree_bureau_code_1_5","c06.Tree_bureau_code_6_10",
                                            "c06.Der_bureau_code_BL01","c06.Der_bureau_code_BL02",
                                            "c06.Der_bureau_code_BL03","c06.Der_bureau_code_BL04",
                                            "c06.Der_bureau_code_BL05","c06.Der_bureau_code_BL06",
                                            "c06.Der_bureau_code_BL07","c06.Der_bureau_code_BL08",
                                            "c06.Der_bureau_code_BL09","c06.Der_bureau_code_BL10"))

k2 <- list(c06.Der_bureau_code_BLXX.DER=c06.Der_bureau_code_BLXX.DER,
           c06.Tree_bureau_code_1_5.DER=c06.Tree_bureau_code_1_5.DER,
           c06.Tree_bureau_code_6_10.DER=c06.Tree_bureau_code_6_10.DER)

k3 <- list(c06.Der_bureau_code_BLXX.DER=NULL,
           c06.Tree_bureau_code_1_5.DER=NULL,
           c06.Tree_bureau_code_6_10.DER=NULL)

k4 <- list(process_list <- c("c06.Der_bureau_code_BLXX.DER","c06.Tree_bureau_code_1_5.DER","c06.Tree_bureau_code_6_10.DER"))
kit_c06_Tree_clear_bureau_lite_code_mix <- list(k1=k1,k2=k2,k3=k3,k4=k4)
kit_c06_Tree_clear_bureau_lite_code_mix$k3$c06.Tree_bureau_code_1_5.DER <- c06.Tree_bureau_code_1_5.FIT(df,y)
kit_c06_Tree_clear_bureau_lite_code_mix$k3$c06.Tree_bureau_code_6_10.DER <- c06.Tree_bureau_code_6_10.FIT(df,y)


# "c06.ciqt9413x","c06.ciqt9415x","c06.ciqt9416x","c06.ciqt9417x",
c06.Der_ciqt_inquiries_over_time.DER <- function(data,tree){
  
  data$c06.Der_ciqt_inquiries_has_recent <- as.numeric(data$c06.ciqt9413x > 0)
  
  data$c06.Der_ciqt_inquiries_has_3month <-  as.numeric(data$c06.ciqt9415x > 0)
  
  data$c06.Der_ciqt_inquiries_has_6month <-  as.numeric(data$c06.ciqt9416x > 0)
  
  data$c06.Der_ciqt_inquiries_has_always <- as.numeric(data$c06.ciqt9417x - data$c06.ciqt9416x >0 & data$c06.ciqt9416x- data$c06.ciqt9413x >0 & data$c06.ciqt9413x > 0)
  
  return(data)
}
c06.Tree_ciqt_inquiries_over_time_mix.FIT <- function(data_temp,y){
  
  minsp <- dim(data_temp)[1]/10
  minbu <- dim(data_temp)[1]/30
  
  data_temp <- data_temp[c("c06.Der_ciqt_inquiries_has_recent","c06.Der_ciqt_inquiries_has_3month",
                           "c06.Der_ciqt_inquiries_has_6month","c06.Der_ciqt_inquiries_has_always",
                           "c06.ciqt9417x")]
  data_temp$y <- as.factor(y)
  fit <- ctree(y~.,data=data_temp,control = ctree_control(minsplit = minsp,minbucket = minbu))
  return(fit)
}
c06.Tree_ciqt_inquiries_over_time_mix.DER <- function(data,tree){
  
  fit_tree <- tree$c06.Tree_ciqt_inquiries_over_time_mix.DER
  
  new <- data[c("c06.Der_ciqt_inquiries_has_recent","c06.Der_ciqt_inquiries_has_3month",
                "c06.Der_ciqt_inquiries_has_6month","c06.Der_ciqt_inquiries_has_always",
                "c06.ciqt9417x")]
  
  data$c06.Tree_ciqt_inquiries_over_time_mix <- predict(fit_tree,newdata=new,type="node")
  
  # data[c("c06.Der_ciqt_inquiries_has_recent","c06.Der_ciqt_inquiries_has_3month",
  #        "c06.Der_ciqt_inquiries_has_6month","c06.Der_ciqt_inquiries_has_always")] <- NULL
  # 
  return(data)
}

tree <- 0
df = c06.Der_ciqt_inquiries_over_time.DER(df,tree)

k1 <- list(raw_var_name = raw_var_name <- c("c06.ciqt9413x","c06.ciqt9415x","c06.ciqt9416x","c06.ciqt9417x"),
           der_var_name = der_var_name <- c("c06.Tree_ciqt_inquiries_over_time_mix",
                                            "c06.Der_ciqt_inquiries_has_recent","c06.Der_ciqt_inquiries_has_3month",
                                            "c06.Der_ciqt_inquiries_has_6month","c06.Der_ciqt_inquiries_has_always"))

k2 <- list(c06.Der_ciqt_inquiries_over_time.DER=c06.Der_ciqt_inquiries_over_time.DER,
           c06.Tree_ciqt_inquiries_over_time_mix.DER=c06.Tree_ciqt_inquiries_over_time_mix.DER)

k3 <- list(c06.Der_ciqt_inquiries_over_time.DER=NULL,
           c06.Tree_ciqt_inquiries_over_time_mix.DER=NULL)

k4 <- list(process_list <- c("c06.Der_ciqt_inquiries_over_time.DER","c06.Tree_ciqt_inquiries_over_time_mix.DER"))
kit_c06_Tree_ciqt_inquiries <- list(k1=k1,k2=k2,k3=k3,k4=k4)
kit_c06_Tree_ciqt_inquiries$k3$c06.Tree_ciqt_inquiries_over_time_mix.DER <- c06.Tree_ciqt_inquiries_over_time_mix.FIT(df,y)


kit_group_US1805_Trial <- list(kit_d04_Tree_hri_code_mix=kit_d04_Tree_hri_code_mix,
                               kit_c06_Tree_clear_bureau_lite_code_mix=kit_c06_Tree_clear_bureau_lite_code_mix,
                               kit_c06_Tree_ciqt_inquiries=kit_c06_Tree_ciqt_inquiries)
kit_group <- kit_group_US1805_Trial


data <- df

exe_kit <- function(data,perf,kit_group){
  
  len_group = length(kit_group)
  
  raw_var_name <- NULL
  der_var_name <-NULL
  exe_seq <- NULL
  objs_tree <- NULL
  func_tree <- NULL
  
  for(i in 1:len_group){
    raw_var_name <- c(raw_var_name,kit_group[[i]]$k1$raw_var_name)
    der_var_name <- c(der_var_name,kit_group[[i]]$k1$der_var_name)
    exe_seq <- c(exe_seq,kit_group[[i]]$k4[[1]])
    objs_tree <- c(objs_tree,kit_group[[i]]$k3)
    func_tree <- c(func_tree,kit_group[[i]]$k2)
  }  
  
  data_raw <- data[raw_var_name]
  head(data_raw)
  
  len_exe = length(exe_seq)
  data_process <- data_raw
  
  for(i in 1:len_exe){
    exe_func_name <- exe_seq[i]
    temp_exe <- func_tree[exe_func_name][[1]]
    temp_exe_obj <- objs_tree[exe_func_name]
    data_process <- temp_exe(data_process,temp_exe_obj)
  }
  
  data_der <- data_process[der_var_name]
  head(data_der)
  
}

data <- data_der
is.numeric(data$c01.bank_account_number)
is.numeric(T)
is.character(as.factor(T))

Data.SMBin_numeric <- function(data,y){
  
  dim(data)
  data <- cbind(data,y)
  
  ct_tree <- list()
  i=33
  for(i in 1:(length(colnames(data))-1)){
    ct = ctree(formula(paste("y~",colnames(data)[i])),data=data)
    ct_tree[[i]] = ct
    print(i)
  }
  return(ct_tree)
}

ct_trans_form <- function(data,tree){
  
  
  
  
}


data <- data[type_chr]
type_num <- unlist(lapply(data,function(x) is.numeric(x)))
type_chr <- unlist(lapply(data,function(x) is.character(x)))

data <- data[type_num | type_chr]
i
if(sum(type_num)!=0){
  df_num <- cbind(data[type_num],y)
  df_num$y <- as.factor(df_num$y)
  for(i in 1:sum(type_num)){
    ct = ctree(formula(paste("y~",colnames(df_num)[i])),data=df_num)
    bins=width(ct)
    n=length(ct)
    range=range(as.numeric(unlist(na.omit(data_ctree[x]))))
    
    bin <- vector(length=0)
    for (j in 1:n) {
      bin=c(bin,ct[j]$node$split$breaks)
    }
    
  }
}
dim(data)



type_mx <- unlist(lapply(data,is.numeric))

colnames(data)


data[is.na(data)] <- 0.01
data <- lapply(data, function(x) as.numeric(as.character(unlist(x))))
data <- as.data.frame(data)
# index = which(colnames(data) == Dependent)
colnames = colnames(data)
smb_list = list()
length(smb_list) = length(colnames)
names(smb_list) = colnames

data_ctree <- cbind(y,data)
data_ctree$y <- as.factor(data_ctree$y)

for(i in 1:length(colnames)){
  x = colnames[i]
  ct = ctree(formula(paste("y~",colnames[i])),data=data_ctree)
  bins=width(ct)
  n=length(ct)
  range=range(as.numeric(unlist(na.omit(data_ctree[x]))))
  
  bin <- vector(length=0)
  for (j in 1:n) {
    bin=c(bin,ct[j]$node$split$breaks)
  }
  breaks=c(-Inf,sort(bin),Inf)
  variable <- as.numeric(unlist(data_ctree[x]))
  variable <- cut(variable,breaks=breaks)
  smb_list[[x]] = breaks
  data[x][,1] = variable
}

data <- apply(data,2,function(x) as.factor(as.character(x)))

return(list(data = data,list = smb_list ))
}

temp_file <- Data.SMBin_numeric(df,y)
temp_file$list


Data.SMBin_numeric_apply <- function(num_mx,bin_num){
  data_set = num_mx
  object_list = bin_num
  
  data_out = data.frame(matrix(0, dim(data_set)[1], dim(data_set)[2]))
  names(data_out) = names(data_set)
  colnames = colnames(data_set)
  
  for(i in 1:length(colnames)){
    x = colnames[i]
    variable <- as.numeric(unlist(data_set[x]))
    data_out[x] = cut( variable, breaks = object_list[[x]])
  }
  return(data_out)
}









head(perf)

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
class(lm.D9)
typeof(lm.D9)
print(lm.D9)
attributes((lm.D9))


class(lm)
typeof(lm)
print(lm)
attributes((lm))







kit <- array()

class(lm)
isS3(lm)

setClass("kit",slots = list(name="character",raw_var_name="character",der_var_name="character",
                            func="character",func_obj="character"))
Age_date_to_days <- new("kit",name="Age",raw_var_name=c("c01.date_of_brith","sys.ApplicationDate"),
                        der_var_name=c("c01.age"))
attributes(data.frame)

setClass("kit",
         slots=list(country="character"),
         contains="function"
)

ff <- function(){
  
  a <- function(x){
    x=c+x
    return(x)
  }
  b <- function(x){
    x=x+1
    return(x)
  }
  c <- 3
  return(environment(c))
  
}
ff()

a <- function(x){
  x=x+1
  return(x)
}
b <- function(x){
  x=x+2
  return(x)
}

body(a)
typeof(a)

aaa_kit <- array()
aaa_kit[1] <- "c01.Date_to_days_age"
aaa_kit[2] <- list(body(a),body(b))

formals(b)
formals(a)

body(b) <- body(a)
b
environment(a)
environment(b)


attributes(b)
mostattributes(a)
attr(a)
class(a)
str(a)

str(body(a))
class(body(a))
t <- body(a)
t <- c("c01.Date_to_days_age","c01.date_of_brith")
tt <- list(a,b)
ttt <- c
kit <- list(t,tt,ttt)



a <- function(x){
  x=x+c
  return(x)
}

model_envir <- new.env()
environment(a) <- model_envir
model_envir$c=2
a(1)

c <- 1

a(1)

exec <- function(){
  
  c <- 2
  tst_func <- kit[[2]][[1]]
  tst_func(1)
  
}
exec()



slot("function")
a <- new("kit",country="kkk")
class(a)


typeof(Age_date_to_days)
UseMethod("sum")

?lm

?setClass
setClass("student", slots=list(name="character", age="numeric", GPA="numeric"))
s <- new("student",name="John", age=21, GPA=3.5)
> s
# <-------------------------------------------------------------------->
# Section Three : Modeling and parameter tuning
# feature selection and generate the model object
# <-------------------------------------------------------------------->






# <-------------------------------------------------------------------->
# Section Four : Packaging
# Improve the efficiency of the whole process
# Transfer them into sets and prepare for future usage
# <-------------------------------------------------------------------->









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

