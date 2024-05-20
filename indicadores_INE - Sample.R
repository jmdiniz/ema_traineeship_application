library(tidyverse)
library(data.table)
library(jsonlite)
library(stringr)

############################################################

# Get Base File

wd <- getwd()
indicadores_raw <- fread(paste0(wd,"/indicadores_raw.csv"), encoding = "UTF-8")
indicadores_available <- indicadores_raw[indicadores_raw$`Disponível no Portal` == "Sim",c(1:7)]
indicadores_relevant <- indicadores_available[indicadores_available$Relevante == "S",]
location_info <- read.csv(paste0(wd,"/geo_linkage_2024_v2.csv"), encoding = "UTF-8", sep = ";")

# Generate Meta Data Link Builder

host_url <- "https://www.ine.pt"
metadata_base_a <- "/ine/json_indicador/pindicaMeta.jsp?varcd="
varcd_cod <- "0004758"
#this is the 'diffusion code'
metadata_base_b <- "&lang="
lang <- "PT"
#lang <- "{lang}"

complete_url_meta <- paste0(host_url, metadata_base_a, varcd_cod, metadata_base_b, lang)

mydata <- fromJSON(complete_url_meta)
info_indicador <- mydata
info_indicador <- info_indicador[-c(1),]

# Extra: Get codes for identified relevant indicators

sugestao_indicadores <- read.csv(paste0(wd,"/sugestao_indicadores.csv"))
sugestao_indicadores$select <- NA
indicadores_extended <- read.csv(paste0(wd,"/indicadores_extended.csv"))
indicadores_extended$select <- NA



# Get All Indicators & Meta Data Indicators

for (i in 1:nrow(indicadores_relevant)){
  varcd_cod <- str_pad(indicadores_relevant[i,4], 7, pad = "0")
  complete_url_meta <- paste0(host_url, metadata_base_a, varcd_cod, metadata_base_b, lang)
  mydata <- fromJSON(complete_url_meta)
  info_indicador <- bind_rows(info_indicador, mydata)
  print(paste0("Great success! This is round ",i,"/",nrow(indicadores_relevant)))
}

a <- as.data.frame(info_indicador[[12]][[1]][[1]])
meta_inf_dim <- a[-c(1:2),c(1:3)]
b <- as.data.frame(info_indicador[[12]][[2]][[1]])
c <- b[1]
c <- as.data.frame(c[[1]])
c <- cbind(info_indicador[[1]][[1]],c)
meta_inf_value <- c[-c(1),]

for (i in 1:nrow(info_indicador)){
  a <- as.data.frame(info_indicador[[12]][[1]][[i]])
  a <- a[,c(1:3)]
  a <- cbind(info_indicador[[1]][[i]],a)
  meta_inf_dim <- rbind(meta_inf_dim, a)
  b <- as.data.frame(info_indicador[[12]][[2]][[i]])
  print(paste0("Running ",i))
  
  for (y in 1:ncol(b)){
    c <- b[y]
    c <- as.data.frame(c[[1]])
    c <- cbind(info_indicador[[1]][[i]],c)
    meta_inf_value <- rbind(meta_inf_value, c)
  }
}

colnames(meta_inf_dim)[1] <- "IndicadorCod"
colnames(meta_inf_value)[1] <- "IndicadorCod"

# Generate Data Link Builder

reference_info_2 <- "https://www.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=322762582&att_display=n&att_download=y"

host_url <- "https://www.ine.pt"
connector_a <- "/ine/json_indicador/pindica.jsp?op=2&varcd="
varcd_cod <- "0008271"
dim_1_promoter <- "&Dim1="
dim_1 <- "S7A2010"
dim_2_promoter <- "&Dim2="
dim_2 <- "18"
dim_3_promoter <- "&Dim3="
dim_3 <- "T"
dim_4_promoter <- "&Dim4="
dim_4 <- "2"
dim_5_promoter <- "&Dim5="
dim_5 <- "6"
#corresponde ao código de difusão
connector_b <- "&lang="
lang <- "PT"

complete_url_data <- paste0(host_url, connector_a, varcd_cod, dim_1_promoter, dim_1, connector_b, lang)
#complete_url_data <- paste0(host_url, connector_a, varcd_cod, dim_1_promoter, dim_1, dim_2_promoter, dim_2, dim_3_promoter, dim_3, dim_4_promoter, dim_4, dim_5_promoter, dim_5, connector_b, lang)

mydata <- fromJSON(complete_url_data)
test <- as.data.frame(mydata[[7]][[1]][[1]])