# 1 - Set up

## Define required packages
packages <- c("lubridate", "tidyverse", "data.table", "ggthemes", "scales", "rio", "readxl", "rvest")

## Check which packages are not installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

## Install missing packages
if(length(new_packages) > 0) {
  install.packages(new_packages)
}

## Load all packages
lapply(packages, library, character.only = TRUE)
rm(new_packages, packages)

## Get the Tableau palettes for 20 colors
lista_tableau <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]$`Tableau 20`$value
color_palette <- c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C", "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5", "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F", "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5", "#1C9B89", "#F7941E", "#7A3B3F", "#A57C52", "#4E4D8B", "#E6437A", "#4682B4", "#D896A3", "#7B8BAF", "#69A68C")

## Define the Primary Care Grouping
aces_name <- "ACES Lisboa Central" 

########################################

# 2 - Loading Data

## Define the base path of data files (to be replaced)

path <- paste(wd(),"SIARS - ACes Lisboa Central/UF - Export - P01_02_01_R01_ Relatório de Indicadores - Comparação de Indicadores entre UF_s_ no Pe.csv")
df_raw <- read.csv(path, header = FALSE, fileEncoding = "UTF-8")

# 3 - Processing Data

## 3.1 - Rows Manipulation

df_clean <- df_raw[-c(1:8,10),-c(1,4)]
df_clean <- replace(df_clean, df_clean == '', NA)

df_clean <- as.data.frame(t(df_clean))

df_clean <- df_clean %>%
  fill(c("9","11"), .direction = "down")

df_clean <- as.data.frame(t(df_clean))

df_key <- df_clean[-c(1:3),c(1,2)]
colnames(df_key) <- c("ID_Indicador", "Nome_Indicador")

df_clean <- df_clean[,-c(1)]

df_clean <- t(df_clean)

colnames(df_clean) <- df_clean[1,]
df_clean <- df_clean[-1,]
colnames(df_clean)[1:3] <- c("Data", "UF", "Tipo_Valor")

uf <- unique(colnames(df_clean)[4:length(colnames(df_clean))])

df_clean <- as.data.frame(df_clean)

df_clean <- df_clean %>% pivot_longer(cols=uf,
                    names_to='Indicador',
                    values_to="Valor")

### 3.3.3 - Wide to long

df_clean$Data <- ym(as.character(df_clean$Data))
df_clean$UF <- as.factor(df_clean$UF)
df_clean$Tipo_Valor <- as.factor(df_clean$Tipo_Valor)
levels(df_clean$Tipo_Valor) = c("Denominador", "Numerador", "Valor")
df_clean$Indicador <- as.factor(df_clean$Indicador)

df_clean$Valor <- as.numeric(gsub(",", "",df_clean$Valor))

## 3.4 - Customize to untis of analysis - Only USF and UCSP

selected_columns <- unique(grep("^USF|^UCSP", df_clean$UF, value = TRUE))
df_clean <- df_clean[df_clean$UF %in% selected_columns, ]

lista_usf <- unique(grep("^USF", df_clean$UF, value = TRUE))
lista_ucsp <- unique(grep("^UCSP", df_clean$UF, value = TRUE))

df_USF <- df_clean[df_clean$UF %in% lista_usf,]
df_UCSP <- df_clean[df_clean$UF %in% lista_ucsp,]

df_USF$Tipo_UF <- "USF"
df_UCSP$Tipo_UF <- "UCSP"

df_ready <- rbind(df_USF,df_UCSP)
rownames(df_ready) <- NULL

df_ready$Tipo_UF <- as.factor(df_ready$Tipo_UF)
df_ready$Tipo_UF <- relevel(df_ready$Tipo_UF, "USF")

### 3.5 - Create subgroups

lista_indicadores <- unique(df_ready$Indicador)

lista_indicadores_perc <- unique(grep("^Perc", lista_indicadores, value = TRUE))
lista_indicadores_prop <- unique(grep("^Prop", lista_indicadores, value = TRUE))
lista_indicadores_max <- setdiff(lista_indicadores, c(lista_indicadores_perc, lista_indicadores_prop))

df_ready <- df_ready %>%
  mutate(Tipo_Indicador = ifelse(Indicador %in% lista_indicadores_perc, "Percentagem", 
                                 ifelse(Indicador %in% lista_indicadores_prop, "Proporção", "Outro")))

df_ready$Tipo_Indicador <- as.factor(df_ready$Tipo_Indicador)

### 3.6 - Extract Data from SDM

base_url = "https://sdm.min-saude.pt/BI.aspx?id="

df_key <- df_key %>%
  mutate(ID_Indicador_Simples = result <- substr(ID_Indicador, 6, 8)) %>%
  mutate(Link_Indicador = paste0(base_url,ID_Indicador_Simples))

for(link in 1:length(df_key$Link_Indicador)) {
  
  page = read_html(df_key$Link_Indicador[link])
  elem <- page %>% html_element("form")
  df <- html_table(elem, header = FALSE)
  
  df_key[link,5] <- as.character(df[16,1])
  
}

colnames(df_ready)[4] <- "Nome_Indicador"
colnames(df_key)[5] <- "Descricao"
df_ready <- right_join(df_ready, df_key[,-c(3)], by = "Nome_Indicador")

### 3.7 - Write Files (CSV)

write.csv(df_ready, "C:/Users/jmdin/Documents/R/Repositories/Indicadores_ACES/Output/Indicadores_Trabalhados_ACES_Lisboa_Central.csv", row.names=TRUE, fileEncoding = "ISO-8859-1")

### 3.8 - Specific Analyses

#### 3.8.1 - Number of registered patients

a <- df_ready[df_ready$Nome_Indicador == 'Proporção utentes c/ "DM não insul. depend."' & df_ready$Data == max(df_ready$Data) & df_ready$Tipo_Valor == "Denominador",]
total_rp <- sum(a$Valor)

#### 3.8.2 - Number of DM patients, by need of insulinotherapy

b <- df_ready[df_ready$Nome_Indicador == 'Proporção utentes c/ "DM não insul. depend."' & df_ready$Data == max(df_ready$Data) & df_ready$Tipo_Valor == "Numerador",]
total_dm_ni <- sum(b$Valor)

c <- df_ready[df_ready$Nome_Indicador == 'Proporção utentes com \"DM insulino depend.\"' & df_ready$Data == max(df_ready$Data) & df_ready$Tipo_Valor == "Numerador",]
total_dm_i <- sum(c$Valor)

#### 3.8.3 - Percentage of quality care in DM

d <- df_ready[df_ready$Nome_Indicador == 'Proporção DM c/ acompanham. adequado' & df_ready$Data == max(df_ready$Data) & df_ready$Tipo_Valor == "Numerador",]

#### 3.8.4 - Percentage of quality care in HT

e <- df_ready[df_ready$Nome_Indicador == 'Proporção de hipertensos, c/ acompanh. adequado' & df_ready$Data == max(df_ready$Data) & df_ready$Tipo_Valor == "Numerador",]

#### 3.8.5 - Percentage of registered patients with DM risk assessment

f <- df_ready[df_ready$Nome_Indicador == 'Proporção utentes com avaliação risco DM2 (3A)' & df_ready$Data == max(df_ready$Data) & df_ready$Tipo_Valor == "Numerador",]

# 4 - Graphs

## 4.1 - Generate all graphs: UCSP e USF

### Initialize an empty list to store the plots
plot_list <- list()

### Loop through each indicator in lista_indicadores

count <- 1

#df_ready <- df_ready[is.na(df_ready$Valor) == F,]
#df_ready <- df_ready[df_ready$Tipo_Valor == "Valor",]
#lista_indicadores <- unique(df_ready$Nome_Indicador)

for (indicador in lista_indicadores) {
  
  # Filter the data for the current indicator
  df_filtered <- subset(df_ready, df_ready$Nome_Indicador == indicador & df_ready$Tipo_Valor == "Valor")
  
  # Create the plot for the current indicator
  if (df_filtered$Tipo_Indicador[lista_indicadores[count]] == "Outro") {
    p <- ggplot(df_filtered, aes(x = Data, y = Valor, fill = UF, color = UF)) +
      geom_bar(stat = "identity", position = 'dodge', colour = NA) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", colour = NA), axis.text = element_text(size = 7), legend.position = "none") +
      labs(x = "\nTempo",
           y = "Valor\n",
           subtitle = paste0("UCSP e USF - ", aces_name, "\n"),
           title = str_wrap(paste0(df_filtered$Descricao, " - ", df_filtered$ID_Indicador), width = 115)) +
      scale_fill_manual(values = color_palette) +
      xlim(min(df_filtered$Data),max(df_filtered$Data)) +
      facet_wrap(vars(UF), ncol = 5)
    
  } else {
    p <- ggplot(df_filtered, aes(x = Data, y = Valor, fill = UF, color = UF)) +
      geom_bar(stat = "identity", position = 'dodge', colour = NA) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", colour = NA), axis.text = element_text(size = 7), legend.position = "none") +
      labs(x = "\nTempo",
           y = "Percentagem (%)\n",
           subtitle = paste0("UCSP e USF - ", aces_name, "\n"),
           title = str_wrap(paste0(df_filtered$Descricao, " - ", df_filtered$ID_Indicador), width = 115)) +
      scale_fill_manual(values = color_palette) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      xlim(min(df_filtered$Data),max(df_filtered$Data)) +
      facet_wrap(vars(UF), ncol = 5)
  }
  
  # Store the plot in the list
  plot_list[[count]] <- p
  
  count <- count + 1
  
}

## 4.3 - Save all graphs: UCSP e USF

lista_indicadores <- unique(df_ready$Descricao)

#Change name to avoid folder creation
lista_indicadores_simples <- gsub(" / ", " ou ", lista_indicadores)
lista_indicadores_simples <- gsub("/", "-", lista_indicadores)
lista_indicadores_simples <- gsub("\"", "", lista_indicadores_simples)

#Initialize a counter
counter <- 1

#Loop through each indicator in lista_indicadores
for(i in lista_indicadores_simples) {
  #Build file name
  set_filename <- paste0("C:/Users/jmdin/Desktop/Geral - UF & UCSP/", substr(i, 1, 100), ".png")
  #Save the plot from plot_list using ggsave
  ggsave(filename = set_filename, 
         device = "png",
         plot = plot_list[[counter]],
         width = 32,
         height = 18,
         units = c("cm"),
         dpi = "retina")
  #Print status
  print(set_filename)
  #Increment the counter
  counter <- counter + 1
}