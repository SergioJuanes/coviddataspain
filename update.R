library(tidyverse)
library(textclean)
library(pdftools)
library(miscTools)

whodata <- read_csv(url("https://covid19.who.int/WHO-COVID-19-global-data.csv"))
whodata$Country_code <- as.character(whodata$Country_code)
whodata$New_cases <- as.double(whodata$New_cases)

#cambio de nombres
cambio_nombres <- function(df){
  #dataframe con columna Country
  
  df[df$Country == "Bahamas",]$Country <- "The Bahamas"
  df[df$Country == "Bolivia (Plurinational State of)", ]$Country <- "Bolivia"
  df[df$Country == "Brunei Darussalam", ]$Country <- "Brueni"
  df[df$Country == "Cabo Verde", ]$Country <- "Cape Verde"
  df[df$Country == "Congo", ]$Country <- "Republic of the Congo"
  df[df$Country == "Côte d’Ivoire", ]$Country <- "Ivory Coast"
  df[df$Country == "Czechia", ]$Country <- "Czech Republic"
  df[df$Country == "Democratic People's Republic of Korea", ]$Country <- "North Korea"
  df[df$Country == "Guinea-Bissau", ]$Country <- "Guinea Bissau"
  df[df$Country == "Holy See", ]$Country <- "Vatican"
  df[df$Country == "Iran (Islamic Republic of)", ]$Country <- "Iran"
  df[df$Country == "Kosovo[1]", ]$Country <- "Kosovo"
  df[df$Country == "Lao People's Democratic Republic", ]$Country <- "Laos"
  df[df$Country == "Micronesia (Federated States of)", ]$Country <- "Federated States of Micronesia"
  df[df$Country == "North Macedonia", ]$Country <- "Macedonia"
  df[df$Country == "Northern Mariana Islands (Commonwealth of the)", ]$Country <- "Northern Mariana Islands"
  df[df$Country == "Republic of Korea", ]$Country <- "South Korea"
  df[df$Country == "Republic of Moldova", ]$Country <- "Moldova"
  df[df$Country == "Russian Federation", ]$Country <- "Russia"
  df[df$Country == "Serbia", ]$Country <- "Republic of Serbia"
  df[df$Country == "Syrian Arab Republic", ]$Country <- "Syria"
  df[df$Country == "The United Kingdom", ]$Country <- "United Kingdom"
  df[df$Country == "Venezuela (Bolivarian Republic of)", ]$Country <- "Venezuela"
  df[df$Country == "Viet Nam", ]$Country <- "Vietnam" 
  df[df$Country == "occupied Palestinian territory, including east Jerusalem", ]$Country <- "Palestinian" 
  
  return(df)
}
whodata <- na.omit(whodata)

whodata <- cambio_nombres(whodata)


write_csv(whodata, "data/whodata.csv")

spain_covid <- read_csv("data/spain_covid_dataset.csv")
number_file <- max(spain_covid$number_file) + 1
pdf_file <- tryCatch(
  pdf_text(paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actualizacion_",
                   number_file,"_COVID-19.pdf", 
                   collapse = "")),
  error=function(e) NULL
)
if(!is.null(pdf_file)){
  datos_unlist <- unlist(str_split(pdf_file[1], "[\\r\\n]+"))
  datos_unlist <- data.frame(str_split_fixed(str_trim(datos_unlist), "\\s{2,}", 11))
  datos_comunidades <- data.frame(apply(datos_unlist, 2, function(x) gsub("^$", NA, trimws(x))))
  datos_comunidades <- datos_comunidades %>% filter(complete.cases(datos_comunidades))
  fecha <- as.Date(format(parsedate::parse_date(datos_unlist[[1]][5]), "%Y-%m-%d"))
  if (is.na(fecha)) {
    fecha <- as.Date(format(parsedate::parse_date(datos_unlist[[1]][6]), "%Y-%m-%d"))
  }
  names(datos_comunidades) <- c("Comunidad", "Total", "diaprevio", "casos14dias", "IA14", "casos7dias", "IA7", "NI1", "NI2", "NI3", "NI4")
  
  datos_comunidades <- data.frame(lapply(datos_comunidades, as.character), stringsAsFactors=FALSE)
  datos_comunidades$Fecha <- fecha
  datos_comunidades$Comunidad <- c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla-La Mancha", "Castilla y León", "Cataluña", "Ceuta", "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Melilla", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja", "España")
  
  datos_comunidades <- datos_comunidades %>% select(Comunidad, Total, IA14, Fecha)
  names(datos_comunidades) <- c("Comunidad", "Casos", "IA14", "Fecha")
  datos_casos <- datos_comunidades
  
  datos_unlist <- unlist(str_split(pdf_file[5], "[\\r\\n]+"))
  datos_unlist <- data.frame(str_split_fixed(str_trim(datos_unlist), "\\s{2,}", 4))
  datos_comunidades <- data.frame(apply(datos_unlist, 2, function(x) gsub("^$", NA, trimws(x))))
  datos_comunidades <- datos_comunidades %>% filter(complete.cases(datos_comunidades))
  
  names(datos_comunidades) <- c("Comunidad", "Fallecidos", "sietedias", "letalidad")
  datos_comunidades$Comunidad <- as.character(datos_comunidades$Comunidad)
  datos_comunidades$Fallecidos <- as.character(datos_comunidades$Fallecidos)
  datos_comunidades$sietedias <- as.character(datos_comunidades$sietedias)
  datos_comunidades$letalidad <- as.character(datos_comunidades$letalidad)
  corregir <- str_detect(datos_comunidades$Comunidad, "notificación")
  
  if(sum(corregir) > 0){
    data_corrected <- unlist(strsplit(unname(unlist(datos_comunidades[corregir,]))[-1], split="\\s{2,}"))
    datos_comunidades <- datos_comunidades[!corregir,]
    datos_comunidades <- as.data.frame(insertRow(as.matrix(datos_comunidades), which(corregir == TRUE), data_corrected))
    row.names(datos_comunidades) <- NULL
  }
  
  datos_comunidades$Comunidad <- as.character(datos_comunidades$Comunidad)
  datos_comunidades$Fallecidos <- as.character(datos_comunidades$Fallecidos)
  datos_comunidades$sietedias <- as.character(datos_comunidades$sietedias)
  datos_comunidades$letalidad <- as.character(datos_comunidades$letalidad)
  corregir <- str_detect(datos_comunidades$Comunidad, "notificación")
  
  corregir2 <- str_detect(datos_comunidades$Comunidad, "0")
  
  if(sum(corregir2) > 0){
    data_corrected <- unlist(strsplit(unname(unlist(datos_comunidades[corregir2,]))[-1], split="\\s{2,}"))
    datos_comunidades <- datos_comunidades[!corregir2,]
    datos_comunidades <- as.data.frame(insertRow(as.matrix(datos_comunidades), which(corregir2 == TRUE), data_corrected))
    row.names(datos_comunidades) <- NULL
  }
  datos_comunidades$Comunidad <- as.character(datos_comunidades$Comunidad)
  datos_comunidades$Fallecidos <- as.character(datos_comunidades$Fallecidos)
  datos_comunidades$sietedias <- as.character(datos_comunidades$sietedias)
  datos_comunidades$letalidad <- as.character(datos_comunidades$letalidad)
  datos_comunidades$Fecha <- fecha
  
  datos_comunidades$Comunidad <- c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla-La Mancha", "Castilla y León", "Cataluña", "Ceuta", "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Melilla", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja", "España")
  
  datos_fallecidos <- datos_comunidades %>% dplyr::select(Comunidad, Fallecidos, Fecha)
  
  spain_covid_nuevos <- cbind(datos_casos, datos_fallecidos %>% select(Fallecidos))
  spain_covid_nuevos$CasosDiarios <- 0
  spain_covid_nuevos$FallecidosDiarios <- 0
  
  spain_covid_nuevos$Casos <- as.numeric(gsub("\\.", "", spain_covid_nuevos$Casos))
  spain_covid_nuevos$Fallecidos <- as.numeric(gsub("\\.", "", spain_covid_nuevos$Fallecidos))
  spain_covid_nuevos$IA14 <- as.numeric(gsub(",", ".", spain_covid_nuevos$IA14))
  
  spain_covid_nuevos$number_file <- number_file
  
  spain_covid <- rbind(spain_covid, spain_covid_nuevos)
  
  spain_covid <- spain_covid %>% dplyr::arrange(Fecha, Comunidad)
  
  data.spain.menos <- spain_covid[-c(1:20),]
  row.names(data.spain.menos) <- NULL
  
  spain_covid[-c(1:20),]$CasosDiarios <- data.spain.menos$Casos - spain_covid[-c((nrow(spain_covid)-19):nrow(spain_covid)),]$Casos
  
  spain_covid[-c(1:20),]$FallecidosDiarios <- data.spain.menos$Fallecidos - spain_covid[-c((nrow(spain_covid)-19):nrow(spain_covid)),]$Fallecidos
  
  write_csv(spain_covid, "data/spain_covid_dataset.csv")
}
