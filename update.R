library(tidyverse)
library(textclean)
library(pdftools)
library(miscTools)
library(parsedate)

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
  datos_unlist <- data.frame(str_split_fixed(str_trim(datos_unlist), "\\s{2,}", 10))
  datos_comunidades <- data.frame(apply(datos_unlist, 2, function(x) gsub("^$", NA, trimws(x))))
  datos_comunidades <- datos_comunidades %>% filter(complete.cases(datos_comunidades))
  print(datos_comunidades)
  fecha <- as.Date(format(parsedate::parse_date(datos_unlist[[1]][5]), "%Y-%m-%d"))
  if (is.na(fecha)) {
    fecha <- as.Date(format(parsedate::parse_date(datos_unlist[[1]][6]), "%Y-%m-%d"))
  }
  
  if (fecha == max(spain_covid$Fecha)) {
    #if (as.POSIXlt(fecha)$wday == 5) {
    #  fecha = fecha + 3
    #} else {
      fecha = fecha + 1
    #}
  }
  print(datos_comunidades)
  names(datos_comunidades) <- c("Comunidad", "Total", "diaprevio", "casos14dias", "IA14", "casos7dias", "IA7", "Fallecidos", "ultimos7", "letalidad")
  print(datos_comunidades)
  datos_comunidades <- data.frame(lapply(datos_comunidades, as.character), stringsAsFactors=FALSE)
  datos_comunidades$Fecha <- fecha
  
  datos_comunidades$Comunidad <- c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla-La Mancha", "Castilla y León", "Cataluña", "Ceuta", "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Melilla", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja", "España")
  
  datos_comunidades <- datos_comunidades %>% select(Comunidad, Total, IA14, Fecha, Fallecidos)
  names(datos_comunidades) <- c("Comunidad", "Casos", "IA14", "Fecha", "Fallecidos")
  spain_covid_nuevos <- datos_comunidades
  
  
  spain_covid_nuevos$CasosDiarios <- 0
  spain_covid_nuevos$FallecidosDiarios <- 0
  
  spain_covid_nuevos$Casos <- as.numeric(gsub("\\.", "", spain_covid_nuevos$Casos))
  spain_covid_nuevos$Fallecidos <- as.numeric(gsub("\\.", "", spain_covid_nuevos$Fallecidos))
  spain_covid_nuevos$IA14 <- gsub("\\.", "", spain_covid_nuevos$IA14)
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
                                                                

uci_covid_spain <- read_csv("data/ucispain.csv")
number_file <- max(uci_covid_spain$number_file) + 1
pdf_file <- tryCatch(
  pdf_text(paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actualizacion_",
                   number_file,"_COVID-19.pdf", 
                   collapse = "")),
  error=function(e) NULL
)
if(!is.null(pdf_file)){
  datos_unlist <- unlist(str_split(pdf_file[3], "[\\r\\n]+"))
  #datos_unlist <- gsub("Mancha", "Mancha ", datos_unlist)
  datos_unlist <- data.frame(str_split_fixed(str_trim(datos_unlist), "\\s{2,}", 9))
  datos_comunidades <- data.frame(apply(datos_unlist, 2, function(x) gsub("^$", NA, trimws(x))), stringsAsFactors = FALSE)
  datos_comunidades <- datos_comunidades %>% filter(complete.cases(datos_comunidades))
  #datos_comunidades <- datos_comunidades[-which(datos_comunidades$X1 == "CCAA"),]
  names(datos_comunidades) <- c("CCAA", "Total_Ingresados", "Tasa_hosp", "PercCamasCovid", "UCI", "Tasa_UCI", "PerCamasUCI", "Ingresos24h", "Altas24h")
  datos_comunidades$CCAA <- c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla-La Mancha", "Castilla y León" , "Cataluña", "Ceuta", "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Melilla", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja", "España")

  fecha <- as.Date(format(parsedate::parse_date(unlist(str_split(pdf_file[1], "[\\r\\n]+"))[5]), "%Y-%m-%d"))
  if(is.na(fecha)){
    fecha <-as.Date(format(parsedate::parse_date(unlist(str_split(pdf_file[1], "[\\r\\n]+"))[6]), "%Y-%m-%d"))
  }
                                       

  datos_comunidades$fecha <- fecha
  
  uci_covid_ultimos <- datos_comunidades
  
  uci_covid_ultimos$PercCamasCovid <- sub("%", "", uci_covid_ultimos$PercCamasCovid)
  uci_covid_ultimos$PercCamasCovid <- sub(",", "\\.", uci_covid_ultimos$PercCamasCovid)
  uci_covid_ultimos$PerCamasUCI <- sub("%", "", uci_covid_ultimos$PerCamasUCI)
  uci_covid_ultimos$PerCamasUCI <- sub(",", "\\.", uci_covid_ultimos$PerCamasUCI)
  uci_covid_ultimos[2:7] <- data.frame(lapply(uci_covid_ultimos[2:7], as.numeric))
  uci_covid_ultimos$Total_Ingresados <- str_replace_all(uci_covid_ultimos$Total_Ingresados, ".000", "")
  uci_covid_ultimos$Total_Ingresados <- str_replace_all(uci_covid_ultimos$Total_Ingresados, "\\.", "")
  
  uci_covid_ultimos <- uci_covid_ultimos %>% dplyr::select(-c(Tasa_hosp, Tasa_UCI))
  uci_covid_ultimos$number_file <- number_file
  
  uci_covid_spain <- rbind(uci_covid_spain, uci_covid_ultimos)
  write_csv(uci_covid_spain, "data/ucispain.csv")
  
}
