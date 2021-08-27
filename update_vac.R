library(tidyverse)
library(textclean)
library(pdftools)
library(miscTools)
library(parsedate)

spain_vac <- read.csv(get_path("vacspain.csv"), stringsAsFactors = FALSE)

fecha <- as.Date(max(unique(spain_vac$fecha))) + 1
pdf_file <- tryCatch(
  pdf_text(paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Informe_GIV_comunicacion_", str_replace_all(fecha, "-", ""), ".pdf")),
  error=function(e) NULL
)
if (is.null(pdf_file)){
  fecha <- as.Date(Sys.Date())
  pdf_file <- tryCatch(
    pdf_text(paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Informe_GIV_comunicacion_", str_replace_all(fecha, "-", ""), ".pdf")),
    error=function(e) NULL
  )
}
if (!is.null(pdf_file)){
  datos_unlist <- unlist(str_split(pdf_file[3], "[\\r\\n]+"))
  datos_unlist <- data.frame(str_split_fixed(str_trim(datos_unlist), "\\s{2,}", 11))
  datos_comunidades <- data.frame(apply(datos_unlist, 2, function(x) gsub("^$", NA, trimws(x))))
  datos_comunidades$X11 <- NULL
  datos_comunidades <- datos_comunidades %>% filter(complete.cases(datos_comunidades))
  names(datos_comunidades) <- c("Comunidad", "Pfizer", "Moderna", "AstraZeneca", "Janssen", "Entregadas", "Administradas", "PercAdministradas", "Personas1Dosis", "Personas2Dosis")
  
  datos_comunidades <- data.frame(lapply(datos_comunidades, as.character), stringsAsFactors=FALSE)
  datos_comunidades <- data.frame(lapply(datos_comunidades, str_replace_all, pattern = "\\.", replacement = ""))
  datos_comunidades <- data.frame(lapply(datos_comunidades, str_replace_all, pattern = "%", replacement = ""))
  datos_comunidades <- data.frame(lapply(datos_comunidades, str_replace_all, pattern = ",", replacement = "\\."))
  datos_comunidades <- data.frame(lapply(datos_comunidades, as.character), stringsAsFactors=FALSE)
  datos_comunidades[2:10] <- data.frame(lapply(datos_comunidades[2:10], as.numeric))
  datos_comunidades$Comunidad <- c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha" , "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "Ceuta", "Melilla", "Fuerzas Armadas", "Sanidad Exterior", "España")
  datos_comunidades$fecha <- as.Date(fecha)
  vacunacion <- datos_comunidades
  vacunacion <- vacunacion[-which(vacunacion$Comunidad == "Fuerzas Armadas"),]
  vacunacion <- vacunacion[-which(vacunacion$Comunidad == "Sanidad Exterior"),]
  rownames(vacunacion) <- NULL
  pobcoms <- data.frame(Comunidad = c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha" , "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "Ceuta", "Melilla"), stringsAsFactors = FALSE, Poblacion = c(8464411, 1329391, 1018784, 1171543, 2175952, 582905, 2394918, 2045221, 7780479, 5057353, 1063987, 2701819, 319914, 6779888, 1511251, 661197, 2220504, 84202, 87076))
  
  pobcoms <- rbind(pobcoms, data.frame(Comunidad = "España", Poblacion = sum(pobcoms$Poblacion)))
  
  vacunacion <- left_join(vacunacion, pobcoms, by = "Comunidad")
  vacunacion$percvacunados <- round(100*vacunacion$Personas2Dosis/vacunacion$Poblacion, 2)
  
  #spain_vac <- read.csv("data/vacspain.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
  spain_vac$fecha <- as.Date(spain_vac$fecha)
  spain_vac <- rbind(spain_vac, vacunacion)
  
  write_csv(spain_vac, "data/vacspain.csv")
}
