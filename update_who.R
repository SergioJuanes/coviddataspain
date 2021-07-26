library(tidyverse)

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

print(max(whodata$Date_reported))

write_csv(whodata, "data/whodata.csv")
