library(shiny)
library(shiny.router)
library(geojsonio)
library(highcharter)
library(shinyjs)
library(tidyverse)
library(shinycssloaders)
library(shinyBS)
library(xts)
library(readr)
library(DT)


#carga de datos de españa
data.spain <- read.csv(url("https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/spain_covid_dataset.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
data.spain$Comunidad <- as.character(data.spain$Comunidad)
data.spain$Fecha <- as.Date(data.spain$Fecha)

#carga de datos en las ucis de españa
data.uci.spain <- read.csv(url("https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/ucispain.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")

#datos de vacunación
data.spain.vac <- read.csv(url("https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/vacspain.csv"), stringsAsFactors = FALSE)

#mapa de españa
spain.map <- geojson_read("https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/simple_spain.geojson")

#datos mundiales
whodata <- read_csv(url("https://covid19.who.int/WHO-COVID-19-global-data.csv"))
popcsv <- read_csv(url("https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/population.csv"))

spainpopulation <- (popcsv %>% dplyr::filter(Country == "Spain"))$PopTotal

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
  
  return(df)
}

whodata <- cambio_nombres(whodata)
popcsv <- cambio_nombres(popcsv)

#poblacion por comunidades
pobcoms <- data.frame(Comunidad = c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha" , "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "Ceuta", "Melilla"), stringsAsFactors = FALSE, Poblacion = c(8464411, 1329391, 1018784, 1171543, 2175952, 582905, 2394918, 2045221, 7780479, 5057353, 1063987, 2701819, 319914, 6779888, 1511251, 661197, 2220504, 84202, 87076))

#mapa mundial
data(worldgeojson, package = "highcharter")

#añadimos nombre en español
nombres_esp <- read.csv(url("https://gist.githubusercontent.com/brenes/1095110/raw/c8f208b03485ba28f97c500ab7271e8bce43b9c6/paises.csv"), stringsAsFactors = FALSE, encoding = "UTF-8") %>% dplyr::select(nombre, iso2)
whodata <- left_join(whodata, nombres_esp, by = c("Country_code" = "iso2"))
whodata[which(whodata$Country == "Kosovo"),]$nombre <- "Kosovo"

#definimos la función de botón

main_container <- function(...){
  div(class="main-container",
      ...
  )
}

valueBox <- function(value, subtitle, color) {
  div(class = "col-lg-12 col-md-8",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = ("col-xs-11 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          value
                      ),
                      div(subtitle)
                  )
              )
          )
      )
  )
}


world_page <- div(class="container_text",
                  div(
                    valueBox(format(Sys.Date() - 1, "%d-%m-%Y"), "Fecha de los datos", "#4DB1FF"),
                    tabsetPanel(
                      tabPanel("Mapa",
                               fluidRow(
                                 column(4, 
                                        highchartOutput("highchartnuevoscasos", width = "100%", height = "270px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        highchartOutput("highchartnuevosfallecidos", width = "100%", height = "270px") %>% withSpinner(type = 6, color = "#000CCC")
                                 ),
                                 column(8,
                                        highchartOutput("highchartmapamundial", width="100%", height="600px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        h6("*Porcentajes de casos y fallecidos respecto a la población total de cada país.")
                                 )
                               )
                        
                      ),
                      tabPanel("Tabla",
                               div(style="margin-bottom:10px;"),
                               DTOutput("datatableworld")
                      )
                    )


                  )
)

spain_page <- div(class="container_text",
                  div(
                    div(
                      column(12,
                             valueBox(format(as.Date(max(data.spain$Fecha)), "%d-%m-%Y"), "Fecha de los datos", "#4DB1FF"),
                      )
                    ),
                    tabsetPanel(
                      tabPanel("Mapas",
                               fluidRow(
                                 div(style="margin-bottom:10px;"),
                                 column(4,
                                        highchartOutput("highchartnuevoscasosspain", width = "100%", height = "270px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        highchartOutput("highchartnuevosfallecidosspain", width = "100%", height = "270px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        highchartOutput("highchartucispain", width = "100%", height = "260px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 ),
                                 column(8,
                                        highchartOutput("highchartincidenciaaucumulada", width="100%", height="800px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 )
                               )
                               
                               
                      ),
                      tabPanel("Gráfica",
                               div(style="margin-bottom:10px;"),
                               selectInput("ccaa", "Elige la CCAA", choices = unique(data.spain$Comunidad)),
                               tabsetPanel(
                                 tabPanel("Casos",
                                          highchartOutput("highchartcasosdiarios", height = "500px") %>% withSpinner(type = 6, color = "#000CCC")
                                 ),
                                 tabPanel("Incidencia acumulada",
                                          highchartOutput("highchartincidenciadiarios", height = "500px") %>% withSpinner(type = 6, color = "#000CCC")
                                 ),
                                 tabPanel("Fallecidos",
                                          highchartOutput("highchartfallecidosdiarios", height = "500px") %>% withSpinner(type = 6, color = "#000CCC")
                                 )                               
                               )
                               
                               
                      ),
                      tabPanel("Vacunación",
                              div(
                                highchartOutput("highchartvacspain", height = "800px") %>% withSpinner(type = 6, color = "#000CCC")
                              )         
                      ),
                      tabPanel("Tabla",
                               div(style="margin-bottom:10px;"),
                               DTOutput("datatablespain")
                      )
                    )
                    
                    
                  )
                )

prediction_spain_page <- div(class="container_text",
                  div(
                    h3("Prediction Page")
                  )
)

spain_page_server <- function(input, output, session) {
  
  output$diatext <- renderText({
    req(input$dia)
    date <- input$dia
    format(as.Date(date), "%d-%m-%Y")
  }) 
  
  
  output$highchartcasosdiarios <- renderHighchart({
    data <- data.spain %>% dplyr::filter(Comunidad == input$ccaa)
    series1 <- xts(x = data$Casos, order.by = as.Date(data$Fecha))
    highchart(type = "stock") %>%
      hc_add_series(name = "Casos", series1, color = "#00FFAA") %>% 
      hc_xAxis(plotLines = list(
        list(
          label = list(text = "Se acaba el stock de papel higiénico"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
        ),
        list(
          label = list(text = "Niños menores de 14 años pueden salir"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-04-27", tz = "UTC"))
        ),
        list(
          label = list(text = "Se permite salir a pasear/hacer deporte"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
        ),
        list(
          label = list(text = "Apertura de los primeros bares"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
        ),
        list(
          label = list(text = "Fin del primer estado de alarma"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = "Cominezo de la segunda ola de la pandemia"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
        ),
        list(
          label = list(text = "Se aprueba la primera vacuna contra el COVID-19"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
        ),
        list(
          label = list(text = "Nueva cepa del COVID-19"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-23", tz = "UTC"))
        )
        )
      )
  })
  
  output$highchartincidenciadiarios <- renderHighchart({
    data <- data.spain %>% dplyr::filter(Comunidad == input$ccaa)
    series1 <- xts(x = data$IA14, order.by = as.Date(data$Fecha))
    highchart(type = "stock") %>%
      hc_add_series(name = "Incidencia acumulada", series1, color = "#00FFAA") %>% 
      hc_xAxis(plotLines = list(
        list(
          label = list(text = "Se acaba el stock de papel higiénico"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
        ),
        list(
          label = list(text = "Niños menores de 14 años pueden salir"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-04-27", tz = "UTC"))
        ),
        list(
          label = list(text = "Se permite salir a pasear/hacer deporte"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
        ),
        list(
          label = list(text = "Apertura de los primeros bares"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
        ),
        list(
          label = list(text = "Fin del primer estado de alarma"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = "Cominezo de la segunda ola de la pandemia"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
        ),
        list(
          label = list(text = "Se aprueba la primera vacuna contra el COVID-19"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
        ),
        list(
          label = list(text = "Nueva cepa del COVID-19"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-23", tz = "UTC"))
        )
      )
      )
  })
  
  output$highchartfallecidosdiarios <- renderHighchart({
    data <- data.spain %>% dplyr::filter(Comunidad == input$ccaa)
    series1 <- xts(x = data$Fallecidos, order.by = as.Date(data$Fecha))
    highchart(type = "stock") %>%
      hc_add_series(name = "Fallecidos", series1, color = "#00FFAA") %>% 
      hc_xAxis(plotLines = list(
        list(
          label = list(text = "Se acaba el stock de papel higiénico"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
        ),
        list(
          label = list(text = "Niños menores de 14 años pueden salir"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-04-27", tz = "UTC"))
        ),
        list(
          label = list(text = "Se permite salir a pasear/hacer deporte"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
        ),
        list(
          label = list(text = "Apertura de los primeros bares"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
        ),
        list(
          label = list(text = "Fin del primer estado de alarma"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = "Cominezo de la segunda ola de la pandemia"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
        ),
        list(
          label = list(text = "Se aprueba la primera vacuna contra el COVID-19"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
        ),
        list(
          label = list(text = "Nueva cepa del COVID-19"),
          color = "#000000",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-23", tz = "UTC"))
        )
      )
      )
  })
  
  output$highchartincidenciaaucumulada <- renderHighchart({
    myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClickedspain', event.point.Comunidad);}") 
    data <- data.spain %>% dplyr::filter(Fecha == max(unique(data.spain$Fecha)))
    data$tasacasos <- round(100*(data$Casos/spainpopulation), 2)
    data <- left_join(data, pobcoms, by = "Comunidad")
    data$tasamuertos <- round(100*(data$Fallecidos/data$Poblacion), 2)
    
    
    mapa <- highchart(type="map") %>%
      hc_add_series_map(spain.map, 
                        data,
                        "IA14", c("NAME", "Comunidad"), 
                        nullColor="#F0F0F0", borderColor="#fff",
                        borderWidth=1) %>%
      hc_title(text = "Incidencia acumulada") %>%
      hc_tooltip(headerFormat="<b>{point.point.Comunidad}</b><br>",
                 pointFormat="Incidencia acumulada: <b>{point.IA14}</b> <br> Número de casos: <b>{point.Casos}</b> <br> Porcentaje de casos: <b>{point.tasacasos}</b>%* <br> Número de fallecidos: <b>{point.Fallecidos}</b> <br> <br> Porcentaje de fallecidos: <b>{point.tasamuertos}</b>%") %>%
      hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc), cursor = "pointer"))
    hc_colorAxis(mapa, minColor = "#CC99FF", maxColor = "#6400C7")
    hc_colorAxis(mapa, min = 0, max = round(max(na.omit(data)$IA14)+50, -2), minColor = "#00CCFF", maxColor = "#0F00FF")  
    
  })

  data.spain.filtrado <- reactive({
    if(!is.null(input$hcClickedspain)){
      data.spain %>% dplyr::filter(Comunidad == input$hcClickedspain) %>% select(Comunidad, Fecha, Casos, Fallecidos)
    } else{
      data <- data.spain
      data <- data[-which(data$Comunidad == "España"),]
      data <- data %>% group_by(Fecha) %>% select(Fecha, Casos, Fallecidos)
      aggregate(. ~ Fecha, data, sum)
    }
  })
  
  output$highchartnuevoscasosspain <- renderHighchart({
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Casos en ", unique(data.spain.filtrado()$Comunidad))
    } else {
      titulo <- "Casos en toda España"
    }
    data.spain.filtrado() %>%
      hchart("line", hcaes(x = Fecha, y = Casos), name = "Casos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  output$highchartucispain <- renderHighchart({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Situación en las UCIS en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Situación en las UCIS en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    col_stops <- data.frame(
      q = c(0, 0.4, 0.7, 0.9),
      c = c('#55FF33', '#A3DF0D', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        center = list('50%', '70%'),
        size = '110%',
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = data$PerCamasUCI,
        dataLabels = list(
          y = -40,
          borderWidth = 0,
          useHTML = TRUE,
          format = '<p style="font-size:25px;text-align:center;margin-bottom:0px;"> {point.y}% </p><p style="font-size:12px;margin-top:0px">De camas UCI ocupadas</p>'
        )
      ) %>% 
      hc_size(height = 300)
  })
  
  output$highchartvacspain <- renderHighchart({
    
    data.spain.covid.filter <- data.spain.vac %>% dplyr::filter(fecha == max(data.spain.vac$fecha))
    data.spain.covid.filter$PercPfizer <- round(data.spain.covid.filter$Pfizer/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$PercModerna <- round(data.spain.covid.filter$Moderna/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$PercAstraZeneca <- round(data.spain.covid.filter$AstraZeneca/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$PercJanssen <- round(data.spain.covid.filter$Janssen/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$percvacunados <- round(data.spain.covid.filter$percvacunados ,2)
    
    highchart(type="map") %>%
      hc_add_series_map(spain.map, 
                        data.spain.covid.filter,
                        "percvacunados", c("NAME", "Comunidad"), 
                        nullColor="#F0F0F0", borderColor="#fff",
                        borderWidth=1) %>%
      hc_title(text = "Porcentaje de la población vacunada") %>%
      hc_tooltip(
                 headerFormat="<b>{point.point.Comunidad}</b><br>",
                 pointFormat='Porcentaje de personas vacunadas (2 dosis): <b>{point.percvacunados}</b>% <br> Porcentaje de dosis administradas: <b>{point.PercAdministradas}</b>% <br> Dosis entregadas de <span style="color:blue">Pfizer</span>: <b>{point.PercPfizer}</b>% <br> Dosis entregadas de <span style="color:green">Moderna</span>: <b>{point.PercModerna}</b>% <br> Dosis entregadas de <span style="color:purple">AstraZeneca</span>: <b>{point.PercAstraZeneca}</b>% <br> Dosis entregadas de <span style="color:#FFE400">Janssen</span>: <b>{point.PercJanssen}</b>%') %>%
      hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))))
    hc_colorAxis(mapa, minColor = "#CC99FF", maxColor = "#6400C7")
    hc_colorAxis(mapa, min = min(data.spain.covid.filter$percvacunados)-5, max = max(data.spain.covid.filter$percvacunados)+5, minColor = "#00CCFF", maxColor = "#0F00FF")  
    
  })
  
  output$highchartnuevosfallecidosspain <- renderHighchart({
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Fallecidos en ", unique(data.spain.filtrado()$Comunidad))
    } else {
      titulo <- "Fallecidos en toda España"
    }
    data.spain.filtrado() %>%
      hchart("line", hcaes(x = Fecha, y = Fallecidos), name = "Muertos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  output$highchartmapamundial <- renderHighchart({
    myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClicked', event.point.name);}")    
    data <- whodata
    casos14dias <- data[data$Date_reported == (Sys.Date() - 1),]$Cumulative_cases - data[data$Date_reported == (Sys.Date() - 15),]$Cumulative_cases 
    data <- data %>% dplyr::filter(Date_reported == (Sys.Date() - 1))
    data <- left_join(data, popcsv, by = c("Country" = "Country"))
    data$ia <- round(casos14dias/(data$PopTotal - casos14dias) *100000, 2)
    
    data$tasamuertos <- round(100*(data$Cumulative_deaths/data$PopTotal), 2)
    data$tasacasos <- round(100*(data$Cumulative_cases/data$PopTotal), 2)
    
    hc_mapa <- highchart() %>%
      hc_add_series_map(worldgeojson, data, value = "ia", joinBy = c("name", "Country"), name = "Porcentaje de muertes de la población") %>%
      hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc))) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_title(text = "Incidencia acumulada") %>%
      hc_tooltip(headerFormat="<b>{point.point.nombre}</b><br>",
                 pointFormat="Incidencia acumulada: <b>{point.ia}</b> <br> Número de casos: <b>{point.Cumulative_cases}</b> <br> Porcentaje de casos: <b>{point.tasacasos}</b>%* <br> <br> Número de fallecidos: <b>{point.Cumulative_deaths}</b> <br> <br> Porcentaje de fallecidos: <b>{point.tasamuertos}</b>%*")
    hc_colorAxis(hc_mapa, min = 0, max = round(max(na.omit(data)$ia)+500, -3), minColor = "#00CCFF", maxColor = "#0F00FF")  
  })
  
  world.filtrado <- reactive({
    if(!is.null(input$hcClicked)){
      whodata %>% dplyr::filter(Country == input$hcClicked) %>% dplyr::select(Date_reported, New_cases, Cumulative_cases, New_deaths, Cumulative_deaths, nombre)
    } else{
      data <- whodata %>% dplyr::select(Date_reported, New_cases, Cumulative_cases, New_deaths, Cumulative_deaths) %>% group_by(Date_reported)
      aggregate(. ~ Date_reported, data, sum)
    }
  })
  
  output$highchartnuevoscasos <- renderHighchart({
    if(!is.null(input$hcClicked)){
      titulo <- paste0("Casos en ", unique(world.filtrado()$nombre))
    } else {
      titulo <- "Casos en todo el mundo"
    }
    world.filtrado() %>%
      hchart("line", hcaes(x = Date_reported, y = New_cases), name = "Casos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  output$highchartnuevosfallecidos <- renderHighchart({
    if(!is.null(input$hcClicked)){
      titulo <- paste0("Fallecidos en ", unique(world.filtrado()$nombre))
    } else {
      titulo <- "Fallecidos en todo el mundo"
    }
    world.filtrado() %>%
      hchart("line", hcaes(x = Date_reported, y = New_deaths), name = "Muertos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })

  output$datatableworld <- renderDT({
    data <- whodata
    casos14dias <- data[data$Date_reported == (Sys.Date() - 1),]$Cumulative_cases - data[data$Date_reported == (Sys.Date() - 15),]$Cumulative_cases 
    data <- data %>% dplyr::filter(Date_reported == (Sys.Date() - 1))
    data <- left_join(data, popcsv, by = c("Country" = "Country"))
    data$ia <- round(casos14dias/(data$PopTotal - casos14dias) *100000, 2)
    data <- data %>% dplyr::arrange(desc(Cumulative_cases))
    data <- data %>% dplyr::select("nombre", "Cumulative_cases", "New_cases", "Cumulative_deaths", "New_deaths", "ia")
    names(data) <- c("País", "Casos acumulados", "Nuevos casos en 24h", "Fallecidos acumulados", "Nuevos fallecidos en 24h", "IA 14 días")
    data <- na.omit(data)
    datatable(data, rownames = FALSE)
  })
  
  output$datatablespain <- renderDT({
    data <- data.spain
    data <- data %>% dplyr::filter(Fecha == max(unique(data$Fecha)))
    data <- data %>% dplyr::select(Comunidad, Casos, Fallecidos, IA14) %>% dplyr::arrange(desc(Casos))
    datatable(data, rownames = FALSE)
  })


  
}
prediction_spain_page_server <- function(input, output, session) {}

router <- make_router(
  route("world", world_page),
  route("spain", spain_page, spain_page_server),
  route("prediction_spain", prediction_spain_page, prediction_spain_page_server)
)

menu_button <- function(link = "world", ...) {
  a(class="button-guay",
    href=route_link(link),
    span(class="button-text", ...)
  )
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  title = "Dashboard COVID-19",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css"),
    HTML('<script src="https://kit.fontawesome.com/260fafe623.js" crossorigin="anonymous"></script>')
  ),
  div(class="big-container",
      div(id="dashboard-header",
          div(class="mini-header",
              menu_button(link="world", "Mundial"),
              menu_button(link="spain", "España"),
              menu_button(link="prediction_spain", "Predicción en España")
          )
      ),
      router$ui
  ),
  div(id="footer",
      div(
        HTML("Created by: Sergio Juanes Tébar")
      )
  )
)
server <- function(input, output, session) {
  router$server(input, output, session)
}
shinyApp(ui, server)
