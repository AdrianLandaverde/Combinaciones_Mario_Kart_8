library(shiny)
library(shinydashboard)
library(dplyr, quietly = TRUE)

sidebar <- dashboardSidebar(
  width=300,
  sidebarMenu(
    hr(),
    menuItem("Estadísticas Velocidad", tabName = "estadisticas1", icon= icon("dashboard"),
             sliderInput(inputId = "ObjVelociTierra", label = h4("Velocidad en Tierra"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjVelociAgua", label = h4("Velocidad en Agua"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjVelociAntigravedad", label = h4("Velocidad en Antigravedad"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjVelociAire", label = h4("Velocidad en Aire"), min = 0, 
                         max = 6, value = 3, step=0.25)),
    
    menuItem("Estadísticas Aceleración", tabName = "estadisticas2", icon = icon("clock-o"),
             sliderInput(inputId = "ObjPeso", label = h4("Peso"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjAceleracion", label = h4("Aceleración"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjMiniturbo", label = h4("Miniturbo"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjTraccionDentro", label = h4("Tracción Dentro"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjTraccionFuera", label = h4("Tracción Fuera"), min = 0, 
                         max = 6, value = 3, step=0.25)),
    
    menuItem("Estadísticas Manejo", tabName = "estadisticas3", icon = icon("gamepad"),
             sliderInput(inputId = "ObjManejoTierra", label = h4("Manejo en Tierra"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjManejoAgua", label = h4("Manejo en Agua"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjManejoAntigravedad", label = h4("Manejo en Antigravedad"), min = 0, 
                         max = 6, value = 3, step=0.25),hr(),
             sliderInput(inputId = "ObjManejoAire", label = h4("Manejo en Aire"), min = 0, 
                         max = 6, value = 3, step=0.25)),hr(),
    
    menuItem("Multiplicadores Velocidad", tabName = "multiplicadores1", icon = icon("dashboard"),
             sliderInput("MultiVelociTierra", label = h4("Velocidad en Tierra"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiVelociAgua", label = h4("Velocidad en Agua"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiVelociAntigravedad", label = h4("Velocidad en Antigravedad"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiVelociAire", label = h4("Velocidad en Aire"), min = 0, 
                         max = 3, value = 1, step=0.5)),
    
    menuItem("Multiplicadores Aceleración", tabName = "multiplicadores2", icon = icon("clock-o"),
             sliderInput("MultiPeso", label = h4("Peso"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiAceleracion", label = h4("Aceleración"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiMiniturbo", label = h4("Miniturbo"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiTraccionDentro", label = h4("Tracción Dentro"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiTraccionFuera", label = h4("Tracción Fuera"), min = 0, 
                         max = 3, value = 1, step=0.5)),
    
    menuItem("Multiplicadores Manejo", tabName = "multiplicadores2", icon = icon("gamepad"),
             sliderInput("MultiManejoTierra", label = h4("Manejo en Tierra"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiManejoAgua", label = h4("Manejo en Agua"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiManejoAntigravedad", label = h4("Manejo en Antigravedad"), min = 0, 
                         max = 3, value = 1, step=0.5),hr(),
             sliderInput("MultiManejoAire", label = h4("Manejo en Aire"), min = 0, 
                         max = 3, value = 1, step=0.5)),hr()
  )
)

body <- dashboardBody(
  h1("Combinación Más Cercana a las estadísticas elegidas"),
  verbatimTextOutput("pruebas"),
  fluidRow(
    column(width = 7,
      h2("Personajes: "),
      imageOutput("imagenPersonajes",width = "auto", height="auto"),
      h2("Karts: "),
      imageOutput("imagenKarts",width = "auto", height="auto"),
      h2("LLantas: "),
      imageOutput("imagenLlantas",width = "auto", height="auto"),
      h2("Alas: "),
      imageOutput("imagenAlas",width = "auto", height="auto")
    ),
    column(width = 5,
      h2("Estadísticas: "),
      infoBoxOutput("velocidadTierraBox"),
      infoBoxOutput("velocidadAguaBox"),
      infoBoxOutput("velocidadAntigravedadBox"),
      infoBoxOutput("velocidadAireBox"),
      infoBoxOutput("pesoBox"),
      infoBoxOutput("aceleracionBox"),
      infoBoxOutput("miniturboBox"),
      infoBoxOutput("traccionDentroBox"),
      infoBoxOutput("traccionFueraBox"),
      infoBoxOutput("manejoTierraBox"),
      infoBoxOutput("manejoAguaBox"),
      infoBoxOutput("manejoAntigravedadBox"),
      infoBoxOutput("manejoAireBox"),
      infoBoxOutput("promedioBox"),
      infoBoxOutput("porcentajeBox")
    )
  ),
  dataTableOutput("tablaCombinaciones")
)

header<- dashboardHeader(title="Combinanciones Mario Kart 8", titleWidth = 300)

ui <- dashboardPage(header, 
                    sidebar, 
                    body)
server <- function(input, output) {
  combinaciones<- read.csv("combinaciones.csv")
  nombres<- c("PE","AC","TD","TF","MT","VT","VA","VG","VI","MT","MA","MG","MI")
  colnames(combinaciones)[9:21]<- nombres
  combinaciones[,9:21]<- combinaciones[,9:21]*.25+.75 
  
  combinaciones$Promedio<- apply(combinaciones[,c(9:21)],1,mean,na.rm=TRUE)
  
  objPeso<- reactive({input$ObjPeso})
  objAceleracion<- reactive({input$ObjAceleracion})
  objTraccionDentro<- reactive({input$ObjTraccionDentro})
  objTraccionFuera<- reactive({input$ObjTraccionFuera})
  objMiniturbo<- reactive({input$ObjMiniturbo})
  objVelocidadTierra<- reactive({input$ObjVelociTierra})
  objVelocidadAgua<- reactive({input$ObjVelociAgua})
  objVelocidadAntigravedad<- reactive({input$ObjVelociAntigravedad})
  objVelocidadAire<- reactive({input$ObjVelociAire})
  objManejoTierra<- reactive({input$ObjManejoTierra})
  objManejoAgua<- reactive({input$ObjManejoAgua})
  objManejoAntigravedad<- reactive({input$ObjManejoAntigravedad})
  objManejoAire<- reactive({input$ObjManejoAire})
  
  multiPeso<- reactive({input$MultiPeso})
  multiAceleracion<- reactive({input$MultiAceleracion})
  multiTraccionDentro<- reactive({input$MultiTraccionDentro})
  multiTraccionFuera<- reactive({input$MultiTraccionFuera})
  multiMiniturbo<- reactive({input$MultiMiniturbo})
  multiVelocidadTierra<- reactive({input$MultiVelociTierra})
  multiVelocidadAgua<- reactive({input$MultiVelociAgua})
  multiVelocidadAntigravedad<- reactive({input$MultiVelociAntigravedad})
  multiVelocidadAire<- reactive({input$MultiVelociAire})
  multiManejoTierra<- reactive({input$MultiManejoTierra})
  multiManejoAgua<- reactive({input$MultiManejoAgua})
  multiManejoAntigravedad<- reactive({input$MultiManejoAntigravedad})
  multiManejoAire<- reactive({input$MultiManejoAire})
  
  combinacionesFinales<- reactive({

  combinaciones$Porcentaje<- ((sqrt((objPeso()-combinaciones$PE)^2*multiPeso()+
      (objAceleracion()-combinaciones$AC)^2*multiAceleracion()+
      (objTraccionDentro()-combinaciones$TD)^2*multiTraccionDentro()+
      (objTraccionFuera()-combinaciones$TF)^2*multiTraccionFuera()+
      (objMiniturbo()-combinaciones$MT)^2*multiMiniturbo()+
      (objVelocidadTierra()-combinaciones$VT)^2*multiVelocidadTierra()+
      (objVelocidadAgua()-combinaciones$VA)^2*multiVelocidadAgua()+
      (objVelocidadAntigravedad()-combinaciones$VG)^2*multiVelocidadAntigravedad()+
      (objVelocidadAire()-combinaciones$VI)^2*multiVelocidadAire()+
      (objManejoTierra()-combinaciones$MT)^2*multiManejoTierra()+
      (objManejoAgua()-combinaciones$MA)^2*multiManejoAgua()+
      (objManejoAntigravedad()-combinaciones$MG)^2*multiManejoAntigravedad()+
      (objManejoAire()-combinaciones$MI)^2*multiManejoAire())))
  distanciamax<- sqrt(6^2*multiPeso()+6^2*multiAceleracion()+ 6^2*multiTraccionDentro()+
                        6^2*multiTraccionFuera()+ 6^2*multiMiniturbo()+ 6^2*multiVelocidadTierra()+
                        6^2*multiVelocidadAire()+ 6^2*multiVelocidadAntigravedad()+
                        6^2*multiVelocidadAgua()+ 6^2*multiManejoTierra()+
                        6^2*multiManejoAgua()+ 6^2*multiManejoAntigravedad()+
                        6^2*multiManejoAire())
  combinaciones$Porcentaje<-100 - (combinaciones$Porcentaje*100/distanciamax)
  
  orden<- order(-combinaciones$Porcentaje)
  combinaciones<- combinaciones[orden,]
  })
  
  output$velocidadTierraBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,14],"Velocidad Tierra", icon = icon("dashboard"),
      color = "blue",width = NULL)})
  
  output$velocidadAguaBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,15],"Velocidad Agua", icon = icon("dashboard"),
      color = "blue",width = NULL)})
  
  output$velocidadAntigravedadBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,16],"Velocidad Antigravedad", icon = icon("dashboard"),
      color = "blue",width = NULL)})
  
  output$velocidadAireBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,17],"Velocidad Aire", icon = icon("dashboard"),
      color = "blue",width = NULL)})
  
  output$pesoBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,9],"Peso", icon = icon("clock-o"),
      color = "aqua",width = NULL)})
  
  output$aceleracionBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,10],"Aceleración", icon = icon("clock-o"),
      color = "aqua",width = NULL)})
  
  output$miniturboBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,13],"MIniturbo", icon = icon("clock-o"),
      color = "aqua",width = NULL)})
  
  output$traccionDentroBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,11],"Tracción Dentro", icon = icon("clock-o"),
      color = "aqua",width = NULL)})
  
  output$traccionFueraBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,12],"Tracción Fuera", icon = icon("clock-o"),
      color = "aqua",width = NULL)})
  
  output$manejoTierraBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,18],"Manejo Tierra", icon = icon("gamepad"),
      color = "teal",width = NULL)})
  
  output$manejoAguaBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,19],"Manejo Agua", icon = icon("gamepad"),
      color = "teal",width = NULL)})
  
  output$manejoAntigravedadBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,20],"Manejo Antigravedad", icon = icon("gamepad"),
      color = "teal",width = NULL)})
  
  output$manejoAireBox <- renderValueBox({
    valueBox(
      combinacionesFinales()[1,21],"Manejo Aire", icon = icon("gamepad"),
      color = "teal",width = NULL)})
  
  output$promedioBox <- renderValueBox({
    valueBox(
      round(combinacionesFinales()[1,22],2),"Promedio puntos", icon = icon("bar-chart"),
      color = "olive",width = NULL)})
  
  output$porcentajeBox <- renderValueBox({
    valueBox(
      paste0(round(combinacionesFinales()[1,23],2),"%"),"Porcentaje Coincidencia", icon = icon("bar-chart"),
      color = "olive",width = NULL)})
  
  output$imagenPersonajes <- renderImage({
    filename <- paste0(combinacionesFinales()[1,1],".png")
    list(src = filename,
         alt = "Imagen de los personajes", width=NULL,height=NULL)
  }, deleteFile = FALSE)
  
  output$imagenKarts <- renderImage({
    filename <- paste0(combinacionesFinales()[1,3],".png")
    list(src = filename,
         alt = "Imagen de los karts", width=NULL,height=NULL)
  }, deleteFile = FALSE)
  
  output$imagenLlantas <- renderImage({
    filename <- paste0(combinacionesFinales()[1,5],".png")
    list(src = filename,
         alt = "Imagen de las llantas", width=NULL,height=NULL)
  }, deleteFile = FALSE)
  
  output$imagenAlas <- renderImage({
    filename <- paste0(combinacionesFinales()[1,7],".png")
    list(src = filename,
         alt = "Imagen de las alas", width=NULL,height=NULL)
  }, deleteFile = FALSE)
  
  output$tablaCombinaciones<- renderDataTable({
    combinaciones<- combinacionesFinales()
    columnas<- c(2,4,6,8:23)
    combinaciones<- combinacionesFinales()[,columnas]
    combinaciones[,1]<-gsub(";",", ", combinaciones[,1])
    combinaciones[,2]<-gsub(";",", ", combinaciones[,2])
    combinaciones[,3]<-gsub(";",", ", combinaciones[,3])
    combinaciones[,4]<-gsub(";",", ", combinaciones[,4])
    combinaciones
  })
  
  
}

shinyApp(ui = ui, server = server)