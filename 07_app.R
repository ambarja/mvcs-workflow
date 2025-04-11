library(bs4Dash)
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
# Simulación de datos
predios <-  st_read("output/spatial_geometry.gpkg")

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Predios"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Buscar Predios", tabName = "busqueda", icon = icon("search")),
      textInput("predio_input", "Ingrese uno o más ID de predio (separados por coma)", value = "P001, P003"),
      actionButton("buscar", "Buscar y Exportar KML")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "busqueda",
        fluidRow(
          box(width = 12, leafletOutput("mapa", height = 500))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  predios_filtrados <- reactiveVal(NULL)
  
  observeEvent(input$buscar, {
    ids <- unlist(strsplit(input$predio_input, ",\\s*"))
    predio_match <- predios[predios$id_predio %in% ids, ]

    if (nrow(predio_match) > 0) {
      predios_filtrados(predio_match)
      nombre <- if (nrow(predio_match) == 1) predio_match$id_predio[1] else "lista"
      # Exportar KML
      kml_file <- tempfile(fileext = ".kml")
      st_write(predio_match, kml_file, driver = "KML", delete_dsn = TRUE)
      
      showModal(modalDialog(
        title = "Exportación KML",
        paste0("Se exportaron ", nrow(predio_match), " predios."),
        downloadButton("descargar_kml", "Descargar archivo KML"),
        easyClose = TRUE
      ))
      
      output$descargar_kml <- downloadHandler(
        filename = function() paste0(nombre,".kml"),
        content = function(file) file.copy(kml_file, file)
      )
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "No se encontraron predios con los IDs proporcionados.",
        easyClose = TRUE
      ))
    }
  })
  
  output$mapa <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  observe({
    predios <- predios_filtrados()
    if (!is.null(predios) && nrow(predios) > 0) {
      leafletProxy("mapa") %>%
        clearShapes() %>%
        addPolygons(data = predios, color = "blue", weight = 2) %>%
        fitBounds(
          lng1 = st_bbox(predios)[["xmin"]],
          lat1 = st_bbox(predios)[["ymin"]],
          lng2 = st_bbox(predios)[["xmax"]],
          lat2 = st_bbox(predios)[["ymax"]]
        )
    }
  })
}

shinyApp(ui, server)

