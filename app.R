library(shiny)
library(shinyjs)
library(shinyWidgets)
library(terra)
library(stringr)
library(ggplot2)
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

ui = fluidPage(
    useShinyjs(),
    titlePanel(h3(tags$b("Landsat 8 (Level 1) thermal band conversion to Celsius degrees"), align = "center")),
    sidebarLayout(
        sidebarPanel(
            tags$style(".well {background-color: #dddddd;}"),
            fileInput(inputId = "plik_tif", label = "thermal band B10 (.TIF)", buttonLabel = "wybierz...",
                      multiple = FALSE, accept = c(".TIF", ".tif")),
            #helpText("Uwaga! Pobierz plik z poziomu 1."),
            fileInput(inputId = "plik_mtl", label = "MTL file (.txt)", buttonLabel = "wybierz...",
                      multiple = FALSE, accept = c(".txt")),
            fileInput(inputId = "plik_shp", label = "vector mask (.shp/.gpkg)",
                      buttonLabel = "wybierz...", multiple = FALSE, accept = c(".shp", ".gpkg")),
            span(helpText("Max. file size: 100MB"), style = "text-align: center"),
            hr(style = "border-color: #aaaaaa"),
            disabled(numericInput(inputId = "bufor", label = "radius of mask buffer in km", value = 0)),
            disabled(
              span(checkboxInput(inputId = "show_mask", label = "show mask boundary", value = FALSE),
                   style = "text-align: center")
            ),
            disabled(
              span(checkboxInput(inputId = "bbox", label = "mask as bounding box", value = FALSE),
                   style = "text-align: center")
            ),
            numericInput(inputId = "bbox_mult", label = "multiply mask bounding box by:", value = 1.5),
            span(checkboxInput(inputId = "log", label = "logarythmic Y-axis of histogram", value = FALSE),
                 style = "text-align: center"),
            hr(style = "border-color: #aaaaaa"),
            actionButton(inputId = "oblicz", label = "Run",
                         style = "background-color: forestgreen; width: 100%; font-size: 20px"),
            actionButton(inputId = "zapisz", label = "Save",
                         style = "background-color: gray; width: 100%; font-size: 20px"),
            downloadButton(outputId = "pobierz", label = "Save .tif",
                           style = "background-color: gray; width: 100%; font-size: 20px"),
            hr(style = "border-color: #aaaaaa"),
            span(textOutput("info_zapis"), style = "color: forestgreen; text-align: center"),
            span(textOutput("info_zapis2"), style = "color: red; text-align: center")
        ),
        mainPanel(
            plotOutput("raster"),
            tags$head(tags$style("#stats{font-size: 16px}")),
            div(verbatimTextOutput("stats"), style = "text-align: center; width: 90%; padding-left: 20px"),
            plotOutput("histogram", height = "350px")
        )
    )
)

server = function(input, output) {
    options(warn=-1)
    B10 = reactiveVal(NULL)
    mask_name = reactiveVal(NULL)
    mtl_name = reactiveVal(NULL)
    srednia = reactiveVal(NULL)
    minimum = reactiveVal(NULL)
    maksimum = reactiveVal(NULL)
    info = reactiveVal("")
    info2 = reactiveVal("")
    rast_export = reactiveVal(rast())
    czy_export = reactiveVal(0)
    date = reactiveVal(format(Sys.Date(), "%d-%m-%y"))

    enable_checkbox = reactive({
      if(!is.null(input$plik_shp$datapath)){
        return(TRUE)
      } else{
        return(FALSE)
      }
    })

    observeEvent(input$oblicz,
                 {
                   B10(input$plik_tif$datapath)
                   mask_name(input$plik_shp$datapath)
                   mtl_name(input$plik_mtl$datapath)
                   if (identical(str_detect(input$plik_tif$datapath, ".TIF$|.tif$"), logical(0))){
                     showModal(modalDialog(
                       title = "No raster file provided!",
                       paste("Select .tif or .TIF file"),
                       easyClose = TRUE,
                       footer = NULL
                     ))
                   } else if (!str_detect(input$plik_tif$datapath, ".TIF$|.tif$") && !identical(str_detect(input$plik_tif$datapath, ".TIF$|.tif$"), logical(0))){
                     showModal(modalDialog(
                       title = "Wrong format of raster file!",
                       paste("Select .tif or .TIF file"),
                       easyClose = TRUE,
                       footer = NULL
                     ))
                     B10(NULL)
                   } else if (!str_detect(input$plik_mtl$datapath, ".txt$") && !identical(str_detect(input$plik_mtl$datapath, ".txt$"), logical(0))){
                     showModal(modalDialog(
                       title = "Wrong format of MTL file!",
                       paste("Select .txt file"),
                       easyClose = TRUE,
                       footer = NULL
                     ))
                     mtl_name(NULL)
                   } else if (!str_detect(input$plik_shp$datapath, ".shp$|.gpkg$") && !identical(str_detect(input$plik_shp$datapath, ".shp$|.gpkg$"), logical(0))){
                     showModal(modalDialog(
                       title = "Wrong format of vector mask file!",
                       paste("Select .shp or .gpkg file"),
                       easyClose = TRUE,
                       footer = NULL
                     ))
                     mask_name(NULL)
                   }
                 }
    )

    output$raster = renderPlot(
        {
            if (enable_checkbox() == TRUE){
              enable("show_mask")
              enable("bbox")
              enable("bufor")
            }
            req(B10())
            map = rast(B10())
            if (!is.null(mask_name())){
                map = rast(B10())
                mask = vect(mask_name())
                granica = project(mask, crs(map))
                mask = buffer(mask, input$bufor * 1000)
                mask = project(mask, crs(map))
                if (input$bbox == TRUE){
                  mask2 = ext(mask) * input$bbox_mult
                  map = crop(map, mask2)
                } else{
                  map = crop(map, mask)
                  map = mask(map, mask)
                }
            }

            if (!is.null(mtl_name())){
                MTL = read.delim(mtl_name())
                colnames(MTL) = "landsat"
                ml = str_subset(MTL$landsat, "RADIANCE_MULT_BAND_10 = [A-Z0-9\\.\\-]+")
                ml = as.numeric(str_extract(str_remove(ml, "RADIANCE_MULT_BAND_10"), "[A-Z0-9\\.\\-]+"))
                al = str_subset(MTL$landsat, "RADIANCE_ADD_BAND_10 = [A-Z0-9\\.\\-]+")
                al = as.numeric(str_extract(str_remove(al, "RADIANCE_ADD_BAND_10"), "[A-Z0-9\\.\\-]+"))
                k1 = str_subset(MTL$landsat, "K1_CONSTANT_BAND_10 = [A-Z0-9\\.\\-]+")
                k1 = as.numeric(str_extract(str_remove(k1, "K1_CONSTANT_BAND_10"), "[A-Z0-9\\.\\-]+"))
                k2 = str_subset(MTL$landsat, "K2_CONSTANT_BAND_10 = [A-Z0-9\\.\\-]+")
                k2 = as.numeric(str_extract(str_remove(k2, "K2_CONSTANT_BAND_10"), "[A-Z0-9\\.\\-]+"))
                #print(paste(ml, al, k1, k2))
            } else{
                ml = 0.0003342
                al = 0.1
                k1 = 774.8853
                k2 = 1321.0789
            }

            map = (k2 / log((k1 / ((ml * map) + al)) + 1)) - 273.15
            rast_export(map)
            czy_export(1)
            info2("")
            srednia(round(mean(values(map), na.rm = TRUE), 2))
            minimum(round(min(values(map), na.rm = TRUE), 2))
            maksimum(round(max(values(map), na.rm = TRUE), 2))

            data = str_extract(input$plik_tif$name, "[0-9]{8}")
            data = paste0(str_sub(data, 1, 4), "-", str_sub(data, 5, 6), "-", str_sub(data, 7, 8))
            date(format(as.Date(data), "%d-%m-%y"))
            data = format(as.Date(data), "%d.%m.%Y")

            terra::plot(map, col = hcl.colors(10, palette = "RdYlGn", rev = TRUE),
                        plg = list(title = "[°C]"), main = str_c("Date:  ", data))
            if (input$show_mask == TRUE){
              lines(granica, lwd = 2.5, col = "#333333")
            }
        }
    )

    output$stats = renderText(
        {
          req(srednia(), minimum(), maksimum())
          paste("Mean:", srednia(), "°C", "\nMinimum:", minimum(), "°C", "\nMaximum:", maksimum(), "°C")
        }
    )

    output$histogram = renderPlot(
      {
        if (czy_export() == 1){
          req(rast_export())
          val = values(rast_export())
          # hist(val, main = "Histogram", xlab = "temperatura w °C",
          #      ylab = "count", col = hcl.colors(20, palette = "RdYlGn", rev = TRUE),
          #      breaks = c(seq(min(val, na.rm = TRUE), max(val, na.rm = TRUE),
          #                     (max(val, na.rm = TRUE) - min(val, na.rm = TRUE)) / 19)),
          #      border = FALSE)
          p = ggplot(as.data.frame(val), aes(x = val)) +
            geom_histogram(fill = hcl.colors(20, palette = "RdYlGn", rev = TRUE), bins = 20) +
            labs(title = "Histogram", x = "temperatura w °C") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            theme_bw()
          if (input$log == TRUE){
            p = p + scale_y_log10()
          }
          return(p)
        }
      }
    )

    observeEvent(input$zapisz,
                 {
                   if (czy_export() == 1){
                     writeRaster(rast_export(), paste0("B10_converted_", date(), ".tif"), overwrite = TRUE)
                     info2("")
                     info(paste0("Saved to B10_converted_", date(), ".tif"))
                   } else{
                     info2("No data to save!")
                   }
                 }
    )

    output$pobierz = downloadHandler(
      filename = function() {
        paste0("B10_converted_", date(), ".tif")
      },
      content = function(file) {
        if (czy_export() == 1){
          writeRaster(rast_export(), paste0("B10_converted_", date(), ".tif"), overwrite = TRUE)
          info2("")
          info(paste0("Saved to B10_converted_", date(), ".tif"))
        } else{
          info2("No data to save!")
        }
      }
    )

    observeEvent(input$bbox, {
      if(input$bbox == FALSE){
        disable("bbox_mult")
      } else {
        enable("bbox_mult")
      }
    })

    output$info_zapis = renderText({paste(info())})
    output$info_zapis2 = renderText({paste(info2())})
}

shinyApp(ui = ui, server = server)

