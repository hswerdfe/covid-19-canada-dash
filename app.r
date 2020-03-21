library(shiny)
library(shinyjs)
library(leaflet)
source("leaftlet_nrc.r")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
    #get_leaflet_map(),
    leafletOutput("mymap")#,
    #p()#,
    #actionButton("recalc", "New points")
)

server <- function(input, output, session) {
    
    data <- read_viri_health_data()

    data$cases <-
    bind_cols (data$cases,
        st_coordinates(data$cases) %>%
        as_tibble() %>%
        rename(Long := X, Lat := Y)
        )



    data_circles <- data$cases %>% aggregate_points() %>% as_tibble()
    data_points <- data$cases %>% as_tibble() %>% select(Long, Lat, City)

    
    
    #%>% as.matrix()
    points <- eventReactive(input$recalc, {
        data_points
        #cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    #pal(data_circles$size)
    output$mymap <- renderLeaflet({
         get_leaflet_map() %>%
             addCircles(data = data_circles, radius = ~ Size , fillOpacity = 0.8, popup = ~ Popup, color = "#0F3" , fillColor  = ~ pal(Size) ) #%>%
             #addMarkers(data = data_points, popup = ~City)
    })
}

shinyApp(ui, server)
