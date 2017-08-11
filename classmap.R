library(shiny)
# setwd("C:/Users/dusty.turner/Desktop/R Work/mappractice/maps/")
# source("data/helpers.R")
library(ggmap)
library(httr)
library(purrr)
library(dplyr)
library(rvest)
library(stringr)

# refrence for tabs
## https://shiny.rstudio.com/gallery/navbar-example.html

map <- get_map(location = 'united states', zoom = 4,
                     # scale = "auto",
                     maptype = "terrain", source = "google", color = c("color"))

# Define UI for application 
ui <- navbarPage("Map Search",
   tabPanel("Maps",
#   titlePanel("Maps"),
   sidebarLayout(
      sidebarPanel(
        
          textInput("city", "Type in a City", "Kerrville"),
          textInput("state","Type in a State", "Texas"),
          actionButton("Search", "Search"),
          verbatimTextOutput("popavg"),
          tableOutput("citytable")),
        
      
      
      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(
        tabPanel("Main Panel", plotOutput("showmap"), verbatimTextOutput("cityvalidation"))
        ))
   )),
tabPanel("Download",downloadButton('downloadData', 'Download'))
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # get lat long
  values <- reactiveValues()
  string = reactiveValues()

  string$df = data.frame(helper = numeric(0))
  values$df <- data.frame(City = numeric(0), State = numeric(0), pop = numeric(0), lat = numeric(0), long = numeric(0))

  newEntry <- observeEvent(input$Search,{
      cleanedcity = gsub(" ", "", input$city)
      test = paste("http://nominatim.openstreetmap.org/search?city=", cleanedcity,
                   "&countrycodes=US&limit=9&format=json", sep="")
      r <- GET(test)
      mysearch = content(r)
      lat = map_chr(mysearch, "lat")
      long = map_chr(mysearch, "lon")
      state = map_chr(mysearch, "display_name")
      id = map_chr(mysearch, "place_id")
      citydf = data.frame(lat = as.numeric(as.character(lat)), long = as.numeric(as.character(long)), state = state, id = id)
      thiscity = citydf[grep(input$state, citydf$state), ]
      
      if(is.na(as.numeric(thiscity[1,4]))==FALSE) {
        thiscity = thiscity[1,]
        lat1 = thiscity$lat
        long1 = thiscity$long
        a <- paste(
            "http://nominatim.openstreetmap.org/details.php?place_id="
        , as.character(thiscity$id)
        , sep="")
        scrape <- a %>%
          read_html() %>%
          html_nodes(css = "tr+ tr .line") %>%
          html_text()
        if(length(scrape >= 2)) {
          for (i in 1:length(scrape)) {
            a = str_detect(scrape[i], "population")
            if (a == TRUE) {
              break()
            }
          }
      pop= gsub("\\D","",scrape[i])
      newLine <- isolate(c(input$city, input$state, pop))
      isolate(values$df[nrow(values$df) + 1,] <- c(input$city, input$state, pop, lat = lat1, long = long1))
      # isolate(string$df[1] = c("0"))
        } else { 
      # isolate(string$df="0")
      } }
      
  })
  
  tablevalues = reactive({
  newdf = values$df 
  newdf = newdf %>% group_by(newdf$lat) %>% filter(row_number() == 1)
  newdf = arrange(newdf, pop)
  #newdf = newdf[is.factor(newdf$pop),]
  newdf$Population = as.numeric(as.character(newdf$pop))
  return(newdf)
    })

  popaverage = reactive({
    popvec = as.numeric(as.character(tablevalues()$pop)) 
    meanpop=mean(popvec)
    if(is.na(meanpop)==TRUE){
      return("")
    }else{
      return(meanpop)
    }
  })
  ##############

  observeEvent(input$Search, {
    output$showmap = renderPlot({
      ggmap(map) +
        geom_point(data = tablevalues(),
          aes(x = as.numeric(long), y = as.numeric(lat), colour = 1/Population, alpha = 10, size = 5),
          shape = 20 ) +
        # scale_colour_gradient(low = "white") 
        theme(legend.position="none") 
        #guides(fill = FALSE,alpha = TRUE,size = FALSE)
    }, height = 700, width = 1000)
    
    output$citytable <- renderTable({
      tablevalues()[1:3]
    })
      })

  output$popavg = renderPrint({
   cat("The average population size is:", popaverage())
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Class Populations", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(tablevalues()[c(1,2,3,4,5,7)], file)
    },
    contentType = "csv"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")

