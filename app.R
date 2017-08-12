# if (require("shiny")[1]==FALSE) install.packages("shiny")
# if (require("ggmap")[1]==TRUE) install.packages("ggmap")
# if (require("httr")[1]==FALSE) install.packages("httr")
# if (require("purrr")[1]==FALSE) install.packages("purrr")
# if (require("dplyr")[1]==FALSE) install.packages("dplyr")
# if (require("rvest")[1]==FALSE) install.packages("rvest")
# if (require("stringr")[1]==FALSE) install.packages("stringr")

library("shiny")
library("ggmap")
library("httr")
library("purrr")
library("dplyr")
library("rvest")
library("stringr")
library("mailR")




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
      mainPanel(plotOutput("showmap"), verbatimTextOutput("cityvalidation"))
        
   )),
tabPanel("Share",
         sidebarLayout(
           sidebarPanel(
             h5(strong("Download City Information to CSV")),
             downloadButton('downloadData', 'Download'),
             br(),br(),
             textInput("emailaddress", "What is your email address?", "dusty.s.turner@gmail.com"),
             actionButton("Submit", "Submit"),
             radioButtons("savestyle", "Save to location:",
                                c("local" = "downloads",
                                  "network" = "Userdirs")),
             conditionalPanel(
               condition = "input.savestyle == 'downloads'",
               textInput("first", "firstname", "dusty"),
               textInput("last", "lastname", "turner"),
               actionButton("Sendemaila", "Send Email")
               ),
             conditionalPanel(
               condition = "input.savestyle == 'Userdirs'",
              textInput("dirslocation", "USERDIRS Location", "Turner"),
              actionButton("Sendemailb", "Send Email")
               ),
             textInput("subject","subject", "Class Email")
             # textInput("delete", "delete which", "number")

),
          
           # Show a plot of the generated distribution
           mainPanel(tableOutput("emaillist"))
      ))

)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # get lat long
  values <- reactiveValues()
  emails = reactiveValues()
  
  emails$df = data.frame(emailvector = numeric(0))
  values$df <- data.frame(City = numeric(0), State = numeric(0), pop = numeric(0), lat = numeric(0), long = numeric(0))

  newEntry <- observeEvent(input$Search,{
      cleanedcity = gsub(" ", "%20", input$city)
      cleanedstate = input$state
      cleanedstate = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(cleanedstate), perl=TRUE)
      test = paste("http://nominatim.openstreetmap.org/search?city=", cleanedcity,
                   "&countrycodes=US&limit=9&format=json", sep="")
      r <- GET(test)
      mysearch = content(r)
      lat = map_chr(mysearch, "lat")
      long = map_chr(mysearch, "lon")
      state = map_chr(mysearch, "display_name")
      id = map_chr(mysearch, "place_id")
      citydf = data.frame(lat = as.numeric(as.character(lat)), long = as.numeric(as.character(long)), state = state, id = id)
      thiscity = citydf[grep(cleanedstate, citydf$state), ]
      
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
      # newLine <- isolate(c(input$city, input$state, pop))
      cleanedcity2 = input$city
      cleanedcity2 = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(cleanedcity2), perl=TRUE)
      isolate(values$df[nrow(values$df) + 1,] <- c(cleanedcity2, cleanedstate, pop, lat = lat1, long = long1))
      # isolate(string$df[1] = c("0"))
        } else { 
      # isolate(string$df="0")
      } }
      
  })
  
  tablevalues = reactive({
  newdf = values$df 
  newdf = newdf %>% group_by(newdf$lat) %>% filter(row_number() == 1)
  #newdf = newdf[is.factor(newdf$pop),]
  newdf$Population = as.integer(as.character(newdf$pop))
  newdf = arrange(newdf, Population)
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
      tablevalues()[c(1,2,7)]
    })
      })
  
    sendemailto = observeEvent(input$Submit, {
      isolate(emails$df[nrow(emails$df) + 1,] <- c(emailvector = input$emailaddress))
      # isolate(emails$df$emailvector = emails$df$emailvector[-input$delete,])
      # colnames(emails$df) = c("Cadet_Emails")
      return(emails$df)
    })
  
    observeEvent(input$Submit, {
      output$emaillist <- renderTable({
        emails$df
        showitdf = emails$df
        colnames(showitdf) = c("Cadet Emails")
        return(showitdf)
      })
    })
    
    observeEvent(input$Sendemaila, {
      wd = paste("C:/Users/", input$first, ".", input$last, "/Downloads", sep = "")
      setwd(wd)
      for(i in 1:length(emails$df$emailvector)){
        send.mail(from="dusty.turner@usma.edu",
                  to=as.character(emails$df$emailvector[i]),
                  subject = input$subject,
                  body = "This is and email from CPT Dusty Turner",
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "dusty.s.turner", passwd = "stewardesses", ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE,
                  attach.files = "Class Populations.csv",
                  debug = FALSE)
      }
    })
    
    observeEvent(input$Sendemailb, {
      wd = paste("//usmaedu/apollo/math/Userdirs/", input$dirslocation, sep = "")
      setwd(wd)
      for(i in 1:length(emails$df$emailvector)){
        send.mail(from="dusty.turner@usma.edu",
                  to=as.character(emails$df$emailvector[i]),
                  subject = input$subject,
                  body = "This is and email from CPT Dusty Turner",
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "dusty.s.turner", passwd = "stewardesses", ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE,
                  attach.files = "Class Populations.csv",
                  debug = FALSE)
      }
    })

  

  output$popavg = renderPrint({
   cat("The average population size is:", popaverage())
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Class Populations.csv", sep = "_")
    },
    content = function(file) {
      write.csv(tablevalues()[c(1,2,3,4,5,7)], file)
    },
    contentType = "csv"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server", options = list(launch.browser = TRUE))
