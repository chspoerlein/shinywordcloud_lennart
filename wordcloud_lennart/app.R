
library(shiny)
library(tidyverse)
library(wordcloud)
library(viridis)

data <- readxl::read_excel("Woerter.xlsx")

data_new <- gather(data, month, wort, starts_with("Lenny_")) %>%
  mutate(mon=substr(month,7,8))
data_new2 <-gather(data, month, freq, starts_with("Freq_"))
data_new3 <-gather(data, month, deutsch, starts_with("Deutsch"))

# combine data sets
data_NEW <- as_data_frame(c(data_new[,c("mon","wort")],data_new2[,"freq"],data_new3[,"deutsch"]))  %>% 
  filter(wort!="NA") %>% 
  mutate(mon=as.numeric(mon)) %>% 
  mutate(realmon=NA,
         realmon=replace(realmon,mon==12,"September 2017"),
         realmon=replace(realmon, mon==13, "Oktober 2017"),
         realmon=replace(realmon, mon==14, "November 2017"),
         realmon=replace(realmon, mon==15, "Dezember 2017"),
         realmon=replace(realmon, mon==16, "Januar 2018"),
         realmon=replace(realmon, mon==17, "Februar 2018"),
         realmon=replace(realmon, mon==18, "März 2018"),
         realmon=replace(realmon, mon==19, "April 2018"),
         realmon=replace(realmon, mon==20, "Mai 2018"),
         realmon=replace(realmon, mon==21, "Juni 2018"),
         realmon=replace(realmon, mon==22, "Juli 2018"),
         realmon=replace(realmon, mon==23, "August 2018"),
         realmon=replace(realmon, mon==24, "September 2018")) %>%
  filter(mon<18)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Lennarts Wörter"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("month",
                  "Lebensmonat:",
                  min = 12,
                  max = 17,
                  value = 17),
      radioButtons("sprache:",
                   "Wähle eine Sprache:",
                   choices = c("Lennysprech"= "wort",
                               "Deutsch" = "deutsch"),
                   selected="wort"),
      br(),
      br(),
      textInput("suche", "Wortsuche:", value="Papa"),
      actionButton("los", "Suchen!")
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Wortentwicklung",
                 plotOutput("wordcloud", width = "100%", height = "600px"),
                 plotOutput("wordcount")
        ),
        tabPanel("Wortsuche",
                 br(),
                 textOutput("suchbegriff"),
                 br(),
                 tableOutput("woerterbuch")
        ),
        tabPanel("Bilder",
                 imageOutput("Lennpic", width = "60px",height = "600px"))
      )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$wordcloud <- renderPlot({
    # adding random noise to frequency variables
    set.seed(30091985)
    data_NEW$freq<- abs(data_NEW$freq+rnorm(n=nrow(data_NEW),mean=0,sd=1))
    data2 <- data_NEW %>% filter(mon==input$month) %>% rename(woerter=input$sprache)
    
    # draw the wordcloud
    wordcloud(data2$woerter, 
              freq=data2$freq, 
              min.freq=1, 
              random.order=FALSE, 
              rot.per=0, 
              #colors=brewer.pal(8,"RdYlBu"),
              colors=viridis(20),
              use.r.layout=TRUE)
    
  })
  
  output$wordcount <- renderPlot({
    # generate bins based on input$bins from ui.R
    data2 <- data_NEW %>% 
      group_by(mon) %>%
      count()
    # draw the wordcount
    ggplot(data=data2, aes(x=mon, y=n, label=n)) + 
      geom_line(group = 1, size=2, color="grey") + 
      geom_point(size=5) + 
      labs(x="Lebensmonat", y="kumulative Wortanzahl") + 
      theme_minimal() + 
      scale_shape_manual(values=16) +
      geom_line(data=data2 %>% filter(mon<=input$month), aes(x=mon, y=n), group=1, color="black", size=2) +
      geom_label() +
      theme(legend.position = "None", axis.text=element_text(size=15, face="bold"), axis.title = element_text(size=15, face="bold"))
    
  })
  
  output$suchbegriff <- renderText({
    mon_data <- 
      if (min(data_NEW$mon[which(data_NEW$deutsch==input$suche)])!=Inf){
        paste0(input$suche," sagt Lennart seit dem ",min(data_NEW$mon[which(data_NEW$deutsch==input$suche)]),". Lebensmonat.")
      } else {
        paste0("Dieses Wort kennt er entweder noch nicht oder du hast dich verschrieben!")
      }
  })
  
  observeEvent(input$los, {
    output$woerterbuch <- renderTable({
      if (min(data_NEW$mon[which(data_NEW$deutsch==input$suche)])!=Inf){
        worttable <- data.frame(Wort=input$suche, 
                                Lennysprech=data_NEW$wort[data_NEW$deutsch==input$suche][1],
                                Lebensmonat=as.character(min(data_NEW$mon[which(data_NEW$deutsch==input$suche)])),
                                Monat=data_NEW$realmon[data_NEW$mon==min(data_NEW$mon[which(data_NEW$deutsch==input$suche)])][1])     
        worttable
      }
    })
  })
  
  output$Lennpic <- renderImage({
    picname <- normalizePath(file.path(paste0(input$month,".jpg")))
    list(src=picname) #, width="60%", height="60%")
  }, deleteFile=FALSE
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

