if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

#Importing required data
Covid <- read_csv("https://pkgstore.datahub.io/core/covid-19/time-series-19-covid-combined_csv/data/35e416b7d9776765041e852fe62dbf9a/time-series-19-covid-combined_csv.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
dat<-html_table(html_nodes(read_html("https://www.worldometers.info/coronavirus/"), "table")[[1]])[,-c(4,6,8,10:12,14,16:19)]%>% slice(9:223)
code<-read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")[,-2]

#Data processing
for (n in 3:8) {
    dat[,n]<-as.numeric(gsub(",","",dat[,n]))
}

fin_data<-merge(dat,code,by.x ="Country,Other",by.y ="COUNTRY",all.x = TRUE )

fin_data<-fin_data[,-2]

fin_data[207,8]<-"USA"
fin_data[204,8]<-"GBR"
fin_data[202,8]<-"ARE"
fin_data[164,8]<-"KOR"
fin_data[52,8]<-"CZE"
fin_data[58,8]<-"COD"
fin_data[147,8]<-"MKD"
fin_data[14,8]<-"BHM"
fin_data[46,8]<-"COG"
fin_data[67,8]<-"FRO"
fin_data[68,8]<-"FLK"
fin_data[75,8]<-"GMB"
fin_data[101,8]<-"CIV"
fin_data[118,8]<-"MAC"
fin_data[138,8]<-"MMR"
fin_data[37,8]<-"CAF"

#Scraping some html text off https://www.worldometers.info/coronavirus/
names(fin_data)<-str_replace_all(names(fin_data), c(" " = "." , "," = "." ))

scraping_wiki <- read_html("https://www.worldometers.info/coronavirus/")

scraping_wiki %>%
    html_nodes("h1")

scraping_wiki %>%
    html_nodes("h1") %>%
    html_text()

p_nodes <- scraping_wiki %>% 
    html_nodes("p")

body_text <- scraping_wiki %>%
    html_nodes("#maincounter-wrap") %>% 
    html_text()

substr(body_text, start = 1, stop = 207)

body_text[1]<-gsub("\n","",body_text[1])
body_text[2]<-gsub("\n","",body_text[2])
body_text[3]<-gsub("\n","",body_text[3])

date<-Sys.Date()


# Define UI for application 
ui <- fluidPage(
    
    fluidRow(
        column(12,
               plotlyOutput("distPlot",width="100%", height="100%"),
               fluidRow(
                   column(6,
                          htmlOutput("desc"),
                          plotlyOutput("bar"),
                   ),
                   
                   column(width = 6,
                          selectInput(inputId = "type", label = strong("Select the country"),
                                      choices = unique(Covid$`Country/Region`),
                                      selected = "India"),
                          plotlyOutput("line"))
               )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    
    #The interactive Worldmap of COVID19 cases.
    output$distPlot <- renderPlotly({ 
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)
        
        #hover text
        fin_data$hover <- with(fin_data, paste(Country.Other,'<br>',"Population:           ", comma(Population,digits=0),'<br>',"Confirmed cases: ", comma(TotalCases,digits = 0),"<br>","Recovered:           ", comma(TotalRecovered,digits=0)))
        
        # specify map projection/options
        g <- list(
            showframe = FALSE,
            showcoastlines = FALSE,
            projection = list(type = 'Mercator')
        )
        
        fig <- plot_geo(fin_data)
        fig <- fig %>% add_trace(
            z = ~log10(TotalCases), color = ~log10(TotalCases), colors = 'Reds',
            text = ~hover, locations = ~CODE, marker = list(line = l),hoverinfo="text"
        )
        fig <- fig %>% colorbar(title = paste('Total Confirmed','<br>','cases'),tickvals=c(0,2,4,6),ticktext=c(0,100,"10k","1M"),thickness=10)
        fig <- fig %>% layout(
            title = 'Covid-19 Worldwide Impact<br>Source:<a href="https://www.worldometers.info/coronavirus/">Worldmeter</a>',
            
            geo = g,plot_bgcolor='transparent',paper_bgcolor='transparent'
        )
        
    })
    
    #The text showing today's COVID19 statistics.
    output$desc <- renderText({
        paste("as on",date,"<br>","<b>",body_text[1],"<br>",body_text[2],"<br>",body_text[3],"</b>")
    })
    
    #The reactive line plot.
    output$line<-renderPlotly({
        type <- input$type
        tmp<-Covid[Covid$`Country/Region`==type,]
        fig1 <- plot_ly(data=tmp, x = ~Date, y = ~Confirmed, name = 'Confirmed Cases', type = 'scatter', mode = 'lines',
                        line = list(color = 'rgb(205, 12, 24)', width = 4)) 
        fig1 <- fig1 %>% add_trace(y = ~Recovered, name = 'Recovered', line = list(color = 'rgb(0, 128, 0)', width = 4)) 
        fig1 <- fig1 %>% add_trace(y = ~Deaths, name = 'Deaths', line = list(color = 'rgb(0,0,0)', width = 4))
        fig1<-fig1 %>% layout(title=paste('Timeline of COVID-19 Cases in',type),yaxis=list(title="Cases"),xaxis=list(title=""),legend=list(x=0.1,y=0.9),plot_bgcolor='transparent',paper_bgcolor='transparent')
        
    })
    
    # The Interactive Bar plot.
    output$bar<-renderPlotly({
        tmp1<-group_by(Covid,`Country/Region`)
        tmp2<-summarise(tmp1,cases=max(Confirmed))
        tmp2<-arrange(tmp2,desc(cases))
        tmp2<-tmp2[1:20,]
        
        fig25<-plot_ly(data=tmp2,y=~reorder(`Country/Region`,cases),x=~cases,type = "bar",orientation='h',marker = list(color = 'rgb(153,0,0)',line = list(color = 'rgb(0,0,0)', width = 1.5)))
        fig25<-fig25%>%layout(yaxis=list(title=""),xaxis=list(title="Total confirmed cases"),plot_bgcolor='transparent',paper_bgcolor='transparent')
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)