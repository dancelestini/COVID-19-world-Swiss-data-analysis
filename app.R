
## load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(grid)

# load all data sets
dfs = as.data.frame(data.table::fread("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland_openzh.csv"))
dfs1 = as.data.frame(data.table::fread("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_fatalities_switzerland_openzh.csv"))
dfs2 = as.data.frame(data.table::fread("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_hospitalized_switzerland_openzh.csv"))
dfs3 = as.data.frame(data.table::fread("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_tested_switzerland_openzh.csv"))
test = as.data.frame(data.table::fread("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_tests_switzerland_bag.csv"))
dfc1 = read.csv("data/COVID_data_2020-10-28.csv")

# start modifing datasets 
dfc1$date = as.Date(dfc1$date, format = "%Y-%m-%d")
min_date = as.Date(min(dfc1$date), "%Y-%m-%d")
max_date = as.Date(max(dfc1$date), "%Y-%m-%d")

dfs$Date = as.Date(dfs$Date, format = "%Y-%m-%d")
min_date2 = as.Date(min(dfs$Date), "%Y-%m-%d")
max_date2 = as.Date(max(dfs$Date), "%Y-%m-%d")

dfs1$Date = as.Date(dfs1$Date)
dfs2$Date = as.Date(dfs2$Date)
dfs3$Date = as.Date(dfs3$Date)
test$date = as.Date(test$date)

dfc2 = dfc1[dfc1$country == "Switzerland",]
dfs4 = merge(test[,1:5],dfc2[,1:5], by = "date")

cv_today = subset(dfc1, date == "2020-06-15")
cv_reduced = subset(cv_today, cases >=1000)

update = as.Date(max(dfs$Date))

# Plot functions

cumulative_plots = function(mindate,outcom,event,countrys){
    start_date = as.Date(mindate) 
    db = dfc1 
    db$region = db$country
    db = db %>% filter(region %in% countrys)
    outcomes = switch(outcom,
                      "Cases" = db$cases,
                      "Deaths" = db$deaths,
                      "Cases per 100'000" = db$per100k,
                      "Deaths per 100'000" = db$deathsper100k)

    p = ggplot(data = db, aes(x =date,y = outcomes,colour = region, group = 1,))+
        geom_line()+geom_point(size = 1, alpha = 0.8)+
        labs(y="Cumulative cases", x = "Date")+
        theme(axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 14),
              legend.title = element_blank())+
        scale_x_date(limits = c(start_date,max_date2))
    
    events = event 
    # if (outcom == "Cases per 100'000"){
    #     start_date = as.Date("2020-01-01") 
    #     p = ggplot(data = db, aes(x =date,y = outcomes,colour = region, group = 1,))+
    #         geom_line()+geom_point(size = 1, alpha = 0.8)+
    #         geom_line(data=infl,aes(date,mean, color = "Influenza"))+
    #         labs(y="Cumulative", x = "Date")+
    #         theme(axis.title = element_text(face = "bold"))+
    #         scale_x_date(limits = c(start_date,max_date2))
    # }
    for (i in events){
        dates = switch (i,
                        "Start confinement" = as.numeric(as.Date("2020-03-15")),
                        "Second end confinement" = as.numeric(as.Date("2020-05-11")),
                        "First end confinement" = as.numeric(as.Date("2020-04-27")),
                        "Second start confinement" = as.numeric(as.Date("2020-11-02")))
        
        p = p + geom_vline(xintercept = dates, color = "red", linetype = "dotted", size = 0.5)
    }
    ggplotly(p) %>%
        layout(legend = list(orientation = "v", x = 1, y =0.5))
}

new_plots = function(mindate,outcom,event,countrys){
    start_date = as.Date(mindate) 
    db = dfc1 
    db$region = db$country
    db = db %>% filter(region %in% countrys)
    outcomes = switch(outcom,
                      "Cases" = db$new_cases,
                      "Deaths" = db$new_deaths,
                      "Cases per 100'000" = db$newper100k,
                      "Deaths per 100'000" = db$newdeathsper100k)
    
    p = ggplot(data = db, aes(x =date,y = outcomes,fill = region))+
        geom_bar(position="stack", stat="identity")+
        labs(y="Cumulative cases", x = "Date")+
        theme(axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 14),
              legend.title = element_blank())+
        scale_x_date(limits = c(start_date,max_date2))

    
    events = event 
    
    for (i in events){
        dates = switch (i,
                        "Start confinement" = as.numeric(as.Date("2020-03-15")),
                        "Second end confinement" = as.numeric(as.Date("2020-05-11")),
                        "First end confinement" = as.numeric(as.Date("2020-04-27")),
                        "Second start confinement" = as.numeric(as.Date("2020-11-02")))
        
        p = p + geom_vline(xintercept = dates, color = "red", linetype = "dotted", size = 0.5)
    }
    ggplotly(p) %>%
        layout(legend = list(orientation = "v", x = 1, y =0.5))
}

log_plots = function(mindate,outcom,event,countrys){
    start_date = as.Date(mindate) 
    db = dfc1 
    db$region = db$country
    db = db %>% filter(region %in% countrys)
    outcomes = switch(outcom,
                      "Cases" = db$cases,
                      "Deaths" = db$deaths,
                      "Cases per 100'000" = db$per100k,
                      "Deaths per 100'000" = db$deathsper100k)
    
    p = ggplot(data = db, aes(x =date,y = outcomes,colour = region, group = 1,))+
        geom_line()+geom_point(size = 1, alpha = 0.8)+
        labs(y="Cumulative cases", x = "Date")+
        theme(axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 14),
              legend.title = element_blank())+
        scale_x_date(limits = c(start_date,max_date2))+
        scale_y_continuous(trans = "log10")
    
    events = event 
    
    for (i in events){
        dates = switch (i,
                        "Start confinement" = as.numeric(as.Date("2020-03-15")),
                        "Second end confinement" = as.numeric(as.Date("2020-05-11")),
                        "First end confinement" = as.numeric(as.Date("2020-04-27")),
                        "Second start confinement" = as.numeric(as.Date("2020-11-02")))
        
        p = p + geom_vline(xintercept = dates, color = "red", linetype = "dotted", size = 0.5)
    }
    ggplotly(p) %>%
        layout(legend = list(orientation = "v", x = 1, y =0.5))
}

cumulative_plots2 = function(mindate,outcom,event,canton){
    start_date = as.Date(mindate) 
    outcomes = switch(outcom,
                      "Cases" = dfs, 
                      "Deaths" = dfs1,
                      "Hospitalized" = dfs2)
    
        db = outcomes %>% select(Date:CH)%>%
            pivot_longer(.,cols = c(AG:CH),names_to = "Canton",values_to = "Cases") 
        db$region = db$Canton
        db$cases = db$Cases
        db = db %>% filter(region %in% canton)
        
        p = ggplot(data = db, aes(x =Date,y = cases,colour = region, group = 1,))+
            geom_line()+
            labs(y="Cumulative cases", x = "Date")+
            theme(axis.title = element_text(face = "bold"),
                  legend.text = element_text(size = 14),
                  legend.title = element_blank())+
            scale_x_date(limits = c(start_date,max_date2))
    
    events = event 
    
    for (i in events){
        dates = switch (i,
                        "Start confinement" = as.numeric(as.Date("2020-03-15")),
                        "Second end confinement" = as.numeric(as.Date("2020-05-11")),
                        "First end confinement" = as.numeric(as.Date("2020-04-27")),
                        "Second start confinement*" = as.numeric(as.Date("2020-11-02")))
        
        p = p + geom_vline(xintercept = as.numeric(dates), color = "red", linetype = "dotted", size = 0.5)
            #+annotate("text",x = dates,y = -1000,label = "name",color = "black",size = 5)
        
        
    }
    h = 7.1
    if (outcom == "Hospitalized"){
        p = p+ geom_hline(yintercept = 160, color = "black", linetype = "dotted", size = 0.5)+
            annotate("text",label = "Hospital limit",x = as.Date("2020-06-20", "%Y-%m-%d"),y = 170,color = "black",size = 5)
    }
    
    ggplotly(p) %>%
        layout(legend = list(orientation = "v", x = 1, y =0.5))
}

swiss_global2 = function(mindate,outcom,event){
    start_date = as.Date(mindate) 
    outcomes = switch(outcom,
                      "Cases" = dfs, 
                      "Deaths" = dfs1,
                      "Hospitalized" = dfs2)
    
    db = outcomes %>% select(Date:CH)%>%
        pivot_longer(.,cols = c(CH),names_to = "Canton",values_to = "Cases") 
    db$region = db$Canton
    db$cases = db$Cases
    
    p = ggplot(data = db, aes(x =Date,y = cases,colour = region, group = 1,))+
        geom_line(size = 1.2)+
        labs(y="Cumulative", x = "Date")+
        theme(axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 14),
              legend.title = element_blank())+
        scale_x_date(limits = c(start_date,max_date2))
    
    events = event 
    
    for (i in events){
        dates = switch (i,
                        "Start confinement" = as.numeric(as.Date("2020-03-15")),
                        "Second end confinement" = as.numeric(as.Date("2020-05-11")),
                        "First end confinement" = as.numeric(as.Date("2020-04-27")),
                        "Second start confinement*" = as.numeric(as.Date("2020-11-02")))
        
        p = p + geom_vline(xintercept = as.numeric(dates), color = "red", linetype = "dotted", size = 0.5)
        #+annotate("text",x = dates,y = -1000,label = "name",color = "black",size = 5)
        
        
    }
    if (outcom == "Cases"){
        colors = c("New tests per day" = "Darkred","New cases per day" = "Darkblue")
        p = ggplot(data = dfs4, aes(x =date))+
            geom_line(aes(y = new_cases,color = "New cases per day"),size = 1)+
            geom_line(aes(y = n_tests, color = "New tests per day"),size = 1)+
            labs(y="Cumulative", x = "Date")+
            theme(axis.title = element_text(face = "bold"),
                  legend.text = element_text(size = 14),
                  legend.title = element_blank())+
            labs(color = "Legend")+
            scale_x_date(limits = c(start_date,max_date2))
    }
    h = 7.1
    if (outcom == "Hospitalized"){
        p = p+ geom_hline(yintercept = 1400, color = "black", linetype = "dotted", size = 0.5)+
            annotate("text",label = "Hospital limit",x = as.Date("2020-06-20", "%Y-%m-%d"),y = 1490,color = "black",size = 5)
    }
    
    ggplotly(p) %>%
        layout(legend = list(orientation = "v", x = 1, y =0.5))
}


log_plots2 = function(mindate,outcom,event,canton){
    start_date = as.Date(mindate) 
    outcomes = switch(outcom,
                      "Cases" = dfs, 
                      "Deaths" = dfs1,
                      "Hospitalized" = dfs2)
    
    db = outcomes %>% select(Date:CH)%>%
        pivot_longer(.,cols = c(AG:ZH),names_to = "Canton",values_to = "Cases") 
    db$region = db$Canton
    db$cases = db$Cases
    db = db %>% filter(region %in% canton)
    
    p = ggplot(data = db, aes(x =Date,y = cases,colour = region, group = 1,))+
        geom_line()+
        labs(y="Cumulative", x = "Date")+
        theme(axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 14),
              legend.title = element_blank())+
        scale_y_log10()+
        scale_x_date(limits = c(start_date,max_date2))
    
    events = event 
    
    for (i in events){
        dates = switch (i,
                        "Start confinement" = as.numeric(as.Date("2020-03-15")),
                        "Second end confinement" = as.numeric(as.Date("2020-05-11")),
                        "First end confinement" = as.numeric(as.Date("2020-04-27")),
                        "Second start confinement*" = as.numeric(as.Date("2020-11-02")))
        
        p = p + geom_vline(xintercept = as.numeric(dates), color = "red", linetype = "dotted", size = 0.5)
        #+annotate("text",x = dates,y = -1000,label = "name",color = "black",size = 5)
        
        
    }
    h = 7.1
    if (outcom == "Hospitalized"){
        p = p+ geom_hline(yintercept = 160, color = "black", linetype = "dotted", size = 0.5)+
            annotate("text",label = "Hospital limit",x = as.Date("2020-06-20", "%Y-%m-%d"),y = 170,color = "black",size = 5)
    }
    
    ggplotly(p) %>%
        layout(legend = list(orientation = "v", x = 1, y =0.5))
}



## UI 
ui <- bootstrapPage(
    navbarPage(theme = shinytheme("flatly"),collapsible = TRUE,"Swiss COVID-19 data analysis",id="nav",
               
               tabPanel("Global plots",
                        sidebarLayout(
                            sidebarPanel(
                                span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between country")), style="color:#045a8d"),
                                
                                pickerInput("outcome_select", "Outcome:",
                                            choices = c("Cases","Deaths","Cases per 100'000","Deaths per 100'000"),
                                            selected = c("Cases per 100'000"),
                                            multiple = FALSE),
                                pickerInput("event_select", "Events:",
                                            choices = c("Start confinement","First end confinement","Second end confinement","Second start confinement"),
                                            selected = c("Start confinement","Second start confinement"),
                                            multiple = TRUE),
                                pickerInput("country", "Country:",
                                            choices = as.character(cv_reduced[order(-cv_reduced$cases),]$country),
                                            selected =c(as.character(cv_reduced[order(-cv_reduced$cases),]$country)[1:7],"Switzerland"),
                                            options = list(`actions-box` = TRUE,`none-selected-text` = "Please make a selection!", `live-search` = TRUE),
                                            multiple = TRUE),
                                sliderInput("minimun_date","Minimun date:",
                                            min = as.Date(min_date,"%Y-%m-%d"),
                                            max = as.Date(max_date,"%Y-%m-%d"),
                                            value = as.Date(min_date),
                                            timeFormat = "%d %b"),
                                "Select the Events, Outcome and plottings start dates to modify the plot. All the datasets used here are avaible from the OSFP."
                                
                                
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Cumulative",plotlyOutput("plotly_cumulative",height = "600px")),
                                    tabPanel("New cases",plotlyOutput("plotly_new",height = "600px")),
                                    tabPanel("Cumulative (log10)",plotlyOutput("plotly_log",height = "600px"))
                                )
                            )
                            
                        )
                    ),
               
               
               
               
               tabPanel("Canton plots",
                        sidebarLayout(
                            sidebarPanel(
                                span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between cantons")), style="color:#045a8d"),
                                         
                                         pickerInput("outcome_select2", "Outcome:",
                                                     choices = c("Cases","Deaths","Hospitalized"),
                                                     selected = c("Cases"),
                                                     multiple = FALSE),
                                         pickerInput("event_select2", "Events:",
                                                     choices = c("Start confinement","First end confinement","Second end confinement","Second start confinement*"),
                                                     selected = c("Start confinement","Second start confinement*"),
                                                     multiple = TRUE),
                                        pickerInput("canton", "Cantons:",
                                                    choices = c("AG","AI","AR","BE","BL","BS","FR","GE","GL","JU","LU","NE","NW","OW","SG","SH","SO","SZ","TG","TI","UR","VD","VS","ZG","ZH","CH"),
                                                    selected =c("AG","AI","AR","BE","BL","BS","FR","GE","GL","JU","LU","NE","NW","OW","SG","SH","SO","SZ","TG","TI","UR","VD","VS","ZG","ZH"),
                                                    options = list(`actions-box` = TRUE,`none-selected-text` = "Please make a selection!", `live-search` = TRUE),
                                                    multiple = TRUE),
                                         sliderInput("minimun_date2","Minimun date:",
                                                     min = as.Date(min_date2,"%Y-%m-%d"),
                                                     max = as.Date(max_date2,"%Y-%m-%d"),
                                                     value = as.Date(min_date2),
                                                     timeFormat = "%d %b"),
                                         "Select the Events, Outcome and plottings start dates to modify the plot. All the datasets used here are avaible from the OSFP.",
                                            tags$br(),
                                        tags$i(h6("* The second lockdown in Switzerland was less restrictive than the first one but we can also analyse the effect of this one."))
                                        
                                         
                                         
                                         
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Swiss global",plotlyOutput("swiss_global",height = "600px") ),
                                    tabPanel("Cumulative",plotlyOutput("plotly_cumulative2",height = "600px")),
                                    tabPanel("Cumulative (log10)",plotlyOutput("plotly_log2",height = "600px"))
    
                                )
                            )
                            
                        )
                    ),
               tabPanel("Data",
                        pickerInput("datasetchoice", "Dataset : ",
                                    choices = c("World data","Swiss data"),
                                    selected = c("World data"),
                                    multiple = FALSE),
                        verbatimTextOutput("dataset")),
               
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            "This app still in developpement and all the analysis are not showned already. Some more updates will comes in the next days.",tags$br(),
                            "This site has been developped by Dan Celestini. The aim of this app was to compare some COVID-19 data all over the World especificly in Switzerland but also to improve my
                            codings skills.",tags$br(), 
                            "I am a student in Biology at the University of Lausanne and try to achieve a Master in Bioinformatics. This is why this app should help me a lot for my futures projects.",tags$br(),
                            "This page couldn't be done without the helpful shiny app template `COVID-19 tracker`.",tags$br(),
                            tags$a(href="https://vac-lshtm.shinyapps.io/ncov_tracker/?_ga=2.248204743.1561579974.1600520620-2025056928.1600263463","COVID-19 tracker"),tags$br(),
                            
                            tags$br(),tags$h4("Data analysis"),
                            "All over the app you could see some plots with data about the COVID-19 outbreak. The most basic analysis done as been comparing the number of total cases or new cases between country or cantons.
                            Therefore, I also want to go a little but further in the analysis by adding on the plots some keys date in Switzerland. The start confinement date correspond to when the Swiss politicians decided to 
                            close all markets said not essential to the life. The first end confinement was the date where the first not essential markets start to open again. The second end of the confinement was when almost all
                            markets, schools, places where open and without to much restriction. The second lockdown start is based on the date of the French second lockdown. In Switzerland this last one is much less restrictive than the first one 
                            but we will consider it also to see the effect on the curve.",tags$br(),tags$br(),
                            
                            "For conclude, we can observe that the Swiss governement was prety fast in decision taking. Despite we don't knows really the effects of this rapidity in facts. We can also now observe a quite important
                            recovery of the numbers of cases in all the places where the confinement and the stricts mesures as been lifted. Thats may could explain why the world almost stop for 1 months. Therefore the cases is increasing 
                            in the last days, the numbers of deaths is not increasing at the same speed. My explanation for this is maybe now the people that became contamined by the virus is mostly young people and by that we know that the 
                            mortality rate on this category of person is quite low.",tags$br(),tags$br(),
                            
                            tags$h4("Code"),
                            "All the code and the data used to create this Shiny app are available on ",tags$a(href="https://github.com/dancelestini/COVID-19-world-Swiss-data-analysis","Github"),tags$br(),tags$br(),
                
                            tags$h4("Sources"),
                            tags$b("World COVID-19 cases data: "),tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "COVID-19 time series data"),tags$br(),
                            tags$b("SwissCOVID-19 cases data: "),tags$a(href="https://www.corona-data.ch", "OFSP COVID-19 datasets"),tags$br(),
                            tags$b("Shiny app Template design: "),tags$a(href="https://vac-lshtm.shinyapps.io/ncov_tracker/?_ga=2.248204743.1561579974.1600520620-2025056928.1600263463","COVID-19 tracker"),tags$br(),tags$br(),
                            
                            tags$h4("Contact"),
                            "dan.celestini@unil.ch"
                            
                            
                        ))
               
               
        )
    )






server  = function(input,output){
    #plot global data
    output$plotly_cumulative = renderPlotly({
        cumulative_plots(input$minimun_date, input$outcome_select,input$event_select,input$country)
    })
    
    output$plotly_new = renderPlotly({
        new_plots(input$minimun_date, input$outcome_select,input$event_select,input$country)
    })
    
    output$plotly_log = renderPlotly({
        log_plots(input$minimun_date, input$outcome_select,input$event_select,input$country)
    })
    
    #plot Swiss data
    
    output$swiss_global = renderPlotly({
        swiss_global2(input$minimun_date2, input$outcome_select2,input$event_select2)
    })
    
    output$plotly_cumulative2 = renderPlotly({
        cumulative_plots2(input$minimun_date2, input$outcome_select2,input$event_select2,input$canton)
    })
    
    output$plotly_log2 = renderPlotly({
        log_plots2(input$minimun_date2, input$outcome_select2,input$event_select2,input$canton)
    })
    
    #dataset shows
    
    output$dataset = renderPrint({
        orig = options(width = 1000)
        data = switch(input$datasetchoice,
                      "World data" = dfc1,
                      "Swiss data" = dfs[,1:28])
        
        print(tail(data,n=20))
        options(orig)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
