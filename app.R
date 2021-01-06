
library(shiny)
library(stringr)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)


source("R/helpers.R")
#source("R/modules.R")
target_dir="data/"


if (!file.exists(paste0("data/","imdb_data_basic.rds"))){
  update_imdb_data()
}


aux_radio1_choices=list("all","votecount",
                        "rating")
names(aux_radio1_choices)=c("All Matches","Only highest Vote Count",
                            "Only highest rating")

aux_radio2_choices=list("exact","contains")
names(aux_radio2_choices)=c("Exact Match","Contains")

aux_radio3_choices=list("movie","short", "tvMiniSeries","tvMovie","tvSeries","tvShort","tvSpecial","video","videoGame")
names(aux_radio3_choices)=c("Full Movie","Short Movie","TV MiniSeries", "TV Movie","TV Series",
                            "TV Short","TV Special","Video","VideoGame")

aux_radio4_choices=list("Action"  ,    "Adult"   ,    "Adventure" ,  "Animation",   "Biography"  , "Comedy"  ,    "Crime" ,      "Documentary",
                         "Drama"  ,     "Family"  ,    "Fantasy" ,    "Film-Noir" ,  "Game-Show" ,  "History" ,    "Horror",      "Music"  ,    
                        "Musical"   ,  "Mystery"  ,   "News"   ,     "Reality-TV" , "Romance" ,    "Sci-Fi"  ,    "Short"  ,     "Sport"  ,    
                        "Talk-Show"  , "Thriller"  ,  "War"  ,       "Western" )
names(aux_radio4_choices)=c("Action"  ,    "Adult"   ,    "Adventure" ,  "Animation",   "Biography"  , "Comedy"  ,    "Crime" ,      "Documentary",
                            "Drama"  ,     "Family"  ,    "Fantasy" ,    "Film-Noir" ,  "Game-Show" ,  "History" ,    "Horror",      "Music"  ,    
                            "Musical"   ,  "Mystery"  ,   "News"   ,     "Reality-TV" , "Romance" ,    "Sci-Fi"  ,    "Short"  ,     "Sport"  ,    
                            "Talk-Show"  , "Thriller"  ,  "War"  ,       "Western")


ui <- navbarPage("ImDb Application",
                 
                 useShinydashboard(),
                 
                     tabPanel("General",
                              fluidRow(
                                valueBoxOutput("num_records"),
                                valueBoxOutput("data_stamp"),
                                column(3,actionButton(inputId = "update_status","Update Database",
                                                      icon=icon("cloud-download-alt")),
                                       textOutput(outputId = "download_msg"))
                              )),
                     tabPanel("Check Ratings",
                              fluidRow(column(4,textAreaInput(inputId = "moviesToRate",label = "Put movie names below",
                                                      value = "",width = '100%',height = 400),
                                     radioButtons(inputId = "RatingType1",label = "Choose what to print",
                                                  choices = aux_radio1_choices,selected = "votecount" ),
                                     selectInput(inputId = "RatingType2",label = "Matching condition",
                                                 choices = aux_radio2_choices,
                                                 selected = "exact",multiple = FALSE,width = '100%' ),
                                     actionButton(inputId = "rate_titles","Find Movies",
                                                  icon=icon("search"))),
                              column(8,dataTableOutput(outputId = "table_matchedRatings"),
                                     textOutput(outputId = "rating_msg")))
                              ),
                     tabPanel("Find Films",
                              fluidRow(column(3,textInput(inputId = "moviesToFind_name",
                                                          label="Name of the movie",
                                                          value="Matrix"),
                                              selectInput(inputId = "moviesToFind_nameMode",label = "Matching condition",
                                                          choices = aux_radio2_choices,
                                                          selected = "exact",multiple = FALSE,width = '100%' )),
                                       column(3,checkboxGroupInput(inputId = "moviesToFind_type",
                                                          label="Type of the movie",choices=aux_radio3_choices,
                                                          selected=NULL)),
                                       column(3,selectInput(inputId = "moviesToFind_genre",label = "Genre",
                                                            choices = aux_radio4_choices,
                                                            selected = NULL,multiple = FALSE,width = '100%' ),
                                              numericInput(inputId = "moviesToFind_runtimeMin",label="Min. Runtime",
                                                           value=0),
                                              numericInput(inputId = "moviesToFind_runtimeMax",label="Max. Runtime",
                                                           value=1000)),
                                       column(3,sliderInput(inputId = "moviesToFind_year", "Year",
                                                            min = 1874, max = 2030, value = c(2000, 2020)),
                                              sliderInput(inputId = "moviesToFind_rating", "Rating",
                                                          min = 0, max = 10, value = c(7, 10)),
                                              numericInput(inputId = "moviesToFind_votesMin",label="Min. Votes",
                                                           value=1),
                                              numericInput(inputId = "moviesToFind_votesMax",label="Max. Votes",
                                                           value=10000000),
                                              actionButton(inputId = "find_film","Find Movies",
                                                           icon=icon("search")))),
                              textOutput(outputId = "moviesToFind_msg"),
                              fluidRow(column(12,dataTableOutput(outputId = "moviesToFind_table")))),
                 tabPanel("Some Stats",
                          fluidRow(column(2,plotsParametersUI("rating", label="Ratings Dependencies")),
                                   column(5,plotOutput("stats_rat_plot1")),
                                   column(5,plotOutput("stats_rat_plot2"))),
                          fluidRow(column(5,plotOutput("stats_rat_plot3"),offset = 2)),
                          fluidRow(column(2,plotsParametersUI("genre", label="Genre Dependencies")),
                                   column(5,plotOutput("stats_g_plot1")),
                                   column(5,plotOutput("stats_g_plot2"))),
                          fluidRow(column(5,plotOutput("stats_g_plot3"),offset = 2),
                                   column(5,plotOutput("stats_g_plot4")))
                           ),
                 selected = "General"
)
  

server=function(input,output){
  
  #### General ###
  data_imdb_reactive <- reactive({  
    if (input$update_status==0){
      data_imdb=readRDS(paste0(target_dir,"/","imdb_data_basic.rds"))
      return(data_imdb)
    } else if (input$update_status>0){
      id_notif <- showNotification(paste("Attempting ImDb update.. Please wait.."), duration = 0)
      data_imdb=update_imdb_data()
      removeNotification(id_notif)
      data_imdb
    }
  })
  
  
  #### Page 1 ####
  output$download_msg=renderText("")
  observeEvent(input$update_status,{
    if (is.null(data_imdb_reactive()$message)){
      output$download_msg=renderText("")
    } else {
      output$download_msg=renderText(data_imdb_reactive()$message)
    }
  })
  output$num_records=renderValueBox({valueBox(nrow(data_imdb_reactive()$data),"Number of records",
                                              icon=icon("film"),color="light-blue")})
  output$data_stamp=renderValueBox({valueBox(format(data_imdb_reactive()$data_stamp,"%d-%m-%Y"),"Database date",
                                             icon=icon("database"),color="olive")})
  
  
  #### Page 2 ####
  t_titles_to_match=eventReactive(input$rate_titles,{
    c(unlist(str_split(input$moviesToRate,"\n")[[1]]))
  })
  
  
  data_matched_ids<-eventReactive(input$rate_titles,{
    
    if (length(t_titles_to_match())==1){
      if (t_titles_to_match()==""){
        return(NULL)
      }
    } 
    
      temp_data=data_imdb_reactive()$data
      temp_data$aux_num=1:nrow(temp_data)
      
      if (input$RatingType2=="exact"){
        id_list=lapply(t_titles_to_match(),function(y){
          str_to_lower(temp_data$primaryTitle) %in% str_to_lower(y)
        })
      } else if (input$RatingType2=="contains") {
        id_list = lapply(t_titles_to_match(),function(y){
          str_detect(str_to_lower(temp_data$primaryTitle),str_to_lower(y))
        })
      }
      
      if (input$RatingType1=="all") {
        xout= sort(unique(do.call("c",lapply(id_list,function(y){ temp_data$aux_num[y] }))))
      }  else if (input$RatingType1=="votecount") {
        xout=sort(unique(do.call("c",lapply(id_list,function(y){ 
          temp_data_2=temp_data[y,]
          temp_data_2$aux_num[which.max(temp_data_2$numVotes)]
        }))))
      } else if (input$RatingType1=="rating") {
        xout=sort(unique(do.call("c",lapply(id_list,function(y){ 
          temp_data_2=temp_data[y,]
          temp_data_2$aux_num[which.max(temp_data_2$averageRating)]
        }))))
      }
    
    xout
  })
  
  
  data_matched<-eventReactive(input$rate_titles,{
    temp_id=data_matched_ids()
    temp_data=data_imdb_reactive()$data[temp_id,
                                        c("primaryTitle","startYear","runtimeMinutes",
                                          "genres","averageRating","numVotes")]
    if (nrow(temp_data)>100){
      temp_data=temp_data[1:100,]
    }
    if (ncol(temp_data)==6){
      colnames(temp_data)=c("Title","Year","Length (min.)","Genre","Rating","VotesNumber")
    }
    temp_data
  })
  
  rating_msg_react<-eventReactive(input$rate_titles,{
    if (length(data_matched_ids())>100){
      temp_msg="Matched more than 100 movies, restricting to first 100 matches"
    } else {
      temp_msg=paste0("Found ",length(data_matched_ids())," matched movies.")
    }
    temp_msg
  })
  
  
  output$table_matchedRatings=renderDataTable({
    data_matched()
  })
  output$rating_msg=renderText(rating_msg_react())
  
  # output$outputID1 <- renderText({ 
  #   paste0("You have selected this ",input$slide,
  #          " and ", aux_dict_race(as.numeric(input$race)))
  # })
  # output$outputID2 <- renderText({ 
  #   paste0("You have selected this ",input$slide2[1],"-",
  #          input$slide2[2],
  #          " and ", aux_dict_race(as.numeric(input$race)))
  # })
  # output$plotCensus<-renderPlot({
  #   temp_name=aux_dict_race(as.numeric(input$race))
  #   temp_data=counties[[str_to_lower(temp_name)]]
  #   percent_map(temp_data, color = aux_dict_color(as.numeric(input$race)), 
  #               legend.title = paste0("% of ",temp_name))
  # })
 

  
  
  #### Page 3 ####
  
  data_found_ids<-eventReactive(input$find_film,{
    
    temp_data=data_imdb_reactive()$data
    temp_data$aux_num=1:nrow(temp_data)
    
    #cond - name
    if (str_length(input$moviesToFind_name)>2){
      if (input$moviesToFind_nameMode=="exact"){
        t_cond_name=str_to_lower(temp_data$primaryTitle) == str_to_lower(input$moviesToFind_name)
      } else if (input$moviesToFind_nameMode=="contains"){
        t_cond_name=str_detect(str_to_lower(temp_data$primaryTitle), str_to_lower(input$moviesToFind_name))
      } else {
        t_cond_name=rep(TRUE,nrow(temp_data))
      }
    } else {
      t_cond_name=rep(TRUE,nrow(temp_data))
    }
    

    #cond - type
    if (length(input$moviesToFind_type)>0){
      t_cond_type=temp_data$titleType%in%input$moviesToFind_type
    } else {
      t_cond_type=rep(TRUE,nrow(temp_data))
    }
    
    #cond - genre
    if (length(input$moviesToFind_genre)>0){
      t_cond_genre=is.na(temp_data$genres)|str_detect(str_to_lower(temp_data$genres),str_to_lower(input$moviesToFind_genre))
    } else {
      t_cond_genre=rep(TRUE,nrow(temp_data))
    }
    
    #cond - runtime
      t_cond_runtime=is.na(temp_data$runtimeMinutes)|(temp_data$runtimeMinutes>=input$moviesToFind_runtimeMin&
                                                        temp_data$runtimeMinutes<=input$moviesToFind_runtimeMax)
      
      #cond - year
      t_cond_year=is.na(temp_data$startYear)|(temp_data$startYear>=input$moviesToFind_year[1]&
                                                        temp_data$startYear<=input$moviesToFind_year[2])
    
      #cond - rating
      t_cond_rating=is.na(temp_data$averageRating)|(temp_data$averageRating>=input$moviesToFind_rating[1]&
                                                        temp_data$averageRating<=input$moviesToFind_rating[2])
      
      #cond - votes
      t_cond_votes=is.na(temp_data$numVotes)|(temp_data$numVotes>=input$moviesToFind_votesMin&
                                                        temp_data$numVotes<=input$moviesToFind_votesMax)
    
    
      xout=temp_data$aux_num[t_cond_votes&t_cond_rating&t_cond_year&
                               t_cond_runtime&t_cond_genre&t_cond_type&
                               t_cond_name]
      
    unique(xout)
  })
  
  
  data_found<-eventReactive(input$find_film,{
    temp_id=data_found_ids()
    temp_data=data_imdb_reactive()$data[temp_id,
                                        c("primaryTitle","titleType","startYear","runtimeMinutes",
                                          "genres","averageRating","numVotes")]
    if (nrow(temp_data)>100){
      temp_data=temp_data[1:100,]
    }
    if (ncol(temp_data)==7){
      colnames(temp_data)=c("Title","Type","Year","Length (min.)","Genre","Rating","VotesNumber")
    }
    unique(temp_data)
  })
  
  found_msg_react<-eventReactive(input$find_film,{
    if (length(data_found_ids())>100){
      temp_msg="Matched more than 100 movies, restricting to first 100 matches"
    } else {
      temp_msg=paste0("Found ",length(data_found_ids())," matched movies.")
    }
    temp_msg
  })
  
  
  output$moviesToFind_table=renderDataTable({
    data_found()
  })
  output$moviesToFind_msg=renderText(found_msg_react())
   
  
  #### Page 4 ####
  #data_ratings <- plotsParametersServer("rating",df=data_imdb_reactive)
  #data_genre <- plotsParametersServer("genre",df=data_imdb_reactive)
  # data_ratings=reactive({
  #   temp_data=data_imdb_reactive()$data
  #   temp_data=temp_data[!(is.na(temp_data$averageRating)|is.na(temp_data$numVotes)),]
  #   temp_data=temp_data[temp_data$numVotes>=input$min_votes,]
  #   return(temp_data)
  # }) 
  
  #  plot1=reactive({
  #    p=ggplot(data=data_ratings(),aes(x=numVotes,y=averageRating))+geom_smooth()+theme_bw()+
  #    geom_point(alpha=0.01)+scale_x_log10()
  #   return(p)
  # })
  
  # plotParameters=plotsParametersServer("rating")
  # output$stats_rat_plot1=renderText(plotParameters$data())
  
  data_rating=plotsParametersServer("rating",data_reactive=data_imdb_reactive)
  plot1=reactive({
     p=ggplot(data=data_rating$data()$data_full,aes(x=numVotes,y=averageRating))+
       geom_smooth()+theme_bw()+
     geom_jitter(alpha=0.01)+scale_x_log10()
    return(p)
  })
  plot2=reactive({
    p=ggplot(data=data_rating$data()$data_full,aes(x=as.numeric(startYear) ,y=averageRating))+
      geom_smooth()+theme_bw()+
      geom_jitter(alpha=0.01)
    return(p)
  })
  plot3=reactive({
    temp_data=data_rating$data()$data_sep
    temp_data=temp_data[!is.na(temp_data$genres),]
    temp_data$genres=factor(temp_data$genres,
                            levels = names(sort(tapply(temp_data$averageRating,
                                                       temp_data$genres,median,na.rm=TRUE))),
                            ordered = TRUE)
    p=ggplot(data=temp_data,aes(x=genres, fill=genres ,y=averageRating))+
      geom_boxplot(outlier.alpha = 0,outlier.size = 0)+theme_bw()+
      theme(axis.text.x = element_text(angle = 90,vjust=0.3),
            axis.title.x = element_blank(),legend.position = 'none' )
    return(p)
  })
  
  output$stats_rat_plot1=renderPlot(plot1())
  output$stats_rat_plot2=renderPlot(plot2())
  output$stats_rat_plot3=renderPlot(plot3())
  
  
  
  data_genre=plotsParametersServer("genre",data_reactive=data_imdb_reactive)
  plot11=reactive({
    temp_data=data_genre$data()$data_sep
    temp_data=temp_data[!is.na(temp_data$genres),]
    temp_data$genres=factor(temp_data$genres,
                            levels = names(sort(tapply(temp_data$numVotes,
                                                       temp_data$genres,length))),
                            ordered = TRUE)
    p=ggplot(data=temp_data,aes(x=genres,fill=genres))+geom_bar()+theme_bw()+
      theme(axis.text.x = element_text(angle = 90,vjust=0.3),
            axis.title.x = element_blank(),legend.position = 'none' )
    return(p)
  })
  plot21=reactive({
    temp_data=data_genre$data()$data_sep
    temp_data=do.call(rbind,by(temp_data,paste0(temp_data$genres,"_",temp_data$startYear),function(x){
      data.frame(genres=unique(x$genres),
                 startYear=unique(x$startYear),
                 count=nrow(x),stringsAsFactors = FALSE)
    }))
    
    temp_max=max(temp_data$count,na.rm=TRUE)
    p1=ggplot(data=temp_data[str_sub(temp_data$genres,0,1)<"I",],aes(x=as.numeric(startYear) ,y=count,colour=genres))+
      geom_smooth()+ theme_bw()+facet_grid(genres~.)+scale_y_continuous(limits=c(0,temp_max))+
      theme(axis.title.x = element_blank(),legend.position = 'none' )
    p2=ggplot(data=temp_data[str_sub(temp_data$genres,0,1)>"H",],aes(x=as.numeric(startYear) ,y=count,colour=genres))+
      geom_smooth()+ theme_bw()+facet_grid(genres~.)+scale_y_continuous(limits=c(0,temp_max))+
      theme(axis.title.x = element_blank(),legend.position = 'none' )
    return(list(p1=p1,p2=p2))
  })
  plot31=reactive({
    temp_data=data_genre$data()$data_sep
    temp_data=temp_data[!is.na(temp_data$genres),]
    temp_data$genres=factor(temp_data$genres,
                            levels = names(sort(tapply(temp_data$numVotes,
                                                       temp_data$genres,median,na.rm=TRUE))),
                            ordered = TRUE)

    p=ggplot(data=temp_data,aes(x=genres, fill=genres ,y=numVotes))+
      geom_boxplot(outlier.alpha = 0,outlier.size = 0)+theme_bw()+scale_y_log10()+
      theme(axis.text.x = element_text(angle = 90,vjust=0.3),
            axis.title.x = element_blank(),legend.position = 'none' )
    
    return(p)
  })
  
  output$stats_g_plot1=renderPlot(plot11())
  output$stats_g_plot2=renderPlot(plot21()$p1)
  output$stats_g_plot3=renderPlot(plot31())
  output$stats_g_plot4=renderPlot(plot21()$p2)
  # 
}


shinyApp(ui=ui,server=server)