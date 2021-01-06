require(ggplot2)

aux_radio5_choices=list("movie","short", "tvMiniSeries","tvMovie","tvSeries","tvShort","tvSpecial","video","videoGame")
names(aux_radio5_choices)=c("Full Movie","Short Movie","TV MiniSeries", "TV Movie","TV Series",
                            "TV Short","TV Special","Video","VideoGame")


plotsParametersUI<-function(id, label="Plot Parameters"){
  ns=NS(id)
  
  tagList(numericInput(inputId = ns("min_votes"),label=paste0(label,"\n ", "Minimum Vote Count"),value=10000),
          numericInput(inputId = ns("min_year"),label="Years from:",value=1980),
          checkboxGroupInput(inputId = ns("movie_type"),
                             label="Type of the movies:",choices=aux_radio5_choices,
                             selected=c(1),inline=TRUE))
}


# plotsParametersServer<-function(id){
#   moduleServer(id,function(input,output,session){
#     # module_data=reactive({
#     # temp_data=df()$data
#     # temp_data=temp_data[!(is.na(temp_data$averageRating)|is.na(temp_data$numVotes)),]
#     # temp_data=temp_data[temp_data$numVotes>=input$min_votes&
#     #               temp_data$startYear>=input$min_year&
#     #               temp_data$titleType%in%input$movie_type,]
#     # })
#     
#     return(list(data=reactive({input$min_votes}) ))
#   })
# }


plotsParametersServer<-function(id,data_reactive){
  moduleServer(id,function(input,output,session){
    module_data=reactive({
      temp_data=data_reactive()$data[,c("titleType","startYear","runtimeMinutes", 
                                        "genres","averageRating","numVotes")]
      temp_data=temp_data[!(is.na(temp_data$averageRating)|
                              is.na(temp_data$numVotes)|
                              is.na(temp_data$startYear)|
                              is.na(temp_data$genres)),]
      temp_data=temp_data[temp_data$numVotes>=input$min_votes&
                    temp_data$startYear>=input$min_year&
                    temp_data$titleType%in%input$movie_type,]
      temp_data_sep=separate_rows(data=temp_data,"genres",sep=",")
      return(list(data_full=temp_data,data_sep=temp_data_sep))
    })
    
    return(list(data=reactive({module_data()}) ))
  })
}