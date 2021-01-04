require(data.table)

update_imdb_data<-function(target_dir="data/"){
  url_0<-"https://datasets.imdbws.com"
  url_titles=paste0(url_0,"/title.basics.tsv.gz")
  url_ratings=paste0(url_0,"/title.ratings.tsv.gz")
  binary_proceed=TRUE
  
  ## check current data ##
  if (file.exists(paste0(target_dir,"/","imdb_data_basic.rds"))){
    data_imdb=readRDS(paste0(target_dir,"/","imdb_data_basic.rds"))
    time_diff=Sys.time()-data_imdb$data_stamp
    units(time_diff)="days"
    if (time_diff<30){
      xmsg="Current data has been downloaded less than 30 days ago. Request not initiated."
      cat(xmsg)
      binary_proceed=FALSE
    }
  } 
  
  ## downloading
  
  if (binary_proceed){
    download.file(url = url_titles,destfile = paste0(target_dir,"/titles_temp.gz"))
    download.file(url = url_ratings,destfile = paste0(target_dir,"/ratings_temp.gz"))
    
    cat("Processing basic titles file..")
    basic_titles<-fread(paste0(target_dir,"/titles_temp.gz"),encoding = "UTF-8")
    basic_titles=basic_titles[!basic_titles$titleType=="tvEpisode",]
    
    basic_titles$runtimeMinutes=as.numeric(basic_titles$runtimeMinutes)
    basic_titles=basic_titles[!is.na(basic_titles$primaryTitle),]
    
    for (icol in 1:ncol(basic_titles)){
      temp=basic_titles[[icol]]
      cat(paste0("Col ",icol,": ",sum(temp=="\\N",na.rm=TRUE)," NAs detected.."))
      temp[temp=="\\N"]=NA
      basic_titles[[icol]]=temp
    }
    
    cat("Processing ratings file..\n")
    df_ratings<-fread(paste0(target_dir,"/ratings_temp.gz"),encoding = "UTF-8")
    df_ratings=df_ratings[df_ratings$tconst%in%basic_titles$tconst,]
    
    cat("\n Merging files..")
    df_imdb_merged=merge(basic_titles,df_ratings,by="tconst",all.x=TRUE)
  
    cat("\n Saving files..")
    saveRDS(list(data=df_imdb_merged,data_stamp=Sys.time()),
            paste0(target_dir,"/","imdb_data_basic.rds"))
   
    file.remove(paste0(target_dir,"/titles_temp.gz"))
    file.remove(paste0(target_dir,"/ratings_temp.gz"))
    
    xmsg="Data Downloaded and saved"
    cat(xmsg)
  }
  
  list(data=df_imdb_merged,data_stamp=Sys.time(),message=xmsg)
}