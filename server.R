shinyServer(function(input, output){
  
  df <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1,col_names = FALSE)
    names(df)[1:2] <- c("X0","X1")
    zubat <- function(df){
      
      ukraine_map@data$NAME_1 <- c("Черкаська","Чернігівська","Чернівецька","Крим","Дніпропетровська","Донецька","Івано-Франківська","Харківська",
                                   "Херсонська","Хмельницька","Київ","Київська","Кіровоградська","Львівська","Луганська","Миколаївська","Одеська",
                                   "Полтавська","Рівненська","Севастополь","Сумська","Тернопільська","Закарпатська","Вінницька","Волинська","Запорізька","Житомирська")
      ukraine_map$VARNAME_1 <-  c("Черкаська","Чернігівська","Чернівецька","Крим","Дніпропетровська","Донецька","Івано-Франківська","Харківська",
                                  "Херсонська","Хмельницька","Київ","Київська","Кіровоградська","Львівська","Луганська","Миколаївська","Одеська",
                                  "Полтавська","Рівненська","Севастополь","Сумська","Тернопільська","Закарпатська","Вінницька","Волинська","Запорізька","Житомирська")
      
      ukraine_df <- fortify(ukraine_map,region = "VARNAME_1")
      
      test_df <- dplyr::data_frame(
        X0=c("Черкаська","Чернігівська","Чернівецька","Крим","Дніпропетровська","Донецька","Івано-Франківська","Харківська",
             "Херсонська","Хмельницька","Київ","Київська","Кіровоградська","Львівська","Луганська","Миколаївська","Одеська",
             "Полтавська","Рівненська","Севастополь","Сумська","Тернопільська","Закарпатська","Вінницька","Волинська","Запорізька","Житомирська"),
        X1=rep(NA,27)
      )
      df <- rbind(df[1:2],test_df[1:2])
      df <- df %>% distinct(X0,.keep_all=T)
      df$X1[is.na(df$X1)] <- 0
      centroids <- setNames(do.call("rbind.data.frame", by(ukraine_df, ukraine_df$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
      centroids$label <- ukraine_df$id[match(rownames(centroids), ukraine_df$group)]
      centroids <- centroids %>% distinct(label,.keep_all = T)
      centroids$lab <- (df %>% arrange(X0))$X1[-16]
      centroids$lat[10] <- centroids$lat[10]-0.4
      centroids$long[16] <- centroids$long[16]+0.35
      
      centroids <- centroids[centroids$lab>0,]
      
      if(input$typ=="Зубата!"){
        ggplot(df, aes(map_id = X0)) +
          scale_fill_gradient(low="white", high="#4d738a") +
          geom_map(aes(fill = X1), map =ukraine_df, color = "grey") +
          with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 5)) +
          with(centroids,annotate(geom="point",x = long, y=lat+0.25,color="#31a354",fill="#31a354",size=10))+
          with(centroids, annotate(geom="text", x = long, y=lat+0.25, label = lab, size = 6, color="white")) +
          expand_limits(x = ukraine_df$long, y = ukraine_df$lat) + theme_void() + theme(
            legend.position = "none"
          )
      } else {
        df[[2]] <- ifelse(df$X1>0,1,0)
        ggplot(df, aes(map_id = X0)) +
          scale_fill_gradient(low="#deebf7", high="#9ecae1") +
          geom_map(aes(fill = X1), map =ukraine_df, color = "white") +
          #with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 5)) +
          #with(centroids,annotate(geom="point",x = long, y=lat+0.25,color="#31a354",fill="#31a354",size=10))+
          #with(centroids, annotate(geom="text", x = long, y=lat+0.25, label = lab, size = 6, color="white")) +
          expand_limits(x = ukraine_df$long, y = ukraine_df$lat) + theme_void() + theme(
            legend.position = "none"
          )
      }
     
    }
    zubat(df)
  })
  
  
  output$plot <- renderPlot({
   tryCatch(df())
  })
  
  output$contents <- renderTable({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    names(df)[1:2] <- c("X0","X1")
    tryCatch(df) 
  }
  )
  
  output$downloadPlot <-  downloadHandler(
    filename = function(){paste0("zubat_",Sys.Date(),".pdf") },
    content = function(file) {
      cairo_pdf(file, width=15.5, height=10.3)
      print(df())
      dev.off()
    }
  )
  
  output$download<-  downloadHandler(
    filename = function(){paste0("zubat_",Sys.Date(),".png") },
    content = function(file) {
      png(file, width=1550, height=1030)
      print(df())
      dev.off()
    }
  )
})