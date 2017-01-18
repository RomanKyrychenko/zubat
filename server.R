shinyServer(function(input, output){
  
  output$plot <- renderPlot({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    names(df)[1:2] <- c("X0","X1")
    zubat <- function(df){
      ukraine_map <- getData('GADM', country='Ukraine', level=1)
      
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
      
      ggplot(df, aes(map_id = X0)) +
        scale_fill_gradient(low="white", high="#4d738a") +
        geom_map(aes(fill = X1), map =ukraine_df, color = "grey") +
        with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 2.5)) +
        with(centroids, annotate(geom="text", x = long, y=lat+0.15, label = lab, size = 4)) +
        expand_limits(x = ukraine_df$long, y = ukraine_df$lat) + theme_void() + theme(
          legend.position = "none"
        )
    }
    zubat(df)
  })
  
  output$contents <- renderTable({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    names(df)[1:2] <- c("X0","X1")
    df 
  }
  )
  
  output$downloadPlot <-  downloadHandler(
    filename = function(){paste0("zubat",".pdf") },
    content = function(file) {
      pdf(file)
      inFile <- input$file1
      
      if(is.null(inFile))
        return(NULL)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      names(df)[1:2] <- c("X0","X1")
      zubat <- function(df){
        ukraine_map <- getData('GADM', country='Ukraine', level=1)
        
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
        
        ggplot(df, aes(map_id = X0)) +
          scale_fill_gradient(low="white", high="#4d738a") +
          geom_map(aes(fill = X1), map =ukraine_df, color = "grey") +
          with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 2.5)) +
          with(centroids, annotate(geom="text", x = long, y=lat+0.15, label = lab, size = 4)) +
          expand_limits(x = ukraine_df$long, y = ukraine_df$lat) + theme_void() + theme(
            legend.position = "none"
          )
      }
      print(zubat(df))
      dev.off()
    }
  )
  
  output$download<-  downloadHandler(
    filename = function(){paste0("zubat",".png") },
    content = function(file) {
      png(file)
      inFile <- input$file1
      
      if(is.null(inFile))
        return(NULL)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      names(df)[1:2] <- c("X0","X1")
      zubat <- function(df){
        ukraine_map <- getData('GADM', country='Ukraine', level=1)
        
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
        
        ggplot(df, aes(map_id = X0)) +
          scale_fill_gradient(low="white", high="#4d738a") +
          geom_map(aes(fill = X1), map =ukraine_df, color = "grey") +
          with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 2.5)) +
          with(centroids, annotate(geom="text", x = long, y=lat+0.15, label = lab, size = 4)) +
          expand_limits(x = ukraine_df$long, y = ukraine_df$lat) + theme_void() + theme(
            legend.position = "none"
          )
      }
      print(zubat(df))
      dev.off()
    }
  )
})