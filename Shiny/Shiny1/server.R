
shinyServer(function(input, output) {
  library(igraph)
  library(ggplot2)
  wd <- getwd()
  setwd(wd)
  football<-read.graph("football.gml",format=c("gml"))
  V(football)$id <- V(football)
  V(football)$university <- V(football)$label
  V(football)$label <- V(football)
  nodes = V(football)
  
  get_node <- reactive({
    node_id = input$node_sel
    node_id = as.numeric(node_id)
    #print(node_id)
    #print(V(football)$label[node_id])
    university_name = V(football)$university[node_id]
    conference_number = V(football)$value[node_id]
    #print(university_name)
    return(c(university_name,conference_number))
  })
  
  subgprahByNode <- function(g, nodeid) {
    e = E(g)[from(nodeid)]
    sub = subgraph.edges(g, as_ids(e))
  }
  calStats <- function(g, g_type){
    if (g_type == 'full') {
      stats = c("Degree","Degree Std",  "Transitivity", "ASP", "ASP Std", "Density", "Diameter")
    } else {
      stats = c("Degree","Degree Std",  "Transitivity", "ASP", "ASP Std", "Density", "Diameter")
    }}
  output$subControls <- renderUI({
    var = input$subnet
    if (var == "Node"){
      selectInput("subby", "", nodes,selected = 1)
    }
  })
  output$node_university <- renderText({ 
    n = get_node()
    return(paste0("University Name  : ", n[1]))
  })
  output$nodeID <- renderText({ 
   g = football
#   UName =input$UniversityName 
#   print(class(UName))
   print(UName)
   UName="BrighamYoung" #
    #print(UName)
    #Uid=999999
    if(UName != " "){
      NodeID = V(g)[V(g)$label == UName]$id
      NodeID = as.integer(NodeID)
      #g = football
      #print(V(g))
      #x = V(g)$label == UName
      #print(x)
      #id = V(g)[x]$id
      #print(id)
      #Uid = id+1
      print(NodeID)
      return(paste0("Node ID  : ", NodeID))
    }
    else{
      print("no input")
      return("No Search")
    }

  })
  output$node_conference <- renderText({
    n = get_node()
    conference_number = n[2]
    if(n[2] == 0){
      return(paste0("Conference Name  : ", "Atlantic Coast"))
    }
    if(n[2] == 1){
      return(paste0("Conference Name  : ", "Big East"))
    }
    if(n[2] == 1){
      return(paste0("Conference Name  : ", "Big East"))
    }
    if(n[2] == 1){
      return(paste0("Conference Name  : ", "Big East"))
    }
    if(n[2] == 2){
      return(paste0("Conference Name  : ", "Big Ten"))
    }
    if(n[2] == 3){
      return(paste0("Conference Name  : ", "Big Twelve"))
    }
    if(n[2] == 4){
      return(paste0("Conference Name  : ", "Conference USA"))
    }
    if(n[2] == 5){
      return(paste0("Conference Name  : ", "Independents"))
    }
    if(n[2] == 6){
      return(paste0("Conference Name  : ", "Mid-American"))
    }
    if(n[2] == 7){
      return(paste0("Conference Name  : ", "Mountain West"))
    }
    if(n[2] == 8){
      return(paste0("Conference Name  : ", "Pacific Ten"))
    }
    if(n[2] == 9){
      return(paste0("Conference Name  : ", "Southeastern"))
    }
    if(n[2] == 10){
      return(paste0("Conference Name  : ", "Sun Belt"))
    }
    if(n[2] == 11){
      return(paste0("Conference Name  : ", "Western Athletic"))
    }
    
  })
  output$whole_net_plot <- renderPlot({
    colrs <- c("lavender", "lavenderblush", "lavenderblush3","lemonchiffon",
               "lightblue2","lightpink",
               "plum2","rosybrown1","thistle1","aliceblue","goldenrod1","gainsboro")
    if (is.null(input$conference_rm) ){
      if (input$groupby !="None") 
      {
        V(football)$color <- colrs[V(football)$value]
        plot(football, layout = layout.fruchterman.reingold, 
             vertex.color = V(football)$color, edge.color = grey(0.5), edge.arrow.mode = "-")
        legend(x=-1.5, y=-1.1, c("Atlantic Coast","Big East", "Big Ten","Big Twelve",
                                 "Conference USA","Independents","Mid-American",
                                 "Mountain West","Pacific Ten","Southeastern",
                                 "Sun Belt","Western Athletic"), pch=21,
               col="#777777", pt.bg=colrs, pt.cex=1, cex=.8, bty="n", ncol=4)
      }
      else{
        plot(football,layout=layout.fruchterman.reingold)}
    } 
    else {
      if (input$groupby =="None") {
      for (cid in input$conference_rm) {
        conf = substring(cid,0,2)
        conf = strsplit(conf, ":")
        conf = as.numeric(conf)
        print(conf)
        football <- delete.vertices(football,V(football)[V(football)$value == conf])}
 
        plot(football,layout=layout.fruchterman.reingold)
      }
      else{
        print(input$conference_rm)
        for (cid in input$conference_rm) {
        conf = substring(cid,0,2)
        conf = strsplit(conf, ":")
        conf = as.numeric(conf)
        print(conf)
        V(football)$color <- colrs[V(football)$value]
        football <- delete.vertices(football,V(football)[V(football)$value == conf])}
        plot(football, layout = layout.fruchterman.reingold, 
             vertex.color = V(football)$color, edge.color = grey(0.5), edge.arrow.mode = "-")
        #plot(football,layout=layout.fruchterman.reingold)
        legend(x=-1.5, y=-1.1, c("Atlantic Coast","Big East", "Big Ten","Big Twelve",
                                 "Conference USA","Independents","Mid-American",
                                 "Mountain West","Pacific Ten","Southeastern",
                                 "Sun Belt","Western Athletic"), pch=21,
               col="#777777", pt.bg=colrs, pt.cex=1, cex=.8, bty="n", ncol=4)
      }
    }
    num_nodes = vcount(football)

  }, height = 480, width = 480)
  output$top10cent <- renderTable({
    if (input$c_rank != "None"){
      rank = get.vertex.attribute(football,input$c_rank)
      id = get.vertex.attribute(football,'id')
      df = data.frame(id=id,cent = rank)
      top10 = head(arrange(df, desc(cent)),10)
    }
  })

  output$cent_plot <- renderPlot({
    colrs <- c("lavender", "lavenderblush", "lavenderblush3","lemonchiffon",
               "lightblue2","lightpink",
               "plum2","rosybrown1","thistle1","aliceblue","goldenrod1","gainsboro")
#    colrs <- c("gray50", "tomato", "gold","orange","blue","white",
#               "green","purple","pink","red","brown","darkblue")
    bound = as.numeric(input$textInput2)
    if(is.null(input$conference_rm)){
      g = football
    }
    else{
      g = football
      for (cid in input$conference_rm) {
        conf = substring(cid,0,2)
        conf = strsplit(conf, ":")
        conf = as.numeric(conf)
        #print(conf)
        g <- delete.vertices(g,V(g)[V(g)$value == conf])}
    }
    num_nodes = vcount(g)
    #print(num_nodes)
    if (input$c_rank != "None"){
      #add top10 centrality number
      output$top10cent <- renderTable({
        if (input$c_rank != "None"){
          rank = get.vertex.attribute(g,input$c_rank)
          id = get.vertex.attribute(g,'id')
          df = data.frame(id=id,cent = rank)
          top10 = head(arrange(df, desc(cent)),10)}})
        closeness_g = closeness(g)
        btw_g = betweenness(g)
        ev_obj_g = evcent(g)
        eigen_g = ev_obj_g$vector
        V(g)$Degree.Centrality = degree(g)
        V(g)$Closeness.Centrality = closeness_g
        V(g)$Betweenness.Centrality= btw_g
        V(g)$Eigenvalue.Centrality =eigen_g
        v_size = get.vertex.attribute(g,input$c_rank)
        output$cent <- renderPlot({plot(v_size, xlab="Vertex", ylab=input$c_rank)})
#      v_size = get.vertex.attribute(g,"Degree.Centrality")

       if(input$c_rank == "Betweenness.Centrality"){
         V(g)$size = 5
         V(g)[Betweenness.Centrality>=bound]$size = 15
         V(g)$label = NA
         V(g)[Betweenness.Centrality>=bound]$label = V(g)[Betweenness.Centrality>=bound]$id
         print(V(g)$size)
       }
        if(input$c_rank == "Degree.Centrality"){
          V(g)$size = 5
          V(g)[Degree.Centrality>=bound]$size = 15
          V(g)$label = NA
          V(g)[Degree.Centrality>=bound]$label = V(g)[Degree.Centrality>=bound]$id
        }
        if(input$c_rank == "Closeness.Centrality"){
          V(g)$size = 5
          V(g)[Closeness.Centrality>=bound]$size = 15
          V(g)$label = NA
          V(g)[Closeness.Centrality>=bound]$label = V(g)[Closeness.Centrality>=bound]$id
        }
        if(input$c_rank == "Eigenvalue.Centrality"){
          V(g)$size = 5
          V(g)[Eigenvalue.Centrality>=bound]$size = 15
          V(g)$label = NA
          V(g)[Eigenvalue.Centrality>=bound]$label = V(g)[Eigenvalue.Centrality>=bound]$id
        }
#       V(g)$size = scale(v_size) + 12
    } else {
      V(g)$size = rep.int(1,num_nodes) + 7
    }
    if (input$groupby !="None") {
      V(g)$color <- colrs[V(g)$value]
    }
    else{
      g = g 
    }
    plot(g, layout = layout.fruchterman.reingold, 
         vertex.color = V(g)$color, 
         vertex.label = V(g)$label, 
         vertex.label.color="black",
         edge.color = grey(0.5),
         edge.arrow.mode = "-")
    
  },height = 550, width = 550)
  output$betweness_plot <- renderPlot({
    bound = as.numeric(input$textInput1)
    colrs <- c("lavender", "lavenderblush", "lavenderblush3","lemonchiffon",
               "lightblue2","lightpink",
               "plum2","rosybrown1","thistle1","aliceblue","goldenrod1","gainsboro")
    V(football)$name = V(football)$label
    if (is.null(input$conference_rm) ){
      V(football)$btn = betweenness(football, directed = F)
      output$betweeness <- renderPlot({plot(V(football)$btn, xlab="Vertex", ylab="Betweenness")})
      V(football)$size = 5
      V(football)[btn>=bound]$size = 15
      V(football)$label = NA
      V(football)[btn>=bound]$label = V(football)[btn>=bound]$name
      if (input$groupby !="None") 
      {
        V(football)$color <- colrs[V(football)$value]
        plot(football, layout = layout.fruchterman.reingold, 
             vertex.size = V(football)$size, 
             vertex.color = V(football)$color, 
             vertex.label = V(football)$label, 
             vertex.label.color="black",
             edge.color = grey(0.5),
             edge.arrow.mode = "-")
      }
      else{
        plot(football, layout = layout.fruchterman.reingold, 
             vertex.size = V(football)$size, 
             vertex.color = V(football)$color, 
             vertex.label = V(football)$label, 
             vertex.label.color="black",
             edge.color = grey(0.5),
             edge.arrow.mode = "-")}
      }
    else{
      for (cid in input$conference_rm) {
        conf = substring(cid,0,2)
        conf = strsplit(conf, ":")
        conf = as.numeric(conf)
        print(conf)
        football <- delete.vertices(football,V(football)[V(football)$value == conf])}
        V(football)$btn = betweenness(football, directed = F)
        output$betweeness <- renderPlot({plot(V(football)$btn, xlab="Vertex", ylab="Betweenness")})
        V(football)$size = 5
        V(football)[btn>=bound]$size = 15
        V(football)$label = NA
        V(football)[btn>=bound]$label = V(football)[btn>=bound]$name
        if (input$groupby !="None") 
        {
          V(football)$color <- colrs[V(football)$value]
        }
        plot(football, layout = layout.fruchterman.reingold, 
             vertex.size = V(football)$size, 
             vertex.color = V(football)$color, 
             vertex.label = V(football)$label, 
             vertex.label.color="black",
             edge.color = grey(0.5),
             edge.arrow.mode = "-")

    }
        
  },height = 600, width = 600)
  
  output$sub_net_plot <- renderPlot({
    var = input$subnet
    if (var == "Node"){
#      print(input$subby)
      g_1 = subgprahByNode(football, as.numeric(input$subby))
    }

    plot(g_1,  vertex.size =12, vertex.label=V(g_1)$id)
 #   output$subStat <- renderTable(calStats(g_1, 'sub'))
  }, height = 500, width = 500)
  
  output$centrality_table <- renderTable({
    if (is.null(input$conference_rm) ){
      btn = mean(betweenness(football))
      clo = mean(closeness(football))
      deg = mean(degree(football))
      g_undirected <- as.undirected(football, mode='collapse')
      ev_obj <- evcent(g_undirected)
      eigen <- mean(ev_obj$vector)
      mydf <- do.call(rbind, Map(data.frame, Closeness=clo, 
                                 Degree=deg,
                                 Betweenness=btn, 
                                 Eigen=eigen))
      return(mydf)
    }
    else{
      for (cid in input$conference_rm) {
        conf = substring(cid,0,2)
        conf = strsplit(conf, ":")
        conf = as.numeric(conf)
        football <- delete.vertices(football,V(football)[V(football)$value == conf])}
      btn = mean(betweenness(football))
      clo = mean(closeness(football))
      deg = mean(degree(football))
      g_undirected <- as.undirected(football, mode='collapse')
      ev_obj <- evcent(g_undirected)
      eigen <- mean(ev_obj$vector)
      mydf <- do.call(rbind, Map(data.frame, Closeness=clo, 
                                 Degree=deg,
                                 Betweenness=btn, 
                                 Eigen=eigen))
      return(mydf)
      
    }
  })
  
  output$comparsion_barplot <- renderPlot({
    g = football
    closeness_g = closeness(g)
    btw_g = betweenness(g)
    ev_obj_g = evcent(g)
    eigen_g = ev_obj_g$vector
    V(g)$Degree.Centrality = degree(g)
    V(g)$Closeness.Centrality = closeness_g
    V(g)$Betweenness.Centrality= btw_g
    V(g)$Eigenvalue.Centrality =eigen_g
    xnames <- c("Atlantic Coast","Big East","Big Ten","Big Twelve","Conference USA","Independents",
               "Mid-American","Mountain West","Pacific Ten","Southeastern","Sun Belt","Western Athletic")
    colrs <- c("lavender", "lavenderblush", "lavenderblush3","lemonchiffon",
               "lightblue2","lightpink",
               "plum2","rosybrown1","thistle1","aliceblue","goldenrod1","gainsboro")
    #    colrs <- c("gray50", "tomato", "gold","orange","blue","white",
#               "green","purple","pink","red","brown","darkblue")
    bet <- 0
    for (i in 0:11){
      #print(i)
      bet[i+1] <- sum(V(g)[value==i]$Betweenness.Centrality)/length(V(g)[value==i]$Betweenness.Centrality)
    }
    deg <- 0
    for (i in 0:11){
      deg[i+1] <- sum(V(g)[value==i]$Degree.Centrality)/length(V(g)[value==i]$Degree.Centrality)
    }
    clo <- 0
    for (i in 0:11){
      clo[i+1] <- sum(V(g)[value==i]$Closeness.Centrality)/length(V(g)[value==i]$Closeness.Centrality)
    }
    eig <- 0
    for (i in 0:11){
      eig[i+1] <- sum(V(g)[value==i]$Eigenvalue.Centrality)/length(V(g)[value==i]$Eigenvalue.Centrality)
    }
    if(input$c_rank == "Betweenness.Centrality"){
      bar = barplot(bet,col=colrs)
      xlabbb <-seq(0:11)
      axis(1, at=bar, labels=xlabbb)
    }
    else if(input$c_rank == "Degree.Centrality"){
      bar = barplot(deg,col=colrs)
      xlabbb <-seq(0:11)
      axis(1, at=bar, labels=xlabbb)
    }
    else if(input$c_rank == "Closeness.Centrality"){
      bar = barplot(clo,col=colrs)
      xlabbb <-seq(0:11)
      axis(1, at=bar, labels=xlabbb)
    }
    else if(input$c_rank == "Eigenvalue.Centrality"){
      bar = barplot(eig,col=colrs)
      xlabbb <-seq(0:11)
      axis(1, at=bar, labels=xlabbb)
    }
    else{
      matrix <- rbind(bet,deg,clo,eig)
      matrix <- as.matrix(matrix)
      xlabbb <-seq(0:11)
      bar = barplot(matrix,col=c("lightblue","lightpink","yellow","lightgreen"))
      axis(1, at=bar, labels=xlabbb)
      legend("topleft", c("Betweeness","Degree","Closeness","Eigenvalue"), cex=0.8, bty="n", 
             fill = c("lightblue","lightpink","yellow","lightgreen"))
    }
  })
  
  output$overview <- renderPlot({
    g = football
    closeness_g = closeness(g)
    btw_g = betweenness(g)
    ev_obj_g = evcent(g)
    eigen_g = ev_obj_g$vector
    V(g)$Degree.Centrality = degree(g)
    V(g)$Closeness.Centrality = closeness_g
    V(g)$Betweenness.Centrality= btw_g
    V(g)$Eigenvalue.Centrality =eigen_g
    bet <- 0
    for (i in 1:115){
     #print(i)
      bet[i] <- V(g)[i]$Betweenness.Centrality
    }
    deg <- 0
    for (i in 1:115){
      deg[i] <- V(g)[i]$Degree.Centrality
    }
    clo <- 0
    for (i in 1:115){
      clo[i] <- V(g)[i]$Closeness.Centrality
    }
    eig <- 0
    for (i in 1:115){
      eig[i] <- V(g)[i]$Eigenvalue.Centrality
    }
    matrix <- rbind(bet,deg,clo,eig)
    matrix <- as.matrix(matrix)
    xlabbb <-seq(1:115)
#    barplot(matrix,col=c("lightblue","lightpink","yellow","lightgreen"))
    bar <- barplot(matrix,col=c("lightblue","lightpink","yellow","orange"),xlab="Node ID")
    axis(1, at=bar, labels=xlabbb)
    
    legend("top", c("Betweeness","Degree","Closeness","Eigenvalue"), cex=0.5, bty="n", 
           fill = c("lightblue","lightpink","yellow","orange"))
  })
  
  output$cluster_barplot <- renderPlot({
    matrix_row_to_col <- get.adjacency(football)
    matrix_row_to_col <-as.matrix(matrix_row_to_col)
    matrix_row_to_col
    matrix_col_to_row <- t(matrix_row_to_col)
    
    matrix <- rbind(matrix_row_to_col,matrix_col_to_row  )
    
    cors <- cor(matrix_row_to_col)
    
    dissimilarity <- 1 - cors
    dist <- as.dist(dissimilarity)
    
    hclust <- hclust(dist)
    plot(hclust)
    
  },height = 600, width = 600)
  output$blockmodel_barplot <- renderPlot({
    matrix_row_to_col <- get.adjacency(football)
    matrix_row_to_col <-as.matrix(matrix_row_to_col)
    matrix_row_to_col
    matrix_col_to_row <- t(matrix_row_to_col)
    
    matrix <- rbind(matrix_row_to_col,matrix_col_to_row  )
    cors <- cor(matrix_row_to_col)
    dissimilarity <- 1 - cors
    dist <- as.dist(dissimilarity)
    hclust <- hclust(dist)
    
    num_clusters = input$textInput3
    num_clusters = as.numeric(num_clusters)
    clusters <- cutree(hclust, k = num_clusters)
    cluster_cor_mat <- clusterCorr(cors,clusters)
    gcor(cluster_cor_mat, cors)
    
    # Task valued
    mean <- mean(matrix_row_to_col)
    blockmodel <- blockmodel(matrix_row_to_col, clusters)
    plot(blockmodel,main="blockmodel")
    
  },height = 600, width = 600)
  
  output$stat <- renderTable({
    var = input$subby
    print(var)
    var = as.numeric(var)
    g = football
    closeness_g = closeness(g)
    btw_g = betweenness(g)
    ev_obj_g = evcent(g)
    eigen_g = ev_obj_g$vector
    V(g)$Degree.Centrality = degree(g)
    V(g)$Closeness.Centrality = closeness_g
    V(g)$Betweenness.Centrality= btw_g
    V(g)$Eigenvalue.Centrality =eigen_g
    clo <- V(g)[var]$Closeness.Centrality
    bet <- V(g)[var]$Betweenness.Centrality
    deg <- V(g)[var]$Degree.Centrality
    eig <- V(g)[var]$Eigenvalue.Centrality

    mydf <- do.call(rbind, Map(data.frame, Closeness=clo, 
                               Degree=deg,
                               Betweenness=bet, 
                               Eigen=eig))
    return(mydf)
      
    })
  
})




