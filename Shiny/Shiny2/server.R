shinyServer(function(input, output) {
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
  output$top10cent <- renderTable({
    if (input$c_rank != "None"){
      rank = get.vertex.attribute(football,input$c_rank)
      id = get.vertex.attribute(football,'id')
      df = data.frame(id=id,cent = rank)
      top10 = head(arrange(df, desc(cent)),10)
    }
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
  output$community_plot <- renderPlot({
    if (is.null(input$conference_rm) ){
      com = walktrap.community(football, steps=10)
      # return the result of community
      V(football)$sg = com$membership
      # color the node based on the result of community
      V(football)$color = NA
      V(football)$color = rainbow(max(V(football)$sg))[V(football)$sg]
      
      plot(football, layout = layout.fruchterman.reingold, 
           vertex.color = V(football)$color, edge.color = grey(0.5), edge.arrow.mode = "-")
    } 
    else {
      for (cid in input$conference_rm) {
        conf = substring(cid,0,2)
        conf = strsplit(conf, ":")
        conf = as.numeric(conf)
        print(conf)
        football <- delete.vertices(football,V(football)[V(football)$value == conf])
        com = walktrap.community(football, steps=10)
        # 返回每个节点的分组结果
        V(football)$sg = com$membership
        # 按照分组结果赋予节点不同的颜色
        V(football)$color = NA
        V(football)$color = rainbow(max(V(football)$sg))[V(football)$sg]
        
        plot(football, layout = layout.fruchterman.reingold, 
             vertex.color = V(football)$color, edge.color = grey(0.5), edge.arrow.mode = "-")
      }}
    
  }, height = 600, width = 600)
  output$block <- renderPlot({
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
#    library(sna)
#    info <- request
#    inf <- network(t1,directed=TRUE)
#    mon <- network(t2,directed=TRUE)
    
#    eq <- equiv.clust(list(inf,mon), mode="digraph")
#    n <- as.numeric(input$number)
#    b <- blockmodel(info, eq, k=n)
    blockmodel$order.vector
    blockmodel$block.membership
#    plot(blockmodel)
    
    foot <- get.adjacency(football)
    foot <- as.matrix(foot)
    footnetwork <- as.network(foot,"adjacency")
    den <- network.density(footnetwork)
    
    bimage <- blockmodel$block.model
    bimage
    bimage[bimage < den] <- 0   
    bimage
    bimage[is.nan(bimage)] <- 1
    bimage
    
    gplot(bimage, diag=TRUE, 
          edge.lwd=bimage*5, 
          label=colnames(bimage),
          vertex.cex=sqrt(table(blockmodel$block.membership))*1.46,
          gmode="digraph", vertex.sides=60,
          vertex.col=grey(diag(bimage)/3))
    
  },height = 300, width = 400)
})




