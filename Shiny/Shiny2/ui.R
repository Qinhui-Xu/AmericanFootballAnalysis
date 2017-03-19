library(ggplot2)
library(igraph)
library(network)
library(NetCluster)
wd <- getwd()
setwd(wd)
football<-read.graph("football.gml",format=c("gml"))
nodes = V(football)
remove = c("0:Atlantic Coast","1:Big East","2:Big Ten","3:Big Twelve","4:Conference USA","5:Independents",
          "6:Mid-American","7:Mountain West","8:Pacific Ten","9:Southeastern","10:Sun Belt",
          "11:Western Athletic")
groubys = c("None","Conference")
cntrlty = c("None", "Degree.Centrality", "Closeness.Centrality", "Betweenness.Centrality", "Eigenvalue.Centrality")
sub_net_by = "Node"

shinyUI(pageWithSidebar(
  headerPanel(title="Social Network Visualization of Football"),
  
  sidebarPanel(
    h3("Node Info"),
    selectInput("node_sel", "Node", nodes, selected = '1', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
    textOutput("node_university"),
    textOutput("node_conference"),
    tags$head(tags$style(".shiny-text-output{
                                 font-size: 15px;
                         font-style: italic;
                         font-weight: bold
                         }
                         p {
                         text-indent: 50px;
                         font-size:20px
                         }
                         "
    )),
    hr(),

    
    
    hr(),
    fluidRow()
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Background",
               br(),
               br(),
               p("The file football.gml contains the network of American football games
                  between Division IA colleges during regular season Fall 2000, as compiled
                 by M. Girvan and M. Newman.  The nodes have values that indicate to which
                 conferences they belong.")),

      #tabPanel("Conference Comparsion",plotOutput("comparsion_barplot"),height="700px"),
      tabPanel("Cluster",plotOutput("cluster_barplot"),height="600px"),
      tabPanel("Community",
               plotOutput("community_plot", height="400px")#,
               #div(p("Statistics"), tableOutput("subStat") , style="margin-top:300px")
      ),
      tabPanel("blockmodel",
               textInput("textInput3","Model Number",value="9"),
               plotOutput("blockmodel_barplot",height="600px"),
              plotOutput("block"))

  )
)))

