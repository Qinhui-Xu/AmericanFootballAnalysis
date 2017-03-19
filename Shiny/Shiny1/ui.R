library(igraph)
library(ggplot2)
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
#    hr(),
#    textInput("UniversityName","University Search", value=" "),
#    textOutput("nodeID"),
    hr(),
    selectInput("conference_rm", "Remove Conference", c("None",remove), selected = '', multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
    hr(),
#    selectInput("c_rank", "Centrality Rank", cntrlty, selected = 'None', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
#    hr(),
    selectInput("groupby", "Group By", groubys, selected = 'None', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
    selectInput("c_rank", "Centrality Rank", cntrlty, selected = 'None', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
    hr(),
    div(style="display:inline-block",selectInput("subnet", "Sub_Net_By", sub_net_by, selected = 'Node', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
    div(style="display:inline-block; width:80px",uiOutput("subControls")),
#    uiOutput("grControls"),
#    uiOutput('Training'),
#    uiOutput('Validation'),
    
    
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
                 conferences they belong."),
               plotOutput("overview",height="500px")),
      tabPanel("Network",
               h4("Whole-Network"),
               plotOutput("whole_net_plot", height="500px"),
               tableOutput("centrality_table")
      ),

      tabPanel("Centrality Plot",
               textInput("textInput2","centralityValue",value="0"),
               plotOutput("cent_plot",height="500px"),
               plotOutput("cent")),#,
               #tableOutput("top10cent")),
#      tabPanel("Betwenness Plot",
#               textInput("textInput1","betweenessValue",value="150"),
#               plotOutput("betweness_plot", height="500px"),plotOutput("betweeness",height="300px")),
      tabPanel("Sub-Network",
               plotOutput("sub_net_plot", height="500px"),
               tableOutput("stat")
               #div(p("Statistics"), tableOutput("subStat") , style="margin-top:300px"),
               ),
      tabPanel("Conference Comparsion",plotOutput("comparsion_barplot"),height="700px")
      #tabPanel("Cluster",plotOutput("cluster_barplot"),height="700px")#,
      #tabPanel("blockmodel",
      #          textInput("textInput3","Model Number",value="6"),
      #          plotOutput("blockmodel_barplot"),height="700px")
  )
)))

