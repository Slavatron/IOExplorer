

library(shiny)
library(htmltools)
# Define UI 
fluidPage(

  # HEADER
  headerPanel(
    list(
      fluidRow(
        column(2,
               imageOutput("header_image",height=100,width=100)),
        column(10,h2("BMS-038 Companion Website",br(),"IPOP portal for Immunotherapy in collaboration with cBio"))),
      hr())
    ),
    sidebarPanel(width=3,
      
# Sidebar with controls to select samples
# Need to figure out how to get these trees to self-populate based on the uploads
      selectInput("study","STUDY:",
                  c("Riaz - Melanoma" = "bms_038",
                    "Sims - Glioma" = "cumc_gbm")
      ),
######## CONDITIONAL PANELS ABOUT THE SAMPLES FROM EACH GROUP #######   -- NEED TO MAKE THESE PULL FROM A DATABASE  
      conditionalPanel("input.study == 'bms_038'",
          radioButtons("sample_name", "SAMPLE ID:",
                       c("Pt3_on" = "Pt3_on_S0089498",
                         "Pt3_pre" = "Pt3_pre_S0089487",
                         "Pt5_on" = "Pt5_on_S0089494",
                         "Pt5_pre" = "Pt5_pre_S0089505")
          )
      ),
      conditionalPanel("input.study == 'cumc_gbm'",
                       radioButtons("sample_name", "SAMPLE ID:",
                                    c("L04_PBMC_A" = "TCR22A.BC1",
                                      "L04_PBMC_B" = "TCR23B.BC2",
                                      "L04_TIL_A" = "TCR23A.BC3",
                                      "L04_TIL_B" = "TCR22B.BC4",
                                      "G13_PBMC_A" = "TCR111A.BC1",
                                      "G13_PBMC_B" = "TCR106B.BC2",
                                      "G13_TIL_A" = "TCR106A.BC1",
                                      "G13_TIL_B" = "TCR105B.BC2")
                        )
      ),

hr()
),
      
    # Show a tabset: View the distribution,
    mainPanel(width = 9,
      tabsetPanel(type = "tabs",
###################### TAB: FREQUENCY DISTRIBUTION ###################
                  tabPanel("Frequency Distribution",
                  wellPanel(width = 9,style = "background-color: #ffffff;",
                    fluidRow(
                      column(width = 3,align = "left",
                             h4(textOutput("sample_name")),
                             hr(),
                             selectInput("rep_type", label=NULL,
                                         list("VJcombos", 
                                              "aaCDR3", 
                                              "totCDR3",
                                              "ntCDR3"))
                            ),
                      column(width = 3,
                             selectInput("plot_type",label=NULL,
                                         list("Distribution", 
                                              "Histogram")),
                          radioButtons("Ylog", label=NULL,
                                        c("Log" = "log",
                                          "Linear" = "lin"),width='150px',inline=TRUE),
                          downloadButton("downloadPlot", "Download Plot",plotOutput("distplot"))
                            ),
                      column(width = 3,
                          conditionalPanel("input.plot_type == 'Histogram'",
                              sliderInput("n","Number of Bins",
                                  min = 10, max = 30, value = 20)
                                           
#                          ) 
                                                      ))
                            ),
                    fluidRow(
                          column(width = 9,
                                 plotOutput("plot1"))
                            ),
                    hr(),
                    fluidRow(
                      column(width = 2,
                             "Reads:",textOutput("TotalReads")),
                      column(width = 2,
                             "Unique:",textOutput("num")),
                      column(width = 1,
                             "N50:",textOutput("N50")),
                      column(width = 1,
                             "N90:",textOutput("N90")),
                      column(width = 2,
                             "Entropy:",textOutput("ent")),
                      column(width = 2,
                             "Clonality:",textOutput("clon"))
                    )
                    ),
                    selectInput("tutorial",label=NULL,
                                list("Hide Tutorial" = "hide",
                                     "Show Tutorial" = "show")),
                    conditionalPanel("input.tutorial == 'show'",
                        wellPanel(width = 9,style = "background-color: #ffffff;",style = "overflow-x:scroll; max-height: 300px",
                           imageOutput("tutorial_img",width = "800px",height = "200px"),
                           textOutput("tutorial_text")
                              )
                    ),
                      downloadButton("downloadTable", "Download Table"),
                      wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                      DT::dataTableOutput("table"))
),
############## TAB: DISTRIBUTION OF TCR COMPONENTS ###################
                  tabPanel("Distribution of TCR Components",
                    wellPanel(width = 9,style = "background-color: #ffffff;",
                              fluidRow(
                                column(width = 3,align = "left",
                                       h4("Sample Name"),
                                       selectInput("component_type", label=NULL,
                                                   list("AAperVJ", 
                                                        "VJperAA"))
                  ),
                         column(width = 3,align = "center",
                                selectInput("Hdist_type",label=NULL,
                                            list("DotPlot",
                                                 "Density"))
                                ),
                          column(width = 3,align = "right",
                                downloadButton("downloadPlot3", "Download Plot",plotOutput("component_Hhist"))
                         )        
                  ),
#                    h3(textOutput("component_type"))),
                  fluidRow(
                    column(width = 9,align="center",
                           plotOutput("component_Hdist",width="500px",height="300px"),
                           "Median number: ",textOutput("med_T2perT1",inline=TRUE))
                  ),
hr(),
                  fluidRow(
                    column(width = 6,
                           plotOutput("component_Hhist",width="350px",height="300px")),
                    column(width = 3,
                           sliderInput("n2","Number of Bins",
                                       min = 10, max = 30, value = 20),
                           "Mean entropy: ",textOutput("mean_HT2perT1",inline=TRUE),
                           downloadButton("downloadPlot2", "Download Plot",plotOutput("component_Hhist"))
                    ))),
                    downloadButton("downloadTable2", "Download Table"),
                    wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                    DT::dataTableOutput("table2"))
                    
                ),
###################### TAB: USAGE OF TCR COMPONENTS ###############################
           tabPanel("Usage of TCR components",
                      wellPanel(width = 9,style = "background-color: #ffffff;",
            p("[ CUPCAKE REWARD FOR ANYONE WHO CAN PUT A CORRELATION CLUSTERGRAM OF V & J CASSETTE USAGE HERE ]"),
            radioButtons("cassette_col", label="Sort by columns:",
                         c("V" = "vcass",
                           "J" = "jcass"),width='150px',inline=TRUE),
            downloadButton("download_vjtable", "Download Table"),
#            DT::dataTableOutput("vj_table")
            wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
              DT::dataTableOutput("vj_table")
            ),
            hr(),
            p("[ WHOLE CAKE REWARD TO ANYONE WHO CAN RECREATE THE BLOSUM+SEQALIGN 'ImmunoMap' ALGORITHM AND PUT A DENDROGRAM HERE. A CAKE AND A PIE IF YOU DO IT BEFORE THEIR PAPER GETS ACCEPTED ANYWHERE, WHICH I DOUBT WILL BE HARD.")
                  )))
    )
  )