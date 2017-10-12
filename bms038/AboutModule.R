AboutUI = function(id) {
  ns = NS(id)
  tagList(
    tabsetPanel(
      tabPanel("IPOP",
               h2("Immunogenomics and Precision Oncology Platform", style = "text-align:center;"),
               fluidRow(
                 column(4,
                        "Immune-based therapies are redefining how cancer is treated, bringing better clinical outcomes into reach for more and more people. The Immunogenomics and Precision Oncology Platform (IPOP) at Memorial Sloan Kettering merges our expertise in immunology and genomics to drive innovation in this promising clinical arena.",
                        p(),
                        "IPOP is focused on understanding how the immune system recognizes and responds to cancer-specific mutations. Its mission is to develop large-scale immunogenomic discovery capabilities in partnership with clinicians and researchers within the MSK community, as well as industry collaborators.",
                        p(),
                        "Learn more at",
                        tags$a(href="https://www.mskcc.org/research-areas/programs-centers/immunogenomics-and-precision-oncology-platform", "mskcc.org/ipop")
                 ),
                 column(8,
                        #                     h3("Immunogenomics and Precision Oncology Platform"),
                        tags$img(srcset="https://www.mskcc.org/sites/default/files/styles/width_300/public/node/133603/3x2/team2_1200x800.jpg 300w,https://www.mskcc.org/sites/default/files/styles/width_500/public/node/133603/3x2/team2_1200x800.jpg 500w,https://www.mskcc.org/sites/default/files/styles/width_700/public/node/133603/3x2/team2_1200x800.jpg 700w", height = "185%", width = "85%")
                        #                 htmlOutput(ns("Ipop_Team"))
                 )
               )
      ),
      tabPanel("Contact",
               h3("Corresponding Authors"),
               "Nils Weinhold",
               tags$a(href="mailto:weinholn@mskcc.org","weinholn@mskcc.org"),
               p(),
               "Timothy A. Chan",
               tags$a(href="mailto:chant@mskcc.org","chant@mskcc.org"),
               h3("Development"),
               "Sviatoslav Kendall",
               tags$a(href="mailto:kendalls@mskcc.org","kendalls@mskcc.org")
      ),
      tabPanel("R Shiny",
               "Companion website developed using",
               tags$a(href="http://shiny.rstudio.com", "R Shiny")
      )
    )
  )
}
About = function(input, output, session) {
  ns = session$ns
  output$Ipop_Team = renderUI({
    tags$img(srcset="https://www.mskcc.org/sites/default/files/styles/width_300/public/node/133603/3x2/team2_1200x800.jpg 300w,https://www.mskcc.org/sites/default/files/styles/width_500/public/node/133603/3x2/team2_1200x800.jpg 500w,https://www.mskcc.org/sites/default/files/styles/width_700/public/node/133603/3x2/team2_1200x800.jpg 700w")
  })
}