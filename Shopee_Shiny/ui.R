library(plotly)
library(shiny)
shinyUI(
  fluidPage( 
    #第一層
    titlePanel("Shopee Anaysis"),
    #第二層標籤（spilt work）
    tabsetPanel(
      tabPanel("銷售",
        fluidRow(
          splitLayout(cellWidths = c("10%","40%","50%"),
            wellPanel(
              h4('Filter:'), 
              checkboxGroupInput("categories",
                                 "Category", 
                                 categories,
                                 selected = categories)
            ),
            wellPanel(
              h4('Keyword Volume:'), 
              br(), 
              textInput("keyword",
                         "Search", width='250px', placeholder="品客" ),
              htmlOutput("volume_order") 
            ),
            wellPanel(
              dateRangeInput("dates", label = h3("Date range"),start = "2016-07-10")
            )
          )
        ),
      
        
        #第三層
        splitLayout(cellWidths = c("95%"),
          # Tab標籤
          tabsetPanel(
            tabPanel("Summary",plotlyOutput("plotly_markers")), 
            tabPanel("各小時訂單分佈",plotlyOutput("plotly_rect")), 
            tabPanel("各產品銷量及銷售額",plotlyOutput("plotly_markers_indiv")),
            tabPanel("訂單", tableOutput("itemtable"))
          ) 
        )
      ),
      
      
      
      
      
      tabPanel("熱銷商品",
               fluidRow(
                 column(12,
                   wellPanel(
                     h4('Keyword Volume:'), 
                     br(), 
                     textInput("marketplace_keyword",
                               "Search", width='250px', placeholder="品客" ),
                     htmlOutput("marketplace_volume") 
                   ),
                   tabsetPanel(
                        tabPanel("表單",dataTableOutput("marketplace_table")),
                        tabPanel("圖表",plotlyOutput("market_plotly_bar"))
                   )
                 )
               )
                 
      )
    )
  )
)