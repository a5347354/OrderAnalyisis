library(shiny)
library(dplyr) 
library(reshape)
library(plotly)

shinyServer(
  function(input, output) {
    #=========================================蝦皮訂單=========================================
    #------------------------------------function------------------------------------
    #function_keyword搜尋並丟出data
    data_filter <- function(goods){
      kwd = input$keyword
      #設定時間
      start_date = as.character(input$dates[1])
      end_date = as.character(input$dates[2])
      #設定時間範圍
      orders_selected = subset(goods, as.Date(goods$訂單成立時間) >= as.Date(start_date) & as.Date(goods$訂單成立時間) <= as.Date(end_date))
      if(kwd  == '' ||  is.na(kwd))
        orders_selected
      else
        #filter(appledaily,category %in% cs, grepl(kwd,content))
        orders_selected[grepl(kwd,orders_selected$商品資訊),]
    }
  
    data_filter_kwd = function(kwd){
      #設定時間
      start_date = as.character(input$dates[1])
      end_date = as.character(input$dates[2])
      #設定時間範圍
      orders_selected = subset(orders, as.Date(orders$訂單成立時間) >= as.Date(start_date) & as.Date(orders$訂單成立時間) <= as.Date(end_date))
      if(kwd  == '' ||  is.na(kwd))
        orders_selected
      else
        #filter(appledaily,category %in% cs, grepl(kwd,content))
        orders_selected[grepl(kwd,orders_selected$商品資訊),]
    }

    
    
    #------------------------------------Input------------------------------------
    #plotly_tatal
    #Tab1 總銷量
    #需用事件eventReactive，才能呈現realtime、interactive
    total_plotly = eventReactive(c(input$dates,input$keyword),{
      #取出兩欄資料-訂單成立時間以及訂單小計
      df = data.frame(訂單成立時間 = data_filter(orders)$訂單成立時間,訂單小計 = data_filter(orders)$訂單小計..TWD.)
      df$訂單成立時間 = as.character.Date(df$訂單成立時間) %>% as.POSIXct(., format="%Y-%m-%d %H:%M")
      df$訂單小計 = as.numeric(df$訂單小計)
      #訂單數量
      ordersDate_counted = format(df$訂單成立時間, "%Y-%m-%d") %>% table()
      #分組小計，目前會累加BUG
      ordersMoney_groupby = df %>% group_by(訂單成立時間) %>% summarise(訂單小計 = sum(訂單小計,na.rm=TRUE))
      
      x = list(title = "Time")
      y = list(title = "點/小時")
      plot_ly(x = format(df$訂單成立時間, "%Y-%m-%d"), 
              y = format(df$訂單成立時間, "%H"),
              mode = "markers",
              #x = names(table(ordersMoney_groupby[,1])
              #y = ordersDate_counted, 
              #text = paste("Clarity:", clarity),
              #color = categories, 
              #size = as.numeric(names(table(ordersMoney_groupby[,2])))
              size = as.numeric(df$訂單小計)) %>% layout(yaxis = y,
                                                     xaxis = x,
                                                     title = "韓吉大仔")
    })
    
    #plotly_hours
    #Tab2 每小時銷量
    #需用事件eventReactive，才能呈現realtime、interactive
    hours_plotly = eventReactive(c(input$dates,input$keyword),{
      ordersTimes = as.character.Date(data_filter(orders)$訂單成立時間) %>% as.POSIXct(., format="%Y-%m-%d %H:%M")
      ordersHours = format(ordersTimes, "%H") %>% as.numeric() %>% table()
      
      x = list(title = "點/小時")
      y = list(title = "訂單量")
      plot_ly(x = names(ordersHours),
              y = ordersHours,
              fill = "tozeroy",
              name = "shopee") %>% layout(yaxis = y, 
                                          xaxis = x , 
                                          title = "韓吉大仔")
    })
    
    
    #Tab3 個別商品
    indiv_items_plotly = reactive({
      #取出兩欄資料-訂單成立時間以及訂單小計
      df = data_filter(item_orders)
      df$訂單成立時間 = as.character.Date(df$訂單成立時間) %>% as.POSIXct(., format="%Y-%m-%d %H:%M")
      df$價格 = df$價格
      df$數量 = as.numeric(df$數量)
      
      x = list(title = "日期")
      y = list(title = "價錢")
      cols <- RColorBrewer::brewer.pal(nlevels(df$商品標籤), "Set1")
      plot_ly(x = format(df$訂單成立時間, "%Y-%m-%d"),
              y = df$價格 ,
              mode = "markers",
              #x = names(table(ordersMoney_groupby[,1])
              #y = ordersDate_counted,
              #text = paste("Clarity:", clarity),
              # color = df$商品標籤,
              # colors = cols,
              group = df$商品標籤,
              #size = as.numeric(names(table(ordersMoney_groupby[,2])))
              size = df$數量
              )  %>% layout(yaxis = y,
                                          xaxis = x,
                                          title = "韓吉大仔")
    })
    
    
    #表格Table
    delt_orders = reactive({
      data_filter(orders)
    })

  
    
    #------------------------------------Output------------------------------------
    #表格Table
    output$itemtable <- renderTable({ 
        delt_orders()
      })
    
    output$plotly_rect <- renderPlotly({
        hours_plotly()
    })
    
    output$plotly_markers_indiv <- renderPlotly({
        indiv_items_plotly()
    })
    
    output$plotly_markers  <- renderPlotly({
        total_plotly()
    })
    
    
    
    
    
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #=========================================蝦皮熱銷=========================================
    #------------------------------------function------------------------------------
    # market_data_filter <- function(){
    #   kwd = input$marketplace_keyword
    # 
    #   if(kwd  == '' ||  is.na(kwd))
    #     items
    #   else
    #     items[grepl(kwd,items$name),]
    #   
    #   colnames(items) = c("商品名稱","價錢","存貨","評價","讚數","星星評價")
    #   return(items)
    # }
    # 
    # #------------------------------------Input------------------------------------
    # #表格Table
    # delt_market_items = reactive({
    #   market_data_filter()
    # })
    # 
    # #
    # hours_plotly = eventReactive(c(input$keyword),{
    #   
    #   x = list(title = "點/小時")
    #   y = list(title = "訂單量")
    #   plot_ly(x = names(ordersHours),
    #           y = ordersHours,
    #           fill = "tozeroy",
    #           name = "shopee") %>% layout(yaxis = y, 
    #                                       xaxis = x , 
    #                                       title = "韓吉大仔")
    # })
    # 
    # #------------------------------------Output------------------------------------
    # 
    # output$marketplace_table <- renderDataTable({
    #   delt_market_items()
    # })
    # 
    # 
    # 
    
  }
)

