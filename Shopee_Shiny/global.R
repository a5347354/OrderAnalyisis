library(rvest)
orders = read.csv("korean10146.shopee-order.csv")
orders = orders[,-c(1,2,3,4,11,12,13,17)]
as.character.Date(orders$訂單成立時間)
#排序
orders = orders[order(orders$訂單成立時間),]
#取出寄送方式當作類別
categories = unique(orders$寄送方式) %>% as.vector()
#產品
products = c("養樂多軟糖",
             "怪獸",
             "火雞辣拌麵",
             "Enaak",
             "農心炸醬麵",
             "迷你香蕉巧克力棒",
             "預感洋芋片",
             "檸檬片",
             "黑旋風巧克力棒",
             "香蕉巧克力派",
             "粉紅品客") 
#測試
library(stringr)
find_products = function(str){
  for(j in 1:length(str)){
    for(i in 1:length(products)){
      if(grepl(products[i],str[j])){
        if(j == 1){
          items = products[i]
        }else{
          items = c(items,products[i])
        }
      }
    }  
  }
  return(items)
}
name = str_match_all(orders$商品資訊,'商品名稱:(.+); 商品選項')
name_items = str_match_all(orders$商品資訊,'選項名稱:(.+)價格')
price = str_match_all(orders$商品資訊,'NT\\$ (.+); 數量')
count = str_match_all(orders$商品資訊,'數量: (.+);')
for(i in 1:length(name)){
  df = data.frame(訂單成立時間 = orders$訂單成立時間[i],
                        商品資訊 = name[[i]][,2],
                        商品標籤 = find_products(name[[i]][,2]),
                        商品選項 = name_items[[i]][,2],
                        價格 = price[[i]][,2], 
                        數量 = count[[i]][,2])
  if(i==1){
    item_orders = df
  }else{
    item_orders = rbind(item_orders,df)
  }
}


#商品資訊
# library(rvest)
# i = 0
# url = paste0("https://shopee.tw/search/?facet=%257B%252266%2522%253A%255B-1%252C2209%255D%257D&keyword=%E9%80%B2%E5%8F%A3%E9%9B%B6%E9%A3%9F&page=",i)
# items_url = GET(url)
# items_url = content(items_url)
# items_url %>% html_nodes("#div")
