## import csv ，转换成数据框

library(readr)
df1 <-  data.frame (read_csv("I_TEST_SOURCE.csv",
                             locale = locale(encoding = "GB2312"))) 

## 批量转换成因子格式

for ( i in 1:4 ) {
  df1[, i] <- as.factor(df1[, i])
}

## 聚合

library(dplyr)



## 计算各dept_1的总和

d_1 <- df1 %>% 
  group_by( dept, dept_1 ) %>% 
  summarise( GMV = sum(GMV) ) %>% 
  ungroup()

## 计算各dept_2的总和

d_2 <- df1 %>% 
  group_by( dept, dept_1, dept_2) %>% 
  summarise( GMV = sum(GMV) ) %>% 
  ungroup()

## 计算各二级在 1 级中的占比
d_3 <- d_2 %>% mutate(per=0)

for ( i in d_2$dept_1 ){
  for (j in d_2$dept_2) {
    d_3$per[d_3$dept_1 == i&d_2$dept_2 == j] = round( d_2[d_2$dept_1 == i&d_2$dept_2 == j, ]$GMV / sum(d_1 [d_1$dept_1 == i, ]$GMV), 2) 
}}

## 计算各品牌级维度的总和

d_4 <- df1 %>% 
  group_by( dept, dept_2, brand) %>% 
  summarise( GMV = sum(GMV) ) %>% 
  ungroup()

## 计算各品牌在 2 级中的占比
d_4 <- d_4 %>% mutate(per=0)

for ( i in d_4$dept_2 ){
  for (j in d_4$brand) {
    d_4$per[d_4$dept_2 == i&d_4$brand == j] = round( d_4[d_4$dept_2 == i&d_4$brand == j, ]$GMV / sum(d_2 [d_2$dept_2 == i, ]$GMV), 2) 
  }}

## 可视化计算各二级在 1 级中的占比,
## 从下图可以看出把scales 和space 都设置成free之后，
## 不仅坐标刻度不一样了，连每个分面的大小也不一样了。
## 横坐标仅显示有的数据，没有的数据在横坐标不进行显示
library(ggplot2)

p <- ggplot(d_3,aes(dept_2,per)) + 
      geom_bar( stat="identity" ) +
      facet_grid(~dept_1, scales= "free" ,space= "free" ) + 
      geom_text(aes(label = per),vjust = -0.25) 
p

q <- ggplot(d_4,aes(brand,per)) + 
  geom_bar( stat="identity" ) +
  facet_grid(~dept_2, scales= "free" ,space= "free" ) + 
  geom_text(aes(label = per),vjust = -0.25) 
q

# library(plotly)
# 
# ## plotly没有切片功能，因此可以用ggplotly进行转换
# 
# ggplotly(p)
