######highcharter########

library("dplyr")
library("highcharter")
library("magrittr")

col <- colorize(runif(10)) #Create vector of color from vector
color_classes(c(0, 10, 20)) #Function to create dataClasses argument in hc_colorAxis

highchart() %>%
  hc_yAxis_multiples(create_yaxis(naxis=2,heights=c(2,1), lineWidth=2)) %>%
  hc_add_series(data=c(1,3,2), yAxis=0) %>%
  hc_add_series(data=c(20, 40, 10), yAxis=1)

#?
datetime_to_timestamp(as.Date("2017-03-05", format="%Y-%m-%d"))

hcaes(x= xval, color=colorvar, group=grvar)


####################################################################################

highchart() %>% 
  hc_title(text = "A nice chart") %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  hc_add_series(data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2,
                        26.5, 23.3, 18.3, 13.9, 9.6)) %>%
  hc_tooltip(crosshairs=TRUE, shared=TRUE) %>%
  hc_yAxis(minorGridLineWidth = 0, gridLineWidth = 0,
           plotBands = list(
             list(from = 10, to = 20, color = "rgba(68, 170, 213, 0.1)", #淺藍
                  label = list(text = "A low band")),
             list(from = 20, to = 25, color = "rgba(0, 0, 0, 0.1)", #淺灰
                  label = list(text = "A medium band")),
             list(from = 25, to = 30, color = "rgba(68, 170, 213, 0.1)",
                  label = list(text = "A high band"))
           ))
  
  
# legend
highchart() %>%
  hc_title(text="Example", style=list(color="#B71C1C")) %>% #暗紅色
  hc_chart(type="line") %>% # column
  hc_xAxis(title=list(text="Year"), categories=id$X) %>%
  hc_add_series(data=id$F, name="Female", color="#1FA67A") %>% #綠色
  hc_add_series(data=id$M, name="Male") %>%
  hc_legend(align = "left", verticalAlign = "top",layout = "horizontal")

## 字體 顏色 主標題 副標題
citytemp
class(citytemp)
highchart() %>%
  hc_title(text="This is <i>斜體</i> and <b>粗體</b>", align = "left",
           style = list(color = "#90ed7d")) %>%
  hc_xAxis(categories= citytemp$month) %>%
  hc_add_series(citytemp$tokyo, name="Tokyo", color=col[1]) %>%
  hc_add_series(citytemp$london, name="London", type="column", color=col[2])  %>%
  hc_add_series(citytemp$new_york, name="New York", color=col[3]) %>%
  hc_subtitle(text = "Citytemp",
              align = "right", 
              style=list( color = "#2b908f", fontWeight = "bold")) #位置靠右



## 3D chart
highchart() %>%
  hc_title(text="citytemp") %>%
  hc_xAxis(categories= citytemp$month) %>%
  hc_add_series(citytemp$tokyo, name="Tokyo") %>%
  hc_add_series(citytemp$london, name="London")  %>%
  hc_chart(type = "column",
         options3d = list(enabled = TRUE, beta = 15, alpha = 15))


## Time series
highchart() %>% 
  hc_title(text = "Monthly Deaths from Lung Diseases in the UK") %>% 
  hc_subtitle(text = "Deaths from bronchitis, emphysema and asthma") %>% 
  hc_add_series_ts(fdeaths, name = "Female") %>%
  hc_add_series_ts(mdeaths, name = "Male") %>%
  hc_xAxis(title=list(text="year"))

# Scatter plot
cars
highchart() %>% 
  hc_add_series_scatter(cars$speed, cars$dist) %>%
  hc_xAxis(title=list(text="speed")) %>%
  hc_yAxis(title=list(text="dist"))

# 泡泡圖
mtcars
highchart() %>% 
  hc_add_series_scatter(mtcars$wt, mtcars$mpg, mtcars$cyl) %>% 
  hc_chart(zoomType = "xy") %>% 
  hc_title(text = "Motor Trend Car Road Tests") %>% 
  hc_xAxis(title = list(text = "Weight"), minorTickInterval = "auto") %>% 
  hc_yAxis(title = list(text = "Miles/gallon")) %>% 
  hc_tooltip(headerFormat = "<b>{series.name} cylinders</b><br>",
             pointFormat = "{point.x} (lb/1000), {point.y} (miles/gallon)")
  # 定義 當滑標指到時顯示的方式

highchart() %>% 
  hc_add_series_scatter( mtcars$wt, mtcars$mpg, mtcars$drat, mtcars$qsec,
                      name = rownames(mtcars), gear = mtcars$gear) %>%
  hc_tooltip(pointFormat = "<b>{point.name}</b><br/>Gear: {point.gear}")








