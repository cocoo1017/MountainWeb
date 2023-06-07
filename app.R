library(shiny)
library(shinydashboard)
library(readxl)
library(leaflet)
library(magrittr)
library(stringr)
library(dplyr)
library(rvest)
library(DT)
library(dashboardthemes)
library(ggplot2)
library(showtext)

showtext.auto(enable = T)
font.add('msjh', 'msjh.ttc')

humidity_df <- read_excel("1.xlsx")
temp_df <- read_excel("2.xlsx")
precp_df <- read_excel("3.xlsx")
airp_df <- read_excel("4.xlsx")
wind_df <- read_excel("5.xlsx")
mount_df <- read_excel("6.xlsx")
info_df <- read_excel("7.xlsx")
disa_df <- read_excel("9.xlsx")

m_choices <- as.vector(info_df$name)

ui <- dashboardPage(
  dashboardHeader(
    title = shinyDashboardLogo(
      theme = "blue_gradient",
      boldText = NULL,
      mainText = "登山資訊視覺化網站系統",
      badgeText = "v1.1"
    )
  ),
  dashboardSidebar(
    selectInput(
      'select1',
      label = '請選取欲攀登的山',
      choices = m_choices,
      selected = 1,
      multiple = F
    ),
    sidebarMenu(
      menuItem(
        '使用說明',
        tabName = 'id1',
        icon = icon("fas fa-address-book")
      ),
      menuItem(
        '登山資訊與山難統計圖表',
        tabName = 'id2',
        icon = icon("fas fa-mountain")
      ),
      menuItem(
        '10天天氣預報與登山建議',
        tabName = 'id3',
        icon = icon("fas fa-temperature-low")
      )
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    tags$head(tags$style(
      HTML(
        '.main-header .logo {
      #font-family: serif;
      font-weight: bold;
      font-size: 20px;
      }'
      )
    )),
    tags$head(tags$style(
      HTML(".main-sidebar {
      font-weight: bold;
      font-size: 12px;
      }")
    )),
    tags$head(tags$style(
      "#text_how{font-weight: bold; font-size: 12px;}"
    )),
    tags$head(tags$style(
      "#text_warn{font-weight: bold; font-size: 12px;}"
    )),
    tags$head(tags$style(
      "#text_info{font-weight: bold; font-size: 10px;}"
    )),
    tags$head(tags$style(
      "#m_map{font-weight: bold; font-size: 11px;}"
    )),
    tags$head(tags$style(
      "#m_weather{font-weight: bold; font-size: 10px;}"
    )),
    tabItems(
      tabItem(tabName = 'id1',
              fluidRow(column(
                width = 12 ,
                box(
                  width = 20,
                  status = "primary",
                  verbatimTextOutput('text_how')
                )
              ))),
      tabItem(tabName = 'id2',
              fluidRow(
                column(width = 7 ,
                       box(
                         width = 20,
                         status = "primary",
                         dataTableOutput('text_info', height = '500')
                       )),
                column(width = 5 ,
                       box(
                         width = 20,
                         status = "primary",
                         leafletOutput('m_map', width = '100%', height =
                                         '500')
                       )),
                column(width = 10 ,
                       box(
                         width = 20,
                         status = "primary",
                         plotOutput('plot_disa', height = '300')
                       ))
              )),
      tabItem(tabName = 'id3',
              fluidRow(
                column(width = 10 ,
                       box(
                         width = 20,
                         status = "primary",
                         verbatimTextOutput('text_warn')
                       )),
                column(
                  width = 12 ,
                  box(
                    width = 20,
                    height = '430',
                    status = "primary",
                    dataTableOutput('m_weather', height = '430')
                  )
                ),
                column(
                  width = 7 ,
                  tabsetPanel(
                    tabPanel('相對溼度', plotOutput('plot_humidity', height = '300px')),
                    tabPanel('氣溫', plotOutput('plot_temp', height = '300px')),
                    tabPanel('降雨量', plotOutput('plot_precp', height = '300px')),
                    tabPanel('氣壓', plotOutput('plot_airp', height = '300px')),
                    tabPanel('風速', plotOutput('plot_wind', height = '300px'))
                  )
                )
              ))
    )
  )
)

m_name <-
  c(
    '山名',
    '測站',
    '難易程度',
    '海拔',
    '登山耗時',
    '近2年山難次數',
    '接駁車',
    '相關山屋',
    '相關路線',
    '所屬園區',
    '入園證/入山證',
    '行政區'
  )
m_month <-
  c('1月',
    '2月',
    '3月',
    '4月',
    '5月',
    '6月',
    '7月',
    '8月',
    '9月',
    '10月',
    '11月',
    '12月')
server <- function(input, output, session) {
  x <- reactive({
    input$select1
  })
  output$text_how <- renderText({
    print(
      '請選取欲攀登的山:
第二個分頁將呈現此座山的相關資訊、地圖(點選座標可看到難易分級資訊)與近5年重大山難統計圖表。
第三個分頁將呈現此座山近15天的天氣預報資訊、上山建議與近5年天氣統計圖表'
    )
  })
  output$text_info <- renderDataTable({
    z <- as.data.frame(info_df[, 1:12][which(info_df$name == x()), ])
    colnames(z) <- m_name
    DT::datatable(t(z))
  })
  output$plot_disa <- renderPlot({
    r <- data.frame(sort(table(disa_df$event)))
    ggplot(r, aes(x = Var1, y = Freq)) +
      labs(title = '近五年重大山難統計圖表', x = '態樣', y = '件數') +
      ylim(c(0, 25)) +
      theme(
        plot.title = element_text(
          size = 20,
          face = 'bold',
          family = 'msjh'
        ),
        axis.text = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        ),
        axis.title = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        )
      ) +
      geom_bar(stat = 'identity',
               fill = 'steelblue4',
               width = .5)
  })
  output$plot_humidity <- renderPlot({
    h <- as.data.frame(humidity_df)
    ggplot(h, aes(x = month, y = h[, x()])) +
      labs(title = '五年月平均相對溼度', x = '月份', y = '相對溼度(%)') +
      ylim(c(0, 100)) +
      theme(
        plot.title = element_text(
          size = 20,
          face = 'bold',
          family = 'msjh'
        ),
        axis.text = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        ),
        axis.title = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        )
      ) +
      scale_x_discrete(limits = month.abb) +
      geom_bar(stat = 'identity',
               fill = 'steelblue4',
               width = .5)
  })
  output$plot_temp <- renderPlot({
    t <- as.data.frame(temp_df)
    ggplot(t, aes(x = month, y = t[, x()])) +
      labs(title = '五年月平均氣溫', x = '月份', y = '溫度(°C)') +
      ylim(c(0, 30)) +
      theme(
        plot.title = element_text(
          size = 20,
          face = 'bold',
          family = 'msjh'
        ),
        axis.text = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        ),
        axis.title = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        )
      ) +
      scale_x_discrete(limits = month.abb) +
      geom_line(aes(group = x()),
                stat = 'identity',
                size = 1,
                colour = 'steelblue4') +
      geom_point(colour = 'steelblue4')
  })
  output$plot_precp <- renderPlot({
    p <- as.data.frame(precp_df)
    ggplot(p, aes(x = month, y = p[, x()])) +
      labs(title = '五年月平均降雨量', x = '月份', y = '降雨量(mm)') +
      ylim(c(0, 800)) +
      theme(
        plot.title = element_text(
          size = 20,
          face = 'bold',
          family = 'msjh'
        ),
        axis.text = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        ),
        axis.title = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        )
      ) +
      scale_x_discrete(limits = month.abb) +
      geom_bar(stat = 'identity',
               fill = 'steelblue4',
               width = .5)
  })
  output$plot_airp <- renderPlot({
    a <- as.data.frame(airp_df)
    ggplot(a, aes(x = month, y = a[, x()])) +
      labs(title = '五年月平均氣壓', x = '月份', y = '氣壓(hPa)') +
      ylim(c(0, 1000)) +
      theme(
        plot.title = element_text(
          size = 20,
          face = 'bold',
          family = 'msjh'
        ),
        axis.text = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        ),
        axis.title = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        )
      ) +
      scale_x_discrete(limits = month.abb) +
      geom_line(aes(group = x()),
                stat = 'identity',
                size = 1,
                colour = 'steelblue4') +
      geom_point(colour = 'steelblue4')
  })
  output$plot_wind <- renderPlot({
    w <- as.data.frame(wind_df)
    ggplot(w, aes(x = month, y = w[, x()])) +
      labs(title = '五年月平均風速', x = '月份', y = '風速(m/s)') +
      ylim(c(0, 15)) +
      theme(
        plot.title = element_text(
          size = 20,
          face = 'bold',
          family = 'msjh'
        ),
        axis.text = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        ),
        axis.title = element_text(
          size = 15,
          face = 'bold',
          family = 'msjh'
        )
      ) +
      scale_x_discrete(limits = month.abb) +
      geom_line(aes(group = x()),
                stat = 'identity',
                size = 1,
                colour = 'steelblue4') +
      geom_point(colour = 'steelblue4')
  })
  output$m_weather <- renderDataTable({
    w <- as.data.frame(info_df[which(info_df$name == x()), ])
    
    webpage <- read_html(w$url)
    
    forecast <- webpage %>%
      html_elements("div")
    
    forecast_date <- c()
    forecast_temp <- c()
    forecast_content <- c()
    forecast_precp <- c()
    forecast_humidity <- c()
    forecast_wind <- c()
    
    for (p in forecast) {
      p_date <- p %>%
        html_element(".DailyContent--daypartDate--2A3Wi") %>%
        html_text2()
      forecast_date <- append(forecast_date , p_date)
      
      p_temp <- p %>%
        html_element(".DailyContent--temp--3d4dn") %>%
        html_text2()
      forecast_temp <- append(forecast_temp , p_temp)
      
      p_content <- p %>%
        html_element(".DailyContent--narrative--hplRl") %>%
        html_text2()
      forecast_content <- append(forecast_content , p_content)
      
      p_precp <- p %>%
        html_element(".DailyContent--value--37sk2") %>%
        html_text2()
      forecast_precp <- append(forecast_precp , p_precp)
      
      p_humidity <- p %>%
        html_element(".DetailsTable--value--1q_qD") %>%
        html_text2()
      forecast_humidity <- append(forecast_humidity , p_humidity)
      
      p_wind <- p %>%
        html_element(".Wind--windWrapper--3aqXJ.DailyContent--value--37sk2") %>%
        html_text2()
      forecast_wind <- append(forecast_wind , p_wind)
      
    }
    
    df_forecast_0 <- data.frame(
      forecast_date,
      forecast_temp,
      forecast_content,
      forecast_precp,
      forecast_humidity,
      forecast_wind
    )
    df_forecast <-
      df_forecast_0[complete.cases(df_forecast_0),] %>%
      distinct ()
    
    w1 <- str_replace_all(df_forecast$forecast_temp, '°' , ' ') %>%
      as.numeric()
    w2 <- str_replace_all(df_forecast$forecast_precp, '%' , ' ') %>%
      as.numeric()
    w3 <-
      str_replace_all(df_forecast$forecast_humidity, '%' , ' ') %>%
      as.numeric()
    w4 <-
      gsub(".* ([0-9]+) km/h", "\\1", df_forecast$forecast_wind) %>%
      as.numeric()
    
    df_forecast <- df_forecast %>% mutate(
      warn = case_when(
        w1 > 26 ~ '氣溫較高，請注意中暑等情況。',
        w1 < 10 ~ '氣溫較低，請加穿衣物。',
        w2 > 30 & w3 > 80 ~ '降雨機率稍高，請留意天氣預報，並請注意腳下濕滑。',
        w4 > 9 ~ '風較大，如要上山請注意自身保暖。',
        T ~ '祝平安歸來~'
      )
    )
    
    colnames(df_forecast) <-
      c('日期', '氣溫', '天氣型態', '降雨機率', '濕度', '風向/風速', '上山建議')
    
    DT::datatable(df_forecast, rownames = FALSE)
  })
  output$text_warn <- renderText({
    print(
      '1.氣溫低於10°C時，請加穿衣物。
2.氣溫高於26°C時，請注意中暑等狀況。
3.降雨機率高於30%且相對溼度高於80%，請留意天氣預報，並請注意腳下濕滑。
4.風速高於10km/h為強風，如要上山請注意自身保暖。
          '
    )
  })
  output$m_map <- renderLeaflet({
    color <- function(mount_df) {
      sapply(mount_df$diff1, function(diff1) {
        if (diff1 == 1) {
          "green"
        } else if (diff1 == 2) {
          "orange"
        } else {
          "red"
        }
      })
    }
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = color(mount_df)
    )
    s <- as.data.frame(mount_df[which(mount_df$name == x()), ])
    leaflet(data = mount_df) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = s$lng ,
              lat = s$lat,
              zoom = 13) %>%
      addAwesomeMarkers(
        ~ lng,
        ~ lat,
        popup = ~ as.character(diff),
        label = ~ as.character(name),
        icon = icons
      )
  })
}


shinyApp(ui = ui, server = server)