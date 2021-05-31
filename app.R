if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shiny")) install.packages("shiny")

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")
if (!require("stringr")) install.packages("stringr")
if (!require("data.table")) install.packages("data.table")
if (!require("plotly")) install.packages("plotly")
if (!require("magrittr")) install.packages("magrittr")
if (!require("rlist")) install.packages("rlist")

library("shinydashboard")
library("shiny")
library("ggplot2")
library("dplyr")
library("scales", include.only = "rescale")
library("stringr", include.only = "str_detect")
library("plotly", include.only = c("plotlyOutput", "renderPlotly", "ggplotly"))
library("magrittr", include.only = "%>%")


## Загрузка данных SCOPUS ----
scopus_good_big_table_subset_cloumns <- data.table::fread("to_app.csv")
rosrid_all_years <- data.table::fread("rosrid_all_years.csv")
pokazateli <- data.table::fread("pokazateli.csv")

scopus_subjects <- read.csv("subjects_to_app.csv", header = F)
grnti_subjects <- read.csv("rosrid_all_years_grnti.csv", header = F)
colnames(scopus_subjects) <- " "
colnames(grnti_subjects) <- " "

title <- scopus_subjects
code <- scopus_subjects
names(code) <- title

## Загрузка данных ПХВД ----
phvd_vuzi <- rlist::list.load("phvd_vuzi.rds")


num_codes_dohodi <- cbind(
  c(1:9, 11, 12),
  phvd_vuzi$БашГУ[c(1:9, 11, 12), 2]
)
num_codes_rasvodi <- cbind(13:22, phvd_vuzi$БашГУ[13:22, 2])

## Загрузка данных по субсидиям ----
subsidii <- rlist::list.load("subsidii.rds")





# UI ---------------------------------------------------------------------------
ui <- dashboardPage(
  ## dashboardHeader ====
  dashboardHeader(title = "Рейтингование вузов"),

  ## sidebarMenu ====
  dashboardSidebar(
    sidebarMenu(
      menuItem("Наукометрия",
        tabName = "nauka", icon = icon("flask"),
        menuSubItem("Общие показатели", tabName = "obsch"),
        menuSubItem("Частные показатели", tabName = "chastn"),
        menuSubItem("Обобщенная метрика", tabName = "metr")
      ),
      menuItem("Экономика",
        tabName = "econ", icon = icon("money-bill-wave"),
        menuSubItem("Динамика доходы", tabName = "econ_din_dohodi"),
        menuSubItem("Динамика расходы", tabName = "econ_din_rashodi"),
        menuSubItem("Финансирование, 2019", tabName = "econ_fin"),
        menuSubItem("МТБ, 2019", tabName = "econ_mtb"),
        menuSubItem("Распределение по грантам", tabName = "grant")
      ),
      menuItem("Изобретения", tabName = "rosrid", icon = icon("satellite")),
      checkboxGroupInput("vuz_vibor", "Выбор вуза:",
        c(
          "БашГУ" = "БашГУ",
          "ИТМО" = "ИТМО",
          "КФУ" = "КФУ",
          "МФТИ" = "МФТИ",
          "НГУ" = "НГУ",
          "РГУ" = "РГУ",
          "САФУ" = "САФУ",
          "СПб Горный" = "СПб Горный",
          "СПбПУ" = "СПбПУ",
          "ТИУ" = "ТИУ",
          "ТПУ" = "ТПУ",
          "ТюмГУ" = "ТюмГУ",
          "УГНТУ" = "УГНТУ"
        ),
        selected = c(
          "БашГУ", "ИТМО", "КФУ", "МФТИ", "НГУ", "РГУ",
          "САФУ", "СПб Горный", "СПбПУ", "ТИУ", "ТПУ",
          "ТюмГУ", "УГНТУ"
        )
      )
    )
  ),

  ## dashboardBody ====
  dashboardBody(
    ### Styling ####
    tags$head(tags$style(HTML("
                                div.box-header {
                                  text-align: center;
                                }
                                "))),
    tabItems(

      ### Общие показатели ####
      tabItem(
        tabName = "obsch",
        fluidRow(
          box(
            width = 12,
            selectInput("select_year_obsch", h3("Рассматриваемый год"),
              choices = list(
                "2015" = 2015, "2016" = 2016,
                "2017" = 2017, "2018" = 2018,
                "2019" = 2019, "2020" = 2020,
                "2015-2020" = 0
              ), selected = 2020
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Общее число публикаций в журналах",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot1.1")
            ),
            box(
              title = "Среднее количество цитирований",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot1.2")
            )
          ),
          column(
            width = 6,
            box(
              title = "Общее число публикаций в журналах Qi",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot1.3")
            ),
            box(
              title = "Среднее количество цитирований в журналах Qi",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot1.4")
            )
          )
        )
      ),
      ### Частные показатели ####
      tabItem(
        tabName = "chastn",
        fluidRow(
          box(
            width = 4,
            selectInput("select_year_chastn", h3("Рассматриваемый год"),
              choices = list(
                "2015" = 2015, "2016" = 2016,
                "2017" = 2017, "2018" = 2018,
                "2019" = 2019, "2020" = 2020,
                "2015-2020" = 0
              ), selected = 2020
            )
          ),
          box(
            width = 8,
            selectizeInput(
              "multi_subject_input", h3("Направление"),
              choices = scopus_subjects, multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Показатель цитируемости университета, взвешенный
              по предметной области (FWCI) по базе данных Scopus
              по направлениям классификации Scopus",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot2.1")
            ),
            box(
              title = "Общее количество публикаций по базе данных Scopus
              по направлениям классификации Scopus",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot2.2"),
            )
          ),
          column(
            width = 6,
            box(
              title = "Показатель цитируемости университета, взвешенный
              по предметной области (FWCI) в журналах Qi по базе данных Scopus
              по направлениям классификации Scopus",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot2.3")
            ),
            box(
              title = "Общее количество публикаций в журналах Qi
              по базе данных Scopus по направлениям классификации Scopus",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot2.4")
            )
          )
        )
      ),

      ### Обобщенная метрика ####
      tabItem(
        tabName = "metr",
        fluidRow(
          box(
            width = 4,
            selectInput("select_year_metr", h3("Рассматриваемый год"),
              choices = list(
                "2015" = 2015, "2016" = 2016,
                "2017" = 2017, "2018" = 2018,
                "2019" = 2019, "2020" = 2020,
                "2015-2020" = 0
              ), selected = 2020
            )
          ),
          box(
            width = 8,
            selectizeInput(
              "multi_subject_input_metr", h3("Направление"),
              choices = scopus_subjects, multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 10,
            box(
              title = "Скорректированная цитируемость = Суммарный показатель
              цитируемости университета к числу соавторов публикации
              по базе данных Scopus с учетом меры научного влияния (SJR)
              научного журнала в среднем по каждой отдельной публикации",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot3.1")
            ), offset = 0, style = "padding-right:1px;"
          ),
          column(
            width = 2,
            box(
              width = NULL, solidHeader = TRUE, status = "primary",
              tableOutput("tab3.1")
            ), offset = 0, style = "padding-left:0px;"
          ),
        ),
        fluidRow(
          column(
            width = 10,
            box(
              title = "Скорректированный FWCI = Суммарный показатель
              цитируемости университета по предметной области (FWCI)
              к числу соавторов публикации по базе данных Scopus
              с учетом меры научного влияния (SJR) научного журнала
              в среднем по каждой отдельной публикации по классификации Scopus",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot3.2")
            ), offset = 0, style = "padding-left:25px;padding-right:1px;"
          ),
          column(
            width = 2,
            box(
              width = NULL, solidHeader = TRUE, status = "primary",
              tableOutput("tab3.2")
            ), offset = 0, style = "padding-left:0px;"
          )
        )
      ),

      ### Динамика доходы ####
      tabItem(
        tabName = "econ_din_dohodi",
        fluidRow(
          column(
            width = 3,
            box(
              title = "Доходы от научной (научно-исследовательской) деятельности",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.2")
            )
          ),
          column(
            width = 3,
            box(
              title = "Доходы от собственности: от средств от использования
              результатов интеллектуальной деятельности (РИД)",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.1")
            )
          ),
          column(
            width = 3,
            box(
              title = "Доходы от научных исследований и разработок",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.3")
            )
          ),
          column(
            width = 3,
            box(
              title = "  Доходы от фундаментальных исследований",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.4")
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            box(
              title = "Доходы от прикладных исследований",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.5")
            )
          ),
          column(
            width = 3,
            box(
              title = "Доходы от научно-технических услуг",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.6")
            )
          ),
          column(
            width = 3,
            box(
              title = " Доходы от разработки научно-проектной
              и проектной документации",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.7")
            )
          ),
          column(
            width = 3,
            box(
              title = "Доходы от реализации товаров, работ, услуг
              производственного характера",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.8")
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            box(
              title = "Доходы от иной научной (научно-исследовательской)
              деятельности",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.9")
            )
          ),
          column(
            width = 3,
            box(
              title = "Доходы от безвоздмездных денежных поступлений
              в виде грантов",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.10")
            )
          ),
          column(
            width = 3,
            box(
              title = "Доходы от безвоздмездных денежных поступлений
              в виде грантов на научную деятельность",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.11")
            )
          ),
          column(
            width = 3,
            box(
              title = "Субсидии на научно-исследовательскую деятельность",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.12")
            )
          )
        )
      ),
      ### Динамика расходы ####
      tabItem(
        tabName = "econ_din_rashodi",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Расходы в виде безвоздмездных перечислений
              организациям и физическим лицам",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.13")
            )
          ),
          column(
            width = 6,
            box(
              title = "Расходы в виде безвоздмездных перечислений
              организациям и физическим лицам, из них:
              гранты, предоставляемые другим организациям и физическим лицам",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.14")
            )
          ),
        ),
        fluidRow(
          column(
            width = 4,
            box(
              title = "Расходы на закупку товаров, работ, услуг",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.15")
            )
          ),
          column(
            width = 4,
            box(
              title = "Расходы на закупку товаров, работ, услуг, в том числе:
              закупку научно-исследовательских и опытно-конструкторских работ",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.16")
            )
          ),
          column(
            width = 4,
            box(
              title = "Расходы на закупку товаров, работ, услуг, в том числе:
              закупку научно-исследовательских и опытно-конструкторских работ",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.17")
            )
          )
        )
      ),

      ### Финансирование, 2019 ####
      tabItem(
        tabName = "econ_fin",
        fluidRow(
          column(
            width = 4,
            box(
              title = "Финансирование из средств других министерств и ведомств",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot6.1")
            )
          ),
          column(
            width = 4,
            box(
              title = "Финансирование из средств РНФ, РФФИ, РФПИ",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot6.2")
            )
          ),
          column(
            width = 4,
            box(
              title = "Финансирование из средств хозяйствующих субъектов",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot6.3")
            )
          ),
        ),
        fluidRow(
          column(
            width = 4,
            box(
              title = "Финансирование из средств зарубежных источников",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot6.4")
            )
          ),
          column(
            width = 4,
            box(
              title = "Объем НИОКР гос.задания Минобрнауки России",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot6.5")
            )
          ),
          column(
            width = 4,
            box(
              title = "Объем финансирования НИОКР по ФЦП",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot6.6")
            )
          )
        )
      ),
      ### МТБ, 2019 ####
      tabItem(
        tabName = "econ_mtb",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Стоимость основных средств",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot7.1")
            )
          ),
          column(
            width = 6,
            box(
              title = "Стоимость основных средств, приобр. за отч. период",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot7.2")
            )
          ),
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Стоимость машин и оборудования",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot7.3")
            )
          ),
          column(
            width = 6,
            box(
              title = "Стоимость машин и оборудования, приобр. за отч. период",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot7.4")
            )
          )
        )
      ),
      ### Распределение по грантам ####
      tabItem(
        tabName = "grant",
        fluidRow(box(
          title = "Новосибирский национальный исследовательский государственный
          университет",
          width = 12
        ), width = 12),
        fluidRow(
          column(
            width = 4,
            box(
              title = "2018 год",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.18")
            )
          ),
          column(
            width = 4,
            box(
              title = "2019 год",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.19")
            )
          ),
          column(
            width = 4,
            box(
              title = "2020 год",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot4.20")
            )
          ),
          fluidRow(box(
            title = "Российский государственный университут нефти и газа
            (национальный исследовательский университет) имени И. М. Губкина ",
            width = 12
          ),
          width = 12
          ),
          fluidRow(
            column(
              width = 4,
              box(
                title = "2018 год",
                width = NULL, solidHeader = TRUE, status = "primary",
                plotlyOutput("plot4.21")
              )
            ),
            column(
              width = 4,
              box(
                title = "2019 год",
                width = NULL, solidHeader = TRUE, status = "primary",
                plotlyOutput("plot4.22")
              )
            ),
            column(
              width = 4,
              box(
                title = "2020 год",
                width = NULL, solidHeader = TRUE, status = "primary",
                plotlyOutput("plot4.23")
              )
            )
          )
        )
      ),
      ### Изобретения ####
      tabItem(
        tabName = "rosrid",
        fluidRow(box(
          width = 12,
          selectizeInput(
            "multi_subject_input_rosrid", h3("Направление ГРНТИ"),
            choices = grnti_subjects, multiple = TRUE
          )
        )),
        fluidRow(
          column(
            width = 6,
            box(
              title = "2017 год",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot5.1")
            ),
            box(
              title = "2018 год",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot5.3")
            )
          ),
          column(
            width = 6,
            box(
              title = "2019 год",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot5.2")
            ),
            box(
              title = "2020 год",
              width = NULL, solidHeader = TRUE, status = "primary",
              plotlyOutput("plot5.4")
            )
          )
        )
      )
    )
  )
)






# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  state <- shiny::reactiveValues()
  ## Наукометрия =====


  ### Общие показатели ####
  observe({
    state$vuzi <- input$vuz_vibor
    state$select_year_obsch <- input$select_year_obsch

    #### Количество публикаций ####
    if (!is.null(state$vuzi)) {
      if (state$select_year_obsch == "0") {
        p1.1 <- scopus_good_big_table_subset_cloumns %>%
          dplyr::filter(vuz_name %in% state$vuzi) %>%
          dplyr::group_by(vuz_name) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          ggplot(aes(x = vuz_name, y = n)) +
          geom_col(fill = "lightblue") +
          labs(
            x = "Наименование вуза",
            y = "Количество публикаций",
            title = "2015-2020 года"
          ) +
          theme_classic() +
          theme(
            plot.title = element_text(hjust = 0.5),
            rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
            text = element_text(size = 15),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
        output$plot1.1 <- renderPlotly(ggplotly(p1.1))
      } else {
        p1.1 <- scopus_good_big_table_subset_cloumns %>%
          dplyr::filter(vuz_name %in% state$vuzi) %>%
          dplyr::filter(Year == state$select_year_obsch) %>%
          dplyr::group_by(vuz_name) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          ggplot(aes(x = vuz_name, y = n)) +
          geom_col(fill = "lightblue") +
          labs(
            x = "Наименование вуза",
            y = "Количество публикаций",
            title = state$select_year_obsch
          ) +
          theme_classic() +
          theme(
            plot.title = element_text(hjust = 0.5),
            rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
            text = element_text(size = 15),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
        output$plot1.1 <- renderPlotly(ggplotly(p1.1))
      }
    }

    #### Среднее количество цитирований ####
    if (state$select_year_obsch == "0") {
      p1.2 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::group_by(vuz_name) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        dplyr::summarise(mean_cit = mean(Citations)) %>%
        ggplot(aes(x = vuz_name, y = mean_cit)) +
        geom_col(fill = "lightblue") +
        labs(
          x = "Наименование вуза",
          y = "Среднее количество цитирований",
          title = "2015-2020 года"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(), #
          panel.grid.minor = element_blank(), #
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot1.2 <- renderPlotly(ggplotly(p1.2))
    } else {
      p1.2 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        filter(Year == state$select_year_obsch) %>%
        dplyr::group_by(vuz_name) %>%
        dplyr::summarise(mean_fwci = mean(`Field-Weighted Citation Impact`)) %>%
        ggplot(aes(x = vuz_name, y = mean_fwci)) +
        geom_col(position = "dodge", fill = "lightblue") +
        labs(
          x = "Наименование вуза",
          y = "Среднее количество цитирований",
          title = state$select_year_obsch
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot1.2 <- renderPlotly(ggplotly(p1.2))
    }


    #### Количество публикаций по квартилям ####
    if (state$select_year_obsch == "0") {
      p1.3 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        dplyr::group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_name, y = n, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Количество публикаций",
          title = "2015-2020 года",
          fill = "Journal\nQuartile"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot1.3 <- renderPlotly(ggplotly(p1.3))
    } else {
      p1.3 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        filter(Year == state$select_year_obsch) %>%
        dplyr::group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_name, y = n, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Количество публикаций",
          title = state$select_year_obsch,
          fill = "Journal\nQuartile"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot1.3 <- renderPlotly(ggplotly(p1.3))
    }


    #### Среднее количество цитирований по квартилям ####
    if (state$select_year_obsch == "0") {
      p1.4 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(mean_cit = mean(Citations, na.rm = T)) %>%
        ggplot(aes(x = vuz_name, y = mean_cit, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Среднее количество цитирований",
          title = "2015-2020 года",
          fill = "Journal\nQuartile"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot1.4 <- renderPlotly(ggplotly(p1.4))
    } else {
      p1.4 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        filter(Year == state$select_year_obsch) %>%
        group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(mean_cit = mean(Citations, na.rm = T)) %>%
        ggplot(aes(x = vuz_name, y = mean_cit, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Среднее количество цитирований",
          fill = "Journal\nQuartile",
          title = state$select_year_obsch
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot1.4 <- renderPlotly(ggplotly(p1.4))
    }
  })


  ### Частные показатели ####
  observe({
    state$vuzi <- input$vuz_vibor
    state$multi_subject_input <- input$multi_subject_input
    state$select_year_chastn <- input$select_year_chastn
    #### Среднее значение FWCI ####
    if (!is.null(state$vuzi)) {
      if (state$select_year_chastn == "0") {
        p2.1 <- scopus_good_big_table_subset_cloumns %>%
          dplyr::filter(vuz_name %in% state$vuzi) %>%
          dplyr::filter(str_detect(
            `All Science Journal Classification (ASJC) field name`,
            paste(state$multi_subject_input, collapse = "|")
          )) %>%
          dplyr::group_by(vuz_name) %>%
          dplyr::summarise(mean_fwci = mean(`Field-Weighted Citation Impact`)) %>%
          ggplot(aes(x = vuz_name, y = mean_fwci)) +
          geom_col(fill = "lightblue") +
          labs(
            x = "Наименование вуза, SCOPUS",
            y = "Среднее значение FWCI",
            title = "2015-2020 года"
          ) +
          theme_classic() +
          theme(
            plot.title = element_text(hjust = 0.5),
            rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
            text = element_text(size = 15),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
        output$plot2.1 <- renderPlotly(ggplotly(p2.1))
      } else {
        p2.1 <- scopus_good_big_table_subset_cloumns %>%
          dplyr::filter(vuz_name %in% state$vuzi) %>%
          dplyr::filter(str_detect(
            `All Science Journal Classification (ASJC) field name`,
            paste0(state$multi_subject_input, collapse = "|")
          )) %>%
          dplyr::filter(Year == state$select_year_chastn) %>%
          dplyr::group_by(vuz_name) %>%
          dplyr::summarise(mean_fwci = mean(`Field-Weighted Citation Impact`)) %>%
          ggplot(aes(x = vuz_name, y = mean_fwci)) +
          geom_col(fill = "lightblue") +
          labs(
            x = "Наименование вуза",
            y = "Среднее значение FWCI",
            title = state$select_year_chastn
          ) +
          theme_classic() +
          theme(
            plot.title = element_text(hjust = 0.5),
            rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
            text = element_text(size = 15),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
        output$plot2.1 <- renderPlotly(ggplotly(p2.1))
      }
    }


    #### Количество публикаций ####
    if (state$select_year_chastn == "0") {
      p2.2 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input, collapse = "|")
        )) %>%
        dplyr::group_by(vuz_name) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_name, y = n)) +
        geom_col(fill = "lightblue") +
        labs(
          x = "Наименование вуза",
          y = "Количество публикаций",
          title = "2015-2020 года"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot2.2 <- renderPlotly(ggplotly(p2.2))
    } else {
      p2.2 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input, collapse = "|")
        )) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        filter(Year == state$select_year_chastn) %>%
        dplyr::group_by(vuz_name) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_name, y = n)) +
        geom_col(fill = "lightblue") +
        labs(
          x = "Наименование вуза",
          y = "Количество публикаций",
          title = state$select_year_chastn
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot2.2 <- renderPlotly(ggplotly(p2.2))
    }


    #### Среднее значение FWCI по квартилям ####
    if (state$select_year_chastn == "0") {
      p2.3 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input, collapse = "|")
        )) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        dplyr::group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(mean_fwci_sjr = mean(`Field-Weighted Citation Impact`)) %>%
        ggplot(aes(x = vuz_name, y = mean_fwci_sjr, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Среднее значение FWCI",
          title = "2015-2020 года",
          fill = "Journal\nQuartile"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot2.3 <- renderPlotly(ggplotly(p2.3))
    } else {
      p2.3 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input, collapse = "|")
        )) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        filter(Year == state$select_year_chastn) %>%
        dplyr::group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(mean_fwci_sjr = mean(`Field-Weighted Citation Impact`)) %>%
        ggplot(aes(x = vuz_name, y = mean_fwci_sjr, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Среднее значение FWCI",
          title = state$select_year_chastn,
          fill = "Journal\nQuartile"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot2.3 <- renderPlotly(ggplotly(p2.3))
    }


    #### Количество публикаций по квартилям ####
    if (state$select_year_chastn == "0") {
      p2.4 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input, collapse = "|")
        )) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_name, y = n, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Количество публикаций",
          title = "2015-2020 года",
          fill = "Journal\nQuartile"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot2.4 <- renderPlotly(ggplotly(p2.4))
    } else {
      p2.4 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input, collapse = "|")
        )) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        filter(Year == state$select_year_chastn) %>%
        group_by(vuz_name, `SJR Best Quartile`) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_name, y = n, fill = `SJR Best Quartile`)) +
        geom_col(position = "dodge") +
        labs(
          x = "Наименование вуза",
          y = "Количество публикаций",
          fill = "Journal\nQuartile",
          title = state$select_year_chastn
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      output$plot2.4 <- renderPlotly(ggplotly(p2.4))
    }
  })


  ### Обобщенная метрика ####
  #### Скоррект. цитируемость, SCOPUS ####
  observe({
    state$vuzi <- input$vuz_vibor
    if (!is.null(state$vuzi)) {
      state$n_cit_corrected_year_metr <- input$select_year_metr
      if (state$n_cit_corrected_year_metr == "0") {
        dat_p3.1 <- scopus_good_big_table_subset_cloumns %>%
          dplyr::filter(vuz_name %in% state$vuzi) %>%
          dplyr::mutate(n_cit_corrected = Citations * SJR / `Number of Authors`) %>%
          dplyr::group_by(vuz_name) %>%
          dplyr::summarise(mean_n_cit_corrected = mean(n_cit_corrected, na.rm = T)) %>%
          dplyr::arrange_at(2, dplyr::desc)

        p3.1 <- dat_p3.1 %>%
          mutate(vuz_name = factor(vuz_name, levels = vuz_name)) %>%
          ggplot(aes(x = vuz_name, y = mean_n_cit_corrected)) +
          geom_col(fill = "lightblue") +
          labs(
            x = "Наименование вуза, SCOPUS",
            y = "Скоррект. цитируемость, SCOPUS",
            title = "2015-2020 года"
          ) +
          theme_classic() +
          theme(
            plot.title = element_text(hjust = 0.5),
            rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
            text = element_text(size = 15),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )

        output$plot3.1 <- renderPlotly(ggplotly(p3.1))
        tab_out_3.1 <- dat_p3.1 %>%
          mutate(mean_n_cit_corrected_rs = rescale(mean_n_cit_corrected)) %>%
          select_at(c(1, 3)) %>%
          arrange_at(2, dplyr::desc) %>%
          `colnames<-`(c("Вуз", "Метрика"))

        output$tab3.1 <- shiny::renderTable(tab_out_3.1)
      } else {
        dat_p3.1 <- scopus_good_big_table_subset_cloumns %>%
          dplyr::filter(vuz_name %in% state$vuzi) %>%
          dplyr::filter(Year == state$n_cit_corrected_year_metr) %>%
          dplyr::mutate(n_cit_corrected = Citations * SJR / `Number of Authors`) %>%
          dplyr::group_by(vuz_name) %>%
          dplyr::summarise(mean_n_cit_corrected = mean(n_cit_corrected, na.rm = T)) %>%
          dplyr::arrange_at(2, dplyr::desc)

        p3.1 <- dat_p3.1 %>%
          mutate(vuz_name = factor(vuz_name, levels = vuz_name)) %>%
          ggplot(aes(x = vuz_name, y = mean_n_cit_corrected)) +
          geom_col(fill = "lightblue") +
          labs(
            x = "Наименование вуза",
            y = "Скоррект. цитируемость, SCOPUS",
            title = state$n_cit_corrected_year_metr
          ) +
          theme_classic() +
          theme(
            plot.title = element_text(hjust = 0.5),
            rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
            text = element_text(size = 15),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )

        output$plot3.1 <- renderPlotly(ggplotly(p3.1))

        tab_out_3.1 <- dat_p3.1 %>%
          mutate(mean_n_cit_corrected_rs = rescale(mean_n_cit_corrected)) %>%
          select_at(c(1, 3)) %>%
          arrange_at(2, dplyr::desc) %>%
          `colnames<-`(c("Вуз", "Метрика"))

        output$tab3.1 <- shiny::renderTable(tab_out_3.1)
      }
    }
  })
  #### Скоррект. FWCI, SCOPUS####
  observe({
    state$select_fwci_corrected_year <- input$select_year_metr

    if (is.null(input$multi_subject_input_metr)) {
      state$multi_subject_input_metr <- unlist(scopus_subjects)
    } else {
      state$multi_subject_input_metr <- input$multi_subject_input_metr
    }

    if (state$select_fwci_corrected_year == "0") {
      dat3.2 <- scopus_good_big_table_subset_cloumns %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        dplyr::mutate(fwci_corrected = `Field-Weighted Citation Impact` * SJR / `Number of Authors`) %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input_metr, collapse = "|")
        )) %>%
        dplyr::group_by(vuz_name) %>%
        dplyr::summarise(fwci_corrected_mean = mean(fwci_corrected, na.rm = T)) %>%
        dplyr::arrange_at(2, dplyr::desc)

      p3.2 <- dat3.2 %>%
        mutate(vuz_name = factor(vuz_name, levels = vuz_name)) %>%
        ggplot(aes(x = vuz_name, y = fwci_corrected_mean)) +
        geom_col(fill = "lightblue") +
        labs(
          x = "Наименование вуза",
          y = "Скоррект. FWCI, SCOPUS",
          title = "2015-2020 года"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )

      tab_out_3.2 <- dat3.2 %>%
        mutate(fwci_corrected_mean_rs = rescale(fwci_corrected_mean)) %>%
        select_at(c(1, 3)) %>%
        arrange_at(2, dplyr::desc) %>%
        `colnames<-`(c("Вуз", "Метрика"))

      output$plot3.2 <- renderPlotly(ggplotly(p3.2))
      output$tab3.2 <- shiny::renderTable(tab_out_3.2)
    } else {
      dat3.2 <- scopus_good_big_table_subset_cloumns %>%
        filter(Year == state$select_fwci_corrected_year) %>%
        dplyr::filter(vuz_name %in% state$vuzi) %>%
        dplyr::mutate(fwci_corrected = `Field-Weighted Citation Impact` * SJR / `Number of Authors`) %>%
        dplyr::filter(str_detect(
          `All Science Journal Classification (ASJC) field name`,
          paste(state$multi_subject_input_metr, collapse = "|")
        )) %>%
        dplyr::group_by(vuz_name) %>%
        dplyr::summarise(fwci_corrected_mean = mean(fwci_corrected, na.rm = T)) %>%
        dplyr::arrange_at(2, dplyr::desc)

      p3.2 <- dat3.2 %>%
        mutate(vuz_name = factor(vuz_name, levels = vuz_name)) %>%
        ggplot(aes(x = vuz_name, y = fwci_corrected_mean)) +
        geom_col(fill = "lightblue") +
        labs(
          x = "Наименование вуза",
          y = "Скоррект. FWCI, SCOPUS",
          title = state$select_fwci_corrected_year
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
      tab_out_3.2 <- dat3.2 %>%
        mutate(fwci_corrected_mean_rs = rescale(fwci_corrected_mean)) %>%
        select_at(c(1, 3)) %>%
        arrange_at(2, dplyr::desc) %>%
        `colnames<-`(c("Вуз", "Метрика"))

      output$plot3.2 <- renderPlotly(ggplotly(p3.2))
      output$tab3.2 <- shiny::renderTable(tab_out_3.2)
    }
  })


  ## Экономика ====


  ### Динамика доходы ####
  eco_1_graph_foo <- function(phvd_vuzi, code, num_codes) {
    vuzi_subset <- lapply(
      phvd_vuzi, "[",
      num_codes[num_codes[, 2] == code][1], 6:8
    )
    to_plot <- data.frame((dplyr::bind_rows(vuzi_subset, .id = "df")))
    colnames(to_plot) <- c("vuz", "2020", "2021", "2022")
    tmp <- tidyr::pivot_longer(to_plot, c("2020", "2021", "2022"))
    colnames(tmp) <- c("Вуз", "Год", "Значение")
    p_tmp <- ggplot(tmp) +
      geom_line(aes(x = Год, y = Значение, group = Вуз, colour = Вуз),
        size = 1.2
      ) +
      labs(
        x = "Год",
        y = "Руб."
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5),
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
      )
    return(p_tmp)
  }

  #### Доходы от научной (научно-исследовательской) деятельности ####
  vuzi_subset_12200 <- lapply(
    phvd_vuzi, "[",
    num_codes_dohodi[num_codes_dohodi[, 2] == 12200][1], 4:8
  )
  to_plot_12200 <- data.frame((dplyr::bind_rows(vuzi_subset_12200, .id = "df")))
  colnames(to_plot_12200) <- c("vuz", "2018", "2019", "2020", "2021", "2022")
  tmp_12200 <- tidyr::pivot_longer(
    to_plot_12200,
    c("2018", "2019", "2020", "2021", "2022")
  )
  colnames(tmp_12200) <- c("Вуз", "Год", "Значение")
  p_tmp_12200_1 <- ggplot(tmp_12200) +
    geom_line(aes(x = Год, y = Значение, group = Вуз, colour = Вуз),
      size = 1.2
    ) +
    labs(
      x = "Наименование вузa",
      y = "Руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent")
    )

  #### Субсидии на научно-исследовательскую деятельность ####
  subs_subset <- lapply(subsidii, "[", 2, c(2, 10, 18))
  to_plot_subs_subset <- data.frame((dplyr::bind_rows(subs_subset, .id = "df")))
  colnames(to_plot_subs_subset) <- c("vuz", "2018", "2019", "2020")
  to_plot_subs_subset <- tidyr::pivot_longer(
    to_plot_subs_subset,
    c("2018", "2019", "2020")
  )
  colnames(to_plot_subs_subset) <- c("Вуз", "Год", "Значение")
  to_plot_subs_subset <- ggplot(to_plot_subs_subset) +
    geom_line(aes(x = Год, y = Значение, group = Вуз, colour = Вуз), size = 1.2) +
    labs(
      x = "Год",
      y = "Руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent")
    )

  #### Графики доходов ####
  p4.1 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 11300, num_codes_dohodi))
  p4.2 <- ggplotly(p_tmp_12200_1)
  p4.3 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 12210, num_codes_dohodi))
  p4.4 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 12211, num_codes_dohodi))
  p4.5 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 12212, num_codes_dohodi))
  p4.6 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 12220, num_codes_dohodi))
  p4.7 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 12221, num_codes_dohodi))
  p4.8 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 12230, num_codes_dohodi))
  p4.9 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 12240, num_codes_dohodi))
  p4.10 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 14100, num_codes_dohodi))
  p4.11 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 14110, num_codes_dohodi))
  p4.12 <- ggplotly(to_plot_subs_subset)

  output$plot4.1 <- renderPlotly(p4.1)
  output$plot4.2 <- renderPlotly(p4.2)
  output$plot4.3 <- renderPlotly(p4.3)
  output$plot4.4 <- renderPlotly(p4.4)
  output$plot4.5 <- renderPlotly(p4.5)
  output$plot4.6 <- renderPlotly(p4.6)
  output$plot4.7 <- renderPlotly(p4.7)
  output$plot4.8 <- renderPlotly(p4.8)
  output$plot4.9 <- renderPlotly(p4.9)
  output$plot4.10 <- renderPlotly(p4.10)
  output$plot4.11 <- renderPlotly(p4.11)
  output$plot4.12 <- renderPlotly(p4.12)




  #### Динамика расходы ####
  p4.13 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 24000, num_codes_rasvodi))
  p4.14 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 24200, num_codes_rasvodi))
  p4.15 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 26000, num_codes_rasvodi))
  p4.16 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 26100, num_codes_rasvodi))
  p4.17 <- ggplotly(eco_1_graph_foo(phvd_vuzi, 26110, num_codes_rasvodi))

  output$plot4.13 <- renderPlotly(p4.13)
  output$plot4.14 <- renderPlotly(p4.14)
  output$plot4.15 <- renderPlotly(p4.15)
  output$plot4.16 <- renderPlotly(p4.16)
  output$plot4.17 <- renderPlotly(p4.17)




  ### Финансирование, 2019 ####
  #### Объем финансирования НИОКР по ФЦП ####
  p6.1 <- pokazateli %>%
    select_at(c(1, 2)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Общий объем финансиро-вания, тыс. р.`),
      fill = "lightblue"
    ) +
    labs(
      x = "Наименование вузa",
      y = "Тыс. руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot6.1 <- renderPlotly(ggplotly(p6.1))
  #### Объем НИОКР гос.задания Минобрнауки России ####
  p6.2 <- pokazateli %>%
    select_at(c(1, 3)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Финансиро-вание из средств Минобрнауки России,  тыс. р.`),
      fill = "lightblue"
    ) +
    labs(
      x = "Наименование вузa",
      y = "Тыс. руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot6.2 <- renderPlotly(ggplotly(p6.2))

  #### Финансирование из средств других министерств и ведомств ####
  p6.3 <- pokazateli %>%
    select_at(c(1, 4)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Финансиро-вание из средств других министерств и ведомств, тыс.р.`),
      fill = "lightblue"
    ) +
    labs(
      x = "Наименование вузa",
      y = "Тыс. руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot6.3 <- renderPlotly(ggplotly(p6.3))

  #### Финансиро-вание из средств РНФ, РФФИ, РФПИ ####
  p6.4 <- pokazateli %>%
    select_at(c(1, 5)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Финансиро-вание из средств РНФ, РФФИ, ФПИ, тыс. р.`),
      fill = "lightblue"
    ) +
    labs(
      x = "Наименование вузa",
      y = "Тыс. руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot6.4 <- renderPlotly(ggplotly(p6.4))

  #### Финансирование из средств хозяйствующих субъектов ####
  p6.5 <- pokazateli %>%
    select_at(c(1, 6)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Финансиро-вание из средств хозяйствую-щих субъектов, тыс. р.`),
      fill = "lightblue"
    ) +
    labs(
      x = "Наименование вузa",
      y = "Тыс. руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot6.5 <- renderPlotly(ggplotly(p6.5))

  #### Финансирование из средств зарубежных источников ####
  p6.6 <- pokazateli %>%
    select_at(c(1, 7)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Финансирование из средств зарубежных источников, тыс. р.`),
      fill = "lightblue"
    ) +
    labs(
      x = "Наименование вузa",
      y = "Тыс. руб."
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot6.6 <- renderPlotly(ggplotly(p6.6))





  ### Распределение по грантам ####

  subs_grant <- lapply(subsidii, "[", 2, c(8, 9, 16, 17, 24, 25))
  to_plot_subs_grant <- data.frame((dplyr::bind_rows(subs_grant, .id = "df")))
  to_plot_subs_grant_ngu_gru <- to_plot_subs_grant[to_plot_subs_grant$df %in% c("НГУ", "РГУ"), ]

  subset_to_piechart <- to_plot_subs_grant_ngu_gru %>%
    mutate(
      ne_grant_2018 = platn_summary_2018 - plant_grant_2018,
      ne_grant_2019 = platn_summary_2019 - plant_grant_2019,
      ne_grant_2020 = platn_summary_2020 - plant_grant_2020
    ) %>%
    select_at(c(1, 3, 5, 7, 8:10))

  #### НГУ, 2018 ####
  ngu18 <- subset_to_piechart %>%
    filter(df == "НГУ") %>%
    select_at(c(2, 5))

  to_pie_chart <- data.frame(
    group = c("Грант", "Не грант"),
    value = unlist(ngu18)
  )

  p4.18 <- plot_ly(to_pie_chart,
    labels = ~group,
    values = ~value, type = "pie"
  )
  #### НГУ, 2019 ####
  ngu19 <- subset_to_piechart %>%
    filter(df == "НГУ") %>%
    select_at(c(3, 6))

  to_pie_chart <- data.frame(
    group = c("Грант", "Не грант"),
    value = unlist(ngu19)
  )

  p4.19 <- plot_ly(to_pie_chart,
    labels = ~group,
    values = ~value, type = "pie"
  )

  #### НГУ, 2020 #####
  ngu20 <- subset_to_piechart %>%
    filter(df == "НГУ") %>%
    select_at(c(4, 7))

  to_pie_chart <- data.frame(
    group = c("Грант", "Не грант"),
    value = unlist(ngu20)
  )

  p4.20 <- plot_ly(to_pie_chart,
    labels = ~group,
    values = ~value, type = "pie"
  )

  #### РГУ, 2018 ####
  rgu18 <- subset_to_piechart %>%
    filter(df == "РГУ") %>%
    select_at(c(2, 5))

  to_pie_chart <- data.frame(
    group = c("Грант", "Не грант"),
    value = unlist(rgu18)
  )

  p4.21 <- plot_ly(to_pie_chart,
    labels = ~group,
    values = ~value, type = "pie"
  )

  #### РГУ, 2019 ####
  rgu19 <- subset_to_piechart %>%
    filter(df == "РГУ") %>%
    select_at(c(3, 6))

  to_pie_chart <- data.frame(
    group = c("Грант", "Не грант"),
    value = unlist(rgu19)
  )

  p4.22 <- plot_ly(to_pie_chart,
    labels = ~group,
    values = ~value, type = "pie"
  )
  #### РГУ, 2020 ####
  rgu20 <- subset_to_piechart %>%
    filter(df == "РГУ") %>%
    select_at(c(4, 7))

  to_pie_chart <- data.frame(
    group = c("Грант", "Не грант"),
    value = unlist(rgu20)
  )

  p4.23 <- plot_ly(to_pie_chart,
    labels = ~group,
    values = ~value, type = "pie"
  )

  output$plot4.18 <- renderPlotly(p4.18)
  output$plot4.19 <- renderPlotly(p4.19)
  output$plot4.20 <- renderPlotly(p4.20)
  output$plot4.21 <- renderPlotly(p4.21)
  output$plot4.22 <- renderPlotly(p4.22)
  output$plot4.23 <- renderPlotly(p4.23)





  ### МТБ, 2019 ####

  #### Стоимость основных средств ####
  p7.1 <- pokazateli %>%
    select_at(c(1, 24)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Стоимость основных средств, тыс. р.`),
      fill = "lightblue"
    ) +
    labs(x = "Наименование вузa", y = "Тыс. руб.") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot7.1 <- renderPlotly(ggplotly(p7.1))

  #### Стоимость основных средств, приобр. за отч. период ####
  p7.2 <- pokazateli %>%
    select_at(c(1, 25)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Стоимость основных средств, приобр. за отч. период, тыс.р.`),
      fill = "lightblue"
    ) +
    labs(x = "Наименование вузa", y = "Тыс. руб.") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot7.2 <- renderPlotly(ggplotly(p7.2))

  #### Стоимость машин и оборудования ####
  p7.3 <- pokazateli %>%
    select_at(c(1, 26)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Стоимость машин и оборудования, тыс.р.`),
      fill = "lightblue"
    ) +
    labs(x = "Наименование вузa", y = "Тыс. руб.") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot7.3 <- renderPlotly(ggplotly(p7.3))

  #### Стоимость машин и оборудования, приобр. за отч. период ####
  p7.4 <- pokazateli %>%
    select_at(c(1, 27)) %>%
    ggplot(aes(x = vuz_name)) +
    geom_col(aes(y = `Стоимость машин и оборудования, приобр. за отч. период, тыс.р.`),
      fill = "lightblue"
    ) +
    labs(x = "Наименование вузa", y = "Тыс. руб.") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      rect = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      text = element_text(size = 15),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  output$plot7.4 <- renderPlotly(ggplotly(p7.4))







  ## Изобретения ====
  observe({
    ### 2017 год ####
    state$vuzi <- input$vuz_vibor
    if (!is.null(state$vuzi)) {
      state$multi_subject_input_rosrid <- input$multi_subject_input_rosrid

      p5.1 <- rosrid_all_years %>%
        dplyr::filter(vuz_short %in% state$vuzi) %>%
        dplyr::filter(str_detect(
          grnti,
          paste0(state$multi_subject_input_rosrid, collapse = "|")
        )) %>%
        dplyr::filter(V2 == 2017) %>%
        dplyr::group_by(vuz_short, rid_type) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_short, y = n, fill = rid_type)) +
        geom_col() +
        labs(
          x = "Наименование вуза",
          y = "Количество РИД"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        guides(fill = guide_legend(title = "Тип РИД"))

      output$plot5.1 <- renderPlotly(ggplotly(p5.1))
    }

    ### 2018 год ####
    if (!is.null(state$vuzi)) {
      state$multi_subject_input_rosrid <- input$multi_subject_input_rosrid

      p5.2 <- rosrid_all_years %>%
        dplyr::filter(vuz_short %in% state$vuzi) %>%
        dplyr::filter(str_detect(
          grnti,
          paste0(state$multi_subject_input_rosrid, collapse = "|")
        )) %>%
        dplyr::filter(V2 == 2018) %>%
        dplyr::group_by(vuz_short, rid_type) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_short, y = n, fill = rid_type)) +
        geom_col() +
        labs(
          x = "Наименование вуза",
          y = "Количество РИД"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        guides(fill = guide_legend(title = "Тип РИД"))

      output$plot5.2 <- renderPlotly(ggplotly(p5.2))
    }

    ### 2019 год ####
    if (!is.null(state$vuzi)) {
      state$multi_subject_input_rosrid <- input$multi_subject_input_rosrid

      p5.3 <- rosrid_all_years %>%
        dplyr::filter(vuz_short %in% state$vuzi) %>%
        dplyr::filter(str_detect(
          grnti,
          paste0(state$multi_subject_input_rosrid, collapse = "|")
        )) %>%
        dplyr::filter(V2 == 2019) %>%
        dplyr::group_by(vuz_short, rid_type) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_short, y = n, fill = rid_type)) +
        geom_col() +
        labs(
          x = "Наименование вуза",
          y = "Количество РИД"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        guides(fill = guide_legend(title = "Тип РИД"))

      output$plot5.3 <- renderPlotly(ggplotly(p5.3))
    }

    ### 2020 год ####
    if (!is.null(state$vuzi)) {
      state$multi_subject_input_rosrid <- input$multi_subject_input_rosrid

      p5.4 <- rosrid_all_years %>%
        dplyr::filter(vuz_short %in% state$vuzi) %>%
        dplyr::filter(str_detect(
          grnti,
          paste0(state$multi_subject_input_rosrid, collapse = "|")
        )) %>%
        dplyr::filter(V2 == 2020) %>%
        dplyr::group_by(vuz_short, rid_type) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        ggplot(aes(x = vuz_short, y = n, fill = rid_type)) +
        geom_col() +
        labs(
          x = "Наименование вуза",
          y = "Количество РИД"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        guides(fill = guide_legend(title = "Тип РИД"))

      output$plot5.4 <- renderPlotly(ggplotly(p5.4))
    }
  })
}

shinyApp(ui, server)
