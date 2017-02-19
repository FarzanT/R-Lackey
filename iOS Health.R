library(xml2)
library(purrr)
library(ggplot2)
library(dplyr)
health_data <- read_xml("apple_health_export/export.xml")
steps <- xml_find_all(health_data, ".//Record[@type='HKQuantityTypeIdentifierStepCount']") %>% map(xml_attrs) %>% map_df(as.list)
glimpse(steps)
steps %>% select(startDate, value) %>%
  group_by(Date = as.Date(paste(substr(startDate, 1, 7), "01", sep = "-")))%>% summarise(count = sum(as.numeric(value))) %>%
  ggplot(aes(Date, count)) + geom_col(fill = "skyblue3") + theme_bw() + labs(y = "monthly step count", title = "Steps by month September 2014 - January 2017 as measured by iOS")


