# Load packages
library("dplyr")
library("reshape")

# Read in files
data_folder <- 'data'
data_files <- list.files(data_folder)
for (file in data_files) {
  df_name <- tolower(gsub('.csv', '', file))
  try(
    read.csv(file.path(data_folder, file), na.strings = NA) %>%
      setNames(tolower(names(.))) %>% 
      assign(df_name, ., envir=.GlobalEnv)
  )
}

# Demographic data
pop_list <- list(
  "Total Population" = 'all',
  "By Gender" = 'gender',
  "By Age" = 'age.group',
  "By District" = 'district.name'
)
population$age.group <- 
  ifelse(population$age %in% c('0-4', '5-9', '10-14', '15-19'), '0-19',
         ifelse(population$age %in% c('20-24', '25-29', '30-34', '35-39'), '20-39',
                                      ifelse(population$age %in% c('40-44', '45-49', '50-54', '55-59'), '40-59',
                                             ifelse(population$age %in% c('60-64', '65-69', '70-74', '75-79'), '60-79',
                                                    '80+'))))
population <- population %>%
  group_by(year, district.name, gender, age.group) %>%
  summarize(number = sum(number))

# Unemployment data
years <- unique(unemployment$year)
year_list <- list()
for (year in years) {
  year_list[[toString(year)]] <- year
}
unemployment$month <- factor(unemployment$month, levels = c('January',
                                                            'February',
                                                            'March',
                                                            'April',
                                                            'May',
                                                            'June',
                                                            'July',
                                                            'August',
                                                            'September',
                                                            'October',
                                                            'November',
                                                            'December'))
unempl_by_year <- unemployment %>%
  group_by(year) %>%
  summarize(number = sum(number))
unempl_by_month <- unemployment %>%
  group_by(year, month) %>%
  summarize(number = sum(number))
unempl_by_district <- unemployment %>%
  group_by(year, district.name) %>%
  summarize(number = sum(number)) %>%
  arrange(desc(number)) %>%
  filter(!district.name %in% 'No consta')

# Accident data
accidents_2017$month <- factor(accidents_2017$month, levels = c('January',
                                                                'February',
                                                                'March',
                                                                'April',
                                                                'May',
                                                                'June',
                                                                'July',
                                                                'August',
                                                                'September',
                                                                'October',
                                                                'November',
                                                                'December'))
accidents_2017$weekday <- factor(accidents_2017$weekday, levels = c('Monday',
                                                                    'Tuesday',
                                                                    'Wednesday',
                                                                    'Thursday',
                                                                    'Friday',
                                                                    'Saturday',
                                                                    'Sunday'))
districts <- unique(accidents_2017$district.name)[order(unique(accidents_2017$district.name))]
district_list <- list()
for (district in districts) {
  district_list[[district]] <- district
}
acc_by_day <- accidents_2017 %>%
  count(district.name, day)
acc_by_time <- accidents_2017 %>%
  count(district.name, day, hour)
acc_by_week <- accidents_2017 %>%
  count(district.name, weekday) %>%
  mutate(n = n/52)