# library(dplyr)
# library(plotly)
# setwd('C:/Users/Sam/Code/da62/life/')


# df -----
df <- (
  read.csv('data/life2.csv')
  %>% rename(
    Country             = Country,
    Continent           = Continent,
    Population          = Population,
    Year                = Year,
    Status              = Status,
    
    Life_expectancy     = Life.expectancy,
    Adult_mortality     = Adult.Mortality,
    Infant_deaths       = infant.deaths,
    Child_deaths        = under.five.deaths,
    Measles_cases       = Measles,
    Child_HIV_deaths    = HIV.AIDS,
    
    Hepatitis_B_immunization = Hepatitis.B,
    Polio_immunization       = Polio,
    Diphtheria_immunization  = Diphtheria,
    
    Alcohol             = Alcohol,
    BMI                 = BMI,
    Thinness_under_20   = thinness..1.19.years,
    
    GDP                 = GDP,
    Health_GDP          = percentage.expenditure,
    Health_vs_gov       = Total.expenditure,
    Schooling           = Schooling,
  )
)

df$Avg_immunization = (
  df$Hepatitis_B_immunization
  + df$Polio_immunization
  + df$Diphtheria_immunization
  ) / 3

df$log_GDP = log(df$GDP)

df$Life_expectancy_bin <- cut(df$Life_expectancy, c(40, 50, 60, 70, 80, 90))

countries = sort(unique(df$Country))
years     = sort(unique(df$Year))


# df_mean -----
df_mean <- (
  df
  %>% group_by(Country, Continent, Status)
  %>% summarise(across(where(is.numeric), ~mean(.x, na.rm=TRUE)))
  %>% ungroup()
)

df_mean$Life_expectancy_bin <- cut(df_mean$Life_expectancy, c(40, 50, 60, 70, 80, 90))

# part 1 -----
min_year = 2015
max_year = 2015
df_life <- (
  df
  %>% filter(Year>=min_year & Year<=max_year)
  # %>% group_by(Country, Continent, Status)
  %>% group_by(Country)
  %>% summarise(Life_expectancy=mean(Life_expectancy))
  %>% ungroup()
)
df_life <- df_life %>% arrange(desc(Life_expectancy))
df_life$rank <- seq(length(countries))

df_cs <- (
  df
  %>% filter(Year>=min_year & Year<=max_year)
  %>% group_by(Country, Continent, Status)
  # %>% group_by(Country)
  %>% summarise(Life_expectancy=mean(Life_expectancy))
  %>% ungroup()
)
df_cs <- df_cs %>% arrange(desc(Life_expectancy))
df_cs$rank <- seq(length(countries))



plot1a <- plot_ly(
  data = df_life,
  x    = ~reorder(Country, Life_expectancy),
  y    = ~Life_expectancy,
  color= ~Country,
  type = 'bar'
) %>% layout(
  title='',
  xaxis=list(title='Country',
             tickangle=45),
  hoverlabel=list(font=list(size=18)),
  showlegend=FALSE
)

plot1b <- plot_ly(
  data = df_cs,
  x    = ~reorder(Country, Life_expectancy),
  y    = ~Life_expectancy,
  color= ~Continent,
  type = 'bar'
) %>% layout(
  title='',
  xaxis=list(title='Country',
             tickangle=45),
  hoverlabel=list(font=list(size=18))
)
plot1b

plot1c <- plot_ly(
  data = df_cs,
  x    = ~reorder(Country, Life_expectancy),
  y    = ~Life_expectancy,
  color= ~Status,
  type = 'bar'
) %>% layout(
  title='',
  xaxis=list(title='Country',
             tickangle=45),
  hoverlabel=list(font=list(size=18))
)
plot1c

df_top <- df_life %>% filter(rank<=10)
df_bottom <- df_life %>% filter(rank>nrow(df_life)-10)


# part 2 -----
plot2a <- ggplot(df_mean,
                 aes(x=GDP,
                     y=Life_expectancy,
                     )
                 ) +
  xlab('GDP (in USD)') +
  ylab('Life expectancy') + 
  geom_point(aes(colour=factor(Status), text=paste('Country:', Country))) +
  geom_smooth() +
  scale_color_brewer(palette="Set1") +
  scale_x_log10()

plot2b <- ggplot(df_mean,
                 aes(x=GDP,
                     y=Life_expectancy,
                     )
                 ) +
  xlab('GDP (in USD)') +
  ylab('Life expectancy') + 
  geom_point(aes(colour=factor(Continent), text=paste('Country:', Country))) +
  geom_smooth() +
  scale_color_brewer(palette="Set1") +
  scale_x_log10()

plot2c <- ggplot(df_mean,
                 aes(x=Health_GDP,
                     y=Life_expectancy,
                 )
) +
  xlab('Health expenditure per capita (in USD)') +
  ylab('Life expectancy') + 
  geom_point(aes(colour=factor(Status), text=paste('Country:', Country))) +
  geom_smooth() +
  scale_color_brewer(palette="Set1") +
  scale_x_log10()

plot2d <- ggplot(df_mean,
                 aes(x=Health_GDP,
                     y=Life_expectancy,
                 )
) +
  xlab('Health expenditure per capita (in USD)') +
  ylab('Life expectancy') + 
  geom_point(aes(colour=factor(Continent), text=paste('Country:', Country))) +
  geom_smooth() +
  scale_color_brewer(palette="Set1") +
  scale_x_log10()

df_time_status <- df %>%
  group_by(Status, Year) %>%
  summarise(sd = sd(Life_expectancy, na.rm=TRUE),
            Life_expectancy = mean(Life_expectancy)) %>%
  ungroup()
df_time_status$low  = df_time_status$Life_expectancy - df_time_status$sd
df_time_status$high = df_time_status$Life_expectancy + df_time_status$sd

plot6a <- ggplot(df_time_status,
                 aes(x=Year,
                     y=Life_expectancy,
                     ymin=low,
                     ymax=high,
                     fill=Status,
                     color=Status,
                     )
                 ) +
  geom_line(size=2) +
  geom_ribbon(alpha=0.2,
              color=NA) +
  scale_color_brewer(palette="Set1")

df_time_continent <- df %>%
  group_by(Continent, Year) %>%
  summarise(sd = sd(Life_expectancy, na.rm=TRUE),
            Life_expectancy = mean(Life_expectancy)) %>%
  ungroup()
df_time_continent$low  = df_time_continent$Life_expectancy - df_time_continent$sd
df_time_continent$high = df_time_continent$Life_expectancy + df_time_continent$sd

plot6b <- ggplot(df_time_continent,
                 aes(x=Year,
                     y=Life_expectancy,
                     ymin=low,
                     ymax=high,
                     fill=Continent,
                     color=Continent,
                 )
) +
  geom_line(size=2) +
  geom_ribbon(alpha=0.2,
              color=NA) +
  scale_color_brewer(palette="Set1")

# part 3 -----
plot3a <- ggplot(df_mean,
                 aes(x=Hepatitis_B_immunization,
                     y=Life_expectancy
                     )
                 ) +
  xlab('Hepatitis B immunization (%)') +
  ylab('Life expectancy') + 
  geom_point(aes(text=paste('Country:', Country)), color='red') +
  geom_smooth()

plot3b <- ggplot(df_mean,
                 aes(x=Polio_immunization,
                     y=Life_expectancy
                     )
                 ) +
  xlab('Polio immunization (%)') +
  ylab('Life expectancy') + 
  geom_point(aes(text=paste('Country:', Country)), color='blue') +
  geom_smooth()

plot3c <- ggplot(df_mean,
                 aes(x=Diphtheria_immunization,
                     y=Life_expectancy
                     )
                 ) +
  xlab('Diphtheria immunization (%)') +
  ylab('Life expectancy') + 
  geom_point(aes(text=paste('Country:', Country)), color='green') +
  geom_smooth()
plot3c

plot3d <- ggplot(df_mean,
                 aes(x=Avg_immunization,
                     y=Life_expectancy
                     )
                 ) +
  xlab('Immunization (%)') +
  ylab('Life expectancy') + 
  geom_point(aes(text=paste('Country:', Country))) +
  geom_smooth()
plot3d

# part 4 -----
plot4x <- function(column, colorby){
  p <- ggplot(df_mean,
              aes(x={{column}},
                  y=Life_expectancy,
                  color={{colorby}},
                  )
              ) +
  geom_point(aes(text=paste('Country:', Country))) +
  geom_smooth() +
  scale_x_log10()
  
  return(p)
}
ggplotly(plot4x(Adult_mortality, Status))
ggplotly(plot4x(Adult_mortality, Continent))
ggplotly(plot4x(Child_HIV_deaths, Status))

# part 5 -----

table5a <- function(selected_countries){
  dft <- (
    df
    %>% filter(Year>=min_year & Year<=max_year)
    %>% group_by(Country, Continent, Status)
    %>% summarise(Life_expectancy=mean(Life_expectancy))
    %>% ungroup()
  )
  dft <- dft %>% arrange(desc(Life_expectancy))
  dft$Rank <- seq(length(countries))
  
  dft <- dft %>% filter(Country %in% selected_countries)
  dft <- dft[, c('Rank', 'Country', 'Continent', 'Status', 'Life_expectancy')]
  return(dft)
}
table5a(c('Canada', 'China'))

plot5b <- function(countries){
  p <- ggplot(filter(df, Country %in% countries),
              aes(x=Year,
                  y=Avg_immunization,
                  color=Country)
  ) +
    geom_line()
  return(ggplotly(p))
}
plot5b(c('Canada', 'China'))

plot5x <- function(countries, column){
  p <- ggplot(filter(df, Country %in% countries),
              aes(x=Year,
                  y={{column}},
                  color=Country)
  ) +
    geom_line()
  return(ggplotly(p))
}
plot5xlogy <- function(countries, column){
  p <- ggplot(filter(df, Country %in% countries),
              aes(x=Year,
                  y={{column}},
                  color=Country)
  ) +
    geom_line() +
    scale_y_log10()
  return(ggplotly(p))
}
plot5xlogy(c('Canada', 'China'), Adult_mortality)

# text -----
text0a <- 'Life expectancy refers to the number of years a person can expect to live.  It is commonly estimated based on the average age of death of a population.
Though sharing the same home Earth, life expectancy across different countries can vary significantly.  As a matter of fact, regardless of race and clan, people living in developed counties are expected to live longer lives, whereas those who live in developing counties are expecting theirs shorter.
The public has investigated, as well discovered, some important factors that are influential to life expectancy.  They include, but not limited to, 1) immunization, 2) mortality, 3) economics and 4) social factors.  The effect of these factors on life expectancy can be generally observed across the globe.'

text0b <- 'The dataset that we will use is available on Kaggle as Life Expectancy (WHO) – Statistical Analysis on factors influencing Life Expectancy (https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who).  The information it presents originated from World Health Organization (WHO) and United Nations (UN).  The author of this dataset compiled information from both sources and merge them into a single csv table.'
