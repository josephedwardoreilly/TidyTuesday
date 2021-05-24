library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(brms)
library(patchwork)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-05-18'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$survey)


# Cleaning ----------------------------------------------------------------
x <- x[currency == 'USD',.(
  highest_level_of_education_completed,
  industry,
  annual_salary,
  other_monetary_comp,
  years_of_experience_in_field, gender)]

# Keep certain degree levels
x[highest_level_of_education_completed %in% c("Master's degree", "PhD"),
  highest_level_of_education_completed := 'Postgraduate degree']

x[highest_level_of_education_completed %in% c('Professional degree (MD, JD, etc.)'),
  highest_level_of_education_completed := 'Professional degree']

x <- x[highest_level_of_education_completed %in% c(
  'Postgraduate degree',
  'College degree',
  'Professional degree (MD, JD, etc.)')]


# Drop non-responders for gender, drop non-binary due to the paucity of data
x <- x[!is.na(gender)]
x <- x[gender %in% c('Man', 'Woman')]


# Cleaning the industry column a bit
# Harmonise the biotech/pharma category
x[grepl(
  'biotech|pharma',
  industry,
  ignore.case = TRUE), industry := 'Biotech/Pharma']

# roll academia into higher education 
x[grepl(
  'academ',
  industry,
  ignore.case = TRUE), industry := 'Education (Higher Education)']

# Keep only the most well populated industries 
x <- x[industry %in% x[, .N, by = industry][N > 150, industry]]

# pseudo continuous data
x[years_of_experience_in_field == "1 year or less",
  yoe := runif(.N, 0, 1)]

x[years_of_experience_in_field == "2 - 4 years",
  yoe := runif(.N, 1, 4)]

x[years_of_experience_in_field == "5-7 years",
  yoe := runif(.N, 4, 7)]

x[years_of_experience_in_field == "8 - 10 years",
  yoe := runif(.N, 7, 10)]

x[years_of_experience_in_field == "11 - 20 years",
  yoe := runif(.N, 10, 20)]

x[years_of_experience_in_field == "21 - 30 years",
  yoe := runif(.N, 20, 30)]

x[years_of_experience_in_field == "31 - 40 years", 
  yoe := runif(.N, 30, 40)]

x[years_of_experience_in_field == "41 years or more",
  yoe := runif(.N, 40, 55)]

# clean the other comp column
x[is.na(other_monetary_comp), other_monetary_comp := 0]

# Derive a total annual comp column
x[, total.comp := annual_salary + as.numeric(other_monetary_comp)]

# Drop massive outliers, only focus on 'normal person' salaries
x <- x[total.comp != 0 & total.comp < 500000]


# Hierarchical Regression -------------------------------------------------
z.pg <- brm(
  formula = total.comp ~ (1|industry) + yoe*gender ,
  data = x[highest_level_of_education_completed == 'Postgraduate degree'],
  chains = 2,
  iter = 3000)

z.g <- brm(
  formula = total.comp ~ (1|industry) + yoe*gender ,
  data = x[highest_level_of_education_completed == 'College degree'],
  chains = 2,
  iter = 3000)

z.pro <- brm(
  formula = total.comp ~ (1|industry) + yoe*gender ,
  data = x[highest_level_of_education_completed == 'Professional degree'],
  chains = 2,
  iter = 3000)


# Regression Output Wrangling ---------------------------------------------

## Get sampled linear preds
j <- lapply(
  list(z.g, z.pg, z.pro),
  conditional_effects, 
  effects = 'yoe:gender',
  spaghetti = TRUE, 
  nsamples = 500)

# Extract the estimated linear predictor
k <- lapply(
  j,
  function(x) data.table(lapply(x, attributes)$`yoe:gender`$spaghetti))

# Name them and put into a single data.table
names(k) <- c(
  'College degree', 
  'Postgraduate degree', 
  'Professional degree (MD, JD, etc.)')
k <- rbindlist(k, idcol = 'edu')


## Get the effect of gender on annual increase in wage
# extract post samples
post <- lapply(
  list(z.g, z.pg, z.pro),
  FUN = function(x) data.table(posterior_samples(x)))

# calculate the effect with interaction
jk <- lapply(
  post,
  FUN = function(x) melt(
    x[, .(Woman = b_yoe + `b_yoe:genderWoman`, Man = b_yoe)],
    measure.vars = c('Woman', 'Man'), 
    variable.name = 'gender'))

# Name them, bind them
names(jk) <- c(
  'College degree', 
  'Postgraduate degree', 
  'Professional degree (MD, JD, etc.)')
jk <- rbindlist(jk, idcol = 'edu')



# Plotting ----------------------------------------------------------------

# Define a palette
man.pal <- c('Man' = '#247BA0', 'Woman' = '#F49D6E')

# Sampled linear relationships
p.lin <- ggplot(
  k,
  aes(
    x = effect1__,
    y = estimate__,
    group = sample__, 
    color = gender))+
  geom_line(alpha = 0.05) + 
  scale_color_manual(values = man.pal)+ 
  facet_wrap(.~edu) + 
  xlab('Years Of Field Experience') + 
  ylab('Total Annual Compensation ($)') + 
  theme(
    text = element_text(family = 'Gilroy-Light', color = '#08090A'),
    plot.background = element_rect(fill = '#FFFFFF', color = NA),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = '#F7F7FF', color = NA),
    strip.background = element_rect(fill = '#F7F7FF', color = NA),
    strip.text = element_text(hjust = 1,  colour = '#08090A', family = 'Gilroy-SemiBold')) + 
  guides(colour = FALSE)

# Sampled interaction effect
p.int <- ggplot(
    jk,
    aes(x = factor(gender), y = value, color = gender)) + 
    stat_interval(aes(color_ramp = stat(level))) + 
    facet_wrap(.~edu) + 
    xlab(NULL) + 
    ylab('Growth In Annual Compensation \n Per-Year Of Field Experience ($)') + 
    scale_color_manual(values = man.pal) + 
    guides(color = FALSE, color_ramp = FALSE) + 
  theme(
    text = element_text(family = 'Gilroy-Light', color = '#08090A'),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = '#FFFFFF', color = NA),
    panel.background = element_rect(fill = '#F7F7FF', color = NA),
    strip.background = element_rect(fill = '#F7F7FF', color = NA),
    strip.text = element_text(hjust = 1, colour = '#08090A', family = 'Gilroy-SemiBold'))

# Final plot construction 
# TODO:  add a title to the top plot and a subtitle explaining the content
(p.int/p.lin) + 
  plot_annotation(
    caption = "Visualisation by Joe O'Reilly (josephedwardoreilly.github.com)\nData from TidyTuesday and AskAManager",
    theme = theme(
      plot.margin = margin(5, 0, 10, 0),
      plot.caption = element_text(
        family = 'Gilroy-Regular',
        size = 8,
        colour = '#08090A'),
      plot.background = element_rect(
        fill = '#FFFFFF',
        color = NA))) + 
  ggsave(
    filename = paste0(
      getwd(),
      '/plots/',
      tidy.week,
      '.png'),
    width = 11, height = 7, device = 'png')

