require(tidytuesdayR)
require(data.table)
require(ggplot2)
require(ggtext)
require(patchwork)


# Setting up the environment ----------------------------------------------
# G7/8 excluding the EU (which doesn't vote at UN)
g8 <- data.table(
  country_code = c(
    'CA', 'FR', 'DE', 'IT', 'JP', 'RU', 'GB', 'US'),
  entry = as.Date(
    '1976-06-27'),
  exit = as.Date(
    '2022-01-01'))
g8[country_code == 'RU', `:=`(entry = as.Date('1997-06-20'),
                              exit  = as.Date('2014-03-24'))]

# Focal countries [UN Security Council - permanent members]
focal.countries <- c('CN', "FR", 'GB', 'RU', 'US')

# Background colour
bg <- '#3b3b3b'

# assign the focal countries their own colours
pal.2 <- c(
  CN = '#9792E3',
  FR = '#FEB95F',
  GB = '#F71735',
  RU = '#449DD1',
  US = '#6EEB83')

# Get voting pattern by country
# No need for rolling joins as membership of comparator.org has one start and 
# stop date only [as of 2021]
f_time <- function(
  votes,
  comparator.org = NA,
  country.short = 'US'){
  
  # If no comparator.org, assume UN is the comparator group
  if(!is.data.table(comparator.org)){
    comparator.org <- votes[, .(
      entry = min(date), 
      exit = max(date)), by = country_code]
  }
  
  # Subset the voting data down to the focal country and comparator group
  votes <- votes[country_code %in% c(
    comparator.org$country_code,
    country.short)]
  
  # Pick focal country votes out of all votes
  country.votes <- votes[country_code == country.short, 
                         .(rcid, country.vote = vote)]
  votes <- votes[country_code != country.short]
  
  # Merge focal country votes back on as a column
  votes <- merge(
    votes,
    country.votes,
    by = 'rcid', 
    all.x = TRUE)
  
  # New column identifying if a nation voted with the focal country
  votes[, vote.equal := vote == country.vote]
  # Drop rows where the focal country or a comparator group member didn't vote
  votes <- votes[!is.na(vote.equal)]
  
  # Add comparator group membership dates, used to exclude comparator group 
  # votes taken outside of the period of membership
  votes <- merge(
    votes,
    comparator.org,
    by = 'country_code',
    all.x = TRUE)
  
  # Percentage of votes in comparator that match the focal country
  vote.type <- votes[date %between% .(entry, exit), # time updated 
                     # comparator membership
                     .(perc = round(mean(vote.equal) * 100 , 2),
                       code = ..country.short),
                     by = .(Y, country.vote)]
  
  return(vote.type)
  
}


# Wrangling ---------------------------------------------------------------
tidy.week <- '2021-03-23'
tt_data <- tt_load(tidy.week) 

# Data cleaning and prep
x.rc <- data.table(tt_data$roll_calls) 
x.rc <- x.rc[, .(rcid, date)]
x.uv <- data.table(tt_data$unvotes)

# Merge all info together 
votes <- merge(
  x.rc,
  x.uv,
  by = 'rcid',
  all.x = TRUE)

# Add a year column
votes[, Y := format(date, '%Y')]
# Delete abstaining votes 
votes <- votes[vote != 'abstain']
# Set Federal Republic of Germany to be the predecessor of Germany
votes[country == "Federal Republic of Germany", country_code := 'DE']
# Fix Namibia being NA instead of 'NA'
votes[country == "Namibia", country_code := 'NA']

# Voting patterns
voting.pattern <- rbindlist(
  list(
    rbindlist(
      lapply(
        focal.countries,
        FUN = f_time,
        votes = votes,
        comparator.org = NA))[, comparator := 'UN'],
    rbindlist(
      lapply(
        focal.countries,
        FUN = f_time,
        votes = votes,
        comparator.org = g8))[, comparator := 'G8']))

# Plotting ----------------------------------------------------------------
theme_set(
  theme_minimal(base_size = 15, 
                base_family = 'Roboto'
                ))

theme_update(
  panel.background = element_rect(color = bg, fill = bg),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.spacing = unit(2, "lines"),
  panel.border = element_rect(color = 'grey', fill = NA),
  axis.line.x = element_line(size = 0.2, color = 'grey'),
  axis.title.x = element_blank(),
  axis.title.y = element_text(color = "white", margin = margin(r = 7)),
  axis.text = element_text(color = "white"),
  axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size = 7),
  axis.text.y = element_text(size = 7),
  axis.ticks =  element_blank(),
  axis.ticks.length = unit(.06, "lines"),
  legend.position = "plot",
  plot.title = element_text(hjust = 0, color = "black", 
                            size = 21, margin = margin(t = 10, b = 35)),
  plot.subtitle = element_text(hjust = 0, face = "bold", color = "grey30",
                               size = 14, margin = margin(0, 0, 25, 0)),
  plot.title.position = "plot",
  plot.caption = element_text(color = "white", size = 10, hjust = 1,
                              lineheight = 1.05, margin = margin(30, 0, 0, 0)),
  plot.caption.position = "plot", 
  plot.background = element_rect(fill = bg, color = bg),
  plot.margin = margin(c(10, 5 ,10, 10)),
  strip.text = element_text(color = 'white')
)

# Text labels describing Russian G8/UN participation
df.russia.labels <- data.frame(
  x = c(
    1991.89 - 1980 - 3,
    1998.45 - 1980 - 2,
    2014.25 - 1980 - 3),
  y = rep(21, 3),
  label = c(
    "Dissolution of <br> The Soviet Union",
    "Russia joins <br> The G8",
    "Russia suspended <br> from The G8"),
  country.code = rep('yes', 3),
  code = rep('GB', 3),
  comparator = c('UN', 'G8', 'G8')
)

# Line plots through time 
py <- ggplot(voting.pattern[Y >= 1980 & country.vote == 'yes'],
       aes(x= Y, y = perc, group = code, color = code))+
  geom_line() + 
  geom_vline(xintercept = c(1991.89 - 1980, # Russia begins
                            1998.45 - 1980, # Russia joins
                            2014.25 - 1980), # Russia excluded 
             color = 'grey50', 
             linetype = 'dotted',
             alpha = 0.75) +
  ylab('% of member votes cast in the same direction') +
  scale_color_manual(values = pal.2)+
  scale_x_discrete(breaks = seq(1980, 2019, by=5)) + 
  facet_wrap(.~comparator, strip.position = 'right', ncol = 1) + 
  theme(strip.text = element_blank(),
        strip.background = element_blank()) + 
  geom_richtext(data = df.russia.labels, # Add Russia annotations
            mapping = aes(x = x, y = y, label = label),
            color = 'grey50',
            family = 'Roboto',
            size = 3,
            fill = NA,
            label.color = NA,
            hjust = 0.5,
            vjust = 0)

pn <- ggplot(voting.pattern[Y >= 1980 & country.vote == 'no'],
             aes(x= Y, y = perc, group = code, color = code))+
  geom_line() + 
  geom_vline(xintercept = c(1991.89 - 1980, # Russia begins
                            1998.45 - 1980, # Russia joins
                            2014.25 - 1980), # Russia excluded 
             color = 'grey50', 
             linetype = 'dotted',
             alpha = 0.75) +
  ylab('') +
  scale_color_manual(values = pal.2)+
  scale_y_continuous(position = 'right') +
  scale_x_discrete(breaks = seq(1980, 2019, by=5)) + 
  facet_wrap(comparator~., strip.position = 'right', ncol = 1) +
  theme(axis.text.y = element_blank())

# Main title 
pt <- ggplot() + 
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.8,
      label = "**UN Voting Support For Permanent UN Security Council Members From Other UN Members And The G8**"),
    aes(x, y, label = label),
    color = 'lightgrey',
    box.color = NA,
    fill = NA,
    size = 5,
    width = grid::unit(0.73, "npc"), # 73% of plot panel width
    hjust = 0, vjust = 1, halign = 0.5
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme( #  stop the plot elements being rendered
    axis.line=element_blank(),
    axis.line.x = element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())

# main text
text.CN <- paste0(
  "<span style = 'color:",
  pal.2['CN'],
  ";'>**China**</span>")
text.FR <- paste0(
  "<span style = 'color:",
  pal.2['FR'],
  ";'>**France**</span>")
text.GB <- paste0(
  "<span style = 'color:",
  pal.2['GB'], 
  ";'>**The United Kingdom**</span>")
text.RU <- paste0(
  "<span style = 'color:",
  pal.2['RU'],
  ";'>**Russia**</span>")
text.US <- paste0(
  "<span style = 'color:",
  pal.2['US'],
  ";'>**The United States**</span>")

label <- "The 5 permanent members of the UN security council (China, France, The United Kingdom, Russia, and The United States) obtain varying levels of support from other UN members and members of The Group of 8 (G8). These graphics show the annual % of votes cast by other UN members or G8 members in the same direction as a given Security Council member when they vote either *YES* or *No* at The UN.
  
 When Russia or China vote *YES*, the G8 members regularly vote *NO*, whereas when the other permanent Security Council members vote *YES*, the other G8 nations usually vote *YES* too. When permanent Security Council members vote *YES*, the rest of the UN usually votes *YES* too.
  
  When The United States votes *NO* then G8 members are less likely to also vote *NO* than if France or The United Kingdom vote *NO*. When permanent Security Council members vote *NO* then the rest of the UN rarely vote *NO* too.
  
  Following the dissolution of The Soviet Union, Russia quickly began to vote in the same direction as G8 members, but over time they have drifted from this voting pattern.

When voting *NO* The United States receives the least support from other UN members out of all of the permanent Security Council members"

# Replace mention of each nation with markdown formatted code
label <- gsub('China', text.CN, label)
label <- gsub('France', text.FR, label)
label <- gsub('The United Kingdom', text.GB, label)
label <- gsub('Russia', text.RU, label)
label <- gsub('The United States', text.US, label)

pt.body <- ggplot() + 
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.8,
      label = label),
    aes(x, y, label = label),
    color = 'lightgrey',
    box.color = NA,
    fill = NA,
    size = 3.25,
    width = grid::unit(0.73, "npc"), # 73% of plot panel width
    hjust = 0, vjust = 1, halign = 0.5
  ) +
  labs(
    caption = "Visualisation by Joe O'Reilly (josephedwardoreilly.github.com) - Data taken from TidyTuesday 2021, Week 13")+
  xlim(0, 1) +
  ylim(0, 1) +
  theme( #  stop the plot elements being rendered
    axis.line=element_blank(),
    axis.line.x = element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.caption = element_text(hjust = 0.5, colour = 'grey50'),
    plot.caption.position =  "plot",
    plot.margin = margin(0),
  )

# Subtitles for YES/NO plots
py.title <- ggplot() + 
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.8,
      label = "Yes Votes"),
    aes(x, y, label = label),
    color = 'lightgrey',
    box.color = NA,
    fill = NA,
    size = 4,
    width = grid::unit(0.73, "npc"), # 73% of plot panel width
    hjust = 0, vjust = 1, halign = 0.5
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme( #  stop the plot elements being rendered
    axis.line=element_blank(),
    axis.line.x = element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.margin = margin(0))

pn.title <- ggplot() + 
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.8,
      label = "No Votes"),
    aes(x, y, label = label),
    color = 'lightgrey',
    box.color = NA,
    fill = NA,
    size = 4,
    width = grid::unit(0.73, "npc"), # 73% of plot panel width
    hjust = 0, vjust = 1, halign = 0.5
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme( #  stop the plot elements being rendered
    axis.line=element_blank(),
    axis.line.x = element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.margin = margin(0))


# Arrange the individual elements 
(pt)/(py.title|pn.title)/(py|pn)/(pt.body) + 
  plot_layout(heights = c(.15, .1, 1, .45)) +
  ggsave(
    filename = paste0(getwd(), '/plots/', tidy.week, '.png'),
    width = 16, height = 10.5, device = 'png')







