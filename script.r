library(ggplot2)
library(ggthemes)
library(rnaturalearth)
library(tidyverse)
source('config.r')

temporary_file = 'data.csv'

#download.file(url = url, destfile = temporary_file, method = 'curl')

#Evolution graph
data_set <- tail(read.csv(file = temporary_file )[c('data','confirmados_novos')], last_n_days)
ggplot(data_set, aes(x = as.Date(factor(data, ordered = T), format = "%d-%m-%Y"), y = confirmados_novos, group = 1)) +
    geom_line(color = line_color) +
    geom_point(color = point_color) +
    scale_x_date(expand = c(0.02, 0), date_labels = "%d %b", date_breaks = "1 day") +
    geom_text(aes(label = confirmados_novos), vjust = -1, size = 2.2, color = text_color) +
    labs(
        title= sprintf('Covid19 cases in Portugal, last %i days', last_n_days), 
        caption=sprintf('Source: %s', source), 
        x='Date',
        y='Cases', 
        color=NULL
    ) + 
    theme_hc(style = "darkunica") +
    scale_colour_hc("darkunica") +
    theme(
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.minor = element_blank(),
    )
ggsave(image_file, dpi = 300)

today_number_of_cases <- tail(data_set['confirmados_novos'], n = 1)
date <- tail(data_set['data'], n = 1)
cat(sprintf("%s new cases on %s", today_number_of_cases, date), file = commit_message_file)

# Map
#country <- ne_states(geounit = country_name, returnclass = "sf")
#ggplot(data = country) +
#  geom_sf() +
#  theme_hc(style = "darkunica") +
#  scale_colour_hc("darkunica") +
#  theme(
#    panel.grid = element_blank()
#  )
#ggsave(image_file2, dpi = 300)

# Distribution
temp <- diff(as.matrix(tail(read.csv(file = temporary_file )[4:8], 2)))
df <- data.frame(
  group=regions,
  value=c(temp[1],temp[2],temp[3],temp[4],temp[5])
)

df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

ggplot(df, aes(x = "", y = value, fill = fct_inorder(group))) +
  geom_col(width = 1, color = 1) +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5), color=text_color, size=7 ) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Group")) +
  scale_y_continuous(breaks = df2$pos, labels = df$group) +
  theme_void() + 
  theme(
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 15, color=text_color), 
        legend.position = "none"
  ) +
  scale_fill_manual(values = mycols)

  ggsave(image_file2, dpi = 300)