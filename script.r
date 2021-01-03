library(ggplot2)
source('config.r')

temporary_file = 'data.csv'

download.file(url = url, destfile = temporary_file, method = 'curl')

data_set <- tail(read.csv(file = temporary_file )[c('data','confirmados_novos')], last_n_days)

ggplot(data_set, aes(x = as.Date(factor(data, ordered = T), format = "%d-%m-%Y"), y = confirmados_novos, group = 1)) +
    geom_line(color = line_color) +
    geom_point(color = point_color) +
    scale_x_date(expand = c(0.02, 0), date_labels = "%d %b", date_breaks = "1 day") +
    geom_text(aes(label = confirmados_novos), vjust = -1, size = 2.2) +
    labs(
        title= sprintf('Number of new covid19 cases in Portugal, last %i days', last_n_days), 
        caption=sprintf('Source: %s', source), 
        x='Date',
        y='Number of cases', 
        color=NULL
    ) + 
    theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    )

ggsave(image_file, dpi = 300)

today_number_of_cases <- tail(data_set['confirmados_novos'], n = 1)
date <- tail(data_set['data'], n = 1)
cat(sprintf("%s new cases on %s %s", today_number_of_cases, date, remote_image_url), file = commit_message_file)
