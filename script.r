library(ggplot2)
library(ggthemes)
source('config.r')

# Files
download.file(url = url, destfile = temporary_file, method = 'curl')
data_set <- tail(read.csv(file = temporary_file )[c('data','confirmados_novos')], last_n_days)

# Prediction
confirmed <- tail(data_set[c('confirmados_novos')], prediction_interval)
days <- seq(1, prediction_interval, by = 1)
prediction_data <- data.frame(days, confirmed)
fit <- lm(confirmados_novos ~ days, data = prediction_data)
prediction <- round(predict(fit, data.frame(days = prediction_interval +1 )), 0)

# Message
today <- tail(data_set['confirmados_novos'], n = 1)
cat(sprintf("today: %s | tomorrow: %s", today, prediction), file = commit_message_file)

# Graph
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
ggsave(image_file, dpi = 310)