library(dplyr)
library(plotly)

cleandata <- mutate(cleandata, survey = FLIR & !daylight)

axis <- list(
  tickfont = list(color = 'black', size = 14),
  titlefont = list(color ='black', size = 16),
  showgrid = FALSE
)

y2 <- append(
  list(
    overlaying = "y",
    side = "right",
    title = "Observed hours",
    zeroline = FALSE,
    range = c(0, 5500)),
  axis
)

filter(cleandata, Month == 1|Month == 12, !is.na(survey))%>%
  group_by(Year)%>%
  summarize(prop = sum(survey, na.rm = TRUE)/n(),
            count = n())%>%
  plot_ly()%>%
  add_trace(name = 'Suitable (%)',
            type = 'bar', x = ~Year, y = ~prop,
            text = ~paste('Suitable hours (%) in', Year, '=', round(prop, 2)),
            hoverinfo = 'text')%>%
  add_trace(name = 'Hours',
            type = 'scatter', mode = 'lines',
            x = ~Year, y = ~count,
            yaxis = 'y2')%>%
  layout(
    legend = list(x = 0.8, y = 1, yanchor = 'left'),
    xaxis = append(list(title = 'Year'), axis),
    yaxis = append(list(title = 'Suitable hours (%)'), axis),
    yaxis2 = y2,
    margin = list(t = 0, b = 0, l = 55, r = 55)
  )

logModel <- glm(data = filter(cleandata, Month ==1|Month ==12), survey ~ Year, family = binomial(link = 'logit'))
