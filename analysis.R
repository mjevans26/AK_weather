library(dplyr)
library(plotly)

filter(cleandata, Month == 1|Month == 12)%>%
  group_by(Year)%>%
  summarize(prop = sum(FLIR, na.rm = TRUE)/n())%>%
  plot_ly(type = 'bar', x = ~Year, y = ~prop)%>%
  layout(
    xaxis = list(
      title = 'Year'
    ),
    yaxis = list(
      title = 'Suitable hours (%)'
    )
  )

logModel <- glm(data = filter(cleandata, Month ==1|Month ==12), FLIR ~ Year, family = binomial(link = 'logit'))
