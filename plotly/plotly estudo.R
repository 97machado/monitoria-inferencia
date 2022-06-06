library(plotly)

#Line Plots
plot_ly(x = c(1, 2, 3), y = c(5, 6, 7), type = 'scatter', mode = 'lines')

#Scatter Plots
plot_ly(x = c(1, 2, 3), y = c(5, 6, 7), type = 'scatter', mode = 'markers')

#Bar Plots
plot_ly(x = c(1, 2, 3), y = c(5, 6, 7), type = 'bar', mode = 'markers')

#Bubble Charts
plot_ly(x = c(1, 2, 3), y = c(5, 6, 7), type = 'scatter', mode = 'markers',
        size = c(1, 5, 10), marker = list(color = c('red', 'blue', 'green')))

#Heatmaps
plot_ly(z = volcano, type = 'heatmap')

#AreaPlots
plot_ly(x = c(1, 2, 3), y = c(5, 6, 7), type = 'scatter',
        mode = 'lines', fill = 'tozeroy')

#Legendas
set.seed(123)
x = 1:100
y1 = 2 * x + rnorm(100)
y2 = -2 * x + rnorm(100)
plot_ly(x = x, y = y1, type = 'scatter') %>%
  add_trace(x = x, y = y2) %>%
  layout(legend = list(x = 0.5, y = 1, bgcolor = '#F3F3F3'))

#Eixos
set.seed(123) 
x = 1:100 
y1 = 2 * x + rnorm(100)
y2 = -2 * x + rnorm(100)
axis_template <- list(showgrid = F, zeroline = F, nticks = 20, showline = T,
                      title = 'AXIS', mirror = 'all')
plot_ly(x = x, y = y1, type = 'scatter') %>% 
  layout(xaxis = axis_template, yaxis = axis_template)



#Histograma
x <- rchisq(10000,5,0)
plot_ly(x = x, type = 'histogram') 

#Box-Plot
plot_ly(y = rnorm(50), type = 'box') %>%
  add_trace(y = rnorm(50, 1))

#Histograma 2D
plot_ly (x = rnorm(1000, sd = 10), y = rnorm(1000, sd = 5), type = 'histogram2d')


#Bubblemap
plot_ly(type = 'scattergeo', lon = c(-73.5, 151.2), lat = c(45.5, -33.8),
        marker = list(color = c('red', 'blue'), size = c(30, 50),
                      mode = 'markers'))
#Choropleth Map
plot_ly(type = 'choropleth', locations = c('AZ', 'CA', 'VT'),
        locationmode = 'USA-states', 
        colorscale = 'Viridis', z = c(10, 20, 40)) %>% 
  layout(geo = list(scope = 'usa'))

#ScatterMap
plot_ly(type = 'scattergeo', lon = c(42, 39), lat = c(12, 22), 
        text = c('Rome', 'Greece'), mode = 'markers')

#3D surface plots
# Using a dataframe:
plot_ly(type = 'surface', z = ~volcano)

#3d line plor
plot_ly(type = 'scatter3d', x = c(9, 8, 5, 1), y = c(1, 2, 4, 8), z = c(11, 8, 15, 3),
        mode = 'lines')
#3d scatterplot
plot_ly(type = 'scatter3d', x = c(9, 8, 5, 1), y = c(1, 2, 4, 8), 
        z = c(11, 8, 15, 3), mode = 'markers') 

#
airquality_sept <- airquality[which(airquality$Month == 9),]
airquality_sept$Date <- as.Date(paste(airquality_sept$Month, airquality_sept$Day, 1973, sep = "."), format = "%m.%d.%Y")

p <- plot_ly(airquality_sept) %>%
  add_trace(x = ~Date, y = ~Wind, type = 'bar', name = 'Wind',
            marker = list(color = '#C9EFF9'),
            hoverinfo = "text",
            text = ~paste(Wind, ' mph')) %>%
  add_trace(x = ~Date, y = ~Temp, type = 'scatter', mode = 'lines', name = 'Temperature', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text",
            text = ~paste(Temp, '°F')) %>%
  layout(title = 'New York Wind and Temperature Measurements for September 1973',
         xaxis = list(title = ""),
         yaxis = list(side = 'left', title = 'Wind in mph', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Temperature in degrees F', showgrid = FALSE, zeroline = FALSE))






###
x <- seq(-3, 3, 0.001)
y <- dnorm(x)
plot_ly(x = x, y = y, type = "scatter", mode = "lines") %>%
  add_trace(x = x, y = dt(x, 5), dash = "dot")


#Gerando 10.000 observações da Beta(3,2) a partir da U(0,1) pelo método da aceitação-rejeição:
n <- 0
x_beta <- c()
arquivo <- c()
k <-  dbeta(2/3, 3, 2) #Valor de k obtido quando x = 2/3 na função g(x)
while(n < 10000){
  x_auxiliar <- runif(1,0,1) #Geração da auxiliar q(x) {U(0,1)}
  arquivo <- c(arquivo, x_auxiliar) #Arquivo contendo todos os possíveis valores para a amostra
  razao <- dbeta(x_auxiliar,3,2) / (k * dunif(x_auxiliar, 0, 1)) #Probabilidade de Aceitação
  u <- runif(1,0,1)
  if(u < razao){
    x_beta <- c(x_beta, x_auxiliar)
    n <- n + 1}
}
x_beta #Amostra de tamanho 10.000 da Beta(3,2) a partir da U(0,1) pelo método da aceitação rejeição

library(plotly)

plot_ly(x = seq(0,1,0.01), y = dbeta(seq(0,1,0.01), 3, 2), type = "scatter", 
        mode = "lines", name = "Beta(3,2)") %>%
  add_trace(x = seq(0,1,0.01), y = k * dunif(seq(0,1,0.01), 0, 1), name = "k * U(0,1)") %>%
  layout(title = 'Aceitação-Rejeição Beta(3,2) e U(0,1)')

plot_ly(x = x_beta, type = "histogram", histnorm = "probability") #%>%
add_trace(x = seq(0,1,0.01), y = dbeta(seq(0,1,0.01), 3, 2), type = "scatter", 
          mode = "lines", name = "Densidade")

#
n = 10
plot_ly(x = seq(0.1,10,0.01), y = (4*(seq(0.1,10,0.01))^2 + 27*n*(seq(0.1,10,0.01))^2)/48, type = "scatter", 
        mode = "lines", fill = "tozeroy") %>%
 add_trace(x = seq(0.1,10,0.01), y = (2*(seq(0.1,10,0.01))^2) / ((n+2)*(n+1)) ,
           type = "scatter", 
           mode = "lines") 
  








hist(x_beta, main = "Amostra Aceita", prob = T, ylim = c(0,2))
lines(seq(0,1,0.01), dbeta(seq(0,1,0.01), 3, 2), col = 4, lwd = 2)


plot(seq(0,1,0.01), dbeta(seq(0,1,0.01), 3, 2), type = "l")
lines(seq(0,1,0.01), k * dunif(seq(0,1,0.01), 0, 1), col = 2)





x1 <- NULL
z1 <- NULL
for (i in 1:30) {
  x1 <- rnorm(30)
  z1[i] <- (mean(x1) - 0) / sqrt(var(x1) / 30)  
}


plot_ly(x = ~z1, type = "histogram", histnorm = "probability density", name = "Amostra", )  %>%
add_trace(x = seq(-3,3,0.01), y = dt(seq(-3,3,0.01), 29), type = "scatter", mode = "lines",
          name = "t-student") %>%
layout(title = "Transformação para t-student")


hist(z1, prob = T)
lines(x = seq(-3,3,0.01), y = dt(seq(-3,3,0.01), 999))


plot_ly(x = seq(0,1,0.01), y = dbeta(seq(0,1,0.01), 3, 2), type = "scatter", 
        mode = "lines", name = "Beta(3,2)") %>%
  add_trace(x = seq(0,1,0.01), y = k * dunif(seq(0,1,0.01), 0, 1), name = "k * U(0,1)") %>%
  layout(title = 'Aceitação-Rejeição Beta(3,2) e U(0,1)')
