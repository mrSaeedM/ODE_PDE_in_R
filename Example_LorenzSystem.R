library(deSolve)
library(scatterplot3d)
library(ggplot2)
library(gridExtra)
library(plotly)
## Chaos in the atmosphere
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <-  a * X + Y * Z
    dY <-  b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}

parameters <- c(a = -8/3, b = -10, c = 30)
state      <- c(X = 1, Y = 1, Z = 1)
times      <- seq(0, 60, by = 0.01)

solution <- ode(y = state, times = times, func = Lorenz, parms = parameters)

# Convert solution to data frame for ggplot2
solution_df <- as.data.frame(solution)
# Plot the results using ggplot2

# Plot the results using ggplot2 for each component
plot1 <- ggplot(solution_df, aes(x = time, y = X)) +
  geom_line() +
  labs(title = "X",
       x = "Time",
       y = "y")

plot2 <- ggplot(solution_df, aes(x = time, y = Y)) +
  geom_line() +
  labs(title = "Y",
       x = "Time",
       y = "Y")

plot3 <- ggplot(solution_df, aes(x = time, y = Z)) +
  geom_line() +
  labs(title = "Z",
       x = "Time",
       y = "Z")

plot3 <- ggplot(solution_df, aes(x = time, y = Z)) +
  geom_line() +
  labs(title = "Z",
       x = "Time",
       y = "Z")

# Arrange plots in one panel
arranged_plots = ggarrange(plot1, plot2, plot3, ncol = 1)
#Save arranged plots as a pdf
ggsave("arranged_plots.pdf", arranged_plots)

# Create 3D plot using plotly
plot4 <- plot_ly(data = solution_df, x = ~X, y = ~Y, z = ~Z, type = "scatter3d", mode = "lines") %>%
  layout(scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")))
plot4

