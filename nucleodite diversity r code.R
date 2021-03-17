# Libraries
library(ggplot2)
# Data
data_pi = read.csv("pi.csv",sep = "," , h = T)

# Plot

p <- ggplot(data_pi, aes(x=Midpoint, y=Pi, colour = Pi)) +
  ylim(0,0.033) +
  geom_line() +
  scale_colour_gradient2(low = "#0000b3", mid = "#3333ff" , high = "#ff0000", midpoint=median(data_pi$Pi)) +
  #theme(legend.position="none") +
  geom_hline(yintercept=0.017, color="#00b300", size=.4, linetype = "dotted" ) +
  theme_minimal()

p



g <- ggplot(data_pi, aes(x=Midpoint, y=Pi, colour = Pi)) +
  ylim(0,0.033) +
  geom_line() +
  scale_colour_gradient2(low = "#0000b3", mid = "#3333ff" , high = "#ff0000", midpoint=0.013) +
  #theme(legend.position="none") +
  geom_hline(yintercept=0.015, color="#00b300", size=.4, linetype = "dotted") +
  labs(y = expression(Nucleotide~diversity~(pi)), x = "Nucleotide position") +
  theme_minimal()+
  theme(panel.grid.major = element_line(colour = "grey", size = 0.2, linetype = "dotted")) +
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.2, linetype = "dotted")) +
  theme(plot.background = element_rect(colour = "white", size = 1))+
  theme(axis.text = element_text(colour = "black")) +
  scale_x_continuous(limits=c(0,155000),
                     breaks = seq(0, 155000, by = 25000)) +
  scale_y_continuous(limits=c(0,0.035),
                     breaks = seq(0, 0.035, by = 0.005))



g


library(plotly)
ggplotly(p)

