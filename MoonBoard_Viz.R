# Setup ---------------------------------

library(tidyverse)

board_sesh <- read.csv("/board_sesh.csv", encoding="UTF-8") %>%
  select(1:8) %>%
  # Add a column that shows the distance for every move
  mutate(distance = round(sqrt(((to.x-from.x)^2)+((to.y-from.y)^2)), digits = 2))


# ggplot ---------------------------------

board_sesh %>%
  ggplot() +
  # Add points for every hold/bolt on the board
  geom_point(data = data.frame(x = rep(1:11, 18), y = rep(1:18, 11)),
             aes(x, y), size = 1, color = "#d4d1b4") +
  # Add a segment for every move, have color by climb and size by length of move
  geom_segment(aes(x = from.x, xend = to.x,
                   y = from.y, yend = to.y,
                   color = climb,
                   size = distance), 
               alpha = 0.6) +
  # Use the Wes Anderson Darjeeling 1 color palette for the climbs
  scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
  # Add points to indicate holds that are used
  geom_point(aes(x = from.x, y = from.y, color = climb), size = 6, alpha = 0.8) +
  geom_point(aes(x = to.x, y = to.y, color = climb), size = 6, alpha = 0.8) +
  # Add circles around both start and finish holds
  geom_point(data = subset(board_sesh, hold == "start"),
             aes(x = from.x, y = from.y, color = climb), shape = 1, size = 8) +
  geom_point(data = subset(board_sesh, hold == "end"),
             aes(x = to.x, y = to.y, color = climb), shape = 1, size = 8) +
  # Show x-axis as alphabet
  scale_x_continuous(limits = c(1, 11),
                     breaks = (1:11),
                     labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
                     position = "top") +
  scale_y_continuous(limits = c(1, 18),
                     breaks = (1:18),
                     labels = 1:18) +
  # Use hrbr theme ipsum as a starting point for the plot aesthetics 
  hrbrthemes::theme_ipsum() +
  # Remove minor grid lines, format the major lines, text, and color of panel
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", color = "#d4d1b4"),
        panel.grid.major.y = element_line(linetype = "dashed", color = "#d4d1b4"),
        panel.background = element_rect(fill = "#f0efe4", color = "#d4d1b4"),
        legend.text = element_text(color = "#857b5f"),
        legend.title = element_text(color = "#857b5f", face = "bold"),
        title = element_text(color = "#857b5f", face = "bold"),
        axis.text = element_text(face = "bold", color = "#857b5f")) +
  # Remove the size guide/legend
  guides(size = "none") +
  # Remove the x and y axis labels, add title and subtitle
  labs(x = "",
       y = "",
       color = "Climb",
       size = "",
       title = "MoonBoard Session Visualization",
       subtitle = "Example Session #1") +
  # Fix the ratio of the graph to .75
  coord_fixed(ratio = .75)
