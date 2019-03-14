#Title: Shot Charts
#Description: In this file we will be creating shot charts.
#Inputs: nba-court.jpg, the 6 dataframes created in the make-shots-data.
#Outputs: pdf files of all the players shot charts and then a pdf file of all players combined, and a png file of all players combined.

install.packages("jpeg")
library("grid")
library("jpeg")
library(ggplot2)
court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)
thompson_shot_chart <- ggplot(data = thompson) + 
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) +
  ggtitle("Shot Chart: Klay Thompson (2016 Season)") +
  theme_minimal()
curry_shot_chart <- ggplot(data = curry) + 
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) +
  ggtitle("Shot Chart: Stephen Curry (2016 Season)") +
  theme_minimal()
green_shot_chart <- ggplot(data = green) + 
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) +
  ggtitle("Shot Chart: Graymond Green (2016 Season)") +
  theme_minimal()
iguodala_shot_chart <- ggplot(data = iguodala) + 
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) +
  ggtitle("Shot Chart: Andre Iguodala (2016 Season)") +
  theme_minimal()
durant_shot_chart <- ggplot(data = durant) + 
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) +
  ggtitle("Shot Chart: Kevin Durant (2016 Season)") +
  theme_minimal()

pdf(file = "../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()
pdf(file = "../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
iguodala_shot_chart
dev.off()
pdf(file = "../images/graymond-green-shot-chart.pdf", width = 6.5, height = 5)
green_shot_chart
dev.off()
pdf(file = "../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
durant_shot_chart
dev.off()
pdf(file = "../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
thompson_shot_chart
dev.off()

facetted <- ggplot(data = stacked) + 
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) +
  ggtitle("Shot Charts (2016 Season)") +
  theme_minimal() + facet_wrap(name ~.)
facetted
ggsave("../images/gsw-shot-charts.pdf", facetted, width = 8, height = 7)
ggsave("../images/gsw-shot-charts.png", facetted, width = 8, height = 7)