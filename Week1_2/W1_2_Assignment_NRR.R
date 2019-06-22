# 640 Week 1-2 Assignment
# Raghu Raman Nanduri

# setting up working directory
setwd('C:\\Users\\nrrvlkp\\Documents\\M\\640\\W1-2\\ex1-2')

# Import Library
library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation
#install.packages("xlsx")
#install.packages("readxl")
library(readxl)


# Bar Chart

bar_data <- read_excel("obama-approval-ratings.xlsx")

g <- ggplot(bar_data, aes(x= bar_data$Issue, y =  bar_data$Approve))

p <- g + geom_bar(stat="identity", color="blue", fill="steelblue") + theme_minimal() +
                     ggtitle("Obama approaval ratings by Issues") +
                     xlab("Approval Percentage") + ylab("Issues") 

# Horizontal bar plot
barplot <- p + coord_flip()

barplot

aspect_ratio <- 2.5
height <- 7
ggsave("Bar_chart.png", barplot, height = 10 , width = 8 * aspect_ratio)


# Stacked Bar Chart

stackedbar_data <- read_excel("obama-approval-ratings-pie.xls")
stackedbar_data

p = ggplot(data=stackedbar_data, aes(x=factor(1), y=Percentage,
               fill = factor(Status)),)  +
              geom_bar(stat="identity", width = 1) +
              facet_grid(facets=. ~ Issue) +
              ggtitle("Obama approaval ratings by Issues")


p


aspect_ratio <- 2.5
height <- 7

#saving chart
ggsave("Stackedbar_facets_chart.png", p, height = 10 , width = 8 * aspect_ratio)


p1 <- ggplot(stackedbar_data, aes(x= Issue, y = Percentage, fill = Status))  + 
              geom_bar(stat="identity") + theme_minimal() +
            geom_text(aes(label=Percentage), vjust=1, color="white",check_overlap = FALSE,
                      position = position_dodge(), size=3.5) +
              ggtitle("Obama approaval ratings by Issues")
            
p1
            

# Horizontal Stacked bar plot
stbarplot <- p1 + coord_flip()
stbarplot

#saving chart
ggsave("Stackedbar_chart.png", stbarplot, height = 10 , width = 8 * aspect_ratio)

            
# line chart

wrldpop_data <- read_excel("world-population.xlsx")



line <- ggplot(wrldpop_data, aes(x = Year, y = Population)) +
                 geom_line(aes(colour ="red")) +
                ggtitle("World Population - Trend by Years") 

line

#saving chart
ggsave("line_chart.png", line, height = 10 , width = 8 * aspect_ratio)



# Pie Chart

obama_data <- read_excel("obama-approval-ratings-pie.xlsx")


pie <- ggplot(data = obama_data, aes(x = "", y = Percentage, stat = "identity", fill = Status ) ) +
  geom_bar(stat = "identity", width = 1) + #position="fill") +
  ggtitle("Status by Issues") + xlab("Status") + ylab("Issues") + # Adds titles
  facet_wrap(~Issue) +
  #facet_grid(facets=. ~ Issue) + # Side by side bar chart 
  coord_polar(theta="y") + # side by side pie chart 
  labs(x = NULL, y = NULL, fill = NULL, title = "Obaman approval ratings - by Issues") +
  # adding text labels
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5))+
  # adding custome theme with labels for each issue
  theme_classic() + theme(axis.line = element_blank(),
                            axis.text = element_blank(),
                            axis.ticks = element_blank(),
                            plot.title = element_text(hjust = 0.5, color = "#666666"))

pie

aspect_ratio <- 2.5
height <- 7
# saving the pie chart
ggsave("faceted_pie_charts.png", pie, height = 10 , width = 8 * aspect_ratio)


# Donut Chart

obama_data$ymax = cumsum(obama_data$Percentage)
obama_data$ymin = c(0, head(obama_data$ymax, n=-1))


donut <- ggplot(data = obama_data, aes(x = 2, y = Percentage, stat = "identity", fill = Status ) ) +
  geom_bar(stat = "identity", width = 1) + #position="fill") +
  ggtitle("Status by Issues") + xlab("Status") + ylab("Issues") + # Adds titles
  facet_wrap(~Issue) +
  xlim(0.5, 2.5) + # for creating a donut
  #facet_grid(facets=. ~ Issue) + # Side by side bar chart 
  coord_polar(theta="y") + # side by side pie chart 
  labs(x = NULL, y = NULL, fill = NULL, title = "Obaman approval ratings - by Issues - Donut chart") +
  # adding text labels
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5))+
  # adding custome theme with labels for each issue
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))
 


donut

#saving chart
ggsave("faceted_donut_charts.png", donut, height = 10 , width = 8 * aspect_ratio)
