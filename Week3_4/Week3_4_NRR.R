

# Tree Maps
# Install below packages if need be
#install.packages("treemap")
#install.packages("treemapify")

setwd('C:\\Users\\nrrvlkp\\Documents\\M\\640\\640_git\\DataVizs\\Week3_4')

# Import Library
library("ggplot2")  # Data visualization
library(treemap)
library(treemapify)

# Reading data
treedata <- read.table('Treemapdata.csv' , header=TRUE, sep = ",")
head(treedata)

colnames(treedata)

# converting revenue column to numeric
treedata$REVENUE <- as.numeric(treedata$REVENUE)

png(filename="treemap.png",width=800, height=800)

treechart <- treemap(treedata,
              index=c("GENRE","TOPIC"),
              vSize="REVENUE",
              type="index",
              fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
              fontcolor.labels=c("white","Blue"),    # Color of labels
              fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
              bg.labels=c("transparent"),              # Background color of labels
              align.labels=list(
                c("center", "center"), 
                c("center", "bottom")
              ),                                   # Where to place labels in the rectangle?
              overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
              inflate.labels=F,
              title="Tree map of Genre - Topic - Revenue",                      # Customize your title
              fontsize.title=12
        )    
dev.off()
treechart

#saving chart
aspect_ratio <- 2.5
height <- 7

  

# Stacked Area Chart

library(plotly)
library(readxl)

area_data <- read_excel("obama-approval-ratings.xlsx")

colnames(area_data)

library(reshape2)
reshapeddata <- data.frame(melt(cbind(rownames(area_data),area_data)))

reshapeddata2 <- reshapeddata[, c(2,3,4)]

reshapeddata2$value <- as.numeric(reshapeddata2$value)

stackedarea <- ggplot(reshapeddata2, aes(x=Issue, y=value, fill=variable, group=variable)) +
        geom_area() +
        ggtitle("Obama approaval ratings by Issues") +
        xlab("Issues") + ylab("Ratings")  +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

stackedarea

ggsave("stackedarea.png", stackedarea)

# Area Chart

reshapeddata2app <- reshapeddata2[reshapeddata2$variable == "Approve", ]
reshapeddata2app
  
reshapeddata2disapp <- reshapeddata2[reshapeddata2$variable == "Disapprove", ]
reshapeddata2disapp

reshapeddata2none <- reshapeddata2[reshapeddata2$variable == "None", ]
reshapeddata2none



areachart <- ggplot(show_guide = TRUE) +
  geom_area(data = reshapeddata2disapp, aes(x=Issue, y=value,group=variable),fill='lightpink', alpha=0.6) +
  geom_area(data = reshapeddata2app, aes(x=Issue, y=value,group=variable),fill='lightblue', alpha=0.6) + 
  geom_area(data = reshapeddata2none, aes(x=Issue, y=value,group=variable),fill='orange', alpha=0.6) +
  ggtitle("Obama approaval ratings by Issues") +
    xlab("Issues") + ylab("Ratings")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #scale_color_hue("variable", guide=guide_legend(order=1)) +
  #scale_fill_identity(name = 'Rating', guide = 'legend',labels = c("Disapprove", "Approve", "None")) 
  scale_fill_manual(
    guide=guide_legend(override.aes = list(colour=c("lightpink", "lightblue", "orange"))),
    labels=c("Disapprove", "Approve", "None") 
                  ) +  
  scale_color_manual(name = "", values = c("line.label" = "black")) +
  theme(legend.position = "left")

areachart


# Step Chart
  
step_data <- read_excel("us-postage.xlsx")
step_data
  
stepchart <- ggplot() + 
  geom_step(data=step_data,mapping=aes(x=Year,y=Price),
            linetype=2,color='Red',alpha=1) +
          ggtitle("US Postage Rates") +
          xlab("Year") + ylab("Price")

stepchart

ggsave("stepchart.png", stepchart)

    
    