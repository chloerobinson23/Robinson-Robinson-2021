# Chloe Robinson, Apr 16 2021

library(stringr) # str_split
library(reshape2) # dcast
library(vegan) # rrarefy
library(ggplot2) # ggplot
library(data.table) # setDT
library(gridExtra) # grid.arrange
library(cowplot) # get_legend
library(ggpubr) # normality
library(extrafont) # get more fonts
loadfonts(device = "win")
loadfonts(device = "pdf")

#####################################################################
# Look at richness
#####################################################################

# Read in cat.csv
A <- read.csv(file="bat_data_2020.csv", head=TRUE)

# Create dataframes for vegan

# pivot to make recordings matrix of survey by species using unique recording as value
A.1.calls<-reshape2::dcast(A, Survey_ID ~ Species, value.var = "Recording_ID")

# move sample to rownames then delete
rownames(A.1.calls) <- A.1.calls$Survey_ID
A.1.calls$Survey_ID <- NULL

#remove columns with only zeros
calls.notnull<-A.1.calls[,colSums(A.1.calls) !=0]

#remove rows with only zeros 
calls.notnull2<-calls.notnull[rowSums(calls.notnull) !=0,]

# Convert to presence-absence matrix
#rare.mat[rare.mat>0] <-1

# Convert to df
df<-data.frame(calls.notnull2, stringsAsFactors = FALSE)  

# Get total recordings per survey
df$sums<-rowSums(df)

# Move rownames to first column
df2<-data.frame(df, stringsAsFactors = FALSE)
setDT(df2, keep.rownames = TRUE)[]

# Get separate site and date cols
setDT(df2)[, paste0("S", 1:4) := tstrsplit(rn, "_")]
colnames(df2)[colnames(df2)=="S1"] <- "Site"
colnames(df2)[colnames(df2)=="S2"] <- "Day"
colnames(df2)[colnames(df2)=="S3"] <- "Month"
colnames(df2)[colnames(df2)=="S4"] <- "Habitat"

# create factors
df2$Site <- factor(df2$Site,
                        levels = c("MEC","HAN", "RSP", "ERT"),
                        labels = c("Mitchell-Ellis Creek Park","Hanlon Creek Park","Riverside Park","Eramosa River Trail"))
df2$Day <- factor(df2$Day,
                   levels = c("02","06", "11", "13","14","17","20","27","28","31","05","07","09","12","15","16","19","22","24","04","10","21","23","08","29"),
                   labels = c("02","06", "11", "13","14","17","20","27","28","31","05","07","09","12","15","16","19","22","24","04","10","21","23","08","29"))
df2$Month <- factor(df2$Month,
                   levels = c("Jul","Aug", "Sep"),
                   labels = c("July","August","September"))
df2$Habitat <- factor(df2$Habitat,
                    levels = c("River","Forest"),
                    labels = c("Riverine","Forested"))

# Compare richness by site (scatter)

plot1 <- ggplot(df2) +
  geom_boxplot(aes(x=df2$Site, y=df2$sums)) +
  ggtitle(element_blank()) +
  labs(x="Survey Site", y="Number of Recordings") +
  theme(legend.title=element_blank()) +
  scale_colour_grey(start = 0, end = .6)+
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "Arial"),
        axis.title.x = element_text(angle =0, hjust = 0.5, vjust = 0.2),
        axis.title.y = element_text(angle = 90, hjust = 0.5,vjust = 0),
        axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0),
        legend.position = "none")
plot1

ggsave("Fig1.tiff", plot1, width = 8, height = 6, dpi=800)


# test for normality pkg "ggpubr"
ggdensity(df2$sums, 
          main = "Density plot",
          xlab = "Recording richness")
# not normal

ggqqplot(df2$sums)
# mostly normal

#Shapiro-Wilk test of normality
shapiro.test(df2$sums)
# data:  df2$sums
# W = 0.91499, p-value = 0.00611
# sig diff than normal
