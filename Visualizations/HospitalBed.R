###############################################################################
# File: HospitalBedViz.R
# Date: 03/20/2018
# Author: Raheem Chaudhry
# Description: Generate visualization of the hospital bed analogy.
###############################################################################

###############################################################################
# 0. SET ENVIRONMENT
###############################################################################

# Restart environment with Ctrl + Shift + F10
# Set working directory using setwd() in console or Ctrl + Shift + H

# Make sure you have ImageMagick
# install.imagemagick("https://www.imagemagick.org/script/download.php")
# Sys.setenv(PATH = paste("C:/Program Files/ImageMagick/bin", Sys.getenv("PATH"), sep = ";"))

# Import packages
library(tidyverse)
library(reshape2)
library(ggplot)
library(gganimate)
library(tweenr)

###############################################################################
# 1. GENERATE DATASET
###############################################################################

# Create dataset
day <- 1:10
bed1 <- rep(1, 10) # person ID of person in Bed 1
bed2 <- 2:11 # person ID of person in Bed 2
df <- data.frame(day, bed1, bed2)
df.long <- melt(df,id=c("day"))
df.long <- rename(df.long, bed = variable, personid = value)

# Calculate running averages for wide dataset
df <- mutate(group_by(df, bed1), bed1.lengthofstay = seq(n()))
df <- mutate(group_by(df, bed2), bed2.lengthofstay = seq(n()))
df$numpassedthrough <- with(df, bed1 + bed2 - 1)
df$averagelengthofstay.pit <- with(df, (bed1.lengthofstay + df$bed2.lengthofstay)/2)
df$averagelengthofstay.ever <- with(df, day*2/numpassedthrough)

# Calculate statistics for long dataset
df.long <- mutate(group_by(df.long, personid), runninglengthofstay=seq(n()))

###############################################################################
# 2. PLOT
###############################################################################

df.long <- df.long %>%
  mutate(width = case_when(bed=="bed1" ~ .9,
                           bed=="bed2" ~ .6))

p <- ggplot() + 
  geom_bar(data=df.long, 
           aes(x=factor(day), y=runninglengthofstay, fill=bed, frame=day, alpha = factor(bed)), 
           width=df.long$width, position="identity", stat="identity") + 
  geom_line(data=df,
            aes(x=factor(day), y=averagelengthofstay.pit, color="Currently in room"),
            size=1.3, alpha=1, group=1) + 
  geom_line(data=df, 
            aes(x=factor(day), y=averagelengthofstay.ever, color="Ever in room"),
            size=1.3, alpha=1, group=1) + 
  labs(x="Day", y="Length of stay", 
       title="Average length of stay among patients in a hospital room", 
       subtitle="Between two patients on a given day vs. among all patients ever in the room") + 
  scale_fill_manual(name="Bed in room", guide="legend", 
                    values = c("bed1" = "#bababa", "bed2" = "#888888"), 
                    labels=c("Bed 1", "Bed 2")) +
  scale_color_manual(name="Average days in hospital among those...", 
                     values=c("Currently in room"="#4ecce8", "Ever in room"="#e8554e")) + 
  scale_alpha_manual(values=c("bed1"=.55, "bed2"=.8), guide="none") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major.y=element_line(color="#e2e2e2", linetype="dotted"), 
        panel.grid.minor.y=element_line(color="#e2e2e2", linetype="dotted"),
        axis.ticks=element_blank()) + 
  geom_hline(yintercept=0, color="#bababa")

ggsave("HospitalBed.png", p, width=7.5, height=5)

###############################################################################
# 3. ANIMATE
###############################################################################


df.long <- as.data.frame(df.long)
tdf <- map(seq(nrow(df.long)), ~df.long[c(seq(.x), rep(.x, nrow(df.long) - .x)), ]) %>%
  tween_states(1, 0, "linear", 10)


df <- as.data.frame(df)
tdf <- map(seq(nrow(df)), ~df[c(seq(.x), rep(.x, nrow(df) - .x)), ]) %>%
  tween_states(1, 0, "linear", 100)

p2 <- ggplot() + 
  geom_bar(data=df.long, 
           aes(x=factor(day), y=runninglengthofstay, fill=bed, frame=day*10, cumulative=TRUE, alpha = factor(bed)), 
           width=df.long$width, position="identity", stat="identity") + 
  geom_line(data=tdf, group=1, 
            aes(x=day, y=averagelengthofstay.pit, frame=.frame, cumulative=TRUE,  color="Currently in room"),
            size=1.3, alpha=1) + 
  geom_line(data=tdf, group=1, 
            aes(x=day, y=averagelengthofstay.ever, frame=.frame, cumulative=TRUE,  color="Ever in room"),
            size=1.3, alpha=1) + 
  labs(x="Day", y="Length of stay", 
       title="Average length of stay among patients in a hospital room", 
       subtitle="Between two patients on a given day vs. among all patients ever in the room") + 
  scale_fill_manual(name="Bed in room", guide="legend", 
                    values = c("bed1" = "#bababa", "bed2" = "#888888"), 
                    labels=c("Bed 1", "Bed 2")) +
  scale_color_manual(name="Average days in hospital among those...", 
                     values=c("Currently in room"="#4ecce8", "Ever in room"="#e8554e")) + 
  scale_alpha_manual(values=c("bed1"=.55, "bed2"=.8), guide="none") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major.y=element_line(color="#e2e2e2", linetype="dotted"), 
        panel.grid.minor.y=element_line(color="#e2e2e2", linetype="dotted"),
        axis.ticks=element_blank()) + 
  geom_hline(yintercept=0, color="#bababa")

gganimate(p2, filename="HospitalBed.gif", ani.width=720, ani.height=480, interval = 1/24, title_frame = FALSE)

###############################################################################
# END OF FILE
###############################################################################