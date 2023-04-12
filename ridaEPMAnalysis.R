setwd("/Users/rida/Desktop/EPM")
library(readxl); #read Excel data into R
library(writexl);#export data frames in R into Excel
library(ggplot2); #make various graphs
library(forcats); #allows for manipulation of factors (Categorical variables)
library(ggsci); #color palettes for ggplot2
library(patchwork); #combine multiple ggplots into one graphic
library(ez); #factorial experiment analysis and visualization
library(rstatix); #basic statistical tests (t-test, ANOVA, etc)
library(multcomp); #simultaneous inference in linear models
library(tidyverse) #collection of R packages for data manipulation and visualization, includes %>% (pipe operator from magrittr package)

#### Curated data file ####
EPM <- read_xlsx(sheet = 1, 'EPM Videos.xlsx')
EPM_long <- EPM %>%
  rstatix::select(MouseID, Condition:c("Time spent in open arms")) %>%
  pivot_longer(cols = c("Freeze (first 2 min)"):c("Time spent in open arms"), names_to = "Behavior", values_to = "Response")

fig_stats <- function(x) 
{x %>% summarize(mean = mean(Response),
                 sd   = sd(Response),
                 n    = n(),
                 sem  = sd / sqrt(n))}

EPM_all <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Condition, Sex, Age) %>% 
  fig_stats

EPM_stats_age_sex <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Age, Sex) %>% 
  fig_stats

EPM_stats_age_condition <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Age, Condition) %>% 
  fig_stats

EPM_stats_sex_condition <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Sex, Condition) %>% 
  fig_stats

EPM_stats_condition <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Condition) %>% 
  fig_stats

EPM_stats_age <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Age) %>% 
  fig_stats

EPM_stats_sex <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Sex) %>% 
  fig_stats

p_visual <- ggplot(EPM_all, aes(x = Behavior, y = mean, fill = interaction(Sex,Age,Condition,Behavior))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Visualization of All Possible Combinations") +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#75797c", 
                position = position_dodge(width = 0.9)) + 
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  scale_y_continuous("Average Time (seconds)", breaks= seq(0, 200,20)) +
  theme_classic()

p_visual

p_1 <- ggplot(EPM_stats_condition, aes(x = Behavior, y = mean, fill = reorder(Condition, mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time for Various Behaviors in EPM Based on Condition") +
  scale_fill_manual(name = "Condition", values = c("#fca8d5","#c893ea","#64b5ff")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#75797c", 
                position = position_dodge(width = 0.9)) + 
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 200,20)) +
  theme_classic()

p_1

p_2 <- ggplot(EPM_stats_age, aes(x = Behavior, y = mean, fill = reorder(Age, mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time for Various Behaviors in EPM Based on Age") +
  scale_fill_manual(name = "Condition", values = c("#F18805","#5466A7")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#2F2F2F", 
                position = position_dodge(width = 0.9)) + 
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 200,20)) +
  theme_classic()

p_2

p_3 <- ggplot(EPM_stats_sex, aes(x = Behavior, y = mean, fill = reorder(Sex, mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time for Various Behaviors in EPM Based on Sex") +
  scale_fill_manual(name = "Condition", values = c("#CCABFC","#62BF92")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#424445", 
                position = position_dodge(width = 0.9)) + 
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 200,20)) +
  theme_classic()

p_3

p_4 <- ggplot(EPM_stats_age_sex, aes(x = Behavior, y = mean, fill = interaction(Sex,Age))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time for Various Behaviors in EPM Based on Age and Sex") +
  scale_fill_manual(name = "Group", labels = c("Adolescent Female", "Adult Female", "Adolescent Male", "Adult Male"),
                    values = c("#a9a7d5","#756a92","#b1d0c5","#84a197")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#464747", 
                position = position_dodge(width = 0.9)) + 
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 200,20)) +
  theme_classic()

p_4

p_5 <- ggplot(EPM_stats_age_condition, aes(x = Behavior, y = mean, fill = interaction(Age, Condition))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time for Various Behaviors in EPM Based on Age and Condition") +
  scale_fill_manual(name = "Group", labels = c("Adolescent Homecage", "Adult Homecage", "Adolescent Safety", "Adult Safety", "Adolescent Yoked Fear", "Adult Yoked Fear"),
                    values = c("#f2c4de","#b07e9a","#cdb2e5","#997cb2", "#abcaef", "#7d98b9")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#464747", 
                position = position_dodge(width = 0.9)) + 
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  scale_y_continuous("Average Time (seconds)", breaks= seq(0, 200,20)) +
  theme_classic()

p_5

p_6 <- ggplot(EPM_stats_sex_condition, aes(x = Behavior, y = mean, fill = interaction(Sex, Condition))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time for Various Behaviors in EPM Based on Condition and Sex") +
  scale_fill_manual(name = "Group", labels = c("Female Homecage", "Male Homecage", "Female Safety", "Male Safety", "Female Yoked Fear", "Male Yoked Fear"),
                    values = c("#F7B2B2","#C4F2FF","#FF7979","#76D9F5", "#A46464", "#538593")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#464747", 
                position = position_dodge(width = 0.9)) + 
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  scale_y_continuous("Average Time (seconds)", breaks= seq(0, 200,20)) +
  theme_classic()

p_6

p_eee <- ggplot(EPM_all, aes(Behavior, Condition, Sex, Age)) +
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  geom_raster(aes(fill = mean))

p_eee

p_aaa <- ggplot(EPM_all, aes(Behavior, Sex, fill = mean)) +
  scale_x_discrete(name="Behavior", labels = c("Freezing", "Latency to Middle", "Time in Open Arms")) + 
  geom_raster(aes(fill = mean))

p_aaa

library(egg)
myLimits <- list(
  list("Freezing", 0, 15),
  list("Latency to Middle", 0, 150),
  list("Time in Open Arms", 0, 200)
)

plotHeat <- function(type, MIN, MAX) {
  p <- ggplot(subset(EPM_stats_sex, Behavior == type),
              aes(mean, Behavior, fill = mean, label = "Mean")) +
    geom_tile() +
    geom_text(color = "black", size = 3) +
    scale_fill_continuous(type = "viridis", limits = c(MIN, MAX)) +
    labs(x    = "Groups",
         y    = NULL,
         fill = type) +
    theme_bw()
  # Output x-axis only for the last plot
  if (type != myLimits[[length(myLimits)]][[1]]) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.line.x = element_blank(),
                   axis.ticks.x = element_blank())
  }
  return(p)
}

res <- lapply(myLimits, function(x) {plotHeat(x[[1]], x[[2]], x[[3]])})
ggarrange(plots = res)



