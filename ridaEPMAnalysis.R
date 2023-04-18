setwd("/Users/rida/Desktop/BU/Meyer Lab/EPM")
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
#install.packages("ggpubr")
library(ggpubr)

#### Curated data file ####
EPM <- read_xlsx(sheet = 1, 'AE masterdoc.xlsx')
#dont have rstatix in actual code btw!
EPM_long <- EPM %>%
  rstatix::select(MouseID, Condition:c("Time spent in open arms")) %>%
  pivot_longer(cols = c("Freeze (first 2 min)"):c("Time spent in open arms"), names_to = "Behavior", values_to = "Response")

fig_stats <- function(x) 
{x %>% summarize(mean = mean(Response),
                 sd   = sd(Response),
                 n    = n(),
                 sem  = sd / sqrt(n))}

# to do - have an all, have some for each behavior, and then filter in the speciifc calls or jsut re-organize so 
# the plot is made right after the dataframe is made.

# code from AE code_EPM.R (line 178)
# EPM_time <- EPM %>% filter(Age == "Adolescent", Sex == "Female",
#                            !Epoch == "Total_Distance_traveled_cm", !Epoch == "Mean_Velocity_cm_s", !Epoch == "Open_Time",
#                            !Epoch == "Closed_Frequency", !Epoch == "Open_Frequency", !Epoch == "Middle_Frequency", 
#                            !Epoch == "Investigation_Time", !Epoch == "Investigation_Frequency", !Epoch == "boli")
# EPM_time_model <- anova_test(data = EPM_time, dv = Response, wid = MouseID, between = c(Condition), within = Epoch, effect.size = "pes")
# EPM_time_model

EPM <- read_xlsx(sheet = 1, 'AE masterdoc.xlsx')
EPM_long <- EPM %>%
  rstatix::select(MouseID, Condition:c("Time spent in open arms")) %>%
  pivot_longer(cols = c("Freeze (first 2 min)"):c("Time spent in open arms"), names_to = "Behavior", values_to = "Response")

#visualizing all of the data
EPM_all <- EPM_long %>%
  filter(!is.na(Response)) %>%
  group_by(Behavior, Condition, Sex, Age) %>% 
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

### 3 var plots - group 2 vars at a time

##CONDITION,SEX
EPM_group_c_s <- EPM_long %>% 
  unite(Group, c(Condition, Sex), sep = " ", remove = FALSE)

#freezing
EPM_freezing_c_s <- EPM_group_c_s %>%
  filter(!is.na(Response), Behavior == "Freeze (first 2 min)") %>%
  group_by(Group, Age) %>% 
  fig_stats

p_1a <- ggplot(EPM_freezing_c_s, aes(x = Group, y = mean, fill = Age)) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Freezing in First Two Minutes of EPM") +
  scale_fill_manual(name = "Age",
                    values = c("#67C4DC","#3277A5")) + 
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#0E1456", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0,20,2), limits = c(0,20), expand = c(0, 0)) +
  theme_classic()

p_1a

#latency to middle
EPM_latency_c_s <- EPM_group_c_s %>%
  filter(!is.na(Response), Behavior == "Latency to middle (time stamp)") %>%
  group_by(Group, Age) %>% 
  fig_stats

p_2a <- ggplot(EPM_latency_c_s, aes(x = Group, y = mean, fill = Age)) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Latency to Middle in EPM Grouped by Condition and Sex") +
  scale_fill_manual(name = "Age",
                    values = c("#67C4DC","#3277A5")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#0E1456", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 200,20), limits = c(0,200), expand = c(0, 0)) +
  theme_classic()

p_2a

#time spent in open arms
EPM_openarms_c_s <- EPM_group_c_s %>%
  filter(!is.na(Response), Behavior == "Time spent in open arms") %>%
  group_by(Group, Age) %>% 
  fig_stats

p_3a <- ggplot(EPM_openarms_c_s, aes(x = Group, y = mean, fill = Age)) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Spent in Open Arms in EPM Based on Age") +
  scale_fill_manual(name = "Group",
                    values = c("#67C4DC","#3277A5")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#0E1456", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 250,20), limits = c(0,250), expand = c(0, 0)) +
  theme_classic()

p_3a

figure_c_s <- ggarrange(p_1a,p_2a,p_3a,ncol = 1, nrow = 3,
                        common.legend = TRUE, legend="bottom")
figure_c_s

##CONDITION,AGE
EPM_group_c_a <- EPM_long %>% 
  unite(Group, c(Condition, Age), sep = " ", remove = FALSE)

#freezing
EPM_freezing_c_a <- EPM_group_c_a %>%
  filter(!is.na(Response), Behavior == "Freeze (first 2 min)") %>%
  group_by(Group, Sex) %>% 
  fig_stats

p_1b <- ggplot(EPM_freezing_c_a, aes(x = Group, y = mean, fill = Sex)) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Freezing in EPM") +
  scale_fill_manual(name = "Group",
                    values = c("#90D192","#658066")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#424E43", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0,20,2), limits=c(0,20), expand = c(0, 0)) +
  theme_classic()

p_1b

#latency to middle
EPM_latency_c_a <- EPM_group_c_a %>%
  filter(!is.na(Response), Behavior == "Latency to middle (time stamp)") %>%
  group_by(Group, Sex) %>% 
  fig_stats

p_2b <- ggplot(EPM_latency_c_a, aes(x = Group, y = mean, fill = Sex)) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Latency to Middle in EPM") +
  scale_fill_manual(name = "Group",
                    values = c("#90D192","#658066")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#424E43", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 200,20), limits=c(0,200), expand = c(0, 0)) +
  theme_classic()

p_2b

#time spent in open arms
EPM_openarms_c_a <- EPM_group_c_a %>%
  filter(!is.na(Response), Behavior == "Time spent in open arms") %>%
  group_by(Group, Sex) %>% 
  fig_stats

p_3b <- ggplot(EPM_openarms_c_a, aes(x = Group, y = mean, fill = Sex)) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Spent in Open Arms in EPM") +
  scale_fill_manual(name = "Group",
                    values = c("#90D192","#658066")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#424E43", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 250,20), limits=c(0,250), expand = c(0, 0)) +
theme_classic()

p_3b

figure_c_a <- ggarrange(p_1b,p_2b,p_3b,ncol = 1, nrow = 3,
                        common.legend = TRUE, legend="bottom")
figure_c_a

##AGE, SEX
EPM_group_a_s <- EPM_long %>% 
  unite(Group, c(Age, Sex), sep = " ", remove = FALSE)

#freezing
EPM_freezing_a_s <- EPM_group_a_s %>%
  filter(!is.na(Response), Behavior == "Freeze (first 2 min)") %>%
  group_by(Group, Condition) %>% 
  fig_stats

p_1c <- ggplot(EPM_freezing_a_s, aes(x = Group, y = mean, fill = reorder(Condition, -mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Freezing in EPM Based on Age and Sex") +
  scale_fill_manual(name = "Group",
                    values = c("#fca8d5","#c893ea", "#64b5ff")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#464747", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0,20,2),limits=c(0,20),expand = c(0, 0)) +
  theme_classic()

p_1c

#latency to middle
EPM_latency_a_s <- EPM_group_a_s %>%
  filter(!is.na(Response), Behavior == "Latency to middle (time stamp)") %>%
  group_by(Group, Condition) %>% 
  fig_stats

p_2c <- ggplot(EPM_latency_a_s, aes(x = Group, y = mean, fill = reorder(Condition, -mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Latency to Middle in EPM Based on Age and Sex") +
  scale_fill_manual(name = "Group",
                    values = c("#fca8d5","#c893ea", "#64b5ff")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#464747", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 200,20),limits=c(0,200), expand = c(0, 0)) +
  theme_classic()

p_2c

#time spent in open arms
EPM_openarms_a_s <- EPM_group_a_s %>%
  filter(!is.na(Response), Behavior == "Time spent in open arms") %>%
  group_by(Group, Condition) %>% 
  fig_stats

p_3c <- ggplot(EPM_openarms_a_s, aes(x = Group, y = mean, fill = reorder(Condition, -mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Spent in Open Arms in EPM Based on Age and Sex") +
  scale_fill_manual(name = "Group",
                    values = c("#fca8d5","#c893ea", "#64b5ff")) + #only two colors used
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#464747", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0, 250,20), limits=c(0,250),expand = c(0, 0)) +
  theme_classic()

p_3c

figure_a_s <- ggarrange(p_1c,p_2c,p_3c,ncol = 1, nrow = 3,
                        common.legend = TRUE, legend="bottom")
figure_a_s


##ALL 3
figure_all <- ggarrange(figure_c_s,figure_c_a,figure_a_s, ncol = 3, nrow = 1,
                        common.legend = TRUE, legend="bottom")
figure_all

###STATISTICAL TESTS

##freezing
EPM_freezing <- EPM_long %>%
  filter(!is.na(Response), Behavior == "Freeze (first 2 min)")
#anova --> sex and condition significant
EPM_freezing_model <- anova_test(data = EPM_freezing, dv = Response, wid = MouseID, between = c(Condition, Age, Sex), effect.size = "pes")
EPM_freezing_model
#t-test - sex (stat = -2.55)
EPM_freezing_Posthoc_T_Sex <- EPM_freezing %>% t_test(Response ~ Sex, var.equal = TRUE, paired = FALSE)
EPM_freezing_Posthoc_T_Sex
#t-test - condition --> homecage vs safety significant
EPM_freezing_Posthoc_T_Condition <- EPM_freezing %>% t_test(Response ~ Condition, var.equal = TRUE, paired = FALSE)
EPM_freezing_Posthoc_T_Condition

#latency to middle
EPM_latency <- EPM_long %>%
  filter(!is.na(Response), Behavior == "Latency to middle (time stamp)")
#anova --> condition significant
EPM_latency_model <- anova_test(data = EPM_latency, dv = Response, wid = MouseID, between = c(Condition, Age, Sex), effect.size = "pes")
EPM_latency_model
#t-test - condition --> homecage vs safety & homecage vs yoked are significant
EPM_latency_Posthoc_T_Condition <- EPM_latency %>% t_test(Response ~ Condition, var.equal = TRUE, paired = FALSE)
EPM_latency_Posthoc_T_Condition

#time spent in open arms
EPM_openarms <- EPM_long %>%
  filter(!is.na(Response), Behavior == "Time spent in open arms")
#anova --> condition significant
EPM_openarms_model <- anova_test(data = EPM_openarms, dv = Response, wid = MouseID, between = c(Condition, Age, Sex), effect.size = "pes")
EPM_openarms_model
#t-test - condition --> diff between all three are significant
EPM_openarms_Posthoc_T_Condition <- EPM_openarms %>% t_test(Response ~ Condition, var.equal = TRUE, paired = FALSE)
EPM_openarms_Posthoc_T_Condition

##GRAPHS TO HIGHLIGHT SIGNIFICANT DIFFERENCES

#freezing - condition significant
EPM_freezing_condition <- EPM_long %>%
  filter(!is.na(Response),Behavior == "Freeze (first 2 min)") %>%
  group_by(Behavior, Condition) %>% 
  fig_stats

p_1 <- ggplot(EPM_freezing_condition, aes(x = Condition, y = mean, fill = reorder(Condition, mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Freezing in First Two Minutes of EPM Based on Condition") +
  scale_fill_manual(name = "Condition", values = c("#fca8d5","#c893ea","#64b5ff")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#75797c", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0,13,1), limits=c(0,13), expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none")

p_1

#freezing - sex significant
EPM_freezing_sex <- EPM_long %>%
  filter(!is.na(Response),Behavior == "Freeze (first 2 min)") %>%
  group_by(Behavior, Sex) %>% 
  fig_stats

p_2 <- ggplot(EPM_freezing_sex, aes(x = Sex, y = mean, fill = reorder(Sex, mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Freezing in First Two Minutes of EPM Based on Sex") +
  scale_fill_manual(name = "Condition", values = c("#fca8d5","#c893ea","#64b5ff")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#75797c", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0,12,1), limits=c(0,12), expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none")

p_2

#freezing - condition and sex - different colors but same graph is above
EPM_stats_sex_condition <- EPM_long %>%
  filter(!is.na(Response),Behavior == "Freeze (first 2 min)") %>%
  group_by(Behavior, Sex, Condition) %>% 
  fig_stats

p_3 <- ggplot(EPM_stats_sex_condition, aes(x = Behavior, y = mean, fill = interaction(Sex, Condition))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Freezing in First Two Minutes of EPM Based on Sex and Condition") +
  scale_fill_manual(name = "Group", labels = c("Female Homecage", "Male Homecage", "Female Safety", "Male Safety", "Female Yoked Fear", "Male Yoked Fear"),
                    values = c("#f2c4de","#b07e9a","#cdb2e5","#997cb2", "#abcaef", "#7d98b9")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .1, color = "#464747", 
                position = position_dodge(width = 0.9)) + 
  theme(axis.title.x = element_blank()) +
  scale_y_continuous("Average Time (seconds)",breaks= seq(0,15,1), limits=c(0,15), expand = c(0, 0)) +
  theme_classic()

p_3

#latency to middle - condition significant
EPM_latency_condition <- EPM_long %>%
  filter(!is.na(Response),Behavior == "Latency to middle (time stamp)") %>%
  group_by(Behavior, Condition) %>% 
  fig_stats

p_4 <- ggplot(EPM_latency_condition, aes(x = Condition, y = mean, fill = reorder(Condition, mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Latency to Middle in EPM Based on Condition") +
  scale_fill_manual(name = "Condition", values = c("#fca8d5","#c893ea","#64b5ff")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#75797c", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0,170,10), limits=c(0,170), expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none")

p_4

#time spent in open arms - condition significant
EPM_openarms_condition <- EPM_long %>%
  filter(!is.na(Response),Behavior == "Time spent in open arms") %>%
  group_by(Behavior, Condition) %>% 
  fig_stats

p_5 <- ggplot(EPM_openarms_condition, aes(x = Condition, y = mean, fill = reorder(Condition, mean))) + 
  geom_col(position = position_dodge2(width = 0.7)) +
  ggtitle("Mean Time Spent in Open Arms of EPM Based on Condition") +
  scale_fill_manual(name = "Condition", values = c("#fca8d5","#c893ea","#64b5ff")) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .3, size = .3, color = "#75797c", 
                position = position_dodge(width = 0.9)) + 
  scale_y_continuous("Time (seconds)", breaks= seq(0,220,20), limits=c(0,220), expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none")

p_5

#trying out things
figure_condition <- ggarrange(p_1,p_4,p_5, ncol = 1, nrow = 3,common.legend = TRUE, legend="bottom")
figure_condition

#trying out heatmaps
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



