# install.packages("tidyverse")
# install.packages("gapminder")
# install.packages("ggpubr")
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
#library(ggpubr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
setwd(dir = "/Users/anna/Desktop/MA_Term_paper/Research/r_analysis/")

# load data
ptcl_id <- read.csv("dataset-id.csv", encoding = 'utf-8')
ptcl_it <- read.csv("dataset-it.csv", encoding = 'utf-8')
ptcl_td <- read.csv("dataset-td.csv", encoding = 'utf-8')

ptcl <- rbind(ptcl_td, ptcl_id, ptcl_it)



# better labels for beautiful plots
scope.labels <- c(yes = "поверхностная", no = "обратная")
ptcl_order.labels <- c(дт = "даже-только", тд = "только-даже",
                       ди = "даже-именно", ид = "именно-даже",
                       ти = "только-именно", ит = "именно-только")

# plot 1: scope influence (for diff. conditions) - дт 
ggplot(ptcl_td, aes(x = value, y = lin.sem_alignment, colour = lin_ptcl_order)) +
  geom_count(alpha=0.4)+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid(word_order ~ lin_ptcl_order, labeller = labeller(lin_ptcl_order = ptcl_order.labels)) +
  stat_summary(geom="point",
               size=1.5, color="red") + 
  stat_summary(geom="point", shape = 8,
               size=3, color="black", fun = median) + 
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +  
  #scale_color_manual(, values = c("blue", "red")) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7))  +
  scale_y_discrete(labels=c("обратн.","поверх.")) 

# plot 2: scope influence (for diff. conditions) - ди 
ggplot(ptcl_id, aes(x = value, y = lin.sem_alignment, colour = lin_ptcl_order)) +
  geom_count(alpha=0.6)+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid(word_order ~ lin_ptcl_order, labeller = labeller(lin_ptcl_order = ptcl_order.labels)) +
  stat_summary(geom="point",
               size=1.5, color="red") + 
  stat_summary(geom="point", shape = 8,
               size=3, color="black", fun = median) + 
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +  
  #scale_color_manual(, values = c("blue", "red")) +
  scale_color_brewer(palette="Set2") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7))  +
  scale_y_discrete(labels=c("обратн.","поверх.")) 

# plot 3: scope influence (for diff. conditions) - ит 
ggplot(ptcl_it, aes(x = value, y = lin.sem_alignment, colour = lin_ptcl_order)) +
  geom_count(alpha=0.5)+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid(word_order ~ lin_ptcl_order, labeller = labeller(lin_ptcl_order = ptcl_order.labels)) +
  stat_summary(geom="point",
               size=1.5, color="red") + 
  stat_summary(geom="point", shape = 8,
               size=3, color="black", fun = median) + 
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Set2") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7))  +
  scale_y_discrete(labels=c("обратн.","поверх.")) 



# plot 4: the role of linear order  - тд 
ggplot(ptcl_td, aes(x = value, y = lin_ptcl_order, colour = lin_ptcl_order)) +
  geom_count(alpha=0.5)+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid(word_order ~ lin.sem_alignment, labeller = labeller(lin.sem_alignment = scope.labels)) +
  stat_summary(geom="point",
               size=1.5, color="red") + 
  stat_summary(geom="point", shape = 8,
               size=3, color="black", fun = median) +
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7))  +
  scale_y_discrete(labels=c("даже-только","только-даже")) 


# plot 5: the role of linear order  - ид 
ggplot(ptcl_id, aes(x = value, y = lin_ptcl_order, colour = lin_ptcl_order)) +
  geom_count(alpha=0.4)+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid(word_order ~ lin.sem_alignment, labeller = labeller(lin.sem_alignment = scope.labels)) +
  stat_summary(geom="point",
               size=1.5, color="red") + 
  stat_summary(geom="point", shape = 8,
               size=3, color="black", fun = median) +
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7))  +
  scale_y_discrete(labels=c("даже-именно","именно-даже"))

# plot 6: the role of linear order  - ит 
ggplot(ptcl_it, aes(x = value, y = lin_ptcl_order, colour = lin_ptcl_order)) +
  geom_count(alpha=0.4)+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid(word_order ~ lin.sem_alignment, labeller = labeller(lin.sem_alignment = scope.labels)) +
  stat_summary(geom="point",
               size=1.5, color="red") + 
  stat_summary(geom="point", shape = 8,
               size=3, color="black", fun = median) +
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7))  +
  scale_y_discrete(labels=c("именно-только","только-именно"))


# plot 7: the role of subjectness / synt priority - ди 
ptcl_td_surf <- filter(ptcl_td, lin.sem_alignment == 'yes' )
ggplot(ptcl_td_surf, aes(x = value, y = word_order)) +
  geom_count(alpha=0.3, fill = "green")+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid( lin_ptcl_order  ~ ., labeller = labeller(lin_ptcl_order = ptcl_order.labels)) +
  stat_summary(geom="point",
               size=2, color="black") + 
  stat_summary(geom="point", shape = 8,
               size=2, color="darkred", fun = median) + 
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7)) 


# plot 8: the role of subjectness / synt priority - ди 
ptcl_id_surf <- filter(ptcl_id, lin.sem_alignment == 'yes' )
ggplot(ptcl_id_surf, aes(x = value, y = word_order)) +
  geom_count(alpha=0.3, fill = "green")+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid( lin_ptcl_order  ~ ., labeller = labeller(lin_ptcl_order = ptcl_order.labels)) +
  stat_summary(geom="point",
               size=2, color="black") + 
  stat_summary(geom="point", shape = 8,
               size=2, color="darkred", fun = median) + 
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7)) 



# plot 9: the role of subjectness / synt priority - ит
ptcl_it_surf <- filter(ptcl_it, lin.sem_alignment == 'yes' )
ggplot(ptcl_it_surf, aes(x = value, y = word_order)) +
  geom_count(alpha=0.3, fill = "green")+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid( lin_ptcl_order  ~ ., labeller = labeller(lin_ptcl_order = ptcl_order.labels)) +
  stat_summary(geom="point",
               size=2, color="black") + 
  stat_summary(geom="point", shape = 8,
               size=2, color="darkred", fun = median) + 
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7)) 





# plot NO: the role of subjectness vs linear thing - all
ovs <- filter(ptcl, word_order == "OVS")
ovs_pd <- ovs %>%
  mutate(pair = ifelse(lin_ptcl_order == "тд" | lin_ptcl_order == "дт", "дт", ifelse(lin_ptcl_order == "ид" | lin_ptcl_order == "ди", "ди", "ит")))

ggplot(ovs_pd, aes(x = value, y = synt_ptcl_priority, colour = synt_ptcl_priority, , drop = TRUE)) +
  geom_count(alpha=0.5)+
  scale_size_area(max_size = 10) + 
  guides(color = "none") + 
  facet_grid(pair ~ lin.sem_alignment, labeller = labeller(lin.sem_alignment = scope.labels), drop = TRUE) +
  stat_summary(geom="point",
               size=2, color="black") + 
  stat_summary(geom="point", shape = 8,
               size=2, color="darkred", fun = median) + 
  labs(title = "",
       x = "",
       y = "",
       size = "", 
       color = "") +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(labels=c(1:7),breaks=c(1:7)) +
  scale_y_discrete(labels=c("Oд V Sи", "Oд V Sт",
                            "Oи V Sд", "Oи V Sт",
                            "Oт V Sд", "Oт V Sи"), drop = TRUE)

