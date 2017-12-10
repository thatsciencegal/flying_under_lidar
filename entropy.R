library(dplyr)
library(ggplot2)

setwd("F:/Project_Home/Products/Collated Metrics")
osdata <- read.csv("G:/Project_Home/Products/Collated Metrics/os_merged.csv")
osdata2 <- mutate(os_merge_ln, 
                  entropy = PropStrat5*ln_PropStrat5 + Prop515*ln_Prop515 + Prop153*ln_Prop153 + Prop36*ln_Prop36 + Prop69*ln_Prop69 + Prop912*ln_Prop912 + PropAb12*ln_PropAb12)
os_merge_ln <- mutate(osdata,
                      ln_PropStrat5 = log(PropStrat5),
                      ln_Prop515 = log(Prop515),
                      ln_Prop153 = log(Prop153),
                      ln_Prop36 = log(Prop36),
                      ln_Prop69 = log(Prop69),
                      ln_Prop912 = log(Prop912),
                      ln_PropAb12 = log(PropAb12))

os_merge_ln_clean <- mutate(os_merge_ln,
                            ln_PropStrat5 = replace(ln_PropStrat5, ln_PropStrat5 == "-Inf", 0),
                            ln_Prop515 = replace(ln_Prop515, ln_Prop515 == "-Inf", 0),
                            ln_Prop153 = replace(ln_Prop153, ln_Prop153 == "-Inf", 0),
                            ln_Prop36 = replace(ln_Prop36, ln_Prop36 == "-Inf", 0),
                            ln_Prop69 = replace(ln_Prop69, ln_Prop69 == "-Inf", 0),
                            ln_Prop912 = replace(ln_Prop912, ln_Prop912 == "-Inf", 0),
                            ln_PropAb12 = replace(ln_PropAb12, ln_PropAb12 == "-Inf", 0)) 

os_entropy <- mutate(os_merge_ln_clean,
                     Entropy = exp(-(PropStrat5 * ln_PropStrat5 + Prop515 * ln_Prop515 + Prop153 * ln_Prop153 + Prop36 * ln_Prop36 + Prop69 * ln_Prop69 + Prop912 * ln_Prop912 + PropAb12 * ln_PropAb12)))


osdata <- osdata2 %>% 
  mutate(ln_PropStrat5 = replace(ln_PropStrat5, ln_PropStrat5 == "-Inf" | ln_PropStrat5 == "NA" | ln_PropStrat5 == "NaN", 0),
         ln_Prop515 = replace(ln_Prop515, ln_Prop515 =="-Inf" | ln_Prop515 == "NA" | ln_Prop515 == "NaN", 0),
         ln_Prop153 = replace(ln_Prop153, ln_Prop153 == "-Inf" | ln_Prop153 == "NA" | ln_Prop153 == "NaN", 0),
         ln_Prop36 = replace(ln_Prop36, ln_Prop36 == "-Inf" | ln_Prop36 == "NA" | ln_Prop36 == "NaN", 0),
         ln_Prop69 = replace(ln_Prop69, ln_Prop69 == "-Inf" | ln_Prop69 == "NA"| ln_Prop69 == "NaN", 0),
         ln_Prop912 = replace(ln_Prop912, ln_Prop912 == "-Inf" | ln_Prop912 == "NA" | ln_Prop912 == "NaN", 0),
         ln_PropAb12 = replace(ln_PropAb12, ln_PropAb12 == "-Inf" | ln_PropAb12 == "NA" | ln_PropAb12 == "NaN", 0))

names(os_full)[names(os_full) == c("Ret3Ab3", "PerAb3")] <- c("ReturnAb3", "PercentAb3")

os_full <- read.csv("../OSBS_merged_full.csv")

ggplot(os_entropy) +
  geom_boxplot(aes(factor(Cluster6), ElevMean)) +
  xlab("") +
  ylab("Mean Canopy Height (m)") +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) +  
  theme(text = element_text(size = 18))

ggplot(os_entropy) +
  geom_boxplot(aes(factor(Cluster6), ElevStdev)) +
  xlab("") +
  ylab("Rugosity (m)") +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) + 
  theme(text = element_text(size = 18))

ggplot(os_entropy) +
  geom_boxplot(aes(factor(Cluster6), Entropy)) +
  xlab("Cluster") +
  ylab("Entropy") +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) + 
  theme(text = element_text(size = 18))


