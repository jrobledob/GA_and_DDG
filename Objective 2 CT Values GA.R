library(ggplot2)
library(dplyr)
library(rvg)
library(officer)
library(ggpubr)

#----chart theme----
chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                    axis.title.x=element_text(size=18, face = "bold"), 
                    axis.title.y=element_text(size=18, face = "bold"),
                    axis.text.x=element_text(size=14, angle= 0, vjust=0.5),
                    axis.text.y=element_text(size=14),
                    panel.grid.minor.y=element_line(linetype="dotted"),
                    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                    plot.background=element_rect(linetype="blank"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.key = element_rect(fill = "white"))
#----read Ct values----
setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/")
ct_values<- read.csv("MASTER Thesis/Statistical Analysis/Data/Ct_VALUES_OBJ_2_GA_ 2022_08_24.csv")
tree_codes<- read.csv("MASTER Thesis/Statistical Analysis/Data/Tree codes.csv")
colnames(tree_codes)<- c("Tree.Code", "Cultivar", "Status")
#setting tree codes
ct_values<-inner_join(ct_values,tree_codes)
#calculating delta CT
ct_values$delta_Ct<- ct_values$NTC-as.numeric(ct_values$Ct.Value)
#Removing healthy plants and negative values (considered as negatives samples for CLas)
ct_values<- filter(ct_values, delta_Ct>0)

figure_Ct<- ggplot(filter(ct_values, Cultivar=="PA"), aes(x=Status, y=sqrt(delta_Ct), fill= Cultivar))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  geom_boxplot(width=0.1, , fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.7, position=position_jitter(h=0.1, w=0.1))+
  stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.15)+
  geom_signif(map_signif_level = T, test = "t.test", comparisons = list(c("GA-", "GA+")))+
  scale_fill_manual(values=c("#003074", "#F57330"))+
  chartTheme

figure_Ct_SB<- ggplot(filter(ct_values, Cultivar=="SB"), aes(x=Status, y=sqrt(delta_Ct), fill= Cultivar))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  geom_boxplot(width=0.1, , fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.7, position=position_jitter(h=0.1, w=0.1))+
  stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.15)+
  geom_signif(map_signif_level = T, test = "t.test", comparisons = list(c("GA-", "GA+")))+
  scale_fill_manual(values=c("#F57330"))+
  chartTheme

ct_values<- ct_values[-which(ct_values$Tree.Code %in% c("JR48","JR47","JR44","JR43","JR39","JR34","JR33")),]
ggplot(filter(ct_values, Cultivar=="PA"), aes(x=Status, y=sqrt(delta_Ct), fill= Cultivar))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  geom_boxplot(width=0.1, , fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.7, position=position_jitter(h=0.1, w=0.1))+
  stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.15)+
  geom_signif(map_signif_level = T, test = "t.test", comparisons = list(c("GA-", "GA+")))+
  scale_fill_manual(values=c("#003074"))+
  chartTheme

ct_values[-which(ct_values$Tree.Code %in% c("JR48","JR47","JR44","JR43","JR39","JR34","JR33")),]





shapiro.test(filter(ct_values, Cultivar=="PA" )$delta_Ct)
shapiro.test(filter(ct_values, Cultivar=="SB" )$delta_Ct)


#cor_ct<- select(ct_values, Tree.Code, Batch, delta_Ct)
#write.csv(cor_ct, "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/COR_ct_OBJ_1.csv")

# 
# p_dml <- rvg::dml(ggobj = figure_Ct_SB)
# officer::read_pptx() %>%
#   # add slide
#   officer::add_slide() %>%
#   # specify object and location of object
#   officer::ph_with(p_dml, ph_location()) %>%
#   # export slide
#   base::print(
#     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Delta_Ct_SB.pptx"
#   )
# 



















