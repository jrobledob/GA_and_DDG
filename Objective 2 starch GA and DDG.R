library(dplyr)
library(ggplot2)
library(ggpubr)
library(officer)
library(rvg)
library(reshape2)
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
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#----code----
setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
starch<- read.csv("Data/STARCH_OBJ_2 2023_03_27.csv")
tree_codes<- read.csv("Data/Tree codes.csv")
samples_ID<- read.csv("Data/Samples IDs and Comments.csv", skip = 8)
starch<- inner_join(starch, samples_ID)
starch<- select(starch, Sample.ID, Starch.by.mL..mg., Tree.Code, LEAF, Batch, OBSERVATIONS_1, OBSERVATIONS_2)
starch<- inner_join(starch, tree_codes)
starch$`Starch (mg/g)`<- starch$Starch.by.mL..mg./(starch$OBSERVATIONS_2/1000)
area_of_one_punch<- 2.83e-5
starch$`Starch (mg/m2)`<- (starch$Starch.by.mL..mg./3)/area_of_one_punch
starch$`Starch (ug/mm2)`<- ((starch$Starch.by.mL..mg.*1000)/3)/28.27
starch$`log Starch (ug/mm2)`<- log(starch$`Starch (ug/mm2)`)
starch$OBSERVATIONS_1<- as.Date(starch$OBSERVATIONS_1)
starch$age_in_days<- difftime(as.Date('2022-10-11'), starch$OBSERVATIONS_1, units = "days")
age_GA<- tapply(starch[grepl("GA", starch$Status),]$age_in_days, starch[grepl("GA", starch$Status),]$LEAF, mean)
age_DDG<- tapply(starch[grepl("DDG", starch$Status),]$age_in_days, starch[grepl("DDG", starch$Status),]$LEAF, mean)
starch<- aggregate(data=starch, `log Starch (ug/mm2)`~Tree.Code+LEAF+Cultivar+Status, mean)
starch$Log<- starch$`log Starch (ug/mm2)`
summary_starch<- summarySE(starch, measurevar="Log", groupvars=c("Status", "Cultivar"), na.rm = T)
list<- list("Old","Med","Young ")
names(list)<- c(paste("Old",round(age_GA[names(age_GA)=="Old"],0)), paste("Med",round(age_GA[names(age_GA)=="Med"],0)), paste("Young",round(age_GA[names(age_GA)=="Young "],0)))

list_2<- list("Old","Med","Young ")
names(list_2)<- c(paste("Old",round(age_DDG[names(age_DDG)=="Old"],0)), paste("Med",round(age_DDG[names(age_DDG)=="Med"],0)), paste("Young",round(age_DDG[names(age_DDG)=="Young "],0)))



figure_GA<-ggplot(filter(starch, Status=="GA+" | Status=="GA-"), aes(x=Status, y=Log, fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  scale_fill_manual(values =  c("#F2A900","#6A2A60"))+
  geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(aes(x=Status, y=Log, color=LEAF, shape=LEAF), size = 1.4, position=position_jitterdodge())+
  geom_signif(comparisons = list(c("GA+", "GA-")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
  stat_summary(fun = mean, color="#D32737", shape=17, size = 0.30)+
  theme(legend.text = element_text(size=7))+
  chartTheme+
  scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))+
  guides(fill=FALSE)+
  ylab(bquote(Log~Starch~Content~"("*mg/mm^2*")"))+
  chartTheme+
  theme(axis.title.x = element_blank())

figure_GA_by_LEAF<-figure_GA+
  facet_grid(cols = vars(LEAF))

figure_2DDG<-ggplot(filter(starch, Status=="2DDG" | Status=="NO_DDG"), aes(x=Status, y=Log, fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  scale_fill_manual(values =  c("#22884C","#A9A9A9"))+
  geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(aes(x=Status, y=Log, color=LEAF, shape=LEAF), size = 1.4, position=position_jitterdodge())+
  geom_signif(comparisons = list(c("2DDG", "NO_DDG")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
  stat_summary(fun = mean, color="#FA4616", shape=17, size = 0.30)+
  theme(legend.text = element_text(size=7))+
  chartTheme+
  scale_color_manual(values = c("#002657", "#6082B6", "black"))+
  guides(fill=FALSE)+
  ylab(bquote(Log~Starch~Content~"("*mg/mm^2*")"))+
  chartTheme+
  theme(axis.title.x = element_blank())

figure_DDG_by_LEAF<-figure_2DDG+
  facet_grid(cols = vars(LEAF))


#CT Values
cts<- read.csv("Data/OBJ_2_CTs_GA_and_DDG.csv")
tree_codes<- read.csv("Data/Tree codes.csv")
samples_ID<- read.csv("Data/Samples IDs and Comments.csv", skip = 8)
cts<- inner_join(cts, samples_ID)
cts<- select(cts, Sample.ID, delta.ct, Tree.Code, LEAF, Batch)
cts<- inner_join(cts, tree_codes)
cts<- aggregate(data=cts, `delta.ct`~Tree.Code+LEAF+Cultivar+Status, mean)

figure_GA_CT<-ggplot(filter(cts, Status=="GA+" | Status=="GA-"), aes(x=Status, y=delta.ct, fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  scale_fill_manual(values =  c("#F2A900","#6A2A60"))+
  geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(aes(x=Status, y=delta.ct, color=LEAF, shape=LEAF), size = 1.4, position=position_jitterdodge())+
  geom_signif(comparisons = list(c("GA+", "GA-")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
  stat_summary(fun = mean, color="#D32737", shape=17, size = 0.30)+
  theme(legend.text = element_text(size=7))+
  chartTheme+
  scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))+
  guides(fill=FALSE)+
  ylab(bquote(atop(CLas~Genetic~Material~ phantom(), "("*Delta*Ct~")")))+
  chartTheme+
  theme(axis.title.x = element_blank())

figure_GA_CT+
  facet_grid(cols = vars(LEAF))

figure_2DDG_CTS<-ggplot(filter(cts, Status=="2DDG" | Status=="NO_DDG"), aes(x=Status, y=delta.ct, fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  scale_fill_manual(values =  c("#22884C","#A9A9A9"))+
  geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(aes(x=Status, y=delta.ct, color=LEAF, shape=LEAF), size = 1.4, position=position_jitterdodge())+
  geom_signif(comparisons = list(c("2DDG", "NO_DDG")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
  stat_summary(fun = mean, color="#FA4616", shape=17, size = 0.30)+
  theme(legend.text = element_text(size=7))+
  chartTheme+
  scale_color_manual(values = c("#002657", "#6082B6", "black"))+
  guides(fill=FALSE)+
  ylab(bquote(atop(CLas~Genetic~Material~ phantom(), "("*Delta*Ct~")")))+
  chartTheme+
  theme(axis.title.x = element_blank())
  
figure_2DDG_CTS+
  facet_grid(cols = vars(LEAF))

#Foliar Area
foliar_area<- read.csv("Data/OBJ_2_GA_Foliar_Area.csv")
foliar_area<- inner_join(foliar_area, tree_codes)
foliar_area<- melt(foliar_area, id.vars = c("Tree.Code", "Cultivar", "Status"))
figure_foliar_area<- ggplot(foliar_area, aes(x=Status, y=value, fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  scale_fill_manual(values =  c("#F2A900","#6A2A60"))+
  geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(color="#A9A9A9")+
  geom_signif(comparisons = list(c("GA+", "GA-")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
  stat_summary(fun = mean, color="#D32737", shape=17, size = 0.30)+
  theme(legend.text = element_text(size=7))+
  chartTheme+
  guides(fill=FALSE)+
  facet_grid(rows = vars(variable), scales = 'free')+
  ylab(bquote(atop(Foliar~Area~phantom(), "("~UNITS~")")))+
  chartTheme+
  theme(axis.title.x = element_blank())


#Callose
callose<- read.csv("Data/OBJ_2_CALL.csv")
tree_codes<- read.csv("Data/Tree codes.csv")
samples_ID<- read.csv("Data/Samples IDs and Comments.csv", skip = 8)
callose<- inner_join(callose, samples_ID)
callose<- select(callose, Sample.ID, Pch.equi..g., Tree.Code, LEAF, Batch)
callose<- inner_join(callose, tree_codes)
callose<- aggregate(data=callose, `Pch.equi..g.`~Tree.Code+LEAF+Cultivar+Status, mean)


figure_GA_CALL<-ggplot(filter(callose, Status=="GA+" | Status=="GA-"), aes(x=Status, y=Pch.equi..g., fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  scale_fill_manual(values =  c("#F2A900","#6A2A60"))+
  geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(aes(x=Status, y=Pch.equi..g., color=LEAF, shape=LEAF), size = 1.4, position=position_jitterdodge())+
  geom_signif(comparisons = list(c("GA+", "GA-")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
  stat_summary(fun = mean, color="#D32737", shape=17, size = 0.30)+
  theme(legend.text = element_text(size=7))+
  chartTheme+
  scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))+
  guides(fill=FALSE)+
  ylab(bquote(atop(Content~of~Callose~ phantom(), "("*equivalent~mg~of~pachyman*")")))+
  chartTheme+
  theme(axis.title.x = element_blank())

figure_GA_CALL+
  facet_grid(cols = vars(LEAF))





figure_2DDG_CAll<-ggplot(filter(callose, Status=="2DDG" | Status=="NO_DDG"), aes(x=Status, y=Pch.equi..g., fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  scale_fill_manual(values =  c("#22884C","#A9A9A9"))+
  geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(aes(x=Status, y=Pch.equi..g., color=LEAF, shape=LEAF), size = 1.4, position=position_jitterdodge())+
  geom_signif(comparisons = list(c("2DDG", "NO_DDG")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
  stat_summary(fun = mean, color="#FA4616", shape=17, size = 0.30)+
  theme(legend.text = element_text(size=7))+
  chartTheme+
  scale_color_manual(values = c("#002657", "#6082B6", "black"))+
  guides(fill=FALSE)+
  ylab(bquote(atop(Content~of~Callose~ phantom(), "("*equivalent~mg~of~pachyman*")")))+
  chartTheme+
  theme(axis.title.x = element_blank())

figure_2DDG_CAll+
  facet_grid(cols = vars(LEAF))




try<- inner_join(starch, cts, by=c("Tree.Code", "LEAF", "Batch"))
try<- inner_join(try, callose, by=c("Tree.Code", "LEAF", "Batch"))
plot(try$Pch.equi..g., try$delta.ct)
plot(log(try$Pch.equi..g.), try$Log)
plot(try$delta.ct, try$Log)

ggplot(data = try, aes(x= Pch.equi..g., y=`log Starch (ug/mm2)`, color=LEAF))+
  geom_point()+
  facet_grid(~Status)


try$Status<- factor(try$Status, levels = c("GA-", "GA+", "2DDG", "NO_DDG"))
ggplot(data = try, aes(x = Pch.equi..g., y = `log Starch (ug/mm2)`)) +
  geom_point(aes(color=LEAF)) +
  facet_grid(~Status) +
  stat_smooth(method = "lm", se = FALSE, formula = y ~ x, color= "black")+
  theme_classic()










ggplot(data = try, aes(x = delta.ct, y = Pch.equi..g.)) +
  geom_point() +
  facet_grid(~Status) +
  stat_smooth(method = "lm", se = FALSE, formula = y ~ x) 











+
  geom_text(
    data = try %>% group_by(Status) %>% summarise(r2 = round(summary(lm(`log Starch (ug/mm2)` ~ Pch.equi..g., data = .))$r.squared, 3),
                                                  p_value = formatC(summary(lm(`log Starch (ug/mm2)` ~ Pch.equi..g., data = .))$coefficients[8], digits = 2)),
    aes(x = max(Pch.equi..g.), y = max(`log Starch (ug/mm2)`), label = paste0("R-squared: ", r2, "\n", "p-value: ", p_value)),
    hjust = 1, vjust = 1, size = 3, color = "black", fontface = "bold"
  )



data = try %>% group_by(Status) %>% summarise(r2 = round(summary(lm(`log Starch (ug/mm2)` ~ Pch.equi..g., data = .))$r.squared, 3),
                                              p_value = formatC(summary(lm(`log Starch (ug/mm2)` ~ Pch.equi..g., data = .))$coefficients[8], digits = 2))






library(ggplot2)
library(dplyr)
library(broom)

try_summary <- try %>%
  group_by(Status) %>%
  do(model_summary = tidy(lm(`log Starch (ug/mm2)` ~ Pch.equi..g., data = .)))


ggplot(data = try, aes(x = Pch.equi..g., y = `log Starch (ug/mm2)`, color = LEAF)) +
  geom_point() +
  facet_grid(~Status) +
  stat_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  geom_text(
    data = try_summary,
    aes(x = max(Pch.equi..g.), y = max(`log Starch (ug/mm2)`), 
        label = paste0("R-squared: ", round(model_summary$r.squared, 3), "\n", 
                       "p-value: ", formatC(model_summary$p.value, digits = 2))),
    hjust = 1, vjust = 1, size = 3, color = "black", fontface = "bold"
  )


r_squared<- rep(NA, 4)
p_value<- rep(NA, 4)
cont<-1
# Perform linear regression
for (i in c("GA-", "GA+", "2DDG", "NO_DDG")) {
  lm_model <- lm(`log Starch (ug/mm2)` ~ Pch.equi..g., data = filter(try, Status==i))
  r_squared[cont] <- summary(lm_model)$r.squared
  p_value[cont] <- summary(lm_model)$coef[2, 4]
  cont<-cont+1
}
names(r_squared)<- c("GA-", "GA+", "2DDG", "NO_DDG")
names(p_value)<- c("GA-", "GA+", "2DDG", "NO_DDG")

# Extract R-squared and p-value
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coef[2, 4]

# Plot with linear regression line and statistics
ggplot(data = try, aes(x = Pch.equi..g., y = `log Starch (ug/mm2)`)) +
  geom_point() +
  facet_grid(~Status) +
  stat_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  geom_text(x = max(try$Pch.equi..g.), y = max(try$`log Starch (ug/mm2)`),
            label = paste0("R-squared = ", round(r_squared, 3), "\n",
                           "p-value = ", format.pval(p_value, digits = 3)),
            hjust = 1, vjust = 1)
