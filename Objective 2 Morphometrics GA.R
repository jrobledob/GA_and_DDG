library(dplyr)
library(ggplot2)
library(data.table)
library(gplots)
library(officer)
library(rvg)
library(reshape2)
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

morphometric_data<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/MORPHO_GA_OBJ_2.csv")
morphometric_data$Branch.date<- as.Date(morphometric_data$Branch.date, origin = "1899-12-30")
morphometric_data<- inner_join(morphometric_data, read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Tree codes.csv"))
morphometric_data<- morphometric_data[-which(morphometric_data$Tree.Code %in% c("JR48","JR47","JR44","JR43","JR39","JR34","JR33")),]

ggplot(morphometric_data) +
  geom_histogram(aes(x = Branch.date)) +
  scale_x_date(date_labels = "%b-%Y") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_vline(xintercept = as.Date("2022-05-05"))+
  facet_grid(Cultivar~Status)
frequency_table<- dcast(morphometric_data, Cultivar+Status~year(Branch.date)+month(Branch.date))
frequency_table<- filter(frequency_table, Cultivar!="SB")
rownames(frequency_table)<- frequency_table$Status
frequency_table<- frequency_table[,-c(1:2)]
frequency_table<-t(frequency_table)
frequency_table<- frequency_table[which(rownames(frequency_table) %in% c("2021_11","2022_3", "2022_4", "2022_7")),]




line_chart<- as.data.frame(table(list(morphometric_data$Branch.date, morphometric_data$Status)))
line_chart$X.1<-as.Date(line_chart$X.1)
line_chart$cum_sum<- 0
line_chart[which(line_chart$X.2=="GA+"),]$cum_sum<- cumsum(line_chart[which(line_chart$X.2=="GA+"),]$Freq)
line_chart[which(line_chart$X.2=="GA-"),]$cum_sum<- cumsum(line_chart[which(line_chart$X.2=="GA-"),]$Freq)



ggplot(line_chart, aes(x=X.1, y= Freq, color= X.2)) +
  geom_line() +
  scale_x_date(date_labels = "%b-%Y") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_vline(xintercept = as.Date("2022-05-05"))

cum_flush<- ggplot(line_chart, aes(x=X.1, y= cum_sum, color= X.2)) +
      geom_line() +
      geom_point()+
      scale_x_date(date_labels = "%b-%Y") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))+
      geom_vline(xintercept = as.Date("2022-05-05"))+
      chartTheme+
      scale_color_manual(values =  c("#F2A900","#6A2A60"))

# 
# 
# #Save PPT
# p_dml <- rvg::dml(ggobj = cum_flush)
# # initialize PowerPoint slide ----
# officer::read_pptx() %>%
#   # add slide ----
# officer::add_slide() %>%
#   # specify object and location of object ----
# officer::ph_with(p_dml, ph_location()) %>%
#   # export slide -----
# base::print(
#   target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-08 Figures/Line_chart_cumulative_frequencies_flush_GA.pptx"
# )



# 1. convert the data as a table
dt <- as.table(as.matrix(frequency_table))

# 2. Graph
balloonplot(t(dt), main ="Severity of HLB", xlab ="Category of Severity", ylab="Cultivar",
            label = FALSE, show.margins = FALSE)
library("graphics")
mosaicplot(dt, shade = TRUE, las=2,
           main = "housetasks")


library("vcd")
# plot just a subset of the table
assoc(head(dt, 5), shade = TRUE, las=3)


chisq <- chisq.test(frequency_table)
#Severity is not associated with the cultivar, both have the same severity 
chisq
chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)






# load packages
library(ggstatsplot)
library(ggplot2)

# plot
morphometric_data[, "year"] <- format(morphometric_data[,"Branch.date"], "%Y")
morphometric_data[, "month"] <- as.numeric(format(morphometric_data[,"Branch.date"], "%m"))
morphometric_data<- filter(morphometric_data, Cultivar=="PA")
morphometric_data_N_branches<- filter(morphometric_data, month==11|month==3|month==7)


figure_categorical_data<- ggbarstats(
  data = morphometric_data_N_branches,
  x = Status,
  y = month
) +
  labs(caption = NULL)+scale_fill_manual(values =  c("#F2A900","#6A2A60")) # remove caption
figure_categorical_data

morphometric_data$Period<- NA
morphometric_data[which(morphometric_data$month %in% c(10:12)),]$Period<- 1
morphometric_data[which(morphometric_data$month %in% c(1:3)),]$Period<- 2
morphometric_data[which(morphometric_data$month %in% c(4:5)),]$Period<- 3
morphometric_data[which(morphometric_data$month %in% c(6:8)),]$Period<- 4



figure_categorical_data_by_period<- ggbarstats(
  data = morphometric_data,
  x = Status,
  y = Period
) +
  labs(caption = NULL)+scale_fill_manual(values =  c("#F2A900","#6A2A60")) # remove caption
figure_categorical_data_by_period



# #Save PPT
# p_dml <- rvg::dml(ggobj = figure_categorical_data_by_period)
# # initialize PowerPoint slide ----
# officer::read_pptx() %>%
#   # add slide ----
# officer::add_slide() %>%
#   # specify object and location of object ----
# officer::ph_with(p_dml, ph_location()) %>%
#   # export slide -----
# base::print(
#   target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-08 Figures/Number_of_shoots_per_period.pptx"
# )

r1<-morphometric_data$width.base/2
r2<-morphometric_data$with.middle/2
r3<- morphometric_data$Width.tip/2
h1<- morphometric_data$Length.base...middle
h2<- morphometric_data$length.middle...tip


morphometric_data$stem_sf_are<- (pi*(r1+r2))*(sqrt(((r1-r2)^2)+((h1)^2)))+(pi*(r2+r3))*(sqrt(((r2-r3)^2)+((h2)^2)))
#morphometric_data$stem_sf_are_2<- log((pi*(r1+r2))*(sqrt(((r1-r2)^2)+((h1)^2)))+(pi*(r2+r3))*(sqrt(((r2-r3)^2)+((h2)^2))))
morphometric_data<- morphometric_data[,-15]
data_long<- melt(morphometric_data, id.vars=c("Tree.Code","Branch.date","Observations","Status","year","month","Cultivar"))


data_long_1<- data_long[which(data_long$variable %in% c("total.length", "Length.base...middle", "length.middle...tip", "N.Leaves")),]


morpho<- ggplot(data_long_1, aes(x=Status, y=log(value), fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  geom_boxplot(width=0.3, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.3, position=position_jitter(h=0.1, w=0.1))+
  stat_summary(fun = mean, color="black", shape=17, size = 0.15)+
  facet_grid(~variable)+
  geom_signif(map_signif_level=T,comparisons = list(c("GA-","GA+")))+
  chartTheme+
  scale_fill_manual(values =  c("#F2A900","#6A2A60"))


#Save PPT
p_dml <- rvg::dml(ggobj = morpho)
# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, ph_location()) %>%
  # export slide -----
base::print(
  target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-08 Figures/Morphometrics_GA_1.pptx"
)


data_long_2<- data_long[which(data_long$variable %in% c("width.base", "with.middle", "Width.tip","stem_sf_are")),]


morpho_2<- ggplot(data_long_2, aes(x=Status, y=log(value), fill=Status))+
  geom_violin(trim = FALSE, color= "#C7C9C8")+
  geom_boxplot(width=0.3, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
  geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.3, position=position_jitter(h=0.1, w=0.1))+
  stat_summary(fun = mean, color="black", shape=17, size = 0.15)+
  facet_grid(~variable)+
  geom_signif(map_signif_level=T,comparisons = list(c("GA-","GA+")))+
  chartTheme+
  scale_fill_manual(values =  c("#F2A900","#6A2A60"))
morpho_2

#Save PPT
p_dml <- rvg::dml(ggobj = morpho_2)
# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, ph_location()) %>%
  # export slide -----
base::print(
  target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-08 Figures/Morphometrics_GA_2.pptx"
)

