library(ggplot2)
library(tidyverse)
library(dplyr)
library(beyonce)
library(plotly)
library(magrittr)
library(tidyr)
library(cowplot)


#read in data
data <- read_csv("/Users/robertdellinger/Documents/Research/Data/Taylorann_Thesis/Cleaned_Sampled_ID.csv")

#clean data and add in category
data.grouped <- data %>% 
  mutate(standardized_total = Total/Volume_ml,
         standardized_fiber = Fiber/Volume_ml,
         standardized_particles = Microparticles/Volume_ml,
         Volume_L = Volume_ml/1000, #standardizing total counts
         ecosystem.role = case_when(Sample_type == "Niskin" ~ "Water",
                   Sample_type == "Push Core" ~ "Sediment",
                   Sample_type == "Sea Pen" ~ "Secondary Consumer",
                   Sample_type == "Urchin" ~ "Primary Consumer",
                   Sample_type == "Drift Algae" ~ "Primary Producer")) 


drop_na(
  drop_na
)


ecosystem.part <- data.grouped %>% 
  select(ecosystem.role, NMS, standardized_total) %>% 
  group_by(ecosystem.role) %>% 
  summarise(mean.total.plastic=mean(standardized_total, na.rm= TRUE),
            sd.total.plastic=sd(standardized_total, na.rm=TRUE),
            N=n(),
            se=sd.total.plastic/sqrt(N),
            upper_limit=mean.total.plastic+se,
            lower_limit=mean.total.plastic-se)
 
ggplot(ecosystem.part, aes(x=ecosystem.role, y=mean.total.plastic)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit))

  
   na.rm()
se=sd.total.plastic/sqrt(N), 
upper_limit=mean.total.plastic+se, 
lower_limit=mean.total.plastic_N-se 

ggplot(ecosystem.part, aes(x=ecosystem.role, y=mean.total.plastic)) +
  geom_bar(stat="identity") 


#niskin sampkels are water samples along an array of dpeths 
#diffeent organisms to ttest mechanisms of feeding 
#trying to test tto see if plastic particles are present oin the ater but also different kinds of orgniams
# indiscriminate feedrs? 
# algae = autotroph
# sea pig = detritovore 
# urchin - herbivore - primary pathway for energy flow
# sea pen suspension feeders






# First load in the data
data1 <- read_csv("/Users/robertdellinger/Documents/Research/Data/Taylorann_Thesis/Cleaned_Sampled_ID.csv")

niskins<-filter(.data= data1, Sample_type== "Niskin")

#This is the new final totals of the counts with the average of my blanks subtracted




#Standardized the total plastics by the volume of each sample
niskin2<- niskins %>%
  mutate(standardized_total = Total/Volume_ml,
         standardized_fiber = Fiber/Volume_ml,
         standardized_particles = Microparticles/Volume_ml,
         Volume_L = Volume_ml/1000)

#CHOSE TO STANDARDIZE BY ML BECAUSE THAT IS WHAT I SAW IN THE LIT. <3

#Now to subtract the blanks, AVG fiber = 8.5, AVG particle = 2

8.5/1000 = 0.0085
2/1000 = 0.002
10.5/1000 = 0.0105

niskin3<- niskin2 %>%
  mutate(final_total = standardized_total - 0.0105,
         final_fiber = standardized_fiber - 0.0085,
         final_particles = standardized_particles - 0.002)


#Negative numbers are present due to subtraction








#bubble plot
Water_sample_plot<- plot_ly(niskin3, x = ~Depth, y = ~final_total, 
                             type = 'scatter', mode = 'markers', 
                             size = ~final_total, color = ~Sample_type, 
                             colors = 'Paired',
                             sizes = c(15, 50),
                             marker = list(opacity = 0.5, sizemode = 'diameter'),
                             hoverinfo = 'text',
                             text = ~paste('Depth(m):', Depth, 'Total MP:', Total, 'Type:', Sample_type))
Water_sample_plot1<- Water_sample_plot %>% layout(title = 'Total Number of Microplastics by Depth (m)',
                                                   xaxis = list(showgrid = FALSE),
                                                   yaxis = list(showgrid = FALSE),
                                                   showlegend = TRUE)
Water_sample_plot1


plot3<- plot_ly(niskin3, x = ~Depth, y = ~final_total, 
                            type = 'scatter', mode = 'markers', 
                            size = ~final_total, color = ~Pelagic, 
                            colors = 'Paired',
                            sizes = c(15, 50),
                            marker = list(opacity = 0.5, sizemode = 'diameter'),
                            hoverinfo = 'text',
                            text = ~paste('Depth(m):', Depth, 'Total MP:', Total, 'Type:', Sample_type))
plot3<- plot3 %>% layout(title = 'Total Number of Microplastics by Depth (m)',
                                                  xaxis = list(showgrid = FALSE),
                                                  yaxis = list(showgrid = FALSE),
                                                  showlegend = TRUE)

plot3


#Lets do a barplot instead


plot2<- plot_ly(niskin3, x = ~Site, y = ~final_total, 
                            type = 'box', 
                            color = ~Site, 
                            colors = 'Paired',
                            marker = list(opacity = 0.5),
                            hoverinfo = 'text',
                            text = ~paste('Depth(m):', Depth, 'Total MP:', Total, 'Type:', Sample_type))
plot2.5<- plot2 %>% layout(title = 'Microplastics by Site: Increasing Distance from Shore',
                                                  xaxis = list(showgrid = FALSE, title= "Site"),
                                                  yaxis = list(showgrid = FALSE, title = "Total Microplastics"),
                                                  showlegend = FALSE)



#With increasing distance from shore
distance_to_shore_plot<-plot2.5 %>% 
  layout(xaxis = list(categoryorder = "array",
                      categoryarray = c("South Ancapa Slope", "Poti Penninsula", "Footprint Reef", 
                                        "Offshore San Miguel", "Rock Bridge", "North Santa Cruz Island", 
                                        "West San Miguel Shelf", "East Santa Cruz Canyon Wall", 
                                        "Pioneer Canyon South", "Pioneer Canyon North", "Santa Lucia Escarpment", 
                                        "Octocone/Whalle-Fall", "Southwest Davidson Seamount")))

distance_to_shore_plot





















ggplot(Water_Samples, aes(x = Depth, y = normalized_total)) +
  geom_point() +
  stat_smooth()
#this regression looks terrible, did I do something wrong when normalizing the volume?


cor(Water_Samples$normalized_total, Water_Samples$Depth)# 0.072 means there is a very weak relationship
#what else could explain this? maybe I need to separate the water colums/zones due to the uneven sampling?
#I have fewer super deep sea samples than surface



boxplot_niskin<-Water_Samples %>%
  ggplot( aes(x=Pelagic, y=normalized_total, fill=Pelagic)) +
  geom_boxplot() +
  scale_fill_manual(values = beyonce_palette(6))+
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none",
        plot.title = element_text(size=11)) +
  ggtitle("Total number of microplastics found across water depths") +
  xlab("Pelagic Zone")+
  ylab("Number of microplastics/L")

boxplot_niskin

#ANOVA?

# make boxplot
boxplot1<-noniskin %>%
  ggplot( aes(x=Sample_type, y=normalized_total, fill=Sample_type)) +
  geom_boxplot() +
  scale_color_manual("white")+
  scale_fill_manual(values = beyonce_palette(60))+
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="none",
    plot.title = element_text(size=11)) +
  ggtitle("Total number of microplastics found across samples") +
  xlab("Sample Type")+
  ylab("Number of microplastics/L")

boxplot1


#ANOVA?

oneway.test(Sample_type~normalized_total,
            data = noniskin,
            var.equal = TRUE)

