library(ggplot2)
library(readr)
library(dplyr) 
library(RColorBrewer) 
library(gganimate) 
India <- ClimateCountry %>% filter(Country=="India") India$dt <-as.Date(India$dt) India$Year <- format(India$dt,"%Y") India$Month <- format(India$dt,"%m")
India %>% filter(!is.na(AverageTemperature)) %>% filter(Year > 1850) %>% group_by(Year) %>% mutate(no_of_cases= length(Year)) %>% group_by(Month) %>% mutate(avg_tempMonth= mean(AverageTemperature)) %>% filter(no_of_cases==12) %>%
  ggplot(aes(Month,AverageTemperature,group=Year,cumulative = TRUE,alpha=Year,label=Year,frame=Year)) +geom_line(color="grey20") +
  geom_line(aes(Month,avg_tempMonth,frame= 2013 + as.numeric(Month) , group=1),size= 2.3,color=
              "dodgerblue3") +
  #geom_text(aes(x=06,y=20,cumulative=FALSE),size=30,color="grey20") +
  theme_minimal(base_family = "Ubuntu Condensed")+
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))+
  theme(legend.position = "none",axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#EFF2F4"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size=18,face = "bold")) +
  ggtitle("Average Temperature in India",subtitle = "1850 to 2013") +
  labs(caption= "Vamsi Krishna \t Source: Kaggle")
p<-
  India %>% filter(!is.na(AverageTemperature)) %>%
  filter(Year > 1850) %>%
  group_by(Year) %>% mutate(no_of_cases= length(Year)) %>%
  group_by(Month) %>%
  mutate(avg_tempMonth= mean(AverageTemperature)) %>%
  filter(no_of_cases==12) %>%
  ggplot(aes(Month,AverageTemperature,group=Year,cumulative =
               TRUE,alpha=Year,label=Year,frame=Year)) +
  geom_line(color="grey20") +
  geom_line(aes(Month,avg_tempMonth,frame= 2013 + as.numeric(Month) , group=1),size= 2.3,color=
              "dodgerblue3") +
  geom_text(aes(x=06,y=20,cumulative=FALSE),size=30,color="grey20") +
  theme_minimal(base_family = "Ubuntu Condensed")+scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))+ theme(legend.position = "none",axis.title = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), plot.background = element_rect(fill = "#EFF2F4"), axis.text = element_text(size = 12), plot.title = element_text(size=18,face = "bold")) +
  ggtitle("Average Temperature in India",subtitle = "1850 to 2013") + labs(caption= "Vamsi Krishna \t Source: Kaggle")
gganimate(p,"Output.gif",ani.width=810, ani.height=520, interval=0.1,title_frame = F)