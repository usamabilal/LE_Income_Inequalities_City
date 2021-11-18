#Figure 3 ----

#figure out colors 

figure3cv<-cv_decile1%>%
  filter(type=="cv")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Coefficient of Variation",
       x="Decile of Median Household Income",
       y="CV of Life Expectancy", 
       color="Region")+
  # guides(color=F, fill=F)+
  scale_y_continuous(limits=c(0, 4.25), breaks=seq(0, 4, by=1))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())

figure3cv


figure3sd<-cv_decile1%>%
  filter(type=="sd")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region),show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Standard Deviation",
       x="Decile of Median Household Income",
       y="SD of Life Expectancy (years)", 
       color="Region")+
  scale_y_continuous(limits=c(0, 3.25), breaks=seq(0, 3, by=1))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  #  facet_grid(~type)+
  guides(color=F, fill=F)+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())

figure3sd 


figure3mean<-cv_decile%>%
  filter(type=="mean")%>%
  ggplot( aes(x=decile_income, y=value, group=Region)) +
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom= "point",group=1)+
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom="line",group=1)+
  geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Mean",
       x="Decile of Median Household Income",
       y="Life Expectancy (years)", 
       color="Region")+
  guides(color=F, fill=F)+
  scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=2))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())

figure3mean

library(ggpubr)
ggarrange(figure3mean,figure3sd, figure3cv, ncol = 1, nrow=3 )


ggsave("results/figure3_new.pdf", width=8, height=7.5)
ggplotly(figure3mean)
ggplotly(figure3cv)
ggplotly(figure3sd)