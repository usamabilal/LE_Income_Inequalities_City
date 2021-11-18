
rm(list=ls())
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(sf)
library(reldist)
library(GGally)
library(broom)
library(grid)
library(gridExtra)
library(plotly)
library(dineq)
library(car)
library(ggpubr)

load("data/clean_data.rdata")
xwalk_region = region
total_pop_msa<-dta %>% group_by(cbsa_name, cbsa) %>% 
  summarise(total_pop=sum(pop))


# first, calculate absolute ineq indicators by city
# Gap and ratio: need .9 and .1 weighted quantiles
# CV
# GINI from reldist package and mean log deviation from dineq

#we'll run all of this, but then limit to our two measures of interest, absolute and relative 

absolute_ineq<-dta %>% group_by(cbsa) %>% 
  group_modify(~{
    #.x<-dta %>% filter(cbsa==25940)
    quants<-wtd.quantile(.x$le, q = c(.1, .9), weight = .x$pop)
    wtd.mean<-weighted.mean(.x$le, w=.x$pop)
    wtd.sd<-sum(.x$pop/sum(.x$pop) * (.x$le - wtd.mean)^2)
    gini<-gini(.x$le, weights=.x$pop)*100
    data.frame(dif=quants[2]-quants[1],
               ratio=quants[2]/quants[1],
               cv=wtd.sd/wtd.mean*100,
               gini=gini,
               mld.wt=mld.wtd(.x$le, weights=.x$pop)) %>% as_tibble
  }) %>% left_join(region)
absolute_ineq_long<-absolute_ineq %>% gather(type, value, -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("dif", "ratio", "cv", "gini", "mld.wt"),
                     labels=c("Abs. Disparity", "Rel. Disparity",
                              "Coefficient of Variation", "Gini", "Mean Log Deviation")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)

absolute_rel_ineq_long<-absolute_ineq_long%>%
  filter(type %in% c("Abs. Disparity", "Rel. Disparity"))%>%
  mutate(Region_Name=factor(Region_Name), 
         Region_Name=ordered(Region_Name, levels=c("Midwest", "South", "Northeast", "West")))

absolute_rel_ineq_long1<-absolute_rel_ineq_long%>%
  arrange(value)

str(absolute_rel_ineq_long)
#find Coefficient of variation (CV) for each region and measure 
cv<-absolute_rel_ineq_long%>%
  group_by(type, Region_Name)%>%
  summarize(cv=sd(value) / mean(value) * 100)

totals<-absolute_rel_ineq_long%>%
group_by(type, Region_Name)%>%
 summarize(median=median(value))


#Figure 1-----

my_breaks <- function(y) { if (max(y) > 2) seq(0, 12.5, 2.5) else seq(1, 1.16, 0.2) }

f1a<-absolute_rel_ineq_long%>%
  filter(type=="Abs. Disparity")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=0)+
  labs(x="",
       y="Value",
       title="Absolute Disparity")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))
f1a
ggsave("results/figure1_ASM.pdf", f1a, width=20, height=7.5)

f1b<-absolute_rel_ineq_long%>%
  filter(type=="Rel. Disparity")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=1)+
  scale_y_continuous(trans="log") +
  labs(x="",
       y="Value",
       title="Relative Disparity")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_blank(),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))
f1b

library(gridExtra)

figure1<-grid.arrange(f1a,f1b,
                      ncol = 2, nrow = 1)
g <- arrangeGrob(f1a,f1b,  nrow=1) #generates g
ggsave(g, file="results/figure1.pdf", width=15, height=10) #saves g

#view specific MSAs
ggplotly(f1a)

#Table 1 -------
summary_absolute<-absolute_ineq_long %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())


income_ineq<-dta %>% group_by(cbsa) %>% 
  group_modify(~{
    ### Negative Difference
    # .x<-dta %>% filter(cbsa==34060)
    # .x<-.x %>%
    #   mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1)), include.lowest = T)))
    # .x %>% group_by(decile_income) %>%
    #   summarise(le=weighted.mean(le, w=pop)) %>%
    #   filter(decile_income%in%c(1, 10))
    ### Negative Slope
    # .x<-dta %>% filter(cbsa==25980)
    # .x<-.x %>%
    #   mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1)), include.lowest = T)))
    # .x %>% 
    #   ggplot(aes(x = decile_income, y = le))+geom_point() +
    #   geom_smooth(method = "lm")
    .x<-.x %>% 
      mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1)), include.lowest = T)))
    decile_le<-.x %>% group_by(decile_income) %>% 
      summarise(le=weighted.mean(le, w=pop)) %>% 
      filter(decile_income%in%c(1, 10)) %>% pull(le)
    mean_le<-weighted.mean(.x$le, w=.x$pop)
    model<-lm(le~decile_income, data=.x) %>% tidy
    sii<-model %>% filter(term=="decile_income") %>% pull(estimate)
    rii<-sii/mean_le
    data.frame(dif=decile_le[2]-decile_le[1],
               ratio=decile_le[2]/decile_le[1],
               sii=sii, 
               rii=rii) %>% as_tibble
  }) %>% left_join(region)

income_ineq_long<-income_ineq %>% gather(type, value, -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("dif", "ratio", "sii", "rii"),
                     labels=c("Top/Bottom Difference", "Top/Bottom Ratio",
                              "Slope Index of Inequality", "Relative Index of Inequality")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)
summary_income<-income_ineq_long %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())
# > 1M
summary_absolute_large<-absolute_ineq_long %>% 
  filter(total_pop>=1000000) %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())
summary_income_large<-income_ineq_long %>% 
  filter(total_pop>=1000000) %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())

summary_large_abs<-absolute_ineq_long %>% 
  filter(total_pop>=1000000 & type=="Abs. Disparity") %>% 
  arrange(desc(value)) %>% 
  group_by(type) %>% 
  mutate(rank=row_number())%>%
  ungroup()%>%
  select(cbsa_name, Region_Name, rank, value )%>%
  rename(total_dif=value)%>%
  mutate(Region_Name=sub(" Region", "", Region_Name))%>%
  mutate(total_dif=format(total_dif, digits=1, nsmall=1))

summary_large_rel<-absolute_ineq_long %>% 
  filter(total_pop>=1000000 & type=="Rel. Disparity") %>% 
  arrange(desc(value)) %>% 
  group_by(type) %>% 
  mutate(rank=row_number())%>%
  ungroup()%>%
  select(cbsa_name,Region_Name, rank, value )%>%
  rename(total_ratio=value)%>%
  mutate(Region_Name=sub(" Region", "", Region_Name))%>%
  mutate(total_ratio=format(total_ratio, digits=2, nsmall=2))

table1<-cbind(summary_large_abs, summary_large_rel)
fwrite(table1, file="results/table1_ASM.csv")


#Figure 2----

#FINAL MAP NOT CREATED IN R- THE CODE BELOW IS  A SIMPLIFIED VERSION
full_dta<-absolute_ineq_long %>% select(cbsa, type, value) %>% 
  filter(type %in% c("Abs. Disparity"))%>%
  mutate(ineq="Total") %>% 
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>% 
  group_by(type2) %>% 
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels="Total: Abs. Disparity"))
shp<-read_sf("Data/cb_2013_us_cbsa_20m/cb_2013_us_cbsa_20m.shp") %>% 
  mutate(cbsa=as.numeric(GEOID))
regions<-read_sf("Data/cb_2013_us_region_20m/cb_2013_us_region_20m.shp")
  states<-read_sf("Data/cb_2013_us_state_20m/cb_2013_us_state_20m.shp")%>%
    subset(STATEFP%in% c(23, 55))

#export full_dta file as csv for map(but only absolute and relative)
dta_abs_rel<-full_dta%>%
  select(cbsa, Region_Name, "Total: Abs. Disparity", "Total: Rel. Disparity")

write.csv(dta_abs_rel,"Data/dta_abs_rel.csv", row.names = FALSE)

  
shp_with_data<-inner_join(shp, full_dta)
bbox_temp<-st_bbox(shp_with_data)
figure2<-ggplot()+
  geom_sf(data=shp_with_data, size=0,
          aes(geometry=geometry, color=rank, fill=rank))+
  geom_sf(data=regions, size=0.5, color="black", 
          fill=NA,
          aes(geometry=geometry))+
  geom_sf(data=states, color="black",fill="black")+
  scale_fill_binned(name="Rank", type="gradient",
                    show.limits=T,n.breaks=5,labels=round,
                    low="red", high="white")+
  scale_color_binned(name="Rank", type="gradient",
                     show.limits=T,n.breaks=5,labels=round,
                     low="red", high="white")+
  coord_sf(xlim = c(bbox_temp["xmin"], bbox_temp["xmax"]),
           ylim = c(bbox_temp["ymin"], bbox_temp["ymax"]), expand = T) +
  guides(alpha=F, size=F, color=F)+
  theme_void()+
  theme(plot.title = element_blank(),
        strip.text = element_text(size=10, face="bold", hjust=.5),
        panel.background = element_rect(fill = "grey", color=NA),
        legend.position="bottom")
figure2
ggsave("results/figure2_ASM.pdf", width=12, height=5)

#just to view which areas have largest disparities 
absolute_ineq_long1<-absolute_ineq_long%>%
  arrange(type, value)


#Figure 3 ----
le_by_decile<-dta %>% group_by(cbsa) %>% 
  group_modify(~{
    #.x<-dta %>% filter(cbsa==25940)
    .x<-.x %>% 
      mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, seq(0, 1, by=0.1)), include.lowest = T)))
    decile_le<-.x %>% group_by(decile_income) %>% 
      summarise(le=weighted.mean(le, w=pop))
    decile_le
  }) %>% left_join(total_pop_msa) %>% left_join(region) %>% 
  filter(total_pop>=1000000)

#Figure 3b

cv_decile<-le_by_decile %>% group_by(Region, decile_income) %>% 
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(Region)) 

cv_decile_tot<-le_by_decile%>%
  group_by(decile_income)%>%
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(5))
         

cv_decile1<-cv_decile%>%
 bind_rows(cv_decile_tot)%>%
mutate(Region=ordered(Region, levels=c(2, 3, 1, 4,5), labels=c("Midwest", "South", "Northeast", "West", "Overall"))) 

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
  scale_y_continuous(limits=c(0, 4), breaks=seq(0, 4, by=1))+
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
 scale_y_continuous(limits=c(0, 3), breaks=seq(0, 3, by=1))+
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



#view highest and lowest disp by region and level 
view<-le_by_decile%>%
  arrange(Region, decile_income, le)

#Table 2- Analysis examining Pop size and MHI as predictors of disparities 

#join the cbsa dataset (pop & MHI w/ absolute, relative inequities measure)
#the total_pop and pop differ-- more likely to trust pop since it's from tidycensus rather than sum of CT

cbsa_inequities<-absolute_rel_ineq_long%>%
  rename(GEOID=cbsa)%>%
  left_join(cbsa, by="GEOID")%>%
  mutate(poplog=log(pop),
         mhilog=log(mhi),
         mhi_cat=as.numeric(cut(mhi, breaks=c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 130000, 140000, 150000, 160000, 170000, 180000, 190000, 200000), right=T, 
                                      labels=c(1, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 130000,140000, 150000, 160000, 170000, 180000, 190000))))
str(cbsa_inequities)

cbsa_abs<-cbsa_inequities%>%
  filter(type=="Abs. Disparity")%>%
  mutate(region=factor(Region, levels=c(4, 1, 2, 3)))

summary(cbsa_abs$mhi)
summary(cbsa_abs)

#exploratory visualization (scatter plots)
absmhi<-ggplot(cbsa_abs, aes(x=mhi, y=value))+geom_point()
absmhi
logmhi<-ggplot(cbsa_abs, aes(x=mhilog, y=value))+geom_point()
logmhi
abspop<-ggplot(cbsa_abs, aes(x=poplog, y=value))+geom_point()
abspop

#regress inequities on absolute disparities
#model 1- pop (log)
summary(abspop<-lm(value~log(pop), data=cbsa_abs))
confint(abspop,level=0.95)
coef(abspop)*log(1.01)

0.255*log(1.01)

#model 2 - MHI (log) 
summary(abs_mhi<-lm(value~log(mhi), data=cbsa_abs))
confint(abs_mhi,level=0.95)

#model 3- region 
summary(region<-lm(value~ region, data=cbsa_abs))

#Model 4- MHI+ Pop
summary(abs<-lm(value~log(mhi) +log(pop)+ region, data=cbsa_abs))
confint(abs, level=0.95)

#percent change (coef*log(1.1) where .1=10% change
coef(abs)*log(1.1)
confint(abs, level=0.95)*log(1.1)

summary(abs<-lm(value~log(mhi) +log(pop), data=cbsa_abs))
coef(abs)*log(1.1)


#TABLE 2
#find the mean, sd, cv for each decile within regions
cv_decile<-le_by_decile %>% group_by(Region, decile_income) %>% 
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)

fwrite(cv_decile, "results/cv_decile.csv")

#repeat without grouping by region

cv_decile_tot<-le_by_decile %>% group_by(decile_income) %>% 
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)

fwrite(cv_decile_tot, "results/cv_decile_tot.csv")

#Descriptives:  -----
#mean differences by income 
mean<-le_by_decile%>%
group_by(Region_Name, decile_income)%>%
  summarize(mean=mean(le), 
            sd=sd(le))

figure4<- ggplot()+ 
  geom_line(data=mean, aes(x=decile_income, y=mean))+
  geom_point(data=mean, aes(x=decile_income, y=mean))+
  facet_wrap(~Region_Name)

ggplotly(figure3)

## MSA per region 

prop.table(table(region$Region_Name))


#for conclusion-- find 10% and 90th percentile for AMES iowa
ames<-dta %>% as.data.frame() %>% 
  filter(cbsa==11180)

wtd.quantile(ames$le, q = c(.1, .9), weight = ames$pop)

quantile(ames$le, probs = seq(.1, .9, by = .1))

#for results - find years of gap for san jose
sj<-dta%>%
  filter(cbsa==41940)%>%
  arrange(le)

wtd.quantile(sj$le, q = c(.1, .9), weight = sj$pop)

quantile(sj$le, probs = seq(.1, .9, by = .1))

#view quintiles

####### Appendix Figure 1 -----
## correlation between indicators
full_dta<-bind_rows(absolute_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Total"),
                    income_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Income")) %>% 
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>%
  group_by(type2) %>%
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                                      "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Deviation",
                                      "Income: Top/Bottom Difference", "Income: Top/Bottom Ratio",
                                      "Income: Slope Index of Inequality", "Income: Relative Index of Inequality"),
                      labels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                               "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Dev.",
                               "Income: Top/Bottom Difference", "Income:Top/Bottom Ratio",
                               "Income: SII", "Income: RII"))) %>%
  left_join(xwalk_region) %>% 
  select(cbsa, type2, value, Region_Name) %>% 
  spread(type2, value)
cols<-c("Total: Abs. Disparity", "Total: Rel. Disparity",
        "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Dev.", 
        "Income: Top/Bottom Difference", "Income:Top/Bottom Ratio",
        "Income: SII", "Income: RII")
corrs<-ggpairs(data=full_dta, 
               columns = cols, upper = list(continuous = wrap("cor", size=6, color="black", stars=F))) +
  theme_bw() +
  theme(strip.background=element_blank(),
        strip.text = element_text(color="black", size=12, face="bold"),
        axis.text=element_text(color="black"))
ggsave(filename="results/Appendix_figure2.pdf", corrs, width=20, height=15)

#Appendix FIgure 3
hist(dta$mhi)
summary(dta$mhi)

test<- dta%>%
  group_by(cbsa)%>%
  income_20k=cut(mhi, breaks= seq(20000, 250000, by=20000), include.lowest = T, 
labels=c("20,000", "40,000", "60,000", "80,000", "100,000", "120,000", "140,000", "160,000", "180,000", "200,000"))

le_by_decile1<-dta %>% group_by(cbsa) %>% 
  group_modify(~{
    #.x<-dta %>% filter(cbsa==25940)
    .x<-.x %>% 
      mutate(income_20k=cut(mhi, breaks= seq(20000, 250000, by=20000), include.lowest = T))
    decile_le<-.x %>% group_by(income_20k) %>% 
      summarise(le=weighted.mean(le, w=pop))
    decile_le
  }) %>% left_join(total_pop_msa) %>% left_join(xwalk_region) %>% 
  filter(total_pop>=1000000) %>% 
  ungroup() %>% 
  mutate(income_20k = income_20k %>% 
           dplyr::recode("[2e+04,4e+04]"="20,000",
                         "(4e+04,6e+04]"= "40,000",
                         "(6e+04,8e+04]"="60,000",
                         "(8e+04,1e+05]"='80,000',
                         "(1e+05,1.2e+05]"="100,000",
                         "(1.2e+05,1.4e+05]"= "120,000",
                         "(1.4e+05,1.6e+05]"= "140,000",
                         "(1.6e+05,1.8e+05]"="160,000",
                         "(1.8e+05,2e+05]"="180,000",
                         "(2e+05,2.2e+05]"="200,000",
                         "(2.2e+05,2.4e+05]"="220,000"))

figure3<-ggplot(le_by_decile1, 
                aes(x=income_20k, y=le, group=cbsa)) +
  geom_line(data=le_by_decile1 %>% mutate(Region_Name="Midwest Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile1 %>% mutate(Region_Name="Northeast Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile1 %>% mutate(Region_Name="South Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile1 %>% mutate(Region_Name="West Region"), 
            color="gray", alpha=1)+
  geom_line(aes(color=Region_Name))+
  geom_point(aes(fill=Region_Name), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  labs(x="Median Household Income",
       y="Life Expectancy (years)")+
  scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  # scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  facet_wrap(~Region_Name)+
  guides(color=F, fill=F)+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=16),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())
figure3
ggsave("results/figure3_ASM.pdf", width=10, height=7.5)

ggplotly(figure3)

# App Data----


# ___Table 1 ----
{
  top1<-summary_absolute %>% arrange(rank) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  bottom1<-summary_absolute %>% arrange(desc(rank)) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  top2<-summary_income %>% arrange(rank) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  bottom2<-summary_income %>% arrange(desc(rank)) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  table1<-bind_cols(bind_rows(top1%>% arrange(rank), 
                              data.frame(cbsa_name="...", Region_Name="...", rank=NA),
                              bottom1 %>% arrange(rank)) ,
                    bind_rows(top2%>% arrange(rank), 
                              data.frame(cbsa_name="...", Region_Name="...", rank=NA),
                              bottom2 %>% arrange(rank)) )
  
  summary_large<-bind_rows(absolute_ineq_long, income_ineq_long) %>% 
    # filter(total_pop>=1000000) %>% 
    arrange(type, value) %>% 
    group_by(type) %>% 
    mutate(rank=row_number()) %>% 
    group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
    summarise(rank=mean(rank)) %>% 
    arrange(desc(rank)) %>% 
    ungroup() %>% 
    mutate(rank=row_number())
  df_table1<-full_join(absolute_ineq %>% 
                         ungroup() %>%
                         rename(total_dif=dif,
                                total_ratio=ratio), 
                       income_ineq) %>% left_join(total_pop_msa) %>% 
    # filter(total_pop>=1000000) %>% 
    left_join(summary_large %>% select(cbsa, rank)) %>% 
    select(cbsa_name,total_pop, Region_Name, rank, 
           total_dif, total_ratio, cv, gini,mld.wt,
           dif, ratio, sii, rii) %>% arrange(rank) %>% 
    mutate(Region_Name=sub(" Region", "", Region_Name)) %>% 
    ### Formatting for dashboard table
    rename(Name=cbsa_name,
           Region=Region_Name,
           Rank=rank,
           abs_diff=total_dif,
           abs_ratio=total_ratio,
           abs_cv=cv,
           abs_gini=gini,
           abs_mld=mld.wt,
           income_diff = dif,
           income_ratio = ratio,
           income_sii= sii,
           income_rii = rii
    ) %>% 
    mutate(abs_mld =round(abs_mld,5),
           income_rii = round(income_rii,3)) %>% 
    mutate_at(vars(abs_diff,
                   abs_ratio,
                   abs_cv, abs_gini,
                   income_diff, income_ratio,
                   income_sii),
              ~round(.x,2)) %>% 
    select(-Rank) %>% 
    arrange(desc(total_pop))
  df_table1
}


# ___Figure 1 ----
{
  library(glue)
  df_absolute_ineq_long=absolute_ineq_long %>% ungroup()%>% 
    mutate(outcome = "total")
  df_income_ineq_long=income_ineq_long %>% ungroup() %>% 
    mutate(outcome = "income")
  df_fig1 = bind_rows(df_absolute_ineq_long, df_income_ineq_long)%>% 
    group_by(outcome,type) %>% 
    group_modify(~.x %>% 
                   mutate(value_rounded = round(value,3)) %>% 
                   mutate(value = round(value,4)) %>% 
                   group_by(Region_Name) %>% 
                   mutate(median = median(value)) %>% 
                   ungroup() %>% 
                   arrange(desc(median)) ) %>% 
    ungroup() %>% 
    mutate(tooltip = glue(
      '<b>{cbsa_name}</b>
      {type}: {value_rounded} 
      Population: {format(total_pop, big.mark = ",")} 
      ',
    ) %>% as.character()) %>% 
    select(outcome, type,Region_Name, value,total_pop, tooltip ) %>% 
    mutate(outcome = str_to_title(outcome)) 
}

# ___Figure 2 ----
{
  library(HatchedPolygons)
  ## Shapes Files
  sf_cbsa_raw<-read_sf("data/cb_2013_us_cbsa_20m/cb_2013_us_cbsa_20m.shp") %>% 
    mutate(cbsa=as.numeric(GEOID))
  sf_cbsa = sf_cbsa_raw %>% 
    select(cbsa) %>% 
    filter(cbsa%in%(full_dta %>% ungroup() %>% pull(cbsa)))
  sf_regions<-read_sf("Data/cb_2013_us_region_20m/cb_2013_us_region_20m.shp")
  sf_states<-read_sf("Data/cb_2013_us_state_20m/cb_2013_us_state_20m.shp") %>% mutate(state = STUSPS )
  get_hatch_holes = function(sf_state_tmp){
    state_tmp = unique(sf_state_tmp$state)
    sf_polygon_missing_all=st_cast(sf_state_tmp, "POLYGON")
    sf_polygon_missing = sf_polygon_missing_all %>% 
      mutate(area = st_area(sf_polygon_missing_all)) %>% 
      filter(area == max(area)) %>% 
      mutate(state = state_tmp)
    spa = as(sf_polygon_missing, "Spatial")
    res = HatchedPolygons::hatched.SpatialPolygons(spa,density = 2) 
    res %>% st_as_sf()
  }
  sf_ME_hatch = get_hatch_holes(sf_states %>% filter(STUSPS == "ME"))
  sf_WI_hatch = get_hatch_holes( sf_states %>% filter(STUSPS == "WI"))
  sf_fig2_missing_hatched =  bind_rows(sf_ME_hatch,sf_WI_hatch)
  sf_fig2_states_missing = sf_states %>% filter(STUSPS%in%c("ME","WI"))

  ## Data for map
  library(shiny)
  xwalk_region = df_absolute_ineq_long %>% select(cbsa, Region, Region_Name) %>% distinct()
  df_fig2 = bind_rows(absolute_ineq_long %>% select(cbsa, type, value) %>% 
                        mutate(ineq="Total"),
                      income_ineq_long %>% select(cbsa, type, value) %>% 
                        mutate(ineq="Income")) %>% 
    mutate(type2=paste0(ineq, ": ", type)) %>% 
    arrange(desc(value)) %>%
    group_by(type2) %>%
    mutate(rank=row_number()) %>% 
    mutate(type2=factor(type2, levels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                                        "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Deviation",
                                        "Income: Top/Bottom Difference", "Income: Top/Bottom Ratio",
                                        "Income: Slope Index of Inequality", "Income: Relative Index of Inequality"),
                        labels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                                 "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Dev.",
                                 "Income: Top/Bottom Difference", "Income: Top/Bottom Ratio",
                                 "Income: SII", "Income: RII"))) %>%
    left_join(xwalk_region) %>% 
    ungroup() %>% 
    left_join(sf_cbsa_raw %>%
                as.data.frame() %>% 
                select(cbsa, NAME) %>% 
                distinct()) %>% 
    mutate(label = str_c("<b>",NAME,"</b><br>",
                         "<b>",ineq," ",type," :</b>", round(value,2),"<br>",
                         "<b>Rank :</b>",rank
    ) %>% 
      map(~HTML(.x))) %>% 
    mutate(type2 = paste0(ineq,": ", type))  

}

# ___Figure 3 ----
{ ## Figure 3 data
  le_by_decile<-dta %>% group_by(cbsa) %>% 
    group_modify(~{
      #.x<-dta %>% filter(cbsa==25940)
      .x<-.x %>% 
        mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, seq(0, 1, by=0.1)), include.lowest = T)))
      decile_le<-.x %>% group_by(decile_income) %>% 
        summarise(le=weighted.mean(le, w=pop))
      decile_le
    }) %>% left_join(total_pop_msa) %>% left_join(region) %>% ungroup() # %>%  filter(total_pop>=1000000)
  
  #Figure 3b
  
  cv_decile<-le_by_decile %>% group_by(Region, decile_income) %>% 
    summarize(mean=mean(le), 
              sd=sd(le), 
              cv=sd/mean*100)%>%
    pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
    mutate(Region=factor(Region)) %>% ungroup()
  
  cv_decile_tot<-le_by_decile%>%
    group_by(decile_income)%>%
    summarize(mean=mean(le), 
              sd=sd(le), 
              cv=sd/mean*100)%>%
    pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
    mutate(Region=factor(5))
  
  
  df_fig3 <-cv_decile%>%
    bind_rows(cv_decile_tot)%>%
    mutate(Region=ordered(Region, levels=c(2, 3, 1, 4,5), labels=c("Midwest", "South", "Northeast", "West", "Overall"))) 
  
}

# ___UI elements -----
df_fig1_choices_type = df_fig1 %>% 
  count(outcome,type)%>% 
  mutate(outcome = str_to_title(outcome)) %>% 
  mutate(type2 = paste0(outcome,": ", type)) %>% 
  mutate(type = as.character(type)) %>% 
  select(-n)

df_intro_home = read.csv("App/introJS/df_intro_home.csv") %>% as.data.frame() %>% select(-X)
###  Save data for App
save(df_fig1_choices_type,
     file= "App/cleaned_le_income_cities_UIelements.rdata" )
save(
  df_table1,   ## Table 1 
  df_fig1,     ## Figure 1
  df_fig2,     ## Figure 2
  df_fig3,sf_fig2_missing_hatched,sf_fig2_states_missing,     ## Figure 3
  sf_regions,sf_cbsa, ## Shape Files
  file= "App/cleaned_le_income_cities_appBundle.rdata")

