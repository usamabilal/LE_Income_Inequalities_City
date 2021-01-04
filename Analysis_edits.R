#Changes to Figures 

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
load("data/clean_data.rdata")
total_pop_msa<-dta %>% group_by(cbsa_name, cbsa) %>% 
  summarise(total_pop=sum(pop))


# first, lets calculate absolute ineq indicators by city
# Gap and ratio: need .9 and .1 weighted quantiles
# CV
# GINI from reldist package

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
               gini=gini) %>% as_tibble
  }) %>% left_join(region)
absolute_ineq_long<-absolute_ineq %>% gather(type, value, -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("dif", "ratio", "cv", "gini"),
                     labels=c("Abs. Difference", "Rel. Difference",
                              "Coefficient of Variation", "Gini")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)

absolute_rel_ineq_long<-absolute_ineq_long%>%
  filter(type %in% c("Abs. Difference", "Rel. Difference"))


f1a<-ggplot(absolute_rel_ineq_long,aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  facet_wrap(~type, scales="free_y")+
  guides(color=F, fill=F, size=F)+
  labs(x="",
       y="Value",
       title="Total Inequalities in Life Expectancy by MSA")+
  #scale_y_continuous(sec.axis=dup_axis(name = ylab2), limits=ylim)+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=14, color="black"),
        axis.text.y=element_text(size=12, color="black"),
        axis.title.y=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(face="bold", size=25, hjust=0.5))
f1a
ggsave("results/figure1_ASM.pdf", f1a, width=20, height=7.5)

#Table 1 
summary_absolute<-absolute_ineq_long %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())
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
  filter(total_pop>=1000000 & type=="Abs. Difference") %>% 
  arrange(desc(value)) %>% 
  group_by(type) %>% 
  mutate(rank=row_number())%>%
  ungroup()%>%
  select(cbsa_name, Region_Name, rank, value )%>%
  rename(total_dif=value)%>%
mutate(Region_Name=sub(" Region", "", Region_Name))%>%
mutate(total_dif=format(total_dif, digits=1, nsmall=1))

summary_large_rel<-absolute_ineq_long %>% 
  filter(total_pop>=1000000 & type=="Rel. Difference") %>% 
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

#Figure 2
full_dta<-absolute_ineq_long %>% select(cbsa, type, value) %>% 
  filter(type %in% c("Abs. Difference", "Rel. Difference"))%>%
                      mutate(ineq="Total") %>% 
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>% 
  group_by(type2) %>% 
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels=c("Total: Abs. Difference", "Total: Rel. Difference")))
shp<-read_sf("data/cb_2013_us_cbsa_20m/cb_2013_us_cbsa_20m.shp") %>% 
  mutate(cbsa=as.numeric(GEOID))
regions<-read_sf("Data/cb_2013_us_region_20m/cb_2013_us_region_20m.shp")
shp_with_data<-inner_join(shp, full_dta)
bbox_temp<-st_bbox(shp_with_data)
m<-ggplot()+
  geom_sf(data=shp_with_data, size=0,
          aes(geometry=geometry, color=rank, fill=rank))+
  geom_sf(data=regions, size=0.5, color="black", 
          fill=NA,
          aes(geometry=geometry))+
  scale_fill_binned(name="Rank", type="gradient",
                    show.limits=T,n.breaks=5,labels=round,
                    low="red", high="white")+
  scale_color_binned(name="Rank", type="gradient",
                     show.limits=T,n.breaks=5,labels=round,
                     low="red", high="white")+
  coord_sf(xlim = c(bbox_temp["xmin"], bbox_temp["xmax"]),
           ylim = c(bbox_temp["ymin"], bbox_temp["ymax"]), expand = T) +
  guides(alpha=F, size=F, color=F)+
  #labs(title="Renta media por hogar") +
  facet_wrap(~type2)+
  theme_void()+
  theme(plot.title = element_text(size=20, face="bold", hjust=.5),
        strip.text = element_text(size=10, face="bold", hjust=.5),
        panel.background = element_rect(fill = "white", color=NA),
        legend.position="bottom")
m
ggsave("results/figure3_ASM.pdf", width=16, height=5)

#figure 4
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

ggplot(le_by_decile, 
       aes(x=decile_income, y=le, group=cbsa)) +
  geom_line(data=le_by_decile %>% mutate(Region_Name="Midwest Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile %>% mutate(Region_Name="Northeast Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile %>% mutate(Region_Name="South Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile %>% mutate(Region_Name="West Region"), 
            color="gray", alpha=1)+
  geom_line(aes(color=Region_Name))+
  geom_point(aes(fill=Region_Name), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  labs(x="Decile of Median Household Income",
       y="Life Expectancy (years)")+
  scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  facet_wrap(~Region_Name)+
  guides(color=F, fill=F)+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=16),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())
ggsave("results/figure4.pdf", width=10, height=7.5)



###############################################################
####### Appendix Figure 1 
## correlation between indicators
full_dta<-bind_rows(absolute_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Total"),
                    income_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Income")) %>% 
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>% 
  group_by(type2) %>% 
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels=c("Total: Abs. Difference", "Total: Rel. Difference",
                                      "Total: Coefficient of Variation", "Total: Gini",
                                      "Income: Abs. Difference", "Income: Rel. Difference",
                                      "Income: Slope Index of Inequality", "Income: Relative Index of Inequality"),
                      labels=c("Total: Abs. Difference", "Total: Rel. Difference",
                               "Total: Coefficient of Variation", "Total: Gini",
                               "Income: Abs. Difference", "Income: Rel. Difference",
                               "Income: SII", "Income: RII"))) %>% 
  left_join(region) %>% 
  select(cbsa, type2, value, Region_Name) %>% 
  spread(type2, value)
cols<-c("Total: Abs. Difference", "Total: Rel. Difference",
        "Total: Coefficient of Variation", "Total: Gini",
        "Income: Abs. Difference", "Income: Rel. Difference",
        "Income: SII", "Income: RII")
corrs<-ggpairs(data=full_dta, 
               columns = cols, upper = list(continuous = wrap("cor", size=6, color="black"))) +
  theme_bw() +
  theme(strip.background=element_blank(),
        strip.text = element_text(color="black", size=12, face="bold"),
        axis.text=element_text(color="black"))
ggsave(filename="results/Appendix_figure1.pdf", corrs, width=20, height=15)


