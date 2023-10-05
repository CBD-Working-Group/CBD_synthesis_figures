require(dplyr)
require(tidyr)
require(ggplot2)
require(cowplot)


CC <- read.table("./WOS_trends/WOS_Sept28_2023/CC.txt", sep="\t", header=T)
B <- read.table("./WOS_trends/WOS_Sept28_2023/B.txt", sep="\t", header=T)
ID <- read.table("./WOS_trends/WOS_Sept28_2023/ID.txt", sep="\t", header=T)

CC_B <- read.table("./WOS_trends/WOS_Sept28_2023/CC_AND_B.txt", sep="\t", header=T)
CC_ID <- read.table("./WOS_trends/WOS_Sept28_2023/CC_AND_ID.txt", sep="\t", header=T)
B_ID <- read.table("./WOS_trends/WOS_Sept28_2023/B_AND_ID.txt", sep="\t", header=T)
CC_B_ID <- read.table("./WOS_trends/WOS_Sept28_2023/CC_AND_B_AND_ID.txt", sep="\t", header=T)


# totals
sum(CC$Record.Count) + sum(ID$Record.Count) + sum(B$Record.Count) + 
sum(CC_B$Record.Count) + sum(CC_ID$Record.Count) + sum(B_ID$Record.Count) + sum(CC_B_ID$Record.Count)

names(CC)[3] <- "percent_total"
names(B)[3] <- "percent_total"
names(ID)[3] <- "percent_total"
names(CC_B)[3] <- "percent_total"
names(CC_ID)[3] <- "percent_total"
names(B_ID)[3] <- "percent_total"
names(CC_B_ID)[3] <- "percent_total"


CC$Subject <- "CC"
B$Subject <- "B"
ID$Subject <- "ID"
CC_B$Subject <- "CC&B"
CC_ID$Subject <- "CC&ID"
B_ID$Subject <- "B&ID"
CC_B_ID$Subject <- "CC&B&ID"

dat <- rbind(CC, B, ID, CC_B, CC_ID, B_ID, CC_B_ID)
names(dat)[1] <- "year"
names(dat)[2] <- "records"

dat %>% arrange(year) %>% head(20)
# missing some combinations

# remove precent total and add missing combinations, filling with zero for records
dat <- select(dat, -percent_total)
dat <- complete(dat, year, Subject, fill = list(records = 0))

# cumulative sum
dat_long <- dat %>% group_by(Subject) %>%
                            arrange(year) %>%
                            mutate(total = cumsum(records))
tail(dat_long)
# looks correct compared to WOS totals

# red = diversity, 
# blue = climate, 
# yellow = disease, 
# green = disease/climate, 
# orange = diversity/disease, 
# purple = diversity/climate …

# new colorblind palette
# https://jacksonlab.agronomy.wisc.edu/2016/05/23/15-level-colorblind-friendly-palette/

colors <- c(
                        "#006ddb", # blue
                        "#b6dbff", # B & ID - powderblue
                        "#920000", # dark red
                        "#009292", # light teal
                        "#490092", # dark purple
                        "#db6d00", # orange
                        '#F5E1A4', # infectious disease - beige
                        "#ffb6db", # pink
                        "#ffff6d", # bright yellow
                        "#b66dff", # fuschia
                        "#ff6db6", # hot pink
                        "#6db6ff", # skyblue
                        "#924900", # mocha
                        "#009292", # B      - light teal
                        "#24ff24", # lime green
                        "#000000" # black
                        )
colors2<-c("#084C92", #blue B
           "#A3C6E5", # light blue B & ID
           "#990F26", #red CC
           "#349B89", # teal CC & B
           "#3D0F99", # purple CBD
           "#E57D32", # orange CC & ID
           "#CCAA7A" # tan ID
           )

unique(dat_long$Subject)


dat_long$Subject_fct <- factor(dat_long$Subject, 
                        levels=c("B", "B&ID", "CC", "CC&B", "CC&B&ID", "CC&ID","ID"), 
                        labels=c("Biodiversity only", "Biodiv. + Inf. Dis.", "Climate Change only",
                            "Climate + Biodiv.", "Climate + Biodiv. + Inf. Dis.","Climate + Inf. Dis.", "Infectious Disease only"))

# Combo line plot
p_combo <- ggplot(dat_long, aes(fill=Subject_fct, y=total, x=year)) + 
    geom_bar(position="fill", stat="identity") +  scale_fill_manual(values=colors2) + 
    theme_classic() +
    # scale_y_continuous(expand = c(0, 0), labels=function(x) format(x, big.mark = ",", scientific = FALSE),
    #     trans="log10", breaks = c(10, 100, 1000, 10000, 100000,1000000)) + 
        # xlim(1975,2020) +
    scale_x_continuous(breaks = seq(1975,2021,5), limits=c(1975,2021)) +
    xlab("Year") + ylab("Proportion of total publications") +
    # theme(legend.position = c(0.22, 0.8), 
    theme(strip.background = element_blank(),
            strip.text.x = element_text(size=10, face="bold", hjust=0.02),
            legend.title = element_blank())


p_combo

dat_long %>%
  filter(Subject != "B&ID") %>%
  filter(Subject != "CC&B") %>%
  filter(Subject != "CC&B&ID") %>%
  filter(Subject != "CC&ID") -> dat_single
dat_single$Subject<-factor(dat_single$Subject, levels = c("ID", "B", "CC"))

dat_single %>%  
  ggplot(aes(fill=Subject, y=records, x=year)) + 
  geom_area()+
  scale_fill_manual(values=c("#F5E1A4", "#006ddb", "#920000")) + 
  theme_classic() +
  scale_x_continuous(breaks = seq(1975,2021,5), limits=c(1975,2021)) +
  xlab("Year") + ylab("Number of publications") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size=10, face="bold", hjust=0.02),
        legend.title = element_blank())

# Give a specific order:
dat_long$Subject <- factor(dat_long$Subject, levels=c("ID", "B", "CC", "B&ID", "CC&B", "CC&ID", "CC&B&ID") )
dat_long %>%
  ggplot(aes(fill=Subject, y=records, x=year)) + 
  geom_area(stat="identity")+
  scale_fill_manual(values=c("#CCAA7A", "#084C92", "#990F26","#A3C6E5","#349B89", "#E57D32", "#3D0F99")) + 
  theme_classic() +
  scale_x_continuous(breaks = seq(1975,2022,5), limits=c(1975,2022)) +
  xlab("Year") + ylab("Number of publications") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size=10, face="bold", hjust=0.02, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        legend.title = element_blank())->p_all

ggsave("~/Desktop/boxfigure_2023.png", p_all, width=8, height=4)
ggsave("./papers_proportion_oct2022.pdf",p_combo, width=8, height=4)
ggsave("./papers_proportion_oct2022.png",p_combo, width=8, height=4)


# subset to intersections
dat_intersect <- dat_long[dat_long$Subject%in%c("B&ID","CC&B","CC&B&ID","CC&ID"),]



colors_intersect <- c(
                        "#b6dbff", # powderblue
                        "#009292", # light teal
                        "#490092", # dark purple
                        # "#000000", # black
                        "#db6d00", # orange
                        "#ffb6db", # pink
                        "#ff6db6", # hot pink
                        "#b66dff", # fuschia
                        "#920000", # dark red
                        "#924900", # mocha
                        "#6db6ff", # skyblue
                        "#004949", # dark teal  
                        "#ffff6d", # bright yellow
                        "#24ff24", # lime green
                        "#006ddb" # blue
                        )

colors_intersect2 <- c(
  "#A3C6E5", # light blue B & ID
  "#349B89", # teal CC & B
  "#3D0F99", # purple CBD
  "#E57D32" # orange CC & ID
)
# Combo bar plot
p_intersect <- ggplot(dat_intersect, aes(fill=Subject_fct, y=total, x=year)) + 
    geom_bar(position="fill",stat="identity") +  scale_fill_manual(values=colors_intersect) + 
    theme_classic() +
    scale_x_continuous(breaks = seq(1975,2022,5), limits=c(1975,2022)) +
    xlab("Year") + ylab("Proportion of total publications") +
    theme(strip.background = element_blank(),
            strip.text.x = element_text(size=10, face="bold", hjust=0.02),
            legend.title = element_blank())

p_intersect

ggsave("./intersect_papers_proportion_oct2022.pdf",p_intersect, width=8, height=4)
ggsave("./intersect_papers_proportion_oct2022.png",p_intersect, width=8, height=4)



# Combo bar plot
p_intersect2 <- ggplot(dat_intersect, aes(color=Subject_fct, y=records, x=year)) + 
    geom_line(stat="identity", linewidth=1) + 
    scale_color_manual(values=colors_intersect2) + #scale_fill_manual(values=colors_intersect) + 
    theme_classic() +
    scale_x_continuous(breaks = seq(1990,2022,5), limits=c(1990,2022)) +
    xlab("Year") + ylab(" ") +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size=10, face="bold", hjust=0.02),
          axis.text = element_text(size = 10, color = 'black'),
          legend.title = element_blank(),
          legend.position = "none")
ggsave("~/Desktop/box_inlay_2023.png", p_intersect2, height=2, width=4)

p_all / (p_intersect2 + plot_spacer())

