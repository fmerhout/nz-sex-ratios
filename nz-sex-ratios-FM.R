###--------------------------------------------------
### Sex Composition of New Zealand Electorates
### Kieran Healy
### @kjhealy
###--------------------------------------------------


library(ggplot2)

### Install tidyr if you don't have it:
### install.packages("tidyr")
library(tidyr)

# set working directory
setwd('/Users/find_leori/Soc-880-Data-Visualization/nz-sex-ratios')

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)


### Data from Jonathan Marshall
### https://github.com/jmarshallnz/electorates

dat <- read.csv("data/electorates.csv",
                row.names = 1)

# create on observation per age group per electorate
dat.w <- spread(dat, Sex, count)
# create population count per age group per electorate
dat.w$Pop <- dat.w$Male + dat.w$Female
# create male surplus percentage
dat.w$Diff <- dat.w$Male / dat.w$Female
# create percentage of electorate male
dat.w$pMale <- (dat.w$Male /  dat.w$Pop) # intial code unnecessarily complicated (dat.w$Male + dat.w$Female))
# create percentage of electorate female
dat.w$pFemale <- (dat.w$Female /  dat.w$Pop) # initial code unnecessarily compl. (dat.w$Male + dat.w$Female))
# difference between percentages male and female
dat.w$pDiff <- dat.w$pMale - dat.w$pFemale

# create logical indicator for positive percentage difference
dat.w$pos <- dat.w$Diff > 1

# reorder electorate based on absolute diff male/female
dat.w$Electorate <- reorder(dat.w$Electorate, dat.w$Diff, order=TRUE)

# create percentage labels
pct.labs <- c("0.6", "0.8", "1", "1.2")

# create age labels
age.lab <- c("0-4", "5-9", "10-14", "15-19", "20-24",
             "25-29", "30-34", "35-39", "40-44", "45-49",
             "50-54", "55-59", "60-64", "65-69", "70-74",
             "75-79", "80-84", "85 and up")

# create age levels
age.levels <- c("0–4 Years", "5–9 Years", "10–14 Years",
                "15–19 Years", "20–24 Years",
                "25–29 Years", "30–34 Years",
                "35–39 Years", "40–44 Years",
                "45–49 Years", "50–54 Years",
                "55–59 Years", "60–64 Years",
                "65–69 Years", "70–74 Years",
                "75–79 Years", "80–84 Years",
                "85 Years And Over")

# create (empty) factor variable with levels just created and add to df
dat.w$Age <- factor(dat.w$Age, levels = age.levels, ordered = TRUE)

# create ggplot object w/ age on x-axis, y-origin at 1, group by electorate, and color by logical vector
p0 <- ggplot(dat.w, aes(x = Age,
                        ymax = Diff,
                        ymin = 1,
                        group = Electorate,
                        color = pos))

# create object containing visualization
p1 <- p0 + geom_linerange(size=1.2) + # presentation style "interval presented by vertical line"
    labs(x="",
         y="Ratio of Male to Female Population",
         color="Sex Composition") +
    scale_color_manual(labels = c("Majority Female", "Majority Male"), # specify colors and legend
                       values=c("#E69F00", "#0072B2")) +
    scale_x_discrete(labels=age.lab) + # specify labels for x-axis
    scale_y_continuous(breaks=c(0.6, 0.8, 1, 1.2), labels=pct.labs) + # specify values for y-axis and labels
    coord_flip() + # flip visualization on the side 
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6),  # set size of tick labels
          axis.text.x = element_text(size = 6)) +
    theme(legend.position="top") + facet_wrap(~ Electorate, ncol = 4)

# print object to pdf
cairo_pdf(file="figures/nz-surplus-males.pdf", height=30, width=6)
print(p1)
dev.off()

# print object to png
ggsave(
    "figures/nz-surplus-males.png",
    p1,
    width=6,
    height=30,
    dpi=300
    )

# own graphic for HW
library(plyr)
library(grid)

# create count of # of ppl by sex per electorate
dat.a <- ddply(dat, c("Electorate", "Sex"), summarise,
               N = sum(count))

# create count of # of ppl per electorate
dat.a2 <- ddply(dat.a, c("Electorate"), summarise,
                    N_t = sum(N))

# expand dat.a2 to merge with dat.a
dat.a2 <- dat.a2[rep(row.names(dat.a2), 2), 1:2]
# reorder dat.a2 to merge with dat.a
dat.a2 <- dat.a2[order(dat.a2$Electorate),]

# append total number in electorate to dat.a
dat.a$N_t <- dat.a2$N_t

# subset data to only include N for female
dat.s <- dat.a[dat.a$Sex=="Female",]

# create percentage female variable
dat.s$pFem <- dat.s$N / dat.s$N_t

# order dat.s by percentage female
dat.s <- dat.s[order(dat.s$pFem),]

# create logical factor if pFem <.5
dat.s$fewFem <- dat.s$pFem < .5

# create percentage Female variable per Electorate
dat.a$pFem <- ifelse(dat.a$Sex=="Female", dat.a$N/dat.a$N_t, (dat.a$N_t-dat.a$N)/dat.a$N_t)

# create object containing bar chart with per Electorate female percentage
p  <- ggplot(dat.s, aes(reorder(Electorate,pFem), y=pFem, fill=fewFem)) + 
        geom_bar(stat="identity") +
        labs(x="",
             y="Proportion of Population Female",
             fill="Proportion Female") +
        scale_fill_manual(labels = c("\u2265 .50", "< .50"), # specify colors and legend
                           values=c("#DF7401", "#A901DB")) +
        geom_hline(yintercept=.5, alpha=.7) +
        scale_y_continuous(breaks=c(0.47, .50, .53)) +
        coord_flip(ylim = c(.47,.53)) +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 12),  # set size of tick labels
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 18),
              legend.position = c(.9,0.2),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
                legend.background = element_rect(color = "black")) 

# print object to pdf
cairo_pdf(file="figures/nz-proportion-female.pdf", height=11, width=13)
print(p)
dev.off()

# print object to png
ggsave(
  "figures/nz-proportion-female.png",
  p,
  width=13,
  height=11,
  dpi=300
)



