
# load packages
library(tidyverse)
library(qgraph)
library(igraph)
library(summarytools)

# %%%%%%%%%%%%%%% #
####  NETWORK ####
# %%%%%%%%%%%%% #

### calculating rank score
# Information extracted from supplementary S5.
ranking <- read_csv("edgelist_robust.csv")
#adding weight for total number of studies
ranking$strenght <- ranking$significance * 12/ranking$appearance


edgelist <- select(ranking, from, to, strenght)
 
dep_labels <-  c("Mood", "Anhed", "Weight","Sleep", "Psychom","Fatigue","Worthl","Concent","Suicid")

 
 ## base network
basenet <- qgraph(edgelist, directed=FALSE, layout = "spring",
                  edge.label.color="black", legend = FALSE)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#### SPINGLASS COMMUNITIES #####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

spingraph <- as.igraph(basenet, attributes = TRUE)


## create object
com_spinglass <- matrix(NA, nrow=9,ncol=1000)
com_totspin <- matrix(NA, nrow=1,ncol=1000)

## spinglass run (1000 times)
for (i in 1:1000) {
  set.seed(i)
  spinglass <- spinglass.community(spingraph)
  com_totspin [1,i] <- max(spinglass$membership)
  com_spinglass[,i] <- spinglass$membership
}


#save n of communities distribution
# two is most likely number or clusters
tot_spin <- factor(t(as.vector(com_totspin)))
sink("spinglass communities.txt", append=TRUE, split=TRUE)
freq(tot_spin)
sink()


# looking at community configurations
# i.e. which symptoms goes with what group
# they should be all identical, but let's examine just in case

com_spin<- as_tibble((t(com_spinglass)))
names(com_spin)<- dep_labels

# removing all solutions with more than 2 clusters
com_spin <- com_spin %>% filter( Mood != 3 & Anhed != 3 & Weight != 3 & Sleep != 3 & Psychom != 3 &
                                   Fatigue != 3 & Worthl != 3 & Concent != 3 & Suicid  != 3)

# community numbers don't have meaning, need to consolidate results based on real groups
# first cluster = A, second = B

com_spin <- com_spin %>% mutate( D2 = case_when(Anhed == Mood ~ "A", TRUE ~ "B"),
                                 D3 = case_when(Weight == Mood ~ "A", TRUE ~ "B"),  
                                 D4 = case_when(Sleep == Mood ~ "A", TRUE ~ "B"),
                                 D5 = case_when(Psychom == Mood ~ "A", TRUE ~ "B"),
                                 D6 = case_when(Fatigue == Mood ~ "A", TRUE ~ "B"),
                                 D7 = case_when(Worthl == Mood ~ "A", TRUE ~ "B"),
                                 D8 = case_when(Concent == Mood ~ "A", TRUE ~ "B"),
                                 D9 = case_when(Suicid == Mood ~ "A", TRUE ~ "B"))

com_spin$D1 <- "A"

com_spin_org <- select(com_spin, D1,D2,D3,D4,D5,D6,D7,D8,D9)
names(com_spin_org)<- dep_labels

sink("clusters.txt",append=TRUE, split=TRUE )
dfSummary(com_spin_org)
sink()

# All identical solutions!



# %%%%%%%%%%%%%%% #
####    PLOT  ####
# %%%%%%%%%%%%% #

# see spinglass to see how to identify
clust_dep <- as.matrix(com_spin_org[1,])
   
clust_dep <- ifelse(clust_dep == "A", "Affective", "Somatic")


# Plot
# edge labels as they appear in the final plot were done in LaTex and added in QGRAPH
Plot <- qgraph(basenet, repulsion=1.2, esize = 35,
               labels= dep_labels, label.scale.equal = TRUE,  label.norm = "OOOOOOOOOO", label.cex = 4.0,
               groups = clust_dep,color = c('#e05167', '#5983b9')
               )





