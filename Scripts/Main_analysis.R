## ------------------------------------------------------------------------
## 'The use of eponyms can also promote gender equity in modern taxonomy'
## ------------------------------------------------------------------------

# Pétillon, J. et al.

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)
# Author: Stefano Mammola

# Loading R packages ------------------------------------------------------

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("BAT","dplyr","forcats","ggplot2","performance","tibble","tidyr")

# Script settings -----------------------------------------------------------
theme_set(theme_bw())

theme_update(
  legend.background = element_blank(), #No background (legend)
  plot.background = element_blank(), #No background
  panel.grid = element_blank(), #No gridlines
  axis.text  = element_text(size = 10, colour = "grey10"), #Size and color of text
  axis.title = element_text(size = 12, colour = "grey10") #Size and color of text
)

set.seed(42)

# Loading the main database ------------------------------------------------

# Described associated with this publication:
# Mammola, S., Viel, N., Amiar, D., Mani, A., Hervé, C., Heard, S. B., ... & Pétillon, J. (2023). Taxonomic practice, creativity and fashion: what’s in a spider name?. Zoological Journal of the Linnean Society, 198(2), 494-508.
# Check the publication for a full description
db_names <- read.csv(file = "Data/db_etymology.csv", header = TRUE, sep = "\t", as.is = TRUE)

# New database about authors gender and nationality
db_authors <- read.csv(file = "Data/db_authors.csv", header = TRUE, sep = "\t", as.is = TRUE)

# Database with a randomly sample number of eponyms dedicated to scientist and non scientist
db_epo <- read.csv(file = "Data/sampled_eponyms.csv", header = TRUE, sep = "\t", as.is = TRUE)

# Cleaning names ----------------------------------------------------------

# Trim white spaces if needed
db_names$author  <- gsub("\\s+", " ", db_names$author)  # Replace multiple spaces with a single space
db_names$author  <- gsub("\\s+$", "", db_names$author)  # Remove trailing space at the end of the string

db_authors$Name  <- gsub("\\s+", " ", db_authors$Name)  # Replace multiple spaces with a single space
db_authors$Name  <- gsub("\\s+$", "", db_authors$Name)  # Remove trailing space at the end of the string

db_authors$Country  <- gsub("\\s+", " ", db_authors$Country)  # Replace multiple spaces with a single space
db_authors$Country  <- gsub("\\s+$", "", db_authors$Country)  # Remove trailing space at the end of the string

# db_authors: Separate the first name and subset
db_authors <- db_authors |>
  dplyr::mutate(first_name = sub(",.*| &.*", "", Name)) 

# create an unique ID based on country and sex to identify multi-name authors
db_authors$ID <- paste(db_authors$first_name,db_authors$Sex,db_authors$Country, sep = "_")

# Calculate proportion of etymologies by author ---------------------------

# db_name: Separate the first name and select relevant columns
db_names <- db_names |>
  dplyr::mutate(first_name = sub(",.*| &.*", "", author)) |>
  dplyr::select(author, 
                first_name,
                year,
                size,
                shape,
                colour,
                behaviour,
                ecology,
                geography,
                scientists,
                otherPeople,
                modernCulture,
                pastCulture,
                others)

ncol_db_names <- ncol(db_names)

db_authors <- db_authors |>
  dplyr::mutate_if(is.character, as.factor)
 
for(i in 1 : nlevels(db_authors$ID)){
  
  db_i <- db_authors[db_authors$ID == levels(db_authors$ID)[i],]

  db_names_i <- db_names[db_names$author %in% as.character(db_i$Name),]
  
  colSums_i     <- colSums(db_names_i[, 4 : ncol_db_names])
  rowSum_i      <- sum(colSums_i)
  year_min_i    <- min(db_names_i$year, na.rm = TRUE)
  year_max_i    <- max(db_names_i$year, na.rm = TRUE)
  year_range_i  <- year_max_i-year_min_i
  
  #Store
      if(i > 1) {
        db_analysis2 <- c(name = as.character(db_i$first_name)[1], 
                          ID   = as.character(db_i$ID)[1], 
                          sex = as.character(db_i$Sex)[1], 
                          country = as.character(db_i$Country)[1], 
                          continent = as.character(db_i$Continent)[1], 
                          colSums_i, 
                          tot = rowSum_i, 
                          year_min = year_min_i, 
                          year_max = year_max_i, 
                          year_range = year_range_i)
        
        db_analysis  <- rbind(db_analysis, db_analysis2)
        
      } else {
        db_analysis <- c(name = as.character(db_i$first_name)[1], 
                         ID   = as.character(db_i$ID)[1], 
                         sex = as.character(db_i$Sex)[1], 
                         country = as.character(db_i$Country)[1], 
                         continent = as.character(db_i$Continent)[1], 
                         colSums_i, 
                         tot = rowSum_i, 
                         year_min = year_min_i, 
                         year_max = year_max_i, 
                         year_range = year_range_i)
      }
} 

# Set the final database
db_analysis <- data.frame(db_analysis)
db_analysis[,6:ncol(db_analysis)] <- apply(db_analysis[,6:ncol(db_analysis)],2,as.numeric)
rownames(db_analysis) <- NULL

db_analysis <- db_analysis |> 
  dplyr::mutate_if(is.character, as.factor)

head(db_analysis,5)

# double-check very longeve authors ---------------------------------------

db_check <- db_analysis[db_analysis$year_range > 50,] ; db_check <- droplevels(db_check)

par(mfrow = c(5,6), mar = c(rep(2,4)))
for(i in 1 : nlevels(db_check$ID)){ 
  
  db_i <- db_names[db_names$author %in% db_authors[db_authors$ID == levels(db_check$ID)[i],]$Name,]
  dotchart(db_i$year, main = db_i$first_name[1])

}

#after checking WSC: Butler, Costa, Lucas, Edwards, González, Hirst, Machado, Mcheidze, Miller, Müller, Rossi, Saito, Schmidt, Smith, Wang
db_check <- db_check[db_check$name %in% c("Butler",
                                          "Costa",
                                          "Edwards", 
                                          "González", 
                                          "Hirst",
                                          "Lucas",
                                          "Machado", 
                                          "Mcheidze", 
                                          "Miller", 
                                          "Müller", 
                                          "Rossi", 
                                          "Saito", 
                                          "Schmidt", 
                                          "Smith", 
                                          "Wang"),]
db_check <- droplevels(db_check)

dev.off()
par(mfrow = c(4,4), mar = c(rep(2,4)))
for(i in 1 : nlevels(db_check$ID)){ 
  
  db_i <- db_names[db_names$author %in% db_authors[db_authors$ID == levels(db_check$ID)[i],]$Name,]
  dotchart(db_i$year, main = db_i$first_name[1])
  
}

year_split <- c(1900,1950,1990,1980,1960,1900,2000,1990,1990,1900,1810,1960,1940,1960,1970)

db_check <- data.frame(ID = db_check$ID, 
                       Name = db_check$name, 
                       year_split, 
                       sex = db_check$sex,
                       country = db_check$country,
                       continent = db_check$continent)

# Correct the names that are multiple authors
db_analysis <- db_analysis[!db_analysis$ID %in% db_check$ID,] #remove the uncorrect assignment from the analysis database

for(i in 1 : nlevels(db_check$ID)){
  
  db_i <- db_check[db_check$ID == levels(db_check$ID)[i],]
  
  db_names_i <- db_names[db_names$author %in% as.character(db_i$Name),]
  
  db_names_before <- db_names_i[db_names_i$year < db_i$year_split, ]
  db_names_after  <- db_names_i[db_names_i$year > db_i$year_split, ]
  
  
  colSums <- rbind(colSums(db_names_before[, 4 : ncol_db_names]), colSums(db_names_after[, 4 : ncol_db_names]))
  rowSum  <- rowSums(colSums)
  min     <-  c(min(db_names_before$year, na.rm = TRUE), min(db_names_after$year, na.rm = TRUE))
  max     <-  c(min(db_names_before$year, na.rm = TRUE), min(db_names_after$year, na.rm = TRUE))
  
  db_analysis_i <- cbind(name = paste(db_names_i$first_name[1:2], 1:2),
                         ID   = paste(rep(db_i$ID,2), 1:2),
                         sex  = as.character(db_i$sex)[1], 
                         country = as.character(db_i$country)[1], 
                         continent = as.character(db_i$continent)[1],
                         colSums, 
                         tot = rowSum,
                         year_min = min, 
                         year_max = max,
                         year_range = max - min)
  
  db_analysis <- rbind(db_analysis, db_analysis_i)
 
} 

### clean the workspace
all_objects <- ls() ; keep_objects <- c("db_analysis", "db_names", "db_authors", "db_epo")
rm(list = setdiff(all_objects, keep_objects)) ; rm(all_objects, keep_objects)

# Set and check the final database --------------------------------------------------

db_analysis <- db_analysis |> 
  dplyr::mutate_if(is.character, as.numeric)

tail(db_analysis, 30)

str(db_analysis)

# percentage female coauthors

nrow(db_analysis[db_analysis$sex == "F",])/nrow(db_analysis)

# Group etymologies by Type (see Mammola et al., 2023 ZJLS)
db_analysis <- db_analysis |>
  dplyr::mutate(
    morphology = size + shape + colour,  
    ecology2 = behaviour + ecology,
    people = scientists + otherPeople,
    culture = modernCulture + pastCulture + others)

# Remove etymologies for which we didn't find a meaning
db_analysis <- db_analysis[db_analysis$tot > 0,] #removed 5 observations

# Remove missing genders and rename
db_analysis <- db_analysis[db_analysis$sex != "?",] #removed 12 observations

db_analysis <- droplevels(db_analysis)

db_analysis <- db_analysis |>
        dplyr::mutate(gender = dplyr::recode(sex, F = "Female", M = "Male"))

# General plots & stats ------------------------------------------------------

# average ± s.d. number of species description per author
mean(db_analysis$tot, na.rm = TRUE) ; sd(db_analysis$tot, na.rm = TRUE)
range(db_analysis$tot, na.rm = TRUE)

# N authors by Continent
table(db_analysis$continent)

# N authors by Gender
table(db_analysis$gender)

##### plot N authors by continent & gender
(plot_1 <- 
    table(db_analysis$continent, db_analysis$gender) |> 
    data.frame() |> 
    dplyr::mutate(continent = forcats::fct_relevel(Var1, c("Europe", "Asia", "N America", "S America", "Oceania", "Africa"))) |>
    dplyr::mutate(gender = dplyr::recode(Var2, F = "Female", M = "Male")) |>
    ggplot2::ggplot(aes(y = Freq, x = continent, fill = gender))+
    geom_bar(stat="identity", colour = "grey5", size = .4)+
    labs(x = NULL, 
         y = "Number of authors")+
    scale_fill_manual("",values = c("purple","grey20"))+
    #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    theme(legend.position = c(0.8, 0.7))
)
#####

db_analysis_temporal <- db_names |> 
                    dplyr::select(author, year, scientists, otherPeople) |>
                    dplyr::left_join(db_authors |> 
                                           dplyr::rename("author" = "Name") |>
                                           dplyr::select(author, Sex, Continent),
                                     by = "author",
                                     relationship = "many-to-many") |> 
                    dplyr::mutate(continent = forcats::fct_relevel(Continent, 
                                                                   c("Asia", "Africa", "Europe", "N America", "S America", "Oceania"))) |>
                    dplyr::mutate(gender = dplyr::recode(Sex, F = "Female", M = "Male")) |>
                    dplyr::select(author,gender,continent,year, scientists, otherPeople) |> 
                    dplyr::mutate(people = scientists + otherPeople) |> 
                    na.omit()

# Remove missing genders and NA
db_analysis_temporal <- db_analysis_temporal[db_analysis_temporal$gender != "?",]
db_analysis_temporal <- droplevels(db_analysis_temporal)

sum(db_analysis_temporal$people) #how many

(plot_2 <- 
table(db_analysis_temporal$year,db_analysis_temporal$gender,db_analysis_temporal$continent) |>
  data.frame() |>
  dplyr::rename("year" = "Var1","gender" = "Var2", "instances" = "Freq") |>
  dplyr::mutate(year = as.numeric(as.character(year))) |>
  ggplot2::ggplot( aes(x = year, y = instances, color = gender, group = gender)) +
  facet_wrap( ~ Var3, nrow = 3, ncol = 3)+ 
  geom_line(size = .5)+
  scale_x_continuous(breaks = seq(from=1757,to=2010,by=60))+
  scale_color_manual("",values = c("purple","grey20"))+
  labs(x = NULL, 
       y = "Number of species descriptions")
)


# Regression analysis -----------------------------------------------------
db_analysis <- na.omit(db_analysis)

#People
m1 <- glm(cbind(people,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = binomial(link = "logit"), data = db_analysis)

performance::check_overdispersion(m1)
performance::check_collinearity(m1)
summary(aov(m1))
summary(m1)

(plot_trend1 <- 
    ggplot2::ggplot(db_analysis, aes(x = year_min, y = people/tot, fill = gender, color = gender, size = year_range)) + 
    #facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
    geom_point(alpha = 0.6, shape = 21) +
    geom_smooth(se = TRUE, 
                method = "glm", 
                formula = y ~ x,
                method.args = list(family = binomial(link = "logit")), size = 1, alpha = 0.2) +
    scale_color_manual("",values = c("purple","grey20"))+
    scale_fill_manual("",values = c("purple","grey20"))+
    scale_size("Years of\nactivity", breaks = c(1,20,40,60), labels = c(1,20,40,60))+
    labs(x = "Year of first species description", 
         y = "Proportion of etymologies dedicated to people")
)

#Scientists
m2 <- glm(cbind(scientists,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = binomial(link = "logit"), data = db_analysis)

performance::check_overdispersion(m2)
performance::check_collinearity(m2)
summary(aov(m2))
summary(m2)

(plot_trend2 <- 
    ggplot2::ggplot(db_analysis, aes(x = year_min, y = scientists/tot, fill = gender, color = gender, size = year_range)) + 
    #facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
    geom_point(alpha = 0.6, shape = 21) +
    geom_smooth(se = TRUE, 
                method = "glm", 
                formula = y ~ x,
                method.args = list(family = binomial(link = "logit")), size = 1, alpha = 0.2) +
    scale_color_manual("",values = c("purple","grey20"))+
    scale_fill_manual("",values = c("purple","grey20"))+
    scale_size("Years of\nactivity", breaks = c(1,20,40,60), labels = c(1,20,40,60))+
    labs(x = "Year of first species description", 
         y = "Proportion of etymologies dedicated to scientists")
)

#non-scientists
m3 <- glm(cbind(otherPeople,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = binomial(link = "logit"), data = db_analysis)

performance::check_overdispersion(m3)

m3bis <- glm(cbind(otherPeople,tot) ~ gender + scale(year_min) + scale(year_range) + continent,
          family = quasibinomial(link = "logit"), data = db_analysis)

performance::check_collinearity(m3bis)
summary(aov(m3bis))
summary(m3bis)

(plot_trend3 <- 
    ggplot2::ggplot(db_analysis, aes(x = year_min, y = otherPeople/tot, fill = gender, color = gender, size = year_range)) + 
    #facet_wrap( ~ continent, nrow = 3, ncol = 3, scale = "free")+
    geom_point(alpha = 0.6, shape = 21) +
    geom_smooth(se = TRUE, 
                method = "glm", 
                formula = y ~ x,
                method.args = list(family = quasibinomial(link = "logit")), size = 1, alpha = 0.2) +
    scale_color_manual("",values = c("purple","grey20"))+
    scale_fill_manual("",values = c("purple","grey20"))+
    scale_size("Years of\nactivity", breaks = c(1,20,40,60), labels = c(1,20,40,60))+
    labs(x = "Year of first species description", 
         y = "Proportion of etymologies dedicated to non-scientists")
)


# Analysis on eponyms -----------------------------------------------------

db_epo <- db_epo |> 
  tidyr::separate_rows(Gender, sep = ";") |>
  dplyr::mutate_if(is.character, as.factor)  # Splits rows by ";" when a couple of people are in the dedication

db_epo <- db_epo[db_epo$Gender != "",] ; db_epo <- droplevels(db_epo)

#Checking proportion of etymologies in the two groups

(ratio_NonScientist <- table(db_epo$Gender,db_epo$Type)[1]/table(db_epo$Gender,db_epo$Type)[2]) # ratio M / F nonScientist
(ratio_Scientist    <-table(db_epo$Gender,db_epo$Type)[3]/table(db_epo$Gender,db_epo$Type)[4]) # ratio M / F Scientist

# Is this significant? 

# We can test with a null model...
ratio_NonScientist_exp <- c()
ratio_Scientist_exp <- c()

for(i in 1 : 999){

  #randomize gender
  NonScientist_i <- sample(c("M", "F"), size = nrow(db_epo[db_epo$Type == "NonScientist",]), replace = TRUE, prob = c(0.5, 0.5))
  Scientist_i    <- sample(c("M", "F"), size = nrow(db_epo[db_epo$Type == "Scientist",]), replace = TRUE, prob = c(0.5, 0.5))
  
  
  #recalculate
  ratio_NonScientist_i <- table(NonScientist_i)[2] / table(NonScientist_i)[1]# ratio M / F nonScientist
  ratio_Scientist_i    <- table(Scientist_i)[2] / table(Scientist_i)[1] # ratio M / F Scientist
  
  
  #store
  ratio_NonScientist_exp <- append(ratio_NonScientist_exp,ratio_NonScientist_i)
  ratio_Scientist_exp    <- append(ratio_Scientist_exp,ratio_Scientist_i)
  
} 

(sig1 <- round(BAT::ses(obs = ratio_Scientist, est = ratio_Scientist_exp, param = TRUE, p = TRUE),3))

(sig2 <- round(BAT::ses(obs = ratio_NonScientist, est = ratio_NonScientist_exp, param = TRUE, p = TRUE),3))


(ratio1 <- data.frame(ratio_Scientist_exp) |>
    ggplot2::ggplot(aes(x = ratio_Scientist_exp))+
    labs(x = "Male/female ratio in eponyms dedicated to scientists", y = NULL)+
    geom_histogram(fill = "grey50", bins = 40)+
    geom_segment(aes(x = ratio_Scientist, xend = ratio_Scientist, y = 0, yend = 100), 
                 col = "grey10", linewidth = 1) +
    geom_point(aes(x = ratio_Scientist, y = 100), color = "grey10", size = 3)+
    annotate("text", x = 4, y = 300, 
             label = paste0("SES = ",sig1[1]," ; p < 0.001"), color = "grey10", 
             size = 4, hjust = 0.5))

(ratio2 <- data.frame(ratio_NonScientist_exp) |>
    ggplot2::ggplot(aes(x = ratio_NonScientist_exp))+
    labs(x = "Male/female ratio in eponyms dedicated to non-scientists", y = NULL)+
    geom_histogram(fill = "grey50", bins = 40)+
    geom_segment(aes(x = ratio_NonScientist, xend = ratio_NonScientist), y = 0, yend = 27)+
    geom_point(aes(x = ratio_NonScientist, y = 27), color = "grey10", size = 3)+
    annotate("text", x = 1.5, y = 80, 
             label = paste0("SES = ",sig2[1]," ; p < 0.001"), color = "grey10", 
             size = 4, hjust = 0.5))


# # Difference between the two
# 
# (observed_diff <- ratio_Scientist - ratio_NonScientist)
# 
# # Is this significant? 
# 
# # We can test with a null model...
# 
# db_epo_i <- db_epo
# expected_diff <- c()
# 
# for(i in 1 : 999){
#   #randomize gender
#   
#   db_epo_i$Gender <- sample(db_epo_i$Gender)
#     
#   #recalculate
#   (ratio_NonScientist_i <- table(db_epo_i$Gender,db_epo_i$Type)[1]/table(db_epo_i$Gender,db_epo$Type)[2]) # ratio M / F nonScientist
#   (ratio_Scientist_i <-table(db_epo_i$Gender,db_epo_i$Type)[3]/table(db_epo_i$Gender,db_epo$Type)[4]) # ratio M / F Scientist
# 
#   expected_diff_i <- ratio_Scientist_i - ratio_NonScientist_i
#   
#   #store
#   expected_diff <- append(expected_diff,expected_diff_i)
#   
# } 
# 
# #clean
# rm(db_epo_i,ratio_NonScientist_i,ratio_Scientist_i,expected_diff_i,i)
# 
# (sig3 <- round(BAT::ses(obs = observed_diff, est = expected_diff, param = TRUE, p = TRUE),3))
# 
# (ratio3 <- data.frame(expected_diff) |>
#   ggplot2::ggplot(aes(x = expected_diff))+
#   labs(x = "Difference in male/female ratio between scientist vs non-scientists eponyms", y = NULL)+
#   geom_histogram(fill = "grey50", bins = 40)+
#   geom_segment(aes(x = observed_diff, xend = observed_diff, y = 0, yend = 47), 
#                col = "grey10", linewidth = 1) +
#   geom_point(aes(x = observed_diff, y = 47), color = "grey10", size = 3)+
#   annotate("text", x = 2, y = 120, 
#            label = paste0("SES = ",sig3[1]," ; p < 0.001"), color = "grey10", 
#            size = 4, hjust = 0.5))

############################################################################

# Combining & saving figures

############################################################################

pdf(file = "Figures/Figure_1.pdf", width = 14, height = 7)

ggpubr::ggarrange(plot_1, plot_2, 
                  common.legend = TRUE,
                  hjust = 0,
                  align = "h",
                  labels = c("A", "B"),
                  ncol=2, nrow=1)


dev.off()

pdf(file = "Figures/Figure_2.pdf", width = 10, height = 10)

ggpubr::ggarrange(plot_trend2, plot_trend3,
                  ratio1, ratio2,
                  common.legend = TRUE,
                  hjust = 0,
                  align = "h",
                  labels = c("A", "B", "C", "D"),
                  ncol=2, nrow=2)

dev.off()


