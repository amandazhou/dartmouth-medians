##GOVT 16 Data Visaulization
##DV Project #3
##Amanda Zhou
##February 20, 2018

library(tidyverse)
library(stringr)
library(ggrepel)


# Useful Functions --------------------------------------------------------

convert_grades <- function(x) {
  A <- factor(x, levels=c("A", "A/A-", "A /A-", "A-", "A-/B+",
                          "B+", "B+/B", "B", "B/B-", "B /B-", "B-",
                          "B-/C+", "C+", "C"))
  values <- c(4, 3.83, 3.83, 3.66, 3.495, 
              3.33, 3.165, 3, 2.83, 2.83 ,2.66,
              2.496, 2.33, 2.00)
  values[A]
}

#Reading in the 15X to 17X dataset and merging with 17F dataset

df <- as.data.frame(read.csv("Project3_AZhou/data/Dartmouth.Medians.15X17X.csv")) %>% 
  select(-c(R, P, Collection.Time)) %>% 
  union(read.csv("Project3_AZhou/data/Dartmouth.Medians.17F.csv")) %>% 
  separate(Course, into = c("Department", "Course.Number", "Section"), sep = "-")

#Cleaning the department labels
DepartmentLabels <- c("AAAS", "AMEL", "AMES", "ANTH", "AMEL", "ARTH", "PHYS/ASTR", "BIOL", "CHEM", "AMEL",
                      "CLST/LAT/GRK", "COCO", "COGS", "COLT", "COSC", "EARS", "ECON", "EDUC", "ENGL", "ENGS", 
                      "ENVS", "FILM", "FREN/ITAL", "FREN/ITAL", "GEOG", "GERM", "GOVT", "AMEL", "HIST", "HUM", 
                      "HUM", "INTS", "FREN/ITAL", "AMEL", "JWST", "LALACS", "CLST/LAT/GRK", "CLST/LAT/GRK", "LALACS", "LING",
                      "QSS", "MATH", "MUS", "MUS", "NAS", "NAS", "PBPL", "PHIL", "PHYS/ASTR", "SPAN", 
                      "PSYC", "COSC", "QSS", "QSS", "REL", "REL", "RUSS", "SART", "SOCY", "SPAN", 
                      "SPEE/WRIT", "THEA", "TUCK", "WGSS", "SPEE/WRIT")

STEM <- c("PHYS/ASTR", "BIOL", "CHEM", "EARS", "ENGS", "MATH", "COSC")

SocialSciences <- c("ANTH", "COGS", "ECON", "EDUC", "ENVS", "GEOG", "GOVT", 
                    "LING", "QSS", "PBPL", "PSYC", "SOCY")


df1 <- df %>%
  select(-c(Source.Url,Url)) %>%
  mutate(Course.Number = as.numeric(Course.Number),
         Section = as.numeric(Section),
         Median.Number = convert_grades(Median),
         Department = factor(Department))

levels(df1$Department) <- DepartmentLabels

#Data wrangling for visualization

Average.Median <- df1 %>%
  group_by(Department) %>%
  summarise(average.median = mean(Median.Number),
            med.median = median(Median.Number),
            average.enrollment = mean(Enrl),
            med.enrollment = median(Enrl),
            totalclasses = n()/2.5)%>% 
  ungroup() %>% 
  mutate(DepartmentType = ifelse(Department %in% STEM, "STEM",
                                 ifelse(Department %in% SocialSciences, "Social Sciences", "Humanities")),
         DepartmentLabel = ifelse(totalclasses > 25, as.character(Department), ""))



# Graphing Median Median Grade against Enrollment -------------------------


#PNG VERSION
ggplot(data = Average.Median, 
       aes(y = average.median, x = med.enrollment),
       alpha = 0.7) + 
  geom_hline(yintercept = 3 + (1/3), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3 + (2/3), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 4, alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, alpha = 0.5, linetype = "dashed") +
  geom_point(aes(size = totalclasses,
                 color = DepartmentType)) +
  geom_label_repel(aes(x = med.enrollment,
                      y = average.median,
                      label = DepartmentLabel),
                  # point.padding = 0.5,
                  # min.segment.length = 5,
                  force = 5,
                  box.padding = .8,
                  segment.alpha = .5) +
  theme_bw() +
  scale_x_continuous(limits = c(0,50)) +
  scale_size_continuous(range = c(1,10)) +
  scale_y_continuous(breaks = seq(3,4,(1/3)), limits = c(3,4), labels = c("B", "B+", "A-", "A")) +
  scale_color_manual(values = c("darkred", "darkgreen", "darkblue")) +
  labs(title = "Count your classmates. The median grade varies across studies", 
       subtitle = "Dartmouth academic departments by class enrollment, grades and annual number of classes offered",
       size = "Number of Classes\nOffered Annually",
       color = "Department Type",
       x  = "Median Class Enrollment",
       y = "Average Median Grade") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "left", ncol = 1))
  

# PDF VERSION (Moved legend)
ggplot(data = Average.Median, 
       aes(y = average.median, x = med.enrollment),
       alpha = 0.7) + 
  geom_hline(yintercept = 3 + (1/3), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3 + (2/3), alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 4, alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, alpha = 0.5, linetype = "dashed") +
  geom_point(aes(size = totalclasses,
                 color = DepartmentType)) +
  geom_label_repel(aes(x = med.enrollment,
                       y = average.median,
                       label = DepartmentLabel),
                   # point.padding = 0.5,
                   # min.segment.length = 5,
                   force = 5,
                   box.padding = .8,
                   segment.alpha = .5) +
  theme_bw() +
  scale_x_continuous(limits = c(0,50)) +
  scale_size_continuous(range = c(1,10)) +
  scale_y_continuous(breaks = seq(3,4,(1/3)), limits = c(3,4), labels = c("B", "B+", "A-", "A")) +
  scale_color_manual(values = c("darkred", "darkgreen", "darkblue")) +
  labs(title = "Count your classmates. The median grade varies across studies", 
       subtitle = "Dartmouth academic departments by class enrollment, grades and annual number of classes offered",
       size = "Number of Classes\nOffered Annually",
       color = "Department Type",
       x  = "Median Class Enrollment",
       y = "Average Median Grade") +
  theme(legend.position = "right") +
  guides(color = guide_legend(title.position = "top", ncol = 1))

ggsave("Project3_AZhou/figures/Figure1.pdf",  width = 10, height = 7.5)

  
#no size

# ggplot(data = Average.Median, 
#        aes(y = average.median, x = med.enrollment),
#        alpha = 0.7) + 
#   geom_point(aes(color = DepartmentType),
#              size = 2) +
#   geom_label_repel(aes(x = med.enrollment,
#                        y = average.median,
#                        label = Department),
#                    # point.padding = 0.5,
#                    # min.segment.length = 5,
#                    force = 3,
#                    segment.alpha = .5) +
#   theme_bw() +
#   scale_x_continuous(limits = c(0,50)) +
#   scale_size_continuous(range = c(1,10)) +
#   scale_y_continuous(breaks = seq(3,4,.2), limits = c(3,4)) +
#   scale_color_manual(values = c("darkred", "darkgreen", "darkblue"))



# Does class size have a relationship on predicted median?  ---------------

# test <- lm(formula = Median.Number ~ Enrl, data = df1)
# summary(test)
# ggplot(data = df1, aes(x = df1$Enrl, y = df1$Median.Number)) + geom_jitter()




