# Statistical tests, classification and machine learning based on the lagoon data
source("helper_functions.R")

library(tidymodels)

laguStat <- st_read("C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/A9_tuloksia/Laguuni_MHAlueella.shp")


lagus <- laguStat %>%
  select(Shap_Ar, krtLjLk, RuopYht, LaitYht, rknnstM, ulksrst, ruovikt, MHtaso4Koo) %>%
  st_drop_geometry() %>%
  filter(!is.na(MHtaso4Koo))

colSums(is.na(lagus)) 
colSums(is.infinite(lagus))

lagus <- lagus %>% 
  select(Shap_Ar, RuopYht, LaitYht, rknnstM, ulksrst, ruovikt) 

tidy(kclustLag)
glance(kclustLag)

kclusts <- tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(lagus, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, lagus)
  )

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

p1 <- 
  ggplot(assignments, aes(x = rknnstM + RuopYht + LaitYht, y = Shap_Ar)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1 + geom_point(data = clusters, size = 10, shape = "x")

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()


set.seed(27)

centers <- tibble(
  cluster = factor(1:3), 
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(5, 0, -3),              # x1 coordinate of cluster center
  x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)

labelled_points <- 
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points) %>% 
  unnest(cols = c(x1, x2))

ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.3)

points <- labelled_points %>% select(-cluster)

kclust <- kmeans(points, centers = 3)
kclust
