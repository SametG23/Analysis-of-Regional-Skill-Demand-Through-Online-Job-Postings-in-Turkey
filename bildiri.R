install.packages("tidyverse")
install.packages("readxl")
install.packages("tidytext")
install.packages("tm")
install.packages("cluster")
install.packages("caret")
install.packages("randomForest")
install.packages("ggplot2")
install.packages(c("purrr", "rlang", "factoextra"))
install.packages("janitor")

Sys.setlocale("LC_ALL", "Turkish")

library(janitor)
library(purrr)
library(rlang)
# Veri i??leme
library(tidyverse)
library(readxl)

# Metin madencili??i
library(tidytext)
library(tm)

# K??meleme
library(cluster)
library(factoextra)
# ML
library(caret)
library(randomForest)

# G??rselle??tirme
library(ggplot2)
veri <- read_excel("C:/Econometrics/DataR/turkiye_online_is_ilanlari.xlsx")
veri <- veri %>%
  mutate(
    bolge = ifelse(il == "??stanbul", "??stanbul", "Anadolu")
  )
beceri_df <- veri %>%
  select(ilan_id, il, bolge, maas_tl, beceriler) %>%
  unnest_tokens(
    beceri,
    beceriler,
    token = "regex",
    pattern = ", "
  )
beceri_siklik <- beceri_df %>%
  count(bolge, beceri, sort = TRUE)

beceri_siklik %>%
  group_by(bolge) %>%
  slice_max(n, n = 10)

tfidf <- beceri_df %>%
  count(bolge, beceri) %>%
  bind_tf_idf(beceri, bolge, n) %>%
  arrange(desc(tf_idf))

tfidf %>%
  group_by(bolge) %>%
  slice_max(tf_idf, n = 8)

il_beceri <- beceri_df %>%
  count(il, beceri) %>%
  pivot_wider(
    names_from = beceri,
    values_from = n,
    values_fill = 0
  )

kume_veri <- il_beceri %>%
  column_to_rownames("il") %>%
  scale()

fviz_nbclust(kume_veri, kmeans, method = "wss")


set.seed(123)
kmeans_model <- kmeans(kume_veri, centers = 3, nstart = 25)

kume_sonuc <- data.frame(
  il = rownames(kume_veri),
  kume = factor(kmeans_model$cluster)
)

fviz_cluster(
  kmeans_model,
  data = kume_veri,
  geom = "point",
  ggtheme = theme_minimal()
)

ml_df <- beceri_df %>%
  count(ilan_id, beceri) %>%
  pivot_wider(
    names_from = beceri,
    values_from = n,
    values_fill = 0
  ) %>%
  left_join(
    veri %>% select(ilan_id, maas_tl),
    by = "ilan_id"
  )

set.seed(123)

train_index <- createDataPartition(ml_df$maas_tl, p = 0.7, list = FALSE)

train <- ml_df[train_index, ]
test  <- ml_df[-train_index, ]


train <- train %>% clean_names()
test  <- test  %>% clean_names()


rf_model <- randomForest(
  maas_tl ~ .,
  data = train,
  importance = TRUE
)

pred <- predict(rf_model, test)

postResample(pred, test$maas_tl)
varImpPlot(rf_model)
write_csv(kume_sonuc, "il_bazli_beceri_kumeleri.csv")







