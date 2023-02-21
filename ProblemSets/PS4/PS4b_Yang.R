library(tidyverse)
library(sparklyr)
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")
df1 <- as_tibble(iris)
df <- copy_to(sc, df1)
class(df1)
class(df)
#df1 is a "tbl_df", "tbl" and "data.frame";
#df is a "tbl_spark" "tbl_sql"   "tbl_lazy"  "tbl"; 
head(df, 10)
head(df1, 10)
df %>% select(Sepal_Length,Species) %>% head %>% print
df %>% filter(Sepal_Length>5.5) %>% head %>% print
df %>% 
  select(Sepal_Length, Species) %>% 
  filter(Sepal_Length > 5.5) %>% 
  head(6) %>% 
  print()
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length),count = n()) %>% head %>% print
df2 %>% arrange(Species) %>% head %>% print
#arrange did not work
