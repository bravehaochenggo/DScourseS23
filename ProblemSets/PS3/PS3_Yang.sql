#!/bin/sh

file_url <- "http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip"

download.file(file_url, destfile = "FL_insurance_sample.csv.zip")

unzip("FL_insurance_sample.csv.zip")

library(sqldf)

df <- read.csv("FL_insurance_sample.csv")

head(df, 10)

sqldf("SELECT DISTINCT county FROM df")

sqldf("SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation FROM df")

total_count <- sqldf("SELECT COUNT(*) as count FROM df")

sqldf("SELECT construction, COUNT(construction) AS count, COUNT(construction)*100/SUM(COUNT(construction)) OVER () AS pct FROM df GROUP BY construction")
