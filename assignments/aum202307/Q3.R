library(ggplot2)
library(reshape2)
library(easyPubMed)
library(sqldf)

# Part A (Partially done ...)
get_data <- function(query) {
  ids <- get_pubmed_ids(query)
  abstracts <- fetch_pubmed_data(ids, format = "xml")
  years <- custom_grep(abstracts, "Year")
  df1 <- data.frame(years = unlist(years))
  df <- sqldf("select years, count(years) as npubs from df1 group by 1")
  df$id <- query
  df
}

get_publication_count <- function(query_vec, years, output_fname) {
  df_list <- lapply(query_vec, get_data)
  combined_df <- do.call(rbind, df_list)
  desired_years <- data.frame(years=years)
  final_df <- sqldf("select a.* from 
                    combined_df a inner join desired_years b on b.years = a.years")
  write.csv(final_df, file=output_fname, row.names = FALSE)
}

query_vec <- c("Damiano Fantini[AU]", "Kenealy W[AU]")
get_publication_count(query_vec, c("2018", "2019", "2020", "2021", "2022", "2023"), "data.csv")
# Part B

data <- read.csv("publications.csv")
data_long <- melt(data, id.vars = c("Year"))
ggplot(data_long, aes(x = Year, y = value)) + geom_line() + 
  facet_wrap(~variable)
