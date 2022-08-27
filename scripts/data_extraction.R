# This script retrieves html data from the web and merges it into a csv file.
# It is not necessary to run this script more than once.

library("tidyverse")
library("rvest")
library("data.table")

data_list <- list()
for (i in 1:10) {
    url <- paste0(
        "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",
        i,
        ".html"
    )
    print(paste0("Retrieving page ", i, "..."))
    html <- read_html(url)
    print(paste0("Retrieved page ", i, "! Now converting chunk to tibble"))
    data_chunk <- html %>%
        html_elements(xpath = "//table") %>%
        html_table()
    print("Converted!")
    data_list[i] <- data_chunk
}

data <- rbindlist(data_list)

write.csv(data, "../stores/data.csv", row.names = FALSE)
