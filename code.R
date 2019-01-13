#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                Ramiro Ramos Jerez - Data Driven Securty                   #
#                                                                              #
#******************************************************************************#


# Install Dependencies, if needed: httr
if (!suppressMessages(suppressWarnings(require("httr", quietly = T)))) {
  suppressMessages(suppressWarnings(install.packages("httr", repos = "http://cran.rstudio.com/", quiet = T, dependencies = T)))
}
library(httr)


# Install Dependencies, if needed: xml2
if (!suppressMessages(suppressWarnings(require("xml2", quietly = T)))) {
  suppressMessages(suppressWarnings(install.packages("xml2", repos = "http://cran.rstudio.com/", quiet = T, dependencies = T)))
}
library(xml2)

# Install Dependencies, if needed: stringr
if (!suppressMessages(suppressWarnings(require("stringr", quietly = T)))) {
  suppressMessages(suppressWarnings(install.packages("stringr", repos = "http://cran.rstudio.com/", quiet = T, dependencies = T)))
}
library(stringr)



## Additional Functions
ExtractDomain <- function(my_url){
  domain <- strsplit(gsub("http://|https://|www\\.", "", my_url), "/")[[c(1, 1)]]
  return(domain)
}

## Crawling y Scrapping


### 1.1 Obtención de la página web

WebCrawler <- function(url) {
  r <- GET(url = url)

  html_all <- read_html(content(r, "text"))
  html_head <- xml2::xml_find_all(html_all, "./head")
  html_body  <- xml2::xml_find_all(html_all, "./body")

  return(list("response_code" = r$status_code,
              "response_headers" = r$header,
              "html_head" = html_head,
              "html_body" = html_body
              )
        )
}



### 1.2 Analisis de el contenido de la web

IdentifyWebTitle <- function(html_head){

  title <-  xml2::xml_find_all(html_head, "//head/title")
  title_text <- xml_text(title)

  return(title_text)
}

### 1.3.	Extracción de enlaces

ExtractLinks <- function(html_body) {

  #xml2::xml_find_all(html_body, "//*/a/@href")

  links <- xml2::xml_find_all(html_body, "//*/a/@href/parent::a")

  links_urls <- xml_attr(links, "href")
  links_text <- xml_text(links)

  df_links <- cbind.data.frame(links_text, links_urls, stringsAsFactors=FALSE)

  # return data frame
  return(df_links)
}

### 1.4 Exploración de enlaces

ParseLinks <- function(url, df_links) {

  mydomain <- ExtractDomain(url)

  num_items <- dim(df_links)[1]

  for(i in 1:num_items) {

    # Check if url is ABSOLUTE OR RELATIVE
    if ( str_detect(df_links$links_urls[i], "^//") ) {
      # Add URL as "ABSOLUTE"
      df_links$links_urls_type[i] <- "ABSOLUTE"
      df_links$links_absolute_urls[i] <- paste0("https:",df_links$links_urls[i])
    }
    else if ( str_detect(df_links$links_urls[i], "^(?:[a-z]+:)?//") ) {
      # Add URL as "ABSOLUTE"
      #cat(i,(df_links$links_urls[i]),"\n")
      df_links$links_urls_type[i] <- "ABSOLUTE"
      df_links$links_absolute_urls[i] <- df_links$links_urls[i]
    }
    else {
      # Add Domain to URL and mark it as "RELATIVE"
      #cat(i,paste0(params$url,df_links$links_urls[i]),"\n")
      df_links$links_urls_type[i] <- "RELATIVE"
      df_links$links_absolute_urls[i] <- paste0(params$url,df_links$links_urls[i])
    }

    df_links$domain[i] <- ExtractDomain(df_links$links_absolute_urls[i])
    df_links$belongs_to_domain[i] <- str_detect(df_links$links_absolute_urls[i], mydomain)

  }


  # return data frame
  return(df_links)
}

Check_Responses <- function(df_links) {

  num_items <- dim(df_links)[1]

  for(i in 1:num_items) {
    # Get Response Code for each URL HEAD(url)
    tmphead <- HEAD(df_links$links_absolute_urls[i])
    df_links$links_response_code[i] <- tmphead$status

    Sys.sleep(time = 0.001)
  }

  # return data frame
  return(df_links)

}

### Gráficos en R

### 2.1 Histograma

### 2.2 Un gráfico de barras

### 2.3 Pie Chart


