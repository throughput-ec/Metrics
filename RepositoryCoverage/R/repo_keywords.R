repo_keywords <- function(con, type) {

  keywords <-  'MATCH (o:OBJECT)-[:isType]-(:TYPE {type: "schema:DataCatalog"})
    RETURN o.name, o.keywords'

  kwd_set <- call_neo4j(keywords, con)
  kdw_df <- data.frame(name = kwd_set$o.name, keywords = kwd_set$o.keywords)

  repos <- kwd_set$o.name  %>% unlist()

  repo_words <- kwd_set$o.keywords %>%
    unlist %>%
    strsplit(",") %>%
    purrr::map(function(x){
      x %>%
      stringr::str_replace_all("\\.", " ") %>%
      stringr::str_replace_all("(^\\s*)|(\\s*$)", "")
    })

  clean_words <- repo_words %>%
    unlist

  unique_clean <- unique(clean_words)

  repomat <- matrix(ncol = length(unique_clean),
                    nrow = length(repo_words),
                    data = 0)

  for(i in 1:length(repo_words)) {
    words <- match(repo_words[[i]], unique_clean)
    repomat[i, words] <- repomat[i, words] + 1
  }

  assertthat::assert_that(all(colSums(repomat) > 0),
    msg = "Some keywords defined are not being added.")

  centers <- 15

  key_kmeans <- kmeans(t(repomat), centers = centers)

  terms <- lapply(1:centers, function(x){
    terms <- key_kmeans$cluster == x
    words <- unique_clean[terms]
    repo_set <- repo_words %>%
      purrr::map(function(x) {any(x %in% words)}) %>%
      unlist()
    list(words,
         repos[repo_set])
  })

  # testss <- data.frame(ss = rep(NA, 33),
  #                      singles = rep(NA, 33),
  #                      mean = rep(NA, 33),
  #                      biggest = rep(NA, 33))
  #
  # for(i in seq(1, 33, by = 4)) {
  #   key_kmeans <- kmeans(t(repomat), centers = i)
  #
  #   tab <- table(key_kmeans$cluster)
  #
  #   testss$ss[i] <- key_kmeans$tot.withinss
  #   testss$singles[i] <- sum(tab == 1)
  #   testss$mean[i] <- mean(tab)
  #   testss$biggest[i] <- max(tab)
  #   cat(i, "\n")
  # }
}
