library(magrittr)

toStandardData <- function(data, id, time, text, label){
  data %>%
    dplyr::rename(id = id,
                  time = time,
                  text = text,
                  label = label) %>%
    dplyr::select(id, time, text, label)
}

toDtm <- function(data){
  data %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::anti_join(tidytext::stop_words) %>%
    dplyr::filter(!stringr::str_detect(word, '\\d')) %>%
    dplyr::count(id, word) %>%
    tidytext::cast_dtm(id, word, n)
}

getLda <- function(data, k, seed = 4321){
  dtm <- toDtm(data)
  lda <- topicmodels::LDA(dtm, k = k, control = list(seed = seed))
  return(lda)
}

getTopics <- function(lda, append){
  topics <- lda %>%
    tidytext::tidy(matrix = 'beta') %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(20, beta) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic, -beta)
  
  names(topics)[1] <- paste0(names(topics)[1], '_', append)
  
  return(topics)
}

markDocuments <- function(lda, topic_count){
  marked <- lda %>%
    tidytext::tidy(matrix = 'gamma') %>%
    dplyr::group_by(document) %>%
    dplyr::top_n(1, wt = gamma) %>%
    dplyr::ungroup() %>%
    dplyr::rename(id = document) 
  # %>%
  #   dplyr::mutate(topic_count) 
  
#  names(marked) <- paste0(names(marked), c('', '_', "_"), c('', topic_count, topic_count))
  output = list()
  output[[paste0('_', topic_count)]] <- marked
  return(output)
}

combineDocumentsWMarked <- function(data, marked){
  marked <- purrr::imap(marked, function(dta, name){
    names(dta) <- paste0(names(dta), c('', name, name))
    dta
  })

  purrr::reduce(marked, .init = data, .f = function(a, b){
    dplyr::left_join(a, b, by = 'id')
  })
}

getDocUmap <- function(dtm){
  maxPca <- ifelse(ncol(dtm) > 100, 100, ncol(dtm))
  pca <- prcomp(as.matrix(dtm), scale. = T)
  pcaUmap <- umap::umap(pca$x[,1:maxPca])
}

getLayout <- function(umap, marked){
  umapLayout <- tibble::tibble(
    id = rownames(umap$layout),
    x = umap$layout[,1],
    y = umap$layout[,2]
  )
  print(umapLayout)
  print(marked)
  umapLayout <- combineDocumentsWMarked(umapLayout, marked)
  
  return(umapLayout)
}

createTree <- function(cDta){
  topics <- names(cDta)[stringr::str_detect(names(cDta), '^topic')]
  nTopics <- as.numeric(stringr::str_remove(topics, 'topic_'))
  topics <- paste0("topic_", sort(nTopics))
  
  cDta <- cDta[c('id', 'time', topics)]
  cDta <- dplyr::bind_rows(list(
    cDta,
    dplyr::mutate(cDta, time = 'All')
  ))
  
  cDta <- purrr::map(1:length(topics), function(d){
    print(c('time', topics[1:d]))
    dta <- cDta %>%
      dplyr::group_by_at(c('time', topics[1:d])) %>%
      dplyr::count(name = 'value') %>%
      dplyr::ungroup() %>%
      dplyr::rename(name = topics[d]) %>%
      tidyr::spread(time, value, fill = 0)
    
    if(d != 1){
      dta <- dta  %>%
        dplyr::group_by_at(topics[1]) %>%
        tidyr::nest() %>%
        dplyr::rename(name = topics[1],
                      children = data)
    }
    
    dta
  })
  
  # cDta <- purrr::reduce(length(topics):1, .init = cDta, function(a, b){
  #   print(b)
  #   if(b == length(topics)){
  #     finalChild <- topics[b]  
  # 
  #     a %>%
  #       dplyr::group_by_at(topics[1:b]) %>%
  #       dplyr::count(name = 'value') %>%
  #       dplyr::ungroup()
  #   } else {
  #     print(a)
  #     a %>%
  #       dplyr::group_by_at(topics[1:b]) %>%
  #       dplyr::rename(topic = topics[b]) %>%
  #       tidyr::nest() %>%
  #       dplyr::mutate(data = purrr::map(data, function(d){
  #         names(d)[1] <- 'name'
  #         d
  #       })) %>%
  #       dplyr::rename(children = data)
  #   }
  # })
  # 
  # cDta = tibble::tibble(
  #   name = 'All Topics',
  #   children = list(cDta)
  # )

  return(cDta)
}

tDta2 <- createTree(cDta)
###
sDta <- toStandardData(dta, 'file', 'month', 'text', 'label')
tDtm <- toDtm(sDta)
lda5 <- getLda(sDta, 5)
lda6 <- getLda(sDta, 6)
lda30 <- getLda(sDta, 30)
lda50 <- getLda(sDta, 50)
topics5 <- getTopics(lda5, 5)
topics6 <- getTopics(lda6, 6)
topics30 <- getTopics(lda30, 30)
topics50 <- getTopics(lda50, 50)

mark5 <- markDocuments(lda5, 5)
mark50 <- markDocuments(lda50, 50)
mark6 <- markDocuments(lda6, 6)
mark30 <- markDocuments(lda30, 30)

cDta630 <- combineDocumentsWMarked(sDta, c(mark30, mark6))
cDta <- combineDocumentsWMarked(sDta, c(mark50, mark5))
tDta630 <- createTree(cDta630)
tDta <- createTree(cDta)
tDta[[2]]
write(paste0('treeData = ', jsonlite::toJSON(tDta2)), '../data/test_tree3.js')
write(paste0('treeData = ', jsonlite::toJSON(tDta630)), '../data/test_tree4.js')
  
#dtmUmap630 <- getDocUmap(tDtm)
dtmUmap <- getDocUmap(tDtm)
# dtmPca <- prcomp(as.matrix(tDtm), scal)
# dtmUmap <- umap::umap(dtmPca$x[,1:100])
umapLayout <- getLayout(dtmUmap, list(mark5, mark50))
umapLayout630 <- getLayout(dtmUmap, list(mark6, mark30))

umapLayout_cheat <- umapLayout %>%
  dplyr::left_join(dplyr::select(cDta, id, time)) %>%
  dplyr::mutate(class = paste0('topic-', topic_5, 
                               ' topic-', topic_5, '-', topic_50, 
                               ' ', time, '-topic-', topic_5, 
                               ' ', time, '-topic-', topic_5, '-', topic_50)) %>%
  dplyr::select(id, x, y, time, class, topic = topic_5)
  
write(paste0('umapData = ', jsonlite::toJSON(umapLayout_cheat)), '../data/test_umap.js')

docText <- cDta %>% 
  dplyr::select(id, time, text, topic_5, topic_50) 

write(paste0('documentData = ', jsonlite:::toJSON(docText)), '../data/test_documents.js')

topics5 %>%
  dplyr::group_by(topic_5)%>%
  dplyr::top_n(1) %>%
  {
    paste0('topic5 = ', jsonlite::toJSON(.))
  } %>%
  write('../data/test_topic_5.js')

topics50 %>%
  dplyr::group_by(topic_50)%>%
  dplyr::top_n(1) %>%
  {
    paste0('topic50 = ', jsonlite::toJSON(.))
  } %>%
  write('../data/test_topic_50.js')
