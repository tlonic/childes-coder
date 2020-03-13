library(shiny)
libraries_loaded <- all(require(shinyjs),
                        require(dplyr),
                        require(childesr),
                        require(DBI),
                        require(pool),
                        require(yaml))

config <- try(read_yaml("config.yaml"))

hotkeys <- '
shinyjs.init = function() {
    $(window).bind("keydown", function(event) {
        if (event.ctrlKey || event.metaKey) {
            switch (String.fromCharCode(event.which).toLowerCase()) {
                case "s":
                    event.preventDefault();
                    $("#save").click();
                    break;
                case "a":
                    event.preventDefault();
                    $("#prev").click();
                    break;	    
                case "d":
                    event.preventDefault();
                    $("#nxt").click();
                    break;
            }
        }
   });
};
'

  
childes_pool <- try(dbPool(
  drv = RSQLite::SQLite(), 
  dbname = "childes.sqlite"))

responses_pool <- try(dbPool(
  drv = RSQLite::SQLite(),
  dbname = "responses.sqlite"
  ))

onStop(function() {
  poolClose(childes_pool)
  poolClose(responses_pool)
})

search_tokens <- try(get_tokens(
      collection = config$search$collection,
      language = config$search$language,
      corpus = config$search$corpus,
      target_child = config$search$target_child,
      role = config$search$role,
      role_exclude = config$search$role_exclude,
      age = config$search$age,
      token = config$search$token,
      stem = config$search$stem,
      part_of_speech = config$search$part_of_speech,
      replace = !isFALSE(config$search$replace),
      connection = childes_pool
    ) %>% collect())

sorted_search_tokens <- try(search_tokens[order(search_tokens$id), ])


isError <- function(x) inherits(x, "try-error")

getSurroundingUtterances <-
  function(utterance_id,
           position = "before",
           window = 3,
           connection) {
    target_utterance <- connection %>%
      tbl("utterance") %>%
      filter(id == utterance_id) %>%
      collect()
    tid <- target_utterance$transcript_id
    order <- target_utterance$utterance_order
    
    if (position == "before") {
      lowerlim <- max(c(1, order - window))
      contexts <- connection %>%
        tbl("utterance") %>%
        filter(transcript_id == tid,
               utterance_order >= lowerlim,
               utterance_order < order) %>%
        collect()
    }
    else{
      contexts <- connection %>%
        tbl("utterance") %>%
        filter(transcript_id == tid,
               utterance_order <= (order + window),
               utterance_order > order) %>%
        collect()
    }
    
    return(contexts[order(contexts$utterance_order), ])
  }

getChildesLink <- function(utterance_id, connection) {
  target_utterance <- connection %>%
    tbl("utterance") %>%
    filter(id == utterance_id) %>%
    select(transcript_id) %>%
    collect()
  
  target_transcript <- connection %>%
    tbl("transcript") %>%
    filter(id == !!target_utterance$transcript_id) %>%
    select(filename, collection_name) %>%
    collect()
  target_transcript$filename <-
    tools::file_path_sans_ext(target_transcript$filename)
  
  url <-
    paste0(
      "https://childes.talkbank.org/browser/index.php?url=",
      target_transcript$collection_name,
      "/",
      target_transcript$filename,
      ".cha#"
    )
  
  return(url)
  
  
}

getUtterances <- function(utterance_ids, connection) {
  utterances <- connection %>%
    tbl('utterance') %>%
    filter(id %in% utterance_ids) %>%
    collect()
  return(utterances)
}

getTargetUtteranceTags <- function(utterance, token_order)  {
  split_gloss <- strsplit(utterance$gloss, ' ')[[1]]
  pre_token <- paste(split_gloss[0:(token_order - 1)], collapse = ' ')
  token <- split_gloss[token_order]
  post_token <- paste(split_gloss[-(0:token_order)], collapse = ' ')
  outTag <- tags$tr(class = 'target_utterance',
		    id = paste0("utterance_", utterance$id),
                    tags$td(
                      paste0(utterance$speaker_name,
                            " (", utterance$speaker_role, ", ",
                            utterance$speaker_code, "):")
                    ),
                    tags$td(pre_token, strong(token), post_token))
  return(outTag)
}

getContextUtteranceTags <- function(utterances) {
  if (nrow(utterances) > 0) {
    format <- function(utterance) {
      outTag <- tags$tr(class = 'context_utterance',
			id = paste0("utterance_",utterance[["id"]]),
                        tags$td(
                          paste0(
                            utterance[["speaker_name"]],
                            " (",
                            utterance[["speaker_role"]],
                            ", ",
                            utterance[["speaker_code"]],
                            "):"
                          )
                        ),
                        tags$td(utterance[["gloss"]]))
    }
    return(tagList(apply(utterances, 1, format)))
  }
  else{
    return(NULL)
  }
}


getFirstToken <- function(enterer, connection) {
  tokens <- connection %>%
    tbl("responses") %>%
    filter(enterer == !!enterer) %>%
    select("token_id") %>%
    collect()
  remainder <- setdiff(sorted_search_tokens$id, tokens$token_id)
  if (length(remainder) > 0) {
    return(head(remainder, 1))
  }
  else{
    return(head(sorted_search_tokens$id, 1))
  }
}

saveData <- function(data, connection) {
  #delete row if it exists
  sql <-
    "DELETE FROM responses WHERE token_id = ?tid AND enterer = ?enterer;"
  delete_query <-
    sqlInterpolate(connection,
                   sql,
                   tid = data$token_id,
                   enterer = data$enterer)
  insert_query <-
    sqlAppendTable(connection, "responses", data, row.names = FALSE)
    poolWithTransaction(connection, function(conn) {
      dbExecute(conn, delete_query)
      dbExecute(conn, insert_query)
    })
}

getData <- function(tid, enterer, connection) {
  responses <- connection %>%
    tbl("responses") %>%
    filter(enterer == !!enterer,
           token_id == tid) %>%
    collect()
  return(responses)
}

getResponseTable <- function(childesConn, responsesConn) {
  tokens <- sorted_search_tokens[unique(c(config$token_columns, "id"))]
  
  responses <- responsesConn %>%
    tbl("responses") %>%
    collect()
  
  utterances <-
    getUtterances(responses[["utterance_id"]], childesConn)
  
  utterances <- utterances[unique(c(config$utterance_columns, "id"))]
  
  responses <-
    inner_join(
      responses,
      tokens,
      by = c("token_id" = "id"),
      copy = TRUE,
      suffix = c(".r", ".t")
    )
  
  responses <-
    inner_join(
      responses,
      utterances,
      by = c("utterance_id" = "id"),
      copy = TRUE,
      suffix = c(".u", ".t")
    )
  
  return(responses)
}

getTokenTable <- function(childesConn) {
  tokens <- sorted_search_tokens[unique(c(config$token_columns, "utterance_id"))]
  
  colnames(tokens)[colnames(tokens) == "id"] <- "token_id"
  
  utterances <- getUtterances(tokens[["utterance_id"]], childesConn)
  
  utterances <- utterances[unique(c(config$utterance_columns, "id"))]
  
  tokens <-
    inner_join(
      tokens,
      utterances,
      by = c("utterance_id" = "id"),
      copy = TRUE,
      suffix = c(".t", ".u")
    )
  
  return(tokens)
}


