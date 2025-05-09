# caricare i seguenti pacchetti
library(dplyr)      #utile per manipolare i dati.
library(qdapRegex)  #lavorare con espressioni regolari e manipolare stringhe.
library(readtext)   #leggere file di testo (se necessario).
library(stringr)    #Utilizzato per lavorare con stringhe, offre funzioni utili come str_count(), str_split(), etc.
library(tidyr)
library(purrr)
library(profvis)
library(jsonlite)
#ESTRAZIONE DEI TASK DALLA COLONNA ANNOTATIONS

#carica il df con gia estratte le info di metadata e subject_data ma con annotations ancora non estratta
load("C:/Users/elisa/Desktop/ReData/df_0.RData") 



extract_all_tasks_in_row <- function(annotations_string) { # La funzione ora accetta direttamente la stringa
  extracted_data <- list()
  
  if (is.character(annotations_string) && nchar(annotations_string) > 0) {
    parsed_annotations <- tryCatch({
      fromJSON(annotations_string)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {
      for (i in 1:nrow(parsed_annotations)) {
        top_level_task <- parsed_annotations[i, , drop = FALSE]
        top_level_task_id <- top_level_task$task
        
        if (!is.null(top_level_task_id)) {
          extracted_data[[top_level_task_id]] <- toJSON(top_level_task, auto_unbox = TRUE)
          
          # Check for nested tasks within 'value'
          if ("value" %in% names(top_level_task) && is.list(top_level_task$value[[1]])) {
            nested_value <- top_level_task$value[[1]]
            if (is.data.frame(nested_value)) {
              for (j in 1:nrow(nested_value)) {
                nested_task <- nested_value[j, , drop = FALSE]
                nested_task_id <- nested_task$task
                if (!is.null(nested_task_id)) {
                  extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id)]] <- toJSON(nested_task, auto_unbox = TRUE)
                }
              }
            }
          }
        }
      }
    }
  }
  return(extracted_data)
}

df_extracted_all <- head(df_0, 100) %>%
  rowwise() %>%
  mutate(all_extracted = list(extract_all_tasks_in_row(annotations))) %>% # Passiamo direttamente la colonna 'annotations'
  unnest_wider(all_extracted, names_sep = "_") %>%
  ungroup()





















separate_tasks_to_columns <- function(df) {
  df_extracted <- df %>%
    mutate(annotations_parsed = lapply(annotations, function(x) {
      tryCatch({
        fromJSON(x)
      }, error = function(e) {
        NULL
      })
    })) %>%
    mutate(all_tasks_json = lapply(annotations_parsed, function(parsed_annotations) {
      extracted_tasks_json <- list()
      if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {
        for (i in 1:nrow(parsed_annotations)) {
          task_data <- parsed_annotations[i, , drop = FALSE]
          task_id <- task_data$task
          task_json <- toJSON(task_data, auto_unbox = TRUE)
          if (!is.null(task_id)) {
            extracted_tasks_json[[task_id]] <- task_json
          }
        }
      }
      return(extracted_tasks_json)
    })) %>%
    unnest_wider(all_tasks_json, names_sep = "_", names_repair = "minimal") %>%
    select(-annotations_parsed)
  
  return(df_extracted)
}





find_nested_task_columns <- function(df) {
  nested_task_columns <- character(0)
  for (col_name in names(df)) {
    # Prova a parsare il primo valore non-NA della colonna come JSON
    first_json_value <- df %>%
      slice_head(n = 100) %>% # Considera le prime 100 righe per performance
      pull(col_name) %>%
      na.omit() %>%
      first()
    
    if (!is.null(first_json_value) && is.character(first_json_value)) {
      parsed_json <- tryCatch({
        fromJSON(first_json_value)
      }, error = function(e) {
        NULL
      })
      
      # Verifica se il JSON parsato ha una struttura che suggerisce task annidati
      if (is.data.frame(parsed_json) && "value" %in% names(parsed_json) && is.list(parsed_json$value) && length(parsed_json$value) > 0 && is.data.frame(parsed_json$value[[1]])) {
        if ("task" %in% names(parsed_json$value[[1]])) {
          nested_task_columns <- c(nested_task_columns, col_name)
        }
      }
    }
  }
  return(nested_task_columns)
}


unnest_nested_tasks_column <- function(df, col_to_unnest) {
  df_unnested <- df %>%
    rowwise() %>%
    mutate(nested_data = {
      task_json_string <- get(col_to_unnest)
      nested_extracted <- list()
      if (!is.na(task_json_string) && is.character(task_json_string) && nchar(task_json_string) > 0) {
        task_data_frame <- tryCatch({
          fromJSON(task_json_string)
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(task_data_frame) && is.data.frame(task_data_frame) && nrow(task_data_frame) > 0) {
          # Accediamo al data.frame nella lista 'value'
          nested_df <- task_data_frame$value[[1]]
          if (is.data.frame(nested_df)) {
            # Iteriamo sulle righe di questo data.frame per trovare i task annidati
            for (i in 1:nrow(nested_df)) {
              nested_task_id <- nested_df$task[i]
              nested_value <- nested_df$value[[i]]
              nested_task_json <- toJSON(list(task = nested_task_id, value = nested_value), auto_unbox = TRUE)
              nested_extracted[[paste0(col_to_unnest, "_nested_", nested_task_id)]] <- nested_task_json
            }
          }
        }
      }
      list(nested_extracted)
    }) %>%
    unnest_wider(nested_data, names_sep = "_") %>%
    ungroup()
  
  return(df_unnested)
}



df_separated_tasks <- separate_tasks_to_columns(head(df_0, 10000))
nested_task_columns <- find_nested_task_columns(df_separated_tasks)
print(nested_task_columns)



df_final <- df_separated_tasks
for (col in nested_task_columns) {
  df_final <- unnest_nested_tasks_column(df_final, col)
}

save(df_final, file = "C:/Users/elisa/Desktop/ReData/df_final_json.RData")



















#########################################################################################


separate_tasks_with_auto_nesting <- function(df) {
  df_extracted <- df %>%
    mutate(annotations_parsed = lapply(annotations, function(x) {
      tryCatch({
        fromJSON(x)
      }, error = function(e) {
        NULL
      })
    })) %>%
    mutate(all_tasks_json = lapply(annotations_parsed, function(parsed_annotations) {
      extracted_tasks_json <- list()
      if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {
        for (i in 1:nrow(parsed_annotations)) {
          task_data <- parsed_annotations[i, , drop = FALSE]
          task_id <- task_data$task
          task_json <- toJSON(task_data, auto_unbox = TRUE)
          if (!is.null(task_id)) {
            extracted_tasks_json[[task_id]] <- task_json
          }
        }
      }
      return(extracted_tasks_json)
    })) %>%
    unnest_wider(all_tasks_json, names_sep = "_", names_repair = "minimal") %>%
    select(-annotations_parsed)
  
  # Funzione per spacchettare i task annidati in una colonna
  unnest_column_auto <- function(df_row, col_name) {
    nested_extracted <- list()
    task_json_string <- df_row[[col_name]]
    if (is.character(task_json_string) && nchar(task_json_string) > 0) {
      task_data_frame <- tryCatch({
        fromJSON(task_json_string)
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(task_data_frame) && is.data.frame(task_data_frame) && nrow(task_data_frame) > 0) {
        if ("value" %in% names(task_data_frame) && is.list(task_data_frame$value[[1]])) {
          nested_value <- task_data_frame$value[[1]]
          for (nested_item in nested_value) {
            if (is.data.frame(nested_item) && "task" %in% names(nested_item)) {
              nested_task_id <- nested_item$task
              nested_task_json <- toJSON(nested_item, auto_unbox = TRUE)
              nested_extracted[[paste0(col_name, "_nested_", nested_task_id)]] <- nested_task_json
            }
          }
        }
      }
    }
    return(nested_extracted)
  }
  
  # Identifichiamo le colonne che potrebbero contenere task annidati (per nome)
  nested_cols_to_unnest <- intersect(names(df_extracted), c("T7", "T10", "T19")) # Aggiungi altri se necessario
  
  # Applichiamo l'unnesting
  df_unnested <- df_extracted %>%
    rowwise() %>%
    mutate(nested_data = list(c(sapply(nested_cols_to_unnest, function(col) unnest_column_auto(., col))))) %>%
    unnest_wider(nested_data, names_sep = "_") %>%
    ungroup()
  
  return(df_unnested)
}

df_separated_auto_nesting <- separate_tasks_with_auto_nesting(head(df_0, 100))




######################################################################################################à

separate_tasks_to_columns <- function(df) {
  df_extracted <- df %>%
    mutate(annotations_parsed = lapply(annotations, function(x) {
      tryCatch({
        fromJSON(x)
      }, error = function(e) {
        NULL
      })
    })) %>%
    mutate(all_tasks_json = lapply(annotations_parsed, function(parsed_annotations) {
      extracted_tasks_json <- list()
      if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {
        for (i in 1:nrow(parsed_annotations)) {
          task_data <- parsed_annotations[i, , drop = FALSE]
          task_id <- task_data$task
          task_json <- toJSON(task_data, auto_unbox = TRUE)
          if (!is.null(task_id)) {
            extracted_tasks_json[[task_id]] <- task_json
          }
        }
      }
      return(extracted_tasks_json)
    })) %>%
    unnest_wider(all_tasks_json, names_sep = "_", names_repair = "minimal") %>%
    select(-annotations_parsed)
  
  return(df_extracted)
}

df_separated_tasks <- separate_tasks_to_columns(head(df_0, 100)) # Esempio con le prime 100 righe
head(df_separated_tasks)












#######################################################################à

extract_entire_tasks <- function(annotations_string) {
  extracted_tasks_json <- list()
  
  if (is.character(annotations_string) && nchar(annotations_string) > 0) {
    annotation_data_frame <- tryCatch({
      fromJSON(annotations_string)
    }, error = function(e) {
      # print(paste("Errore fromJSON:", e))
      NULL
    })
    
    if (!is.null(annotation_data_frame) && is.data.frame(annotation_data_frame)) {
      for (i in 1:nrow(annotation_data_frame)) {
        task_data <- annotation_data_frame[i, , drop = FALSE] # Ottieni la riga come data.frame
        task_id <- task_data$task
        task_json <- toJSON(task_data, auto_unbox = TRUE) # Converti la riga in JSON
        
        if (!is.null(task_id)) {
          extracted_tasks_json[[task_id]] <- task_json
        }
      }
    }
  }
  
  return(extracted_tasks_json)
}

df_extracted_entire_tasks <- head(df_0, 100) %>%
  mutate(entire_tasks_json = lapply(annotations, extract_entire_tasks)) %>%
  unnest_wider(entire_tasks_json, names_sep = "_")

head(df_extracted_entire_tasks)









#####################################################################################################à



extract_top_level_task_presence <- function(row) {
  annotation_data_frame <- tryCatch({
    fromJSON(row$annotations)
  }, error = function(e) {
    NULL
  })
  
  extracted_tasks <- list()
  
  if (!is.null(annotation_data_frame) && is.data.frame(annotation_data_frame)) {
    for (i in 1:nrow(annotation_data_frame)) {
      task_id <- annotation_data_frame$task[i]
      if (!is.null(task_id)) {
        extracted_tasks[[task_id]] <- TRUE # Indica la presenza del task
      }
    }
  }
  
  return(extracted_tasks)
}

df_extracted_task_presence <- head(df_0, 100) %>%
  mutate(annotations_parsed = lapply(annotations, fromJSON),
         present_top_level_tasks = lapply(annotations_parsed, extract_top_level_task_presence)) %>%
  unnest_wider(present_top_level_tasks, names_sep = "_") %>%
  select(-annotations_parsed)






######################################################################################################################à
df_extracted <- tail(df_0, 100) %>%
  mutate(annotations_parsed = lapply(annotations, fromJSON),
         task_values = lapply(annotations_parsed, function(x) {
           task_value_list <- list()
           for (i in 1:nrow(x)) {
             task_id <- x$task[i]
             value <- x$value[[i]]
             if (is.list(value) || is.data.frame(value)) {
               if (length(value) == 1) {
                 task_value_list[[task_id]] <- value[[1]]
               } else {
                 task_value_list[[task_id]] <- toString(unlist(value))
               }
             } else {
               task_value_list[[task_id]] <- value
             }
           }
           return(task_value_list)
         })) %>%
  unnest_wider(task_values, names_sep = "_") %>%
  select( -annotations_parsed) # Rimuovi solo le colonne originali e intermedie


df_extracted <- df_0 %>%
  mutate(annotations_parsed = lapply(annotations, fromJSON),
         task_values = lapply(annotations_parsed, function(x) {
           task_value_list <- list()
           for (i in 1:nrow(x)) {
             task_id <- x$task[i]
             value <- x$value[[i]]
             if (is.list(value) || is.data.frame(value)) {
               if (length(value) == 1) {
                 task_value_list[[task_id]] <- value[[1]]
               } else {
                 task_value_list[[task_id]] <- toString(unlist(value))
               }
             } else {
               task_value_list[[task_id]] <- value
             }
           }
           return(task_value_list)
         })) %>%
  unnest_wider(task_values, names_sep = "_") %>%
  select( -annotations_parsed) # Rimuovi solo le colonne originali e intermedie



head(df_extracted)


df_test <- head(df_0, 100) %>%
  mutate(annotations_parsed = lapply(annotations, fromJSON),
         task_values = lapply(annotations_parsed, extract_data_from_json))

# Visualizza la struttura della prima lista nella colonna task_values
str(df_test$task_values[[1]])

# Visualizza alcune righe della colonna task_values
head(df_test$task_values)

df_extracted <- df_test %>%
  unnest_wider(task_values, names_sep = "_", names_repair = "minimal") %>%
  select(-annotations_parsed)

head(df_extracted)


##########################################################################################
library(dplyr)
library(jsonlite)

extract_label_t11_13 <- function(row) {
  annotation_data_frame <- tryCatch({
    fromJSON(row$annotations)
  }, error = function(e) {
    NULL
  })
  
  extracted_info <- list()
  
  if (!is.null(annotation_data_frame) && is.data.frame(annotation_data_frame)) {
    for (i in 1:nrow(annotation_data_frame)) {
      task_id <- annotation_data_frame$task[i]
      value <- annotation_data_frame$value[[i]]
      
      if (task_id %in% c("T11", "T12", "T13") && is.list(value) && length(value) == 1 && is.data.frame(value[[1]])) {
        # Accediamo al data.frame annidato (value[[1]]) e cerchiamo la colonna 'label'
        if ("label" %in% names(value[[1]])) {
          extracted_info[[task_id]] <- value[[1]]$label
        }
      }
    }
  }
  
  return(extracted_info)
}

df_extracted_label <- head(df_0, 100) %>%
  mutate(annotations_parsed = lapply(annotations, fromJSON),
         label_t11_13 = lapply(annotations_parsed, extract_label_t11_13)) %>%
  unnest_wider(label_t11_13, names_sep = "_") %>%
  select(-annotations_parsed)

head(df_extracted_label)


df_extracted <- head(df_0, 100) %>%
  mutate(annotations_parsed = lapply(annotations, fromJSON),
         task_values = lapply(annotations_parsed, extract_data_from_json)) %>%
  unnest_wider(task_values, names_sep = "_") %>%
  select(-annotations_parsed)

head(df_extracted)

df_test_parsed <- head(df_0, 100) %>%
  mutate(annotations_parsed = lapply(annotations, fromJSON))

# Esamina la struttura del JSON parsato per la prima riga
str(df_test_parsed$annotations_parsed[[1]])

###################################################################################################################







extract_data_from_json <- function(row) {
  annotation_data_frame <- tryCatch({
    fromJSON(row$annotations)
  }, error = function(e) {
    NULL
  })
  
  extracted_info <- list()
  
  if (!is.null(annotation_data_frame) && is.data.frame(annotation_data_frame)) {
    for (i in 1:nrow(annotation_data_frame)) {
      task_id <- annotation_data_frame$task[i]
      value <- annotation_data_frame$value[[i]] # 'value' è una lista di 1 elemento
      task_label <- annotation_data_frame$task_label[i] # Potresti volerla estrarre
      
      # Gestione dei casi in cui 'value' è un data.frame annidato
      if (is.data.frame(value)) {
        # Esempio di estrazione del 'value' da un data.frame annidato
        if ("value" %in% names(value)) {
          if (is.list(value$value) && length(value$value) == 1) {
            extracted_info[[task_id]] <- value$value[[1]]
          } else {
            extracted_info[[task_id]] <- value$value
          }
        } else {
          extracted_info[[task_id]] <- value
        }
      } else {
        extracted_info[[task_id]] <- value
      }
    }
  }
  
  return(extracted_info)
}


df_prime_100 <- head(df_0, 100)

df_extracted_prime_100 <- df_prime_100 %>%
  mutate(extracted_json = map(1:nrow(.), ~ extract_data_from_json(.[.x, ]))) %>%
  unnest_wider(extracted_json, names_sep = "_") %>%
  select(-annotations)

head(df_extracted_prime_100) # Visualizza il risultato



prima_riga <- df_prime_100[1, ]
risultato_funzione <- extract_data_from_json(prima_riga)
print(risultato_funzione)




singola_stringa_json <- head(df_prime_100$annotations, 1)[[1]] # Prendi la prima stringa JSON
oggetto_r <- fromJSON(singola_stringa_json)
str(oggetto_r) # Mostra la struttura dell'oggetto R


df_prime_100 <- head(df_0, 100)

df_extracted_prime_100 <- df_prime_100 %>%
  rowwise() %>%
  mutate(extracted_json = list(extract_data_from_json(cur_data()))) %>%
  unnest_wider(extracted_json, names_sep = "_")



df_extracted <- df_0 %>%
  rowwise()%>%
  mutate(extracted_json = list(extract_data_from_json(cur_data()))) %>%
  unnest_wider(extracted_json, names_sep = "_") 
  







###########################################################      PROVA     ##############################################à

