library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# lettura del CSV scaricato da Zooniverse separato dalle virgole, ci mette una decina di secondi
df <- read.csv("C:/Users/elisa/Desktop/ReData/redata-classifications.csv")  #qui tb è diviso in 14 colonne (separate dalle virgole) e ogni riga è una digitalizzazione di una città di un certo anno

# Creiamo e salviamo una copia del dataframe iniziale. 
df_0 <- df
save(df_0, file = "C:/Users/elisa/Desktop/ReData/df_0.RData")

#load("C:/Users/elisa/Desktop/ReData/df_0.RData")

extract_all_tasks_in_row <- function(annotations_string) { # funzione che prende come argomento un elemento in JSON (elemento della colonna annotations)
  extracted_data <- list() #inizializza una lista vuota che memorizzerà le coppie chiave_valore come una hash table
  
  if (is.character(annotations_string) && nchar(annotations_string) > 0) {  # condizione che verifica che l'elemento sia una stringa valida
    parsed_annotations <- tryCatch({  #struttura di gestione degli errori
      fromJSON(annotations_string)    # tenta di convertire il JSON in un oggetto R
    }, error = function(e) {
      NULL                  #Se la conversione fallisce (ad esempio, se annotations_string non è un JSON valido), viene catturato l'errore, e parsed_annotations viene impostato a NULL.
    })
    
    if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {    #verifica che parsed_annotations non sia NULL e che sia un dataframe
      for (i in 1:nrow(parsed_annotations)) {                      #itera su ogni riga del dataframe parsed_annotations. Ogni riga in parsed_annotations rappresenta un task di "primo livello". Nei dati Zooniverse, un task di primo livello può contenere risposte dirette dell'utente o contenere altri task "annidati".
        top_level_task <- parsed_annotations[i, , drop = FALSE]          #Estrae la riga corrente del dataframe parsed_annotations come un nuovo dataframe. L'argomento drop = FALSE assicura che il risultato sia sempre un dataframe, anche se contiene una sola riga.
        top_level_task_id <- top_level_task$task                      #estrae il valore della colonna "task" dal dataframe top_level_task. Questo valore è l'ID del task (es., "T0", "T1", "T7").
        
        if (!is.null(top_level_task_id)) {                        #Verifica che l'ID del task non sia NULL.
          # Estrai il valore del task di primo livello se presente
          if ("value" %in% names(top_level_task)) {                                    #Se il task ha una colonna "value", il suo valore viene estratto e memorizzato in extracted_data con una chiave che include l'ID del task e "_value" (es., "T0_value").
            extracted_data[[paste0(top_level_task_id, "_value")]] <- top_level_task$value
          } else if ("task_label" %in% names(top_level_task)) {                      #Altrimenti, se il task ha una colonna "task_label", il suo valore viene estratto e memorizzato in extracted_data con una chiave che include l'ID del task e "_label" (es., "T1_label").
            extracted_data[[paste0(top_level_task_id, "_label")]] <- top_level_task$task_label
          } else {
            extracted_data[[top_level_task_id]] <- toJSON(top_level_task, auto_unbox = TRUE)        #Se il task non ha né "value" né "task_label", l'intero dataframe del task viene convertito in una stringa JSON e memorizzato in extracted_data con l'ID del task come chiave. L'argomento auto_unbox = TRUE semplifica la struttura del JSON risultante.
          }
          
          
          # Gestisci i task annidati all'interno di 'value'
          if ("value" %in% names(top_level_task) && is.list(top_level_task$value[[1]])) {      #Questa condizione verifica se il task di primo livello ha una colonna "value" e se il primo elemento di "value" è una lista. Allora è un task che contiene altri task annidati
            nested_value <- top_level_task$value[[1]]                                      #Estrae il primo elemento della lista "value". Ci aspettiamo che sia un dataframe contenente i task annidati.
            
            if (is.data.frame(nested_value)) {                                #Verifica che nested_value sia effettivamente un dataframe.
              for (j in 1:nrow(nested_value)) {                            #itera su ogni riga di questo dataframe, estraendo i dati di ciascun task annidato.
                nested_task <- nested_value[j, , drop = FALSE]            #Estrae la riga corrente del dataframe nested_value
                nested_task_id <- nested_task$task                        #estrae l'ID del task annidato
                
                if (!is.null(nested_task_id)) {
                  # Gestione specifica per task annidati con 'value' come lista di oggetti con 'label'
                  if ("value" %in% names(nested_task) && is.list(nested_task$value) && length(nested_task$value) > 0 && "label" %in% names(nested_task$value[[1]])) {  #verifica se il task annidato ha un campo "value" che è una lista, e se il primo elemento di quella lista ha un campo "label". Se è così, estrae il valore di "label".
                    extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id, "_label")]] <- nested_task$value[[1]]$label
                  } else if ("value" %in% names(nested_task)) {
                    extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id, "_value")]] <- nested_task$value
                  }
                  else if ("select_label" %in% names(nested_task)) {
                    extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id, "_label")]] <- nested_task$select_label
                  } else {
                    extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id)]] <- toJSON(nested_task, auto_unbox = TRUE)
                  }
                }
              }
            }
          }
          # # Gestione specifica per T11, T12, T13 (task di primo livello con 'value' come lista di oggetti con 'label') ci sono solo all'inizio poi vengono inscatolati anche loro in un altro task
          if (top_level_task_id %in% c("T11", "T12", "T13") && "value" %in% names(top_level_task) && is.list(top_level_task$value) && length(top_level_task$value) > 0 && "label" %in% names(top_level_task$value[[1]])) {
            extracted_data[[paste0(top_level_task_id, "_label")]] <- top_level_task$value[[1]]$label
            
          }
        }
      }
    }
  }
  return(extracted_data)
}



#inizio a estrarre started_at e classification_count e filename
tempo_inizio <- Sys.time()
df_1 <- df_0%>%
  mutate(started_at = map_chr(metadata, function(x) {
    tryCatch({
      fromJSON(x)$started_at
    }, error = function(e) {
      NA_character_ # Restituisce NA se la conversione JSON fallisce o 'started_at' non esiste
    })
  })) %>%
  
  mutate(
    classification_count = map_int(subject_data, function(x) {
      tryCatch({
        parsed_json <- fromJSON(x)
        # Estraiamo il primo (e unico) valore della lista esterna
        subject_info <- parsed_json[[1]]
        # Accediamo a 'retired' e poi a 'classifications_count'
        if (!is.null(subject_info$retired)) {
          subject_info$retired$classifications_count
        } else {
          NA_integer_
        }
      }, error = function(e) {
        NA_integer_
      })
    }),
    filename = map_chr(subject_data, function(x) {
      tryCatch({
        fromJSON(x)[[1]]$Filename
      }, error = function(e) {
        NA_character_
      })
    })
  )
tempo_fine <- Sys.time()
cat("Elaborazione completata in:", format(tempo_fine - tempo_inizio ), "\n")

# qui estraggo i task con la funzione 

tempo_inizio <- Sys.time()
df_extracted_grezzo <- df_0 %>%
  rowwise() %>%
  mutate(task = list(extract_all_tasks_in_row(annotations))) %>%
  unnest_wider(task, names_sep = "_") %>%
  ungroup()
tempo_fine <- Sys.time()
cat("Elaborazione completata in:", format(tempo_fine - tempo_inizio ), "\n")

save(df_extracted_grezzo, file = "C:/Users/elisa/Desktop/ReData/df_extracted_grezzo.RData")


# tempo_inizio <- Sys.time()
# df_extracted_100 <- head(df_0,100) %>%
#   rowwise() %>%
#   mutate(task = list(extract_all_tasks_in_row(annotations))) %>%
#   unnest_wider(task, names_sep = "_") %>%
#   ungroup()
# tempo_fine <- Sys.time()
# cat("Elaborazione completata in:", format(tempo_fine - tempo_inizio ), "\n")

nomi_colonne <- names(df_extracted_grezzo)
print(nomi_colonne)

df_extracted_pulito <- df_extracted_grezzo %>%
  select(-c(task_T7_value,
            task_T10_value,
            task_T10_nested_T9_value,
            task_T11_value,
            task_T12_value,
            task_T13_value,
            task_T7_nested_T5_value,
            task_T17_value,
            task_T18_value,
            task_T19_value,
            task_T5_value,
            task_T20_value,
            task_T21_value))
save(df_extracted_pulito, file = "C:/Users/elisa/Desktop/ReData/df_extracted_pulito.RData")

#indice_prima_non_na <- which.min(is.na(df_extracted_final[["task_T18_nested_T1_value"]]))
