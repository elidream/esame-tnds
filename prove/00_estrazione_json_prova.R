


library(readr)
library(jsonlite)
library(dplyr)
library(tidyr)

extract_specific_tasks <- function(annotations_string) {
  extracted_data <- list()
  
  if (is.character(annotations_string) && nchar(annotations_string) > 0) {
    parsed_annotations <- tryCatch({
      fromJSON(annotations_string)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {
      # Funzione interna per estrarre i task specifici, gestendo l'annidamento
      extract_nested <- function(df) {
        for (i in 1:nrow(df)) {
          task_row <- df[i, , drop = FALSE]
          task_id <- task_row$task
          
          if (!is.null(task_id)) {
            # Task che prendono 'value'
            if (task_id %in% c("T1", "T2", "T3", "T4", "T6", "T8", "T14", "T15", "T16") && "value" %in% names(task_row)) {
              extracted_data[[task_id]] <- task_row$value
            }
            # Task che prendono 'label' da 'value' (nel primo elemento di una lista)
            else if (task_id %in% c("T11", "T12", "T13") && "value" %in% names(task_row) && is.list(task_row$value) && length(task_row$value) > 0 && "label" %in% names(task_row$value[[1]])) {
              extracted_data[[task_id]] <- task_row$value[[1]]$label
            }
            # Task che prendono 'label' direttamente o da 'select_label'
            else if (task_id %in% c("T5", "T9")) {
              if ("select_label" %in% names(task_row)) {
                extracted_data[[task_id]] <- task_row$select_label
              } else if ("label" %in% names(task_row)) {
                extracted_data[[task_id]] <- task_row$label
              }
            }
            # Gestione dell'annidamento
            else if ("value" %in% names(task_row) && is.list(task_row$value[[1]]) && is.data.frame(task_row$value[[1]])) {
              extract_nested(task_row$value[[1]])
            }
          }
        }
      }
      
      extract_nested(parsed_annotations)
    }
  }
  return(extracted_data)
}

tempo_inizio <- Sys.time()
df_extracted_specific <- head(df_tibble, 1000) %>%
  rowwise() %>%
  mutate(specific_tasks = list(extract_specific_tasks(annotations))) %>%
  unnest_wider(specific_tasks, names_sep = "_") %>%
  ungroup()
tempo_fine <- Sys.time()
cat("Elaborazione completata in:", format(tempo_fine - tempo_inizio ), "\n")

nomi_colonne_specific <- names(df_extracted_specific)
print(nomi_colonne_specific)

# Ora T5 e T9 dovrebbero contenere le label estratte.



############################################################################################ESTRAZIONE JSON
library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)
library(furrr)
library(parallel)
library(future)

# lettura del CSV scaricato da Zooniverse separato dalle virgole, ci mette una decina di secondi
df <- read.csv("C:/Users/elisa/Desktop/ReData/redata-classifications.csv")  #qui df è diviso in 14 colonne (separate dalle virgole) e ogni riga è una digitalizzazione di una città di un certo anno

# Creiamo e salviamo una copia del dataframe iniziale. 
df_0 <- df
save(df_0, file = "C:/Users/elisa/Desktop/ReData/df_0.RData")

load("C:/Users/elisa/Desktop/ReData/df_0.RData")

extract_all_tasks_in_row <- function(annotations_string) { #funzione prende come input un singolo argomento chiamato annotations_string, che ci aspettiamo sia una stringa contenente un oggetto JSON.
  extracted_data <- list()                                  #lista vuota che verrà utilizzata per memorizzare i dati estratti dai task principali e dai task annidati presenti nella stringa JSON. Le chiavi di questa lista saranno gli ID dei task (es. "T0", "T7_nested_T5") e i valori saranno le stringhe JSON che rappresentano l'intero oggetto del task.
  contained_nested <- FALSE # Flag per indicare se la riga conteneva task annidati
  
  if (is.character(annotations_string) && nchar(annotations_string) > 0) {    #condizione che verifica se annotations_string è effettivamente una stringa e se la sua lunghezza è maggiore di zero (cioè, non è una stringa vuota). Questo controllo serve a evitare errori se la colonna annotations contiene valori non validi.
    parsed_annotations <- tryCatch({                                          #tentiamo di convertire la stringa JSON annotations_string in un oggetto R utilizzando la funzione fromJSON()
      fromJSON(annotations_string)                                            #tryCatch() è una struttura di gestione degli errori. Se la conversione JSON ha successo, il risultato (che ci aspettiamo sia un data.frame) viene assegnato a parsed_annotations.
    }, error = function(e) {
      NULL                                                                   #Se si verifica un errore durante la conversione JSON (ad esempio, se la stringa non è un JSON valido), la funzione error = function(e) { NULL } fa sì che parsed_annotations venga impostato su NULL invece di interrompere l'esecuzione del codice.
    })
    
    if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {  #verifica se parsed_annotations non è NULL (cioè, la conversione JSON è andata a buon fine) e se è un data.frame
      for (i in 1:nrow(parsed_annotations)) {                                 #itera su ogni riga del data.frame parsed_annotations. Ogni riga rappresenta un task principale.
        top_level_task <- parsed_annotations[i, , drop = FALSE]               #In ogni iterazione del ciclo, estraiamo la riga corrente del data.frame parsed_annotations e la assegniamo a top_level_task
        top_level_task_id <- top_level_task$task                              #Estraiamo il valore della colonna task dalla riga corrente (top_level_task). Questo valore è l'ID del task principale (es. "T0", "T7").
        
        if (!is.null(top_level_task_id)) {                                                  #Verifichiamo che l'ID del task principale non sia NULL.
          extracted_data[[top_level_task_id]] <- toJSON(top_level_task, auto_unbox = TRUE)   #Se l'ID del task principale è valido, convertiamo l'intero data.frame top_level_task in una stringa JSON utilizzando toJSON() e la memorizziamo nella lista extracted_data con l'ID del task come chiave. L'argomento auto_unbox = TRUE serve a semplificare la struttura del JSON in alcuni casi 
          
          # Check for nested tasks within 'value'
          if ("value" %in% names(top_level_task) && is.list(top_level_task$value[[1]])) {    #verifica se il task principale ha una colonna chiamata "value" e se il primo elemento di questa colonna è una lista (per i task annidati)
            nested_value <- top_level_task$value[[1]]                                       #assegniamo il primo elemento della lista contenuta nella colonna "value" a nested_value. Ci aspettiamo che questo sia un data.frame contenente i task annidati.
            if (is.data.frame(nested_value)) {                                             #Verifichiamo se nested_value è effettivamente un data.frame.
              contained_nested <- TRUE # Imposta il flag se troviamo task annidati
              for (j in 1:nrow(nested_value)) {                                           #Se nested_value è un data.frame, iteriamo su ogni riga di questo data.frame, che rappresenta un task annidato.
                nested_task <- nested_value[j, , drop = FALSE]                           #Estraiamo la riga corrente del data.frame nested_value (il task annidato).
                nested_task_id <- nested_task$task                                      #Estraiamo la riga corrente del data.frame nested_value (il task annidato).
                if (!is.null(nested_task_id)) {                                        #Verifichiamo che l'ID del task annidato non sia NULL.
                  extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id)]] <- toJSON(nested_task, auto_unbox = TRUE)     #Convertiamo l'intero data.frame nested_task (il task annidato) in una stringa JSON e lo memorizziamo nella lista extracted_data. La chiave in questo caso è una combinazione dell'ID del task principale, la stringa "_nested_" e l'ID del task annidato (es. "T7_nested_T5").
                }
              }
            }
          }
        }
      }
    }
  }
  attr(extracted_data, "contained_nested") <- contained_nested   #attributo a questa lista chiamato "contained_nested" con il valore del flag (se è annidata o no)
  return(extracted_data)  #restituiamo la lista extracted_data contenente le stringhe JSON di tutti i task (principali e annidati) estratti dalla stringa JSON di input.
}











tempo_inizio <- Sys.time()
df_extracted_all <- head(df_0, 1000) %>%
  rowwise() %>%
  mutate(all_extracted = list(extract_all_tasks_in_row(annotations))) %>%    # Passiamo direttamente la colonna 'annotations' e mutate() crea una nuova colonna e in ogni riga c'è la lista dei task
  unnest_wider(all_extracted, names_sep = "_") %>%                           # Prendiamo la colonna all_extracted, che contiene le liste di stringhe JSON, e la trasformiamo in più colonne. names_sep = "_" specifica che se ci sono nomi di colonne che entrano in conflitto, verranno gestiti aggiungendo un underscore e un numero sequenziale
  ungroup()                                                                  #Dopo aver completato l'operazione riga per riga, il dataframe torna alla sua forma normale (non raggruppato per riga)
tempo_fine <- Sys.time()
cat("Elaborazione completata in:", format(tempo_fine - tempo_inizio ), "\n") 


# Identifichiamo le colonne originali (annidate) da rimuovere (basandoci sulla presenza di "_nested_")
disannested_columns <- names(df_extracted_all)[grepl("_nested_", names(df_extracted_all))]         #trovare tutte le colonne i cui nomi contengono "_nested_". Queste sono le nuove colonne create dai task disannidati.
original_nested_columns <- unique(gsub("_nested_.*", "", disannested_columns))                     # unique() per ottenere elenco univoco dei nomi delle colonne originali che contenevano task annidati.

# Rimuoviamo le colonne originali annidate
df_final <- df_extracted_all %>%
  select(!any_of(original_nested_columns))


df_extracted_all_tail <- tail(df_0, 100) %>%
  rowwise() %>%
  mutate(all_extracted = list(extract_all_tasks_in_row(annotations))) %>%    # Passiamo direttamente la colonna 'annotations' e mutate() crea una nuova colonna e in ogni riga c'è la lista dei task
  unnest_wider(all_extracted, names_sep = "_") %>%                           # Prendiamo la colonna all_extracted, che contiene le liste di stringhe JSON, e la trasformiamo in più colonne. names_sep = "_" specifica che se ci sono nomi di colonne che entrano in conflitto, verranno gestiti aggiungendo un underscore e un numero sequenziale
  ungroup()                                                                  #Dopo aver completato l'operazione riga per riga, il dataframe torna alla sua forma normale (non raggruppato per riga)


#####################################################################

plan(future::multisession, workers = parallel::detectCores() - 1)

tempo_inizio <- Sys.time()
df_extracted_all <- head(df_0, 50000) %>%
  mutate(all_extracted = future_map(annotations, extract_all_tasks_in_row)) %>%
  unnest_wider(all_extracted, names_sep = "_") %>%
  ungroup()
tempo_fine <- Sys.time()
cat("Elaborazione parallela completata in:", format(tempo_fine - tempo_inizio ), "\n")

##################################################################àà


extract_tasks_to_hashtable <- function(annotations_string) {
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
          extracted_data[[top_level_task_id]] <- top_level_task
          
          # Check for nested tasks within 'value'
          if ("value" %in% names(top_level_task) && is.list(top_level_task$value[[1]])) {
            nested_value <- top_level_task$value[[1]]
            if (is.data.frame(nested_value)) {
              for (j in 1:nrow(nested_value)) {
                nested_task <- nested_value[j, , drop = FALSE]
                nested_task_id <- nested_task$task
                if (!is.null(nested_task_id)) {
                  extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id)]] <- nested_task
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

tempo_inizio <- Sys.time()
df_extracted_all <- head(df_0, 10000) %>%
  mutate(all_extracted = future_map(annotations, extract_tasks_to_hashtable))
tempo_fine <- Sys.time()
cat("Elaborazione completata in:", format(tempo_fine - tempo_inizio ), "\n")


df_extended <- df_extracted_all %>%
  mutate(all_keys = map(all_extracted, ~ names(.))) %>%
  unnest(all_keys)

df_final <- df_extracted_all %>%
  mutate(
    T0_value = map_chr(all_extracted, ~ .[["T0"]][["value"]] %||% NA_character_),
    T1_value = map_chr(all_extracted, ~ .[["T1"]][["value"]] %||% NA_character_),
    T2_value = map_chr(all_extracted, ~ .[["T2"]][["value"]] %||% NA_character_),
    T3_value = map_chr(all_extracted, ~ .[["T3"]][["value"]] %||% NA_character_),
    T4_value = map_chr(all_extracted, ~ .[["T4"]][["value"]] %||% NA_character_),
    T14_value = map_chr(all_extracted, ~ .[["T14"]][["value"]] %||% NA_character_),
    T15_value = map_chr(all_extracted, ~ .[["T15"]][["value"]] %||% NA_character_),
    T16_value = map_chr(all_extracted, ~ .[["T16"]][["value"]] %||% NA_character_)
    # Aggiungi qui altri task di primo livello che ti interessano
  )










########################################################à

# --------------------------------------------------------------------------
# LIBRERIE NECESSARIE
# --------------------------------------------------------------------------
# install.packages(c("data.table", "jsonlite", "future.apply", "parallel"))
library(data.table)
library(jsonlite)
library(future.apply)
library(parallel)

# --------------------------------------------------------------------------
# DEFINIZIONE DEI PERCORSI E PARAMETRI
# --------------------------------------------------------------------------
file_path_csv <- "C:/Users/elisa/Desktop/ReData/redata-classifications.csv" # MODIFICA QUESTO PERCORSO
file_path_output_rda <- "C:/Users/elisa/Desktop/ReData/df_processed.RData" # MODIFICA QUESTO PERCORSO
# file_path_output_csv <- "C:/Users/elisa/Desktop/ReData/df_processed.csv" # Opzionale

# Definisci qui i task di primo livello e i loro campi che ti interessano
tasks_primo_livello_da_estrarre <- list(
  T0_value = c("T0", "value"),
  T1_value = c("T1", "value"),
  T2_value = c("T2", "value"),
  T3_value = c("T3", "value"),
  T4_value = c("T4", "value"),
  T14_value = c("T14", "value"),
  T15_value = c("T15", "value"),
  T16_value = c("T16", "value")
  # Aggiungi altri task di primo livello qui
)

# --- MODIFICA QUI ---
# Definisci qui i task annidati che ti interessano.
# Specifica il NOME DELLA COLONNA FINALE che desideri,
# l'ID DEL TASK ANNIDATO che stai cercando (es. "T5"),
# e il CAMPO dentro quel task annidato da cui estrarre il valore (es. "value").
tasks_annidati_da_estrarre <- list(
  # Esempio: Vogliamo il campo "value" di qualsiasi task annidato che abbia ID "T5"
  T5_annidato_valore = c("T5", "value"), # Nome colonna: T5_annidato_valore, ID task annidato: T5, Campo: value
  
  # Esempio: Vogliamo il campo "choice" di qualsiasi task annidato che abbia ID "T6"
  T6_annidato_scelta = c("T6", "value"),
  T8_annidato_scelta = c("T8", "value"),
  T9_annidato_scelta = c("T9", "label"),
  T11_annidato_scelta = c("T11", "label"),
  T12_annidato_scelta = c("T12", "label"),
  T13_annidato_scelta = c("T13", "label"),
  # Aggiungi qui altri task annidati che vuoi estrarre.
  # Assicurati che i NOMI COLONNA (es. T5_annidato_valore) siano unici.
)
# --- FINE MODIFICA ---

# --------------------------------------------------------------------------
# FUNZIONE PER ESTRARRE I TASK DA UNA STRINGA JSON
# (Questa funzione rimane invariata rispetto alla versione precedente ottimizzata)
# --------------------------------------------------------------------------
extract_tasks_from_json <- function(annotations_string) {
  extracted_data <- list()
  if (!is.character(annotations_string) || nchar(annotations_string) == 0 || annotations_string == "[]") {
    return(extracted_data)
  }
  
  parsed_annotations <- tryCatch({
    fromJSON(annotations_string, simplifyVector = FALSE)
  }, error = function(e) {
    # cat("Errore di parsing JSON:", e$message, "Input:", annotations_string, "\n")
    NULL
  })
  
  if (is.null(parsed_annotations) || !is.list(parsed_annotations)) {
    return(extracted_data)
  }
  
  for (i in seq_along(parsed_annotations)) {
    top_level_task <- parsed_annotations[[i]]
    if (!is.list(top_level_task) || is.null(top_level_task$task)) {
      next
    }
    top_level_task_id <- top_level_task$task
    extracted_data[[top_level_task_id]] <- top_level_task
    
    if (!is.null(top_level_task$value) && is.list(top_level_task$value) && length(top_level_task$value) > 0) {
      is_list_of_subtasks <- FALSE
      if (!is.null(names(top_level_task$value[[1]])) && "task" %in% names(top_level_task$value[[1]])) {
        is_list_of_subtasks <- TRUE
      }
      
      if (is_list_of_subtasks) {
        for (j in seq_along(top_level_task$value)) {
          nested_task <- top_level_task$value[[j]]
          if (is.list(nested_task) && !is.null(nested_task$task)) {
            nested_task_id <- nested_task$task
            combined_id <- paste0(top_level_task_id, "nested", nested_task_id) # Chiave univoca
            extracted_data[[combined_id]] <- nested_task
          }
        }
      }
    }
  }
  return(extracted_data)
}

# --------------------------------------------------------------------------
# CONFIGURAZIONE DELLA PARALLELIZZAZIONE
# --------------------------------------------------------------------------
available_cores <- detectCores()
workers_to_use <- if (available_cores > 1) available_cores - 1 else 1
plan(multisession, workers = workers_to_use)
cat("Utilizzo di", workers_to_use, "worker(s) per l'elaborazione parallela.\n")

# --------------------------------------------------------------------------
# LETTURA DEL FILE CSV
# --------------------------------------------------------------------------
cat("Inizio lettura del file CSV:", file_path_csv, "\n")
tempo_inizio_lettura <- Sys.time()
tryCatch({
  df_original <- fread(file_path_csv, keepLeadingZeros = TRUE, na.strings = c("", "NA", "null"))
}, error = function(e) {
  stop("Errore durante la lettura del file CSV: ", e$message)
})
tempo_fine_lettura <- Sys.time()
cat("Lettura CSV completata in:", format(tempo_fine_lettura - tempo_inizio_lettura), "\n")

if (!"annotations" %in% names(df_original)) {
  stop("La colonna 'annotations' non è presente nel file CSV.")
}

# --------------------------------------------------------------------------
# ESTRAZIONE DEI DATI JSON IN PARALLELO
# --------------------------------------------------------------------------
cat("Inizio estrazione dei task JSON in parallelo...\n")
tempo_inizio_estrazione <- Sys.time()
list_of_extracted_data <- future_lapply(df_original$annotations, extract_tasks_from_json, .progress = TRUE, .options = future_options(seed = TRUE))
tempo_fine_estrazione <- Sys.time()
cat("Estrazione JSON completata in:", format(tempo_fine_estrazione - tempo_inizio_estrazione), "\n")

# --------------------------------------------------------------------------
# CREAZIONE DELLE NUOVE COLONNE NEL DATA.TABLE
# --------------------------------------------------------------------------
cat("Inizio creazione delle colonne finali...\n")
tempo_inizio_colonne <- Sys.time()

# Aggiungi le colonne per i task di primo livello (INVARIATO)
for (col_name in names(tasks_primo_livello_da_estrarre)) {
  task_info <- tasks_primo_livello_da_estrarre[[col_name]]
  task_id <- task_info[1]
  field_name <- task_info[2]
  
  values <- sapply(list_of_extracted_data, function(row_data) {
    task_content <- row_data[[task_id]]
    if (is.null(task_content) || is.null(task_content[[field_name]])) {
      return(NA_character_)
    }
    value <- task_content[[field_name]]
    if (is.list(value) && length(value) == 0) return(NA_character_)
    if (length(value) > 1) return(paste(unlist(value), collapse="; ")) # Concatena se multivalore
    if (is.atomic(value) && length(value) == 1) return(as.character(value)) # Se è già un singolo valore atomico
    return(as.character(value[[1]])) # Prende il primo elemento se è una lista/vettore di lunghezza 1
  })
  df_original[, (col_name) := values]
}

# --- SEZIONE MODIFICATA PER TASK ANNIDATI ---
# Aggiungi le colonne per i task annidati
# Ora cerchiamo l'ID del task annidato desiderato all'interno delle chiavi di row_data
for (col_name_final in names(tasks_annidati_da_estrarre)) {
  task_config <- tasks_annidati_da_estrarre[[col_name_final]]
  target_nested_task_id <- task_config[1] # Es. "T5" (l'ID del task annidato che cerchiamo)
  field_to_extract <- task_config[2]      # Es. "value" (il campo da estrarre da quel T5 annidato)
  
  # Il pattern da cercare nelle chiavi di row_data (es. deve terminare con "_nested_T5")
  # "$": ancora per la fine della stringa, per assicurare che "_nested_T5" non matchi "_nested_T50"
  search_pattern <- paste0("nested", target_nested_task_id, "$")
  
  extracted_values <- sapply(list_of_extracted_data, function(single_row_extracted_data) {
    found_task_content_object <- NULL
    
    # Itera sulle chiavi dei dati estratti per questa riga (es. "T0", "T7_nested_T5", "T8_nested_T5")
    for (key_in_row_data in names(single_row_extracted_data)) {
      if (grepl(search_pattern, key_in_row_data)) { # Se la chiave corrisponde al pattern (es. finisce con "_nested_T5")
        found_task_content_object <- single_row_extracted_data[[key_in_row_data]]
        break # Trovato il primo task annidato corrispondente, esci da questo ciclo interno
        # Se potrebbero esserci più task annidati con lo stesso ID (es. due "T5")
        # e vuoi gestirli (es. concatenarli), dovrai modificare questa logica (vedi nota sotto).
      }
    }
    
    if (is.null(found_task_content_object) || is.null(found_task_content_object[[field_to_extract]])) {
      return(NA_character_) # O NA_real_, NA_integer_ a seconda del tipo atteso
    }
    
    actual_value <- found_task_content_object[[field_to_extract]]
    
    if (is.list(actual_value) && length(actual_value) == 0) return(NA_character_) # Lista vuota trattata come NA
    if (length(actual_value) > 1) return(paste(unlist(actual_value), collapse="; ")) # Concatena se il campo stesso è multivalore
    if (is.atomic(actual_value) && length(actual_value) == 1) return(as.character(actual_value)) # Se è già un singolo valore atomico
    return(as.character(actual_value[[1]])) # Prende il primo elemento se è una lista/vettore di lunghezza 1
  })
  df_original[, (col_name_final) := extracted_values] # Aggiunge la colonna a df_original
}
# --- FINE SEZIONE MODIFICATA ---

tempo_fine_colonne <- Sys.time()
cat("Creazione colonne finali completata in:", format(tempo_fine_colonne - tempo_inizio_colonne), "\n")

# --------------------------------------------------------------------------
# PULIZIA (OPZIONALE)
# --------------------------------------------------------------------------
# cat("Rimozione colonna 'annotations' originale...\n")
# df_original[, annotations := NULL]
# rm(list_of_extracted_data)
# gc()

# --------------------------------------------------------------------------
# SALVATAGGIO DEI RISULTATI
# --------------------------------------------------------------------------
cat("Inizio salvataggio dei risultati...\n")
tempo_inizio_salvataggio_rda <- Sys.time()
save(df_original, file = file_path_output_rda)
tempo_fine_salvataggio_rda <- Sys.time()
cat("DataFrame processato salvato in:", file_path_output_rda, "in", format(tempo_fine_salvataggio_rda - tempo_inizio_salvataggio_rda), "\n")

# Opzionale: Salva come file CSV
# cat("Salvataggio in CSV...\n")
# tempo_inizio_salvataggio_csv <- Sys.time()
# fwrite(df_original, file = file_path_output_csv, na = "")
# tempo_fine_salvataggio_csv <- Sys.time()
# cat("DataFrame processato salvato in:", file_path_output_csv, "in", format(tempo_fine_salvataggio_csv - tempo_inizio_salvataggio_csv), "\n")

# --------------------------------------------------------------------------
# FINE SCRIPT
# --------------------------------------------------------------------------
cat("Elaborazione completata.\n")
plan(sequential) # Ripristina il piano future

















############   QUESTA SPLITTAVA JSON CI METTEVA UNA VITA , NOSENSE









#ESTRAZIONE JSON
library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)
library(furrr)
library(parallel)
library(future)

# lettura del CSV scaricato da Zooniverse separato dalle virgole, ci mette una decina di secondi
df <- read.csv("C:/Users/elisa/Desktop/ReData/redata-classifications.csv")  #qui df è diviso in 14 colonne (separate dalle virgole) e ogni riga è una digitalizzazione di una città di un certo anno

# Creiamo e salviamo una copia del dataframe iniziale. 
df_0 <- df
save(df_0, file = "C:/Users/elisa/Desktop/ReData/df_0.RData")

load("C:/Users/elisa/Desktop/ReData/df_0.RData")

extract_all_tasks_in_row <- function(annotations_string) { #funzione prende come input un singolo argomento chiamato annotations_string, che ci aspettiamo sia una stringa contenente un oggetto JSON.
  extracted_data <- list()                                  #lista vuota che verrà utilizzata per memorizzare i dati estratti dai task principali e dai task annidati presenti nella stringa JSON. Le chiavi di questa lista saranno gli ID dei task (es. "T0", "T7_nested_T5") e i valori saranno le stringhe JSON che rappresentano l'intero oggetto del task.
  contained_nested <- FALSE # Flag per indicare se la riga conteneva task annidati
  
  if (is.character(annotations_string) && nchar(annotations_string) > 0) {    #condizione che verifica se annotations_string è effettivamente una stringa e se la sua lunghezza è maggiore di zero (cioè, non è una stringa vuota). Questo controllo serve a evitare errori se la colonna annotations contiene valori non validi.
    parsed_annotations <- tryCatch({                                          #tentiamo di convertire la stringa JSON annotations_string in un oggetto R utilizzando la funzione fromJSON()
      fromJSON(annotations_string)                                            #tryCatch() è una struttura di gestione degli errori. Se la conversione JSON ha successo, il risultato (che ci aspettiamo sia un data.frame) viene assegnato a parsed_annotations.
    }, error = function(e) {
      NULL                                                                   #Se si verifica un errore durante la conversione JSON (ad esempio, se la stringa non è un JSON valido), la funzione error = function(e) { NULL } fa sì che parsed_annotations venga impostato su NULL invece di interrompere l'esecuzione del codice.
    })
    
    if (!is.null(parsed_annotations) && is.data.frame(parsed_annotations)) {  #verifica se parsed_annotations non è NULL (cioè, la conversione JSON è andata a buon fine) e se è un data.frame
      for (i in 1:nrow(parsed_annotations)) {                                 #itera su ogni riga del data.frame parsed_annotations. Ogni riga rappresenta un task principale.
        top_level_task <- parsed_annotations[i, , drop = FALSE]               #In ogni iterazione del ciclo, estraiamo la riga corrente del data.frame parsed_annotations e la assegniamo a top_level_task
        top_level_task_id <- top_level_task$task                              #Estraiamo il valore della colonna task dalla riga corrente (top_level_task). Questo valore è l'ID del task principale (es. "T0", "T7").
        
        if (!is.null(top_level_task_id)) {                                                  #Verifichiamo che l'ID del task principale non sia NULL.
          extracted_data[[top_level_task_id]] <- toJSON(top_level_task, auto_unbox = TRUE)   #Se l'ID del task principale è valido, convertiamo l'intero data.frame top_level_task in una stringa JSON utilizzando toJSON() e la memorizziamo nella lista extracted_data con l'ID del task come chiave. L'argomento auto_unbox = TRUE serve a semplificare la struttura del JSON in alcuni casi 
          
          # Check for nested tasks within 'value'
          if ("value" %in% names(top_level_task) && is.list(top_level_task$value[[1]])) {    #verifica se il task principale ha una colonna chiamata "value" e se il primo elemento di questa colonna è una lista (per i task annidati)
            nested_value <- top_level_task$value[[1]]                                       #assegniamo il primo elemento della lista contenuta nella colonna "value" a nested_value. Ci aspettiamo che questo sia un data.frame contenente i task annidati.
            if (is.data.frame(nested_value)) {                                             #Verifichiamo se nested_value è effettivamente un data.frame.
              contained_nested <- TRUE # Imposta il flag se troviamo task annidati
              for (j in 1:nrow(nested_value)) {                                           #Se nested_value è un data.frame, iteriamo su ogni riga di questo data.frame, che rappresenta un task annidato.
                nested_task <- nested_value[j, , drop = FALSE]                           #Estraiamo la riga corrente del data.frame nested_value (il task annidato).
                nested_task_id <- nested_task$task                                      #Estraiamo la riga corrente del data.frame nested_value (il task annidato).
                if (!is.null(nested_task_id)) {                                        #Verifichiamo che l'ID del task annidato non sia NULL.
                  extracted_data[[paste0(top_level_task_id, "_nested_", nested_task_id)]] <- toJSON(nested_task, auto_unbox = TRUE)     #Convertiamo l'intero data.frame nested_task (il task annidato) in una stringa JSON e lo memorizziamo nella lista extracted_data. La chiave in questo caso è una combinazione dell'ID del task principale, la stringa "_nested_" e l'ID del task annidato (es. "T7_nested_T5").
                }
              }
            }
          }
        }
      }
    }
  }
  attr(extracted_data, "contained_nested") <- contained_nested   #attributo a questa lista chiamato "contained_nested" con il valore del flag (se è annidata o no)
  return(extracted_data)  #restituiamo la lista extracted_data contenente le stringhe JSON di tutti i task (principali e annidati) estratti dalla stringa JSON di input.
}



tempo_inizio <- Sys.time()
df_extracted_all <- head(df_0, 100) %>%
  rowwise() %>%
  mutate(all_extracted = list(extract_all_tasks_in_row(annotations))) %>%    # Passiamo direttamente la colonna 'annotations' e mutate() crea una nuova colonna e in ogni riga c'è la lista dei task
  unnest_wider(all_extracted, names_sep = "_") %>%                           # Prendiamo la colonna all_extracted, che contiene le liste di stringhe JSON, e la trasformiamo in più colonne. names_sep = "_" specifica che se ci sono nomi di colonne che entrano in conflitto, verranno gestiti aggiungendo un underscore e un numero sequenziale
  ungroup()                                                                  #Dopo aver completato l'operazione riga per riga, il dataframe torna alla sua forma normale (non raggruppato per riga)
tempo_fine <- Sys.time()
cat("Elaborazione completata in:", format(tempo_fine - tempo_inizio ), "\n") 


# Identifichiamo le colonne originali (annidate) da rimuovere (basandoci sulla presenza di "_nested_")
disannested_columns <- names(df_extracted_all)[grepl("_nested_", names(df_extracted_all))]         #trovare tutte le colonne i cui nomi contengono "_nested_". Queste sono le nuove colonne create dai task disannidati.
original_nested_columns <- unique(gsub("_nested_.*", "", disannested_columns))                     # unique() per ottenere elenco univoco dei nomi delle colonne originali che contenevano task annidati.

# Rimuoviamo le colonne originali annidate
df_final <- df_extracted_all %>%
  select(!any_of(original_nested_columns))


df_extracted_all_tail <- tail(df_0, 100) %>%
  rowwise() %>%
  mutate(all_extracted = list(extract_all_tasks_in_row(annotations))) %>%    # Passiamo direttamente la colonna 'annotations' e mutate() crea una nuova colonna e in ogni riga c'è la lista dei task
  unnest_wider(all_extracted, names_sep = "_") %>%                           # Prendiamo la colonna all_extracted, che contiene le liste di stringhe JSON, e la trasformiamo in più colonne. names_sep = "_" specifica che se ci sono nomi di colonne che entrano in conflitto, verranno gestiti aggiungendo un underscore e un numero sequenziale
  ungroup()                                                                  #Dopo aver completato l'operazione riga per riga, il dataframe torna alla sua forma normale (non raggruppato per riga)



