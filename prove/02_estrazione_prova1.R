# caricare i seguenti pacchetti
library(dplyr)      #utile per manipolare i dati.
library(qdapRegex)  #lavorare con espressioni regolari e manipolare stringhe.
library(readtext)   #leggere file di testo (se necessario).
library(stringr)    #Utilizzato per lavorare con stringhe, offre funzioni utili come str_count(), str_split(), etc.
library(tidyr)
library(purrr)

#ESTRAZIONE DEI TASK DALLA COLONNA ANNOTATIONS

#carica il df con gia estratte le info di metadata e subject_data ma con annotations ancora non estratta
load("C:/Users/elisa/Desktop/ReData/df_notask.RData") 

#FUNZIONE ESTRAZIONE: input -> una stringa di testo (la stringa di ogni riga della colonna annotations) e restituisce un tibble con i T. e il loro valore estratto
extract_specific_info <- function(annotation_string) {            
  task_blocks <- str_split(annotation_string, "(?=\\.\\.\\.task\\.\\.\\.T\\d+)")[[1]]  #divide la stringa in sottostringhe con la lookahed assertion "...task...T". ogni sottostringa inizierà con quel pattern 
  
  
  #Applica una funzione anonima a ciascun elemento-sottostringa (block). prende task_blocks come input e restituisce un tibble con le informazioni estratte (se presenti) dai vari block  o NULL                                                                                    #[[1]] estrae il primo (e unico) elemento della lista restituita da str_split, che è il vettore di sottostringhe 
  extracted_data <- lapply(task_blocks, function(block) {            #lapply() itera automaticamente su ogni elemento della lista task_blocks, assegnando a ogni sottostringa la varabile block         
    
    if (startsWith(block, "...task...T")) {                                       #Verifica se la sottostringa block inizia con la sequenza "...task...T". Se no non ci interessa
      task_id_full <- str_extract(block, "T\\d+")                                 #estrae la prima occorrenza del pattern "T\\d+" (la lettera "T" seguita da una o più cifre) dalla sottostringa block. Cattura l'ID completo del task (es. "T1", "T15")
      task_id_num <- str_remove(task_id_full, "T")  #serve? ( Rimuove la lettera "T" dall'ID completo del task, lasciando solo la parte numerica (es. "1", "15"). )
      task_content <- str_remove(block, "^\\.\\.\\.task\\.\\.\\.T\\d+\\.\\.\\.")   #Rimuove la parte iniziale standard di un blocco di task ("...task...T" seguito da uno o più numeri e poi "...") dall'inizio della sottostringa block
      
      task_id <- paste0("T", task_id_num)    #Ricostruisce l'ID del task completo (es. "T1") utilizzando la parte numerica estratta.
      extracted_value <- NA_character_       #Inizializza una variabile chiamata extracted_value con il valore NA (Not Available) di tipo carattere. Questa variabile conterrà il valore specifico che verrà estratto dal contenuto del task.
      
      # Verifica se l'ID del task corrente rientra in un insieme specifico di task (T1, T2, T3, T4, T6, T8, T14, T15, T16). Per questi task, si presume che il valore di interesse segua la sequenza "value...".
      if (task_id %in% c(paste0("T", 1:4), "T6", "T8", paste0("T", 14:16))) {                        
        value_match <- str_extract(task_content, "value\\.{3,}([-]?\\d*\\.?\\d+)(?=\\.\\.\\.|$)")  #Tenta di estrarre una sottostringa da task_content che inizia con "value..." (seguito da tre o più punti), cattura opzionalmente un segno meno ([-]?), zero o più cifre (\\d*), un punto decimale opzionale (\\.?), una o più cifre (\\d+), e termina prima di "..." o alla fine della stringa ($).
        
        #Se viene trovata una corrispondenza (value_match non è NA), rimuove la parte iniziale "value..." da value_match e assegna il risultato a extracted_value.
        if (!is.na(value_match)) {
          extracted_value <- str_remove(value_match, "value\\.{3,}")
        }
        
        #Verifica se l'ID del task corrente rientra nell'insieme dei task con menu a tendina.
      } else if (task_id %in% c("T5", "T9", "T11", "T12", "T13")) {
        option_true <- str_detect(task_content, "option\\.{2}true")     #Utilizza str_detect per verificare se la sottostringa task_content contiene il pattern "option\\.{2}true"
        
        if (option_true) {
          # Estrai il valore dopo "...label..."
          label_match <- str_extract(task_content, "(?<=\\.{3}label\\.{3})[^\\.]+")  #((?<=...) - lookbehind assertion) dalla sequenza "...label..." e contiene uno o più caratteri che non sono un punto ([^\\.]+).
          if (!is.na(label_match)) {
            extracted_value <- label_match
          }
        } else {
          # Estrai il valore dopo "value..." se option..true non è presente
          value_match <- str_extract(task_content, "(?<=value\\.{3})[^\\.]+")  #Tenta di estrarre una sottostringa che è preceduta da "value..." e contiene uno o più caratteri che non sono un punto. Tanto sono tutte scritte non cifre separate da un punto
          if (!is.na(value_match)) {
            extracted_value <- value_match
          }
        }
      }
      #fine della selezione in base ai diversi task. avremo un extracted_value  estratto in modi diversi in base all'ID del task
      
      if (!is.na(extracted_value)) {                                     #Se la variabile extracted_value non è NA (ovvero, è stato estratto un valore), 
        return(tibble(task_id = task_id, value = extracted_value))      #la funzione restituisce un tibble (un tipo di dataframe fornito dal pacchetto dplyr) con due colonne: task_id (l'ID del task corrente) e value (il valore estratto).
      } else {                                             
        return(NULL)                                                    # Se extracted_value è ancora NA, la funzione restituisce NULL.
      }
      #Se la sottostringa block non inizia con "...task...T" 
    } else {       
      return(NULL)   #la funzione restituisce NULL, indicando che non sono state trovate informazioni rilevanti in quel blocco (sottostringa)
    }
  }) %>% compact() %>% bind_rows()    #Dopo aver applicato la funzione anonima a tutti gli elementi di task_blocks, l'operatore pipe (%>%) passa la lista risultante alla funzione compact() del pacchetto purrr. compact() rimuove tutti gli elementi NULL dalla lista, lasciando solo i tibble contenenti i valori estratti.
                                      #L'operatore pipe passa la lista di tibble risultante a bind_rows() del pacchetto dplyr. Questa funzione combina tutti i tibble presenti nella lista in un unico tibble, aggiungendo le righe di ciascun tibble una dopo l'altra.
  return(extracted_data)            #restituisce il tibble contenente tutti i valori estratti dai diversi task presenti nella stringa annotation_string per la riga corrente del dataframe. Se non sono stati trovati task rilevanti, restituisce un tibble vuoto (perché bind_rows() su una lista vuota restituisce un tibble con 0 righe e 0 colonne).
}


# Inizializza il cronometro per sapere quanto ci mette 
tempo_inizio <- Sys.time()
# Applica la funzione a tutte le righe del dataframe
df_extracted_values <- df_notask %>%
  rowwise() %>%                                                             #Prende il dataframe df_notask e applica un'operazione riga per riga (rowwise())
  mutate(extracted_info = list(extract_specific_info(annotations))) %>%     #applica la funzione extract_specific_info alla colonna annotations e il risultato (un tibble o NULL) viene inserito in una nuova colonna chiamata extracted_info come un elemento di lista (grazie a list()).
  unnest(extracted_info, keep_empty = TRUE) %>%                             #Prende il dataframe risultante e "scompatta" la colonna extracted_info. Se un elemento di lista in extracted_info è un tibble, le sue righe vengono espanse come righe separate nel dataframe, replicando le altre colonne. 
                                                                            # keep_empty = TRUE assicura che le righe originali che avevano un NULL in extracted_info (nessun task estratto) vengano mantenute, con valori NA nelle colonne task_id e value.
  #Rimodella il dataframe da un formato "lungo" a un formato "largo".
  pivot_wider(
    names_from = task_id,    #I valori unici della colonna task_id (es. "T1", "T2", "T5") diventano i nomi delle nuove colonne.
    values_from = value,     # I valori della colonna value vengono utilizzati per popolare le nuove colonne create dai valori di task_id.         
    names_prefix = ""        #Non viene aggiunto alcun prefisso ai nomi delle nuove colonne.   
  ) %>%
  ungroup()                 #Rimuove il raggruppamento creato da rowwise(), riportando il dataframe a una struttura standard.
#Calcola il tempo trascorso
tempo_fine <- Sys.time()
tempo_trascorso <- tempo_fine - tempo_inizio                        #differenza fine-inizio
cat("Elaborazione completata in:", format(tempo_trascorso), "\n")   #format() capisce da solo se mettere il risultato in sec, min, ore


df_task_estratti <- df_extracted_values
save(df_task_estratti, file = "C:/Users/elisa/Desktop/ReData/df_task_estratti.RData")












###########################################################      PROVA     ##############################################à


# Applica la funzione solo alle prime 10000 righe
df_extracted_sample <- df_notask[1:10000,] %>%
  rowwise() %>%
  mutate(extracted_info = list(extract_specific_info(annotations))) %>%
  unnest(extracted_info, keep_empty = TRUE) %>%
  pivot_wider(
    #id_cols = -annotations, # Rimuovi solo la colonna 'annotations' dagli ID
    names_from = task_id,
    values_from = value,
    names_prefix = ""
  ) %>%
  ungroup()


riga_esempio <- df_notask$annotations[140000] # Prendi la prima riga
print(riga_esempio)

risultato_test <- extract_specific_info(riga_esempio)
print(risultato_test)
