# caricare i seguenti pacchetti
library(dplyr)      #utile per manipolare i dati.
library(qdapRegex)  #lavorare con espressioni regolari e manipolare stringhe.
library(readtext)   #leggere file di testo (se necessario).
library(stringr)    #Utilizzato per lavorare con stringhe, offre funzioni utili come str_count(), str_split(), etc.
library(tidyr)


load("C:/Users/elisa/Desktop/ReData/df_filtrato.RData")

#ESTRAZIONE TEMPO DI INIZIO started_at
df_filtrato <- df_filtrato %>%    # %>% è l'operatore pipe. Prende il dataframe df_filtrato e lo "passa" come primo argomento alla funzione che segue sulla riga successiva (si usa quando si eseguono più operazioni sequenziali su un set di dati, rende più veloce e leggibile il codice) 

  mutate(started_at_extracted = str_extract(metadata, "(?<=started_at\\.\\.\\.)\\d{4}-\\d{2}-\\d{2}T\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d+Z")) %>%     
  #mutate() aggiunge una nuova colonna chiamata 'started_at_extracted'
  #str_extract() prende come input la colonna metadata e impone che il pattern che vogliamo estrarre DEVE essere preceduto da "started_at...". (?<=started_at\\.\\.\\.) è una lookbehind assertion serve per trovare ma non viene inserita nella variabile
  #\\d{4}-\\d{2}-\\d{2}T\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d+Z  è il Pattern di cattura dopo la lookbehind assertion, infatti ci sono esattamente 4 cifre (anno) poi un - letterale, poi mese e giorno ora ecc... infatti la stringa iniziale è del tipo: started_at...2024-06-25T12.35.54.905Z 
  
  # 'str_replace()' è una funzione di 'stringr' che sostituisce la prima occorrenza di un pattern in una stringa.
  mutate(started_at_extracted = str_replace(started_at_extracted, "T", " ")) %>%               #questo toglie la T da started_at_extracted
  mutate(started_at_extracted = str_replace(started_at_extracted, "Z", "")) %>%                #questo toglie la Z
  mutate(started_at = as.POSIXct(started_at_extracted, format = "%Y-%m-%d %H.%M.%S")) %>%      # 'as.POSIXct()' è una funzione base di R che converte una stringa in un oggetto di classe POSIXct, che rappresenta date e orari.
  select(-started_at_extracted)  # Rimuovi la colonna temporanea starded_at_extracted perchè ora abbiamo quella pulita creata nella riga precedente ("started_at")



# ESTRAZIONE CLASSIFICATION_COUNT E FILENAME DELLA SCANSIONE
# Estrazione di 'classification_count' dalla colonna 'subject_data'
df_filtrato <- df_filtrato %>%
  mutate(classification_count = str_extract(subject_data, "(?<=classifications_count\\.{2})(\\d+)")) #(\\d+) corrisponde a una sequenza di una o più cifre

# Estrazione di 'Filename' dalla colonna 'subject_data'
df_filtrato <- df_filtrato %>%
  mutate(Filename = str_extract(subject_data, "(?<=Filename\\.{3,4})([^\\.]+)"))   #[^\\.], significa " prendi qualsiasi carattere fino al primo punto"

df_notask <- df_filtrato
save(df_notask, file = "C:/Users/elisa/Desktop/ReData/df_notask.RData")



