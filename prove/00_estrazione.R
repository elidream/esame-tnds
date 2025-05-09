# caricare i seguenti pacchetti
library(dplyr)      #utile per manipolare i dati.
library(qdapRegex)  #lavorare con espressioni regolari e manipolare stringhe.
library(readtext)   #leggere file di testo (se necessario).
library(stringr)    #Utilizzato per lavorare con stringhe, offre funzioni utili come str_count(), str_split(), etc.
library(tidyr)

# lettura del CSV scaricato da Zooniverse separato dalle virgole, ci mette una decina di secondi
df <- read.csv("C:/Users/elisa/Desktop/ReData/redata-classifications.csv")  #qui df è diviso in 14 colonne (separate dalle virgole) e ogni riga è una digitalizzazione di una città di un certo anno

# Creiamo e salviamo una copia del dataframe iniziale. 
df_0 <- df
save(df_0, file = "C:/Users/elisa/Desktop/ReData/df_0.RData")


#Modifico punteggiatura
colonne_da_modificare <- c("metadata", "annotations", "subject_data") # Nomi delle colonne dove modificare la punteggiatura 
 
# Inizializza il cronometro per sapere quanto ci mette il for
tempo_inizio <- Sys.time()
#funzione che va a sostituire i caratteri di punteggiatura inutili in . mentre mantiene i caratteri - _ / invariati nelle colonne metadata, annotations, subject data
df_punti <- df_0 %>%
  mutate(across(any_of(colonne_da_modificare), ~ str_replace_all(., "[[:punct:]&&[^\\-/\\_]]", ".")))
# Calcola il tempo trascorso
tempo_fine <- Sys.time()

tempo_trascorso <- tempo_fine - tempo_inizio                        #differenza fine-inizio
cat("Elaborazione completata in:", format(tempo_trascorso), "\n")   #format() capisce da solo se mettere il risultato in sec, min, ore

#salva il nuovo df_punti con tutte le modifiche ai caratteri 
save(df_punti, file = "C:/Users/elisa/Desktop/ReData/df_punti.RData")

# Ora abbiamo:
# - df_0: Una copia del dataframe originale.
# - df_punti: Il dataframe con la punteggiatura sostituita nelle colonne specificate.  (qui non ha 1.1 strano)


#####################################################################################################################################################

#facciamo una funzione per togliere le righe nel caso in cui c'è la città ma la riga è tutta bianca: 
#il volontario seleziona "Yes. but the row is all blank" oppure non trova la città nella scansione e passa alla città successiva

# Identifica le righe da scartare
scarti_indici <- which(str_count(df_punti$annotations, pattern="task...T") == 1)      #which(...): La funzione which() prende un vettore logico e restituisce gli indici delle posizioni in cui il valore è TRUE. 
                                                                                      #Quindi, scarti_indici conterrà direttamente gli indici delle righe che soddisfano la tua condizione di scarto.

# Crea i dataframe finali in un'unica operazione
scarti_df <- df_punti[scarti_indici, ]          # scarti_df contiene le righe scartate
df_filtrato <- df_punti[-scarti_indici, ]       # Ora df_filtrato contiene le righe mantenute
    

# Salvataggio dei due dataframe
save(df_filtrato, file = "C:/Users/elisa/Desktop/ReData/df_filtrato.RData")
save(scarti_df, file = "C:/Users/elisa/Desktop/ReData/scarti_df.RData")

