#ifndef _Esperimento_Millikan_h_
#define _Esperimento_Millikan_h_

#include "RandomGen.h"

class EsperimentoMillikan {

    public :

    EsperimentoMillikan() :
        m_rgen(1),                  //lista di inizializzazione, inizializza direttamente i datamembri
        m_g(9.80665),
        m_eta(1.81E-5),
        m_qe_input(-1.60219E-19),
        m_sigma_d(3E-5),
        m_sigma_t(0.05),
        m_sigma_V(2),
        m_V_input(600),
        m_d_input(0.005),
        m_r_input(1E-6),
        m_ro(875.3)
    {
        // calcolo v1 e t1
        m_v1_input = (4./3.*m_r_input*m_r_input*m_r_input* m_ro* m_g )/(6.*m_r_input*m_eta) ;
        m_t1_input = m_d_input/m_v1_input;

        // determino v2 e t2
	    m_v2_input = (m_qe_input*m_V_input/m_d_input - 4./3.* M_PI *m_r_input*m_r_input*m_r_input* m_ro* m_g)/(6.*M_PI*m_r_input*m_eta);
        m_t2_input = m_d_input/m_v2_input ;
        
    };

    ~EsperimentoMillikan() {;} ;

    //sprco i dati con incertezza statistica
    void Esegui(){
        m_V_misurato = m_rgen.Gauss(m_V_input, m_sigma_V);
        m_d_misurato = m_rgen.Gauss(m_d_input, m_sigma_d);
        m_t1_misurato = m_rgen.Gauss(m_t1_input, m_sigma_t);
        m_t2_misurato = m_rgen.Gauss(m_t2_input, m_sigma_t);
        
    };

    
    void Analizza() {

        //calcolo v1 e v2 misurati
        m_v1_misurato = m_d_misurato/m_t1_misurato;
        m_v2_misurato = m_d_misurato/m_t2_misurato;
        
        //calcolo raggio
        m_r_misurato = sqrt((18*m_v1_misurato*m_eta)/(4*m_ro*m_g));        

        // calcolo carica elettrone
        m_qe_misurato = (4./3.* M_PI *m_r_misurato*m_r_misurato*m_r_misurato* m_ro* m_g + 6.*M_PI*m_r_misurato*m_v2_misurato*m_eta ) * (m_d_misurato/m_V_misurato);
        
        //carica elettrone con incertezze selezionate
        m_qe_mis_v2 = (4./3.* M_PI *m_r_input*m_r_input*m_r_input* m_ro* m_g + 6.*M_PI*m_r_input*m_v2_misurato*m_eta ) * (m_d_input/m_V_input);
        m_qe_mis_d = (4./3.* M_PI *m_r_input*m_r_input*m_r_input* m_ro* m_g + 6.*M_PI*m_r_input*m_v2_input*m_eta ) * (m_d_misurato/m_V_input);
        m_qe_mis_V = (4./3.* M_PI *m_r_input*m_r_input*m_r_input* m_ro* m_g + 6.*M_PI*m_r_input*m_v2_input*m_eta ) * (m_d_input/m_V_misurato);
        m_qe_mis_r = (4./3.* M_PI *m_r_misurato*m_r_misurato*m_r_misurato* m_ro* m_g + 6.*M_PI*m_r_misurato*m_v2_input*m_eta ) * (m_d_input/m_V_input);
    } ;

    double getVmis() { return m_V_misurato ; } ;
    double getdmis() { return m_d_misurato ; } ;
    double getrmis() { return m_r_misurato ; } ;
    double getqemis() { return m_qe_misurato ; } ;

    double getqemis_v2() { return m_qe_mis_v2 ; } ;
    double getqemis_d() { return m_qe_mis_d ; } ;
    double getqemis_V() { return m_qe_mis_V ; } ;
    double getqemis_r() { return m_qe_mis_r ; } ;
    
    double getV() { return m_V_input ; } ;
    double getd() { return m_d_input ; } ;
    double getr() { return m_r_input ; } ;
    double getqe() { return m_qe_input ; } ;
                                                       
    
    private:
    // generatore di numeri casuali                                           
    RandomGen m_rgen ;

    // parametri dell'apparato sperimentale                                                                                        
    double m_g,  m_eta, m_ro, m_sigma_t, m_sigma_d, m_sigma_V;

    // valori delle quantita' misurabili :                                     
	// input    : valori assunti come ipotesi nella simulazione               
	// misurato : valore dopo la simulazione di misura                                                                                     
    double m_V_input, m_V_misurato;
    double m_d_input, m_d_misurato;
    double m_r_input, m_r_misurato;
    double m_qe_input, m_qe_misurato;
    double m_v1_input, m_v1_misurato;
    double m_v2_input, m_v2_misurato;
    double m_t1_input, m_t1_misurato;
    double m_t2_input, m_t2_misurato;

    // variabili per vedere che errore domina
    double m_qe_mis_v2, m_qe_mis_d, m_qe_mis_V, m_qe_mis_r;
    

};

#endif

