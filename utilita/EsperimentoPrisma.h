#ifndef _Esperimento_Millikan_h_
#define _Esperimento_Millikan_h_

#include "RandomGen.h"

class EsperimentoMillikan {

    public :

    EsperimentoMillikan() :
        m_rgen(1),                  //lista di inizializzazione, inizializza direttamente i datamembri
        m_g(9.80665),
        m_qe(-1.60219E-19),
        m_sigma_d(3E-5),
        m_sigma_t(0.05),
        m_sigma_V(2),
        m_V_input(2.7),
        m_d_input(60000E-18)
    {
        // calcolo v1 e t1
        m_v1_input =  ;
        m_ro_input = sqrt(m_V_input + m_d_input / (m_qe*m_qe) );

        // theta0 e' arbitrario, scelgo M_PI/2.
        m_th0_input = M_PI/2.;

        // determino theta1 e theta2
	    m_eta_input = 2.*asin( m_r_input * sin (0.5 * m_alpha) ) - m_alpha ;
        m_th1_input = m_th0_input + m_eta_input ;
        m_dm2_input = 2.*asin( m_ro_input * sin (0.5 * m_alpha) ) - m_alpha ;
        m_th2_input = m_th0_input + m_dm2_input ;
  	 
    };

    ~EsperimentoMillikan() {;} ;

    //sprco i dati con incertezza statistica
    void Esegui(){
        m_th0_misurato = m_rgen.Gauss(m_th0_input, m_sigmat);
        m_th1_misurato = m_rgen.Gauss(m_th1_input, m_sigmat);
        m_th2_misurato = m_rgen.Gauss(m_th2_input, m_sigmat);
    };

    
    void Analizza() {
        //calcolo dmin
        m_eta_misurato = m_th1_misurato - m_th0_misurato;       
        m_dm2_misurato = m_th2_misurato - m_th0_misurato;  

        // calcolo degli indici di rifrazione n1 e m2
        m_r_misurato = sin(0.5*(m_eta_misurato + m_alpha))/sin(m_alpha*0.5);
        m_ro_misurato = sin(0.5*(m_dm2_misurato + m_alpha))/sin(m_alpha*0.5);

       //calcolo A e B
        m_V_misurato = (m_qe*m_qe*m_ro_misurato*m_ro_misurato - m_g*m_g*m_r_misurato*m_r_misurato )/(m_qe*m_qe - m_g*m_g);
        m_d_misurato = (m_ro_misurato*m_ro_misurato - m_r_misurato*m_r_misurato)/(1/(m_qe*m_qe) - 1/(m_g*m_g));

    } ;

    double getAmis() { return m_V_misurato ; } ;
    double getBmis() { return m_d_misurato ; } ;
    double getn1mis() { return m_r_misurato ; } ;
    double getn2mis() { return m_ro_misurato ; } ;
    double getdm1mis() { return m_eta_misurato ; } ;
    
    
    double getA() { return m_V_input ; } ;
    double getB() { return m_d_input ; } ;
    double getn1() { return m_r_input ; } ;
    double getn2() { return m_ro_input ; } ;
    double getdm1() { return m_eta_input ; } ;
                                                       
    
    private:
    // generatore di numeri casuali                                           
    RandomGen m_rgen ;

    // parametri dell'apparato sperimentale                                                                                        
    double m_g, m_qe,  m_sigma_t, m_sigma_d, m_sigma_V;

    // valori delle quantita' misurabili :                                     
	// input    : valori assunti come ipotesi nella simulazione               
	// misurato : valore dopo la simulazione di misura                                                                                     
    double m_V_input, m_V_misurato;
    double m_d_input, m_d_misurato;
    double m_r_input, m_r_misurato;
    double m_ro_input, m_ro_misurato;
    double m_eta_input, m_eta_misurato;
    double m_v1_input, m_v1_misurato;
    double m_v2_input, m_v2_misurato;
    double m_t1_input, m_t1_misurato;
    double m_t2_input, m_t2_misurato;
    

};

#endif

