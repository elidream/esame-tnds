#ifndef __solutore__
#define __solutore__

#include "FunzioneBase.h"

const double prec_default = 0.001;
const unsigned int nmax_default = 100;

class Solutore {

public:

  // Costruttore
	Solutore(){
		m_a = 0; 
		m_b = 0; 
    m_nmax = nmax_default;
    m_niterations = 0;
		//m_prec = 0;
	};

  virtual double CercaZeri (double xmin,
                            double xmax,
                            const FunzioneBase& f,
                            double prec = prec_default,
                            unsigned int nmax = nmax_default) = 0 ;

	virtual bool Trovato(const FunzioneBase& f, double a, double b) = 0;
	virtual double Incertezza(double a, double b) = 0;

  //void SetPrecisione (double epsilon) { m_prec = epsilon; }
  //double GetPrecisione () { return m_prec; }
  void SetNMaxiterations (unsigned int n) { m_nmax = n; }
  unsigned int GetNMaxiterations () { return m_nmax; }
  unsigned int GetNiterations () { return m_niterations; }

  double GetA(){return m_a;}
  double GetB(){return m_b;}
  

protected:

  double m_a, m_b;
  //double m_prec;
  unsigned int m_nmax; int m_niterations;
	
};

#endif // __solutore__