#ifndef __Bisezione__
#define __Bisezione__

#include <float.h> // Serve per includere DBL_MAX

#include "Solutore.h"
#include <iostream>
using namespace std;

// funzione sign utile negli algoritmi di ricerca zeri. Potete aggiungerla come
// funzione che eredita da FunzioneBase

int sign(double x) {
  if (x < 0)
    return -1;
  else
    return 1;
}

class Bisezione : public Solutore {

public:
  // costruttori
  Bisezione() : Solutore(){};
  ~Bisezione() { ; };

  // metodi
  double CercaZeri(double xmin, double xmax, const FunzioneBase &f, double prec = prec_default,
                   unsigned int nmax = nmax_default) override {

    m_niterations = 0;
    m_nmax = nmax;

    if (xmin < xmax) {
      m_a = xmin;
      m_b = xmax;
    } else {
      m_a = xmax;
      m_b = xmin;
    }

    double fa = f.Eval(m_a);
    double fb = f.Eval(m_b);

    while ((m_b - m_a) > prec) {
      double c = 0.5 * (m_b + m_a);
      double fc = f.Eval(c);

      if (m_niterations > m_nmax)
        break;
      m_niterations++;
      if (sign(fa) * sign(fc) < 0) {
        m_b = c;
        fb = fc;
      } else if (sign(fa) * sign(fc) > 0) {
        m_a = c;
        fa = fc;
      }
    }

    return 0.5 * (m_b + m_a);
  }

  
  bool Trovato(const FunzioneBase &f, double a, double b) override {

    double fa = f.Eval(a);
    double fb = f.Eval(b);
    if (sign(fa) * sign(fb) < 0){
      return true;
    }else{
      return false;
    }
  }

  double Incertezza(double a, double b){           //secondo me più questo
    return 0.5 * abs(b-a);
  };

	
};

#endif