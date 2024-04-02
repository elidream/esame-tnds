#ifndef __EQ_DIFF__
#define __EQ_DIFF__

#include "VectorOperations.h"
#include <cmath>
#include <cfloat>

using namespace std;

// ===============================================================
// Classe astratta, restituisce la derivata nel punto x al tempo t
// ===============================================================

class FunzioneVettorialeBase {

public:
  virtual vector<double> Eval(double t, const vector<double> & x) const = 0;   //metodo vistuale puro, restituisce il vettore delle derivate calcolate rispetto a un vettore di stato x
};

// ===============================================================
// Caso fisico concreto
// ===============================================================

class OscillatoreArmonico : public FunzioneVettorialeBase {

public:

    OscillatoreArmonico(double omega0) { m_omega0 = omega0; };

    vector<double> Eval(double t, const vector<double> &x) const override {
     
        vector<double> v {x[1], -m_omega0*m_omega0*x[0]};
        return v;
    }

private:
  double m_omega0;
};

class PendoloReale : public FunzioneVettorialeBase {

  public:

    PendoloReale(double lunghezza) { m_lunghezza = lunghezza; };

    double g = 9.81;

    vector<double> Eval(double t, const vector<double> &x) const override {
     
        vector<double> v {x[1], -g/m_lunghezza*sin(x[0])};
        return v;
    }

private:
  double m_lunghezza;

};

class OscillatoreSmorzato : public FunzioneVettorialeBase{

  public:

    OscillatoreSmorzato(double omega0, double alpha, double omega) {
      m_omega0 = omega0;
      m_alpha = alpha;
      m_omega = omega;
    }

    vector<double> Eval(double t, const vector<double> &x) const override{

      vector<double> v {x[1], -m_omega0* m_omega0*x[0] - m_alpha*x[1] + sin(m_omega*t)};
      return v;
    }
    
  private:
    double m_omega0;  //pulsazione propria del sistema
    double m_alpha;   //costante di smorzamento
    double m_omega;   //pulsazione della forzante
};

class MotoCampoGravitazionale : public FunzioneVettorialeBase{
  public:
    MotoCampoGravitazionale(double Massa){
      m_massa = Massa;
      m_G = 6.6742e-11;
    }

    vector <double> Eval(double t, const vector<double> &x) const override{
      
      vector<double> v {x[2], x[3], -(m_massa*m_G*x[0])/pow(x[0]*x[0] + x[1]*x[1],3./2.), -(m_massa*m_G*x[1])/pow(x[0]*x[0] + x[1]*x[1],3./2.)};
      return v;
    }

  private:
    double m_massa;
    double m_G;
};

class MotoCampoGrav_disturbato : public FunzioneVettorialeBase{
  public:
    MotoCampoGrav_disturbato(double Massa){
      m_massa = Massa;
      m_G = 6.6742e-11;
    }

    vector <double> Eval(double t, const vector<double> &x) const override{
      double raggio = 3*m_massa/pow(x[0]*x[0] + x[1]*x[1],2);
      vector<double> v {x[2], x[3], -(m_massa*m_G*x[0])/pow(x[0]*x[0] + x[1]*x[1],3./2.)+ raggio*x[0], -(m_massa*m_G*x[1])/pow(x[0]*x[0] + x[1]*x[1],3./2.)+ raggio*x[1]};
      return v;
    }

  private:
    double m_massa;
    double m_G;
};
// ===============================================================
// Classe astratta per un integratore di equazioni differenziali
// ===============================================================

class EquazioneDifferenzialeBase {
public:
  virtual vector<double> Passo(double t, const vector<double> &x, double h, const FunzioneVettorialeBase &f) const = 0;
};

// ===============================================================
// Integratore concreto, metodo di Eulero
// ===============================================================

class Eulero : public EquazioneDifferenzialeBase {
public:

  vector<double> Passo(double t, const vector<double> &x, double h, const FunzioneVettorialeBase &f) const override {

    return x + (f.Eval(t,x))*h;

  }
};


class Kutta : public EquazioneDifferenzialeBase {
public:

  vector<double> Passo(double t, const vector<double> &x, double h, const FunzioneVettorialeBase &f) const override {

    vector<double> K1 = f.Eval(t,x);
    vector<double> K2 = f.Eval(t + h/2., x + h/2.*K1 );
    vector<double> K3 = f.Eval(t + h/2., x + h/2.*K2 );
    vector<double> K4 = f.Eval(t + h, x + h*K3 );
    
    return x + h/6.*(K1 + 2.*K2 + 2.*K3 +K4);

  }

  //rounge kutta a precisione fissata
  vector<double> Passo_prec(double t, const vector<double> &x, double prec, const FunzioneVettorialeBase &f) {
    unsigned int N = 2;
    vector <double> Xn(x.size());
    double h = 0;
    vector<double> X2n = x;

    do{
      Xn = X2n;
      N *= 2.;
      h = (t-0)/N;
      X2n = Passo(t,x,h,f);
    }while(prec < 16./15. * Distanza(Xn, X2n));
    
    return X2n;

  }

  double Passo_prec_fix(double t, const vector<double> &x, double prec, const FunzioneVettorialeBase &f) {
    unsigned int N = 2;
    vector <double> Xn(x.size());
    double h = 0;
    vector<double> X2n = x;

    do{
      Xn = X2n;
      N *= 2.;
      h = (t-0)/N;
      X2n = Passo(t,x,h,f);
    }while(prec < 16./15. * Distanza(Xn, X2n));
    
    return h;

  }

};



#endif // __EQ_DIFF__
