#ifndef __INTEGRAL_H__
#define __INTEGRAL_H__

#include <iostream>
#include <cfloat>
#include "FunzioneBase.h"
#include "RandomGen.h"

using namespace std;

// Base class : generic integrator
class Integral {

public:
  
  	Integral (double a, double b){
    checkInterval (a,b);
    m_nstep = 0;
    m_h = 0; 
    m_sum = 0;
    m_integral =0;
  	}

  	virtual double Integra(unsigned int nstep, FunzioneBase& f) = 0;
  	virtual double Integra(double prec, FunzioneBase& f) = 0;

	protected:

  	void checkInterval (double a, double b){
		m_a = min(a,b);
		m_b = max(a,b);
		if(a > b) m_sign =-1;
		else m_sign = 1;
  	}

 	unsigned int m_nstep;
  	double m_a, m_b;
  	double m_sum, m_integral, m_h;
  	int m_sign;
};

// derived class : Midpoint integrator

class Midpoint : public Integral {

	public:

 	Midpoint (double a, double b) : Integral (a, b){;};

	double Integra(double prec, FunzioneBase& f) override {return 0;};
  	double Integra(unsigned int nstep, FunzioneBase& f) override{

		m_nstep = nstep;
		m_h = (m_b - m_a)/m_nstep;

		m_sum = 0.;
		for (int k = 0; k < m_nstep; k++){
			m_sum += f.Eval(m_a + (k + 0.5)* m_h);
		}
		m_integral = m_sign*m_sum*m_h; 
		return m_integral;
  };

	
};

class Simpson : public Integral {
	
	public:

		Simpson (double a, double b) : Integral (a,b) {;};
		
		double Integra(double prec, FunzioneBase& f) override {return 0;};
		double Integra(unsigned int nstep, FunzioneBase& f) override {
			m_nstep = nstep;
			m_h = (m_b - m_a)/m_nstep;

			m_sum = 0.;
			for(int k = 1; k < m_nstep; k++){
				if (k%2 ==0){
					m_sum += 2* f.Eval(m_a + k*m_h);
				} else {
					m_sum += 4* f.Eval(m_a + k*m_h);
				}
			}

			m_integral = (m_h/3.*(f.Eval(m_a)+m_sum+f.Eval(m_b)));
			return m_integral;	
			
		};

			
};

class Trapezioidi : public Integral {

	public:

		Trapezioidi(double a, double b) : Integral (a,b) {;};

		double Integra(unsigned int nstep, FunzioneBase& f) override {
			m_nstep = nstep;
			m_h = (m_b - m_a)/m_nstep;

			m_sum = 0.;
			for(int k = 1; k < m_nstep-1; k++){
				m_sum += f.Eval(m_a + k*m_h);
			}

			m_integral = (0.5 * f.Eval(m_a) + m_sum + f.Eval(m_b)*0.5) * m_h;
			return m_integral;	
			
		};

		
		double Integra(double prec, FunzioneBase& f) override{            
			unsigned int nstep = 2;
			double In = DBL_MAX;
			m_sum = 0.;
			double I2n = Integra(nstep,f);
			m_h = (m_b - m_a)/m_nstep;

			
			do{
				In = I2n;
				nstep *= 2;
				m_h /= 2.;
				
				for (int k = 1; k < nstep; k += 2){
					m_sum += f.Eval(m_a + k*m_h);
				}
				I2n = m_sign * m_sum * m_h;
			}while(4./3.* fabs(In -I2n) > prec);

			return I2n;
		};

    

};

// ======================
//INTEGRAZIONE MONTECARLO
//=======================

class IntegralMC {

    public:
        IntegralMC (unsigned int seed): m_myrand(seed){};

		//Hit or Miss method
        double IntegraleHoM(double xmin, double xmax, double fmax, FunzioneBase&f, int punti){
            int count = 0;
            double x = 0, y = 0;
          
            for (int i = 0; i < punti; i++){
                x = m_myrand.Unif(xmin, xmax);
                y = m_myrand.Unif(0, fmax);
                if(y < f.Eval(x)){
                    count ++;
                }

            }

            return (xmax - xmin)*fmax*((double)count/punti);

        };

		//Average method
        double IntegraleAVE(double xmin, double xmax, FunzioneBase&f, int punti){
            
            double sum = 0;
            for(int i = 0; i < punti; i++){
                double x = m_myrand.Unif(xmin, xmax);
                sum += f.Eval(x);
            }
            
            return (xmax - xmin)*sum/punti;

        };


    private:
    RandomGen m_myrand;

};

#endif // __INTEGRAL_H__