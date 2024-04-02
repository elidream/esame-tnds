#ifndef __RANDOMGEN_H__
#define __RANDOMGEN_H__

#include <cmath>

class RandomGen {

   public:
     
     RandomGen(unsigned int seed){
        m_seed = seed;
        m_a = 1664525;
        m_c = 1013904223;
        m_m = 1 << 31; // due alla trentuno
      };

      void SetA(unsigned int a) {m_a = a;}
      void SetC(unsigned int c) {m_c = c;}
      void SetM(unsigned int m) {m_m = m;}

      //uniform distribution in [0,1)
      double Rand(){                            
        m_seed = (m_a*m_seed + m_c) % m_m;
        return double (m_seed)/double (m_m);
      };
     
      //uniform distribution in [a,b)
      double Unif(double xmin, double xmax){
        return xmin + (xmax - xmin) * Rand();
      };

      //exponential distribution with inverse function method
      double Exp(double mean){
        return (-1./mean)*log(1-Rand());
      };

      //normal distribution with Box-Muller method
      double Gauss(double mean, double sigma){
        double s = Rand();
        double t = Rand();
        double x = sqrt(-2. * log(1.- s))*cos(2.*M_PI*t);
        return mean + x*sigma;
      };

      //Normal distribution with Accept-Reject method
      double GaussAR(double mean, double sigma){

        double a = mean - 5.*sigma;
        double b = mean + 5.*sigma;
        double max = 1/(sigma*sqrt(2.*M_PI));
        double x = 0, y = 0;

         do{
            x = Unif(a,b);
            y = Unif(0.,max);
         }while(y > max * exp(-pow(((x-mean)/(sigma)),2)*0.5));
            
         return x;
      };

   private:
     unsigned int m_a, m_c, m_m;
     unsigned int m_seed;

};




#endif