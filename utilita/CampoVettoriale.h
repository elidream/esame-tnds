#ifndef __CampoVettoriale_h__
#define __CampoVettoriale_h__

#include "Posizione.h"

//CampoVettoriale eredita da posizione e in più aggiunge altri membri, ovvero le componenti del campo in quel punto

class CampoVettoriale : public Posizione {

  public:

    CampoVettoriale(const Posizione&);
    CampoVettoriale(const Posizione&, double Fx, double Fy, double Fz);
   // CampoVettoriale(double x, double y, double z, double Fx, double Fy, double Fz);
    ~CampoVettoriale(){};

    CampoVettoriale & operator+=( const CampoVettoriale & ) ;
    CampoVettoriale operator+( const CampoVettoriale & ) const;
    
    double getFx() const {return m_Fx;}
    double getFy() const {return m_Fy;}
    double getFz() const {return m_Fz;}

    double setFx();
    double setFy();              //se le voglio cambiare io manualmente
    double setFz();
    

    double Modulo();

  private:

    double m_Fx, m_Fy, m_Fz;

};



CampoVettoriale::CampoVettoriale(const Posizione& p) : Posizione(p) {
    m_Fx = 0;
    m_Fy = 0;
    m_Fz = 0;
}

CampoVettoriale::CampoVettoriale(const Posizione& p, double Fx, double Fy, double Fz): Posizione(p){
    m_Fx = Fx;
    m_Fy = Fy;
    m_Fz = Fz;
}

//operatori per fare somme tra vettori
CampoVettoriale & CampoVettoriale::operator+=(const CampoVettoriale & v){
  return(*this) = (*this) + v;
}

CampoVettoriale CampoVettoriale::operator+(const CampoVettoriale& v) const {
    CampoVettoriale sum(v);
    sum.m_Fx += getFx();
    sum.m_Fy += getFy();
    sum.m_Fz += getFz();
    return sum;
}

double CampoVettoriale::Modulo() {
    return sqrt(m_Fx*m_Fx + m_Fy*m_Fy + m_Fz*m_Fz);
}

#endif // __CampoVettoriale_h__ 

