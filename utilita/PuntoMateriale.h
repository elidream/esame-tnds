#ifndef __PuntoMateriale_h__
#define __PuntoMateriale_h__

#include "Particella.h"
#include "Posizione.h"
#include <cmath>
#define _USE_MATH_DEFINES

#include "CampoVettoriale.h"

//PuntoMateriale è la sorgente del campo sostanzialmente

class PuntoMateriale : public Particella, Posizione {

public:
  //costruttori
  PuntoMateriale(const Particella&, const Posizione&);
  PuntoMateriale(double massa, double carica, const Posizione&);
  PuntoMateriale(double massa, double carica, double x, double y, double z);
  //distruttore
  ~PuntoMateriale(){};
  //metodi
  CampoVettoriale CampoElettrico(const Posizione&) const ;
  CampoVettoriale CampoGravitazionale(const Posizione&) const;

};


//implementazione

PuntoMateriale::PuntoMateriale( const Particella& m, const Posizione& p): 
  Particella(m), Posizione(p){

}

PuntoMateriale::PuntoMateriale(double massa, double carica, double x, double y, double z):
  Particella(massa, carica), Posizione(x,y,z){

  }

CampoVettoriale PuntoMateriale::CampoElettrico(const Posizione& r) const{
    CampoVettoriale E(r);

    double Ex = (GetCarica()*(r.getX()-getX()))/(4*M_PI*8.854187818E-12*pow(Distance(r),3));
    double Ey = (GetCarica()*(r.getY()-getY()))/(4*M_PI*8.854187818E-12*pow(Distance(r),3));
    double Ez = (GetCarica()*(r.getZ()-getZ()))/(4*M_PI*8.854187818E-12*pow(Distance(r),3));

    return CampoVettoriale (r,Ex,Ey,Ez);
}



CampoVettoriale PuntoMateriale::CampoGravitazionale(const Posizione& r) const{
    double Gx = GetMassa()*6.67E-11/pow(r.getX(),2);
    double Gy = GetMassa()*6.67E-11/pow(r.getY(),2);
    double Gz = GetMassa()*6.67E-11/pow(r.getZ(),2);

    return CampoVettoriale (r,Gx,Gy,Gz);
};


#endif // __PuntoMateriale_h__