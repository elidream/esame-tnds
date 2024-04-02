#ifndef __Posizione_h__
#define __Posizione_h__

#include <cmath>

class Posizione {

public:
    //costruttori
    Posizione();
    Posizione(double x, double y, double z);
    //distruttore
    ~Posizione();
    //metodi
    double getX() const;
    double getY() const;
    double getZ() const;
    double getR() const;
    double getPhi() const;
    double getTheta() const;
    double getRho() const;
    void SetZ(double z);
    double Distance(const Posizione&) const;

private:

    double m_x, m_y, m_z;

};



//implementazione dei costruttori e metodi 

Posizione::Posizione() {
    m_x = 0.;
    m_y = 0.;
    m_z = 0.;
}

Posizione::Posizione(double x, double y, double z){
    m_x = x;
    m_y = y;
    m_z = z;
}

Posizione::~Posizione(){
}

//coordinate cartesiane
double Posizione::getX() const {
    return m_x;
}

double Posizione::getY() const {
    return m_y;
}

double Posizione::getZ() const {
    return m_z;
}

//coordinate sferiche
double Posizione::getR() const {
    return sqrt(m_x*m_x + m_y*m_y + m_z*m_z);
}

double Posizione::getPhi() const {
    return atan2(m_y, m_x);
}

double Posizione::getTheta() const {
    return acos(m_z/getR());
}

double Posizione::getRho() const {
    return sqrt(m_x*m_x + m_y*m_y);
}

void Posizione::SetZ(double z){
    m_z = z;
}

//distanza da un altro punto
double Posizione::Distance(const Posizione& b) const {
    return sqrt(pow(getX() - b.getX(), 2)
                + pow(getY() - b.getY(), 2)
                + pow(getZ() - b.getZ(), 2));
}


#endif