#ifndef __Particella_h__
#define __Particella_h__


#include <iostream>

using namespace std;

class Particella {

    public:
        
        Particella();
        Particella(double massa, double carica);

        ~Particella();

        double GetMassa() const {return m_massa;};
        double GetCarica() const {return m_carica;};
        void Print() const;

    protected:
        double m_massa;
        double m_carica;

}; 

#endif

//implementazione 
Particella::Particella(){
    m_massa = 0;
    m_carica = 0;
}


Particella::Particella(double massa, double carica){
    m_massa = massa;
    m_carica = carica;
}

Particella::~Particella(){}

void Particella::Print() const{
    cout << "Particella :  \n Massa = " << m_massa << "\n Carica = " << m_carica << endl;
}