#ifndef __Elettrone_h__
#define __Elettrone_h__


#include <iostream>

using namespace std;

class Elettrone: public Particella {
    public:

        Elettrone();
        ~Elettrone();

        void Print() const;
};

// Costruttore
Elettrone::Elettrone() : Particella(9.1093826E-31, -1.60217653E-19){}  //chiama il costruttore della classe madre Particella e lo inizializza con massa e carica specifiche

Elettrone::~Elettrone(){}

void Elettrone::Print() const{
    cout << "Elettrone:  \n Massa = " << m_massa << "\n Carica = " << m_carica << endl;
}

#endif