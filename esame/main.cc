#include "TCanvas.h"
#include "TGraph.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TAxis.h"

#include <iostream>
#include <vector>
#include <algorithm>
#include "EsperimentoMillikan.h"
#include "funzioni.h"

using namespace std;

int main() {
    //TApplication app("app",0,0);
    unsigned int nsimul = 10000;
    EsperimentoMillikan Milli;

    vector <double> carica(nsimul);

    vector <double> carica_v2(nsimul);  //unica fonte di errore è v2
    vector <double> carica_d(nsimul);
    vector <double> carica_V(nsimul);
    vector <double> carica_r(nsimul);
    TH1F h_carica("Distribuzione carica dell'elettrone", "Distribuzione carica dell'elettrone", 100,  -5*StDev(carica), 5*StDev(carica));


    for(int i = 0; i < nsimul; i++){
        Milli.Esegui();
        Milli.Analizza();

        carica[i] = Milli.getqemis();
        carica_v2[i] = Milli.getqemis_v2();
        carica_d[i] = Milli.getqemis_d();
        carica_V[i] = Milli.getqemis_V();
        carica_r[i] = Milli.getqemis_r();
        h_carica.Fill(carica[i]);

    }

    cout << "Valor medio della carica dell'elettrone: " << Media(carica) 
        << "\t\t\tDeviazione standard : " << StDev(carica) 
        << "\t\t\tErrore Percentuale: " << (StDev(carica)/ fabs(Media(carica)))*100 << endl;

    cout << "Deviazione standard v2 : " << StDev(carica_v2) 
        << "\nDeviazione standard d : " << StDev(carica_d) 
        << "\nDeviazione standard V : " << StDev(carica_V) 
        << "\nDeviazione standard r : " << StDev(carica_r) << endl;


    vector<double> error_max = {StDev(carica_v2), StDev(carica_d), StDev(carica_V), StDev(carica_r)};
    sort(error_max.begin(), error_max.end());

    cout << "errore massimo: " << error_max[3] 
        << "\nL'errore che influisce di più è la velocità, quindi il tempo."<< endl;


    TCanvas c;
    c.cd(1);
    h_carica.GetXaxis()->SetTitle("Carica e[C]");
    h_carica.GetYaxis()->SetTitle("");
    h_carica.Draw();
   
    //app.Run();*/
    c.Print("Milli.root");
    return 0;
}