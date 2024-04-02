
// include header files for all ROOT objetcs used in the program
#include "TGraph.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TApplication.h"
#include "TAxis.h"
#include "TLegend.h"

using namespace std;

   // Build a TApplication objetc : this is used at the end of the program
  // with the myApp.Run() to force the program waiting before the return
  
  //TApplication myApp("myApp",0,0);
  
  // Build 3 TGraph objects      
  
  TGraph myGraph1;
  TGraph myGraph2;
  TGraph myGraph3;

  // Fill the TGraph with some dummy points   

  for ( int k = 0 ; k < 20 ; k++ ) {
    double x = 2*k ;
    double y =  k * k ;
    myGraph1.SetPoint(k, x, 2*y );
    myGraph2.SetPoint(k, x, y );
    myGraph3.SetPoint(k, x, 3*y );
  }

  // build a TCanvas which will hold the graphs and enter into it 

  TCanvas * myCanvas = new TCanvas();
  myCanvas->cd();
  
  // some cosmetics to the TGraphs     

  myGraph1.SetTitle("Place here the title");
  myGraph1.GetXaxis()->SetTitle("x axis []");
  myGraph1.GetYaxis()->SetTitle("y axis []");
  
  myGraph1.SetLineColor(2);
  myGraph2.SetLineColor(3);

  myGraph1.SetMarkerStyle(20);
  myGraph2.SetMarkerStyle(21);

  // Draw : here draw the first graph and draw the second on top of it 
  // Options :                
  //   A => draw axes                         
  //   L => connect the dots with a line 
  //   P => use the Polymarker for each point   
  //   same => Draw on the same pad        

  myGraph1.Draw("ALP");
  myGraph2.Draw("sameLP");
  
  // Create and draw the legend    
  
  TLegend *mylegend = new TLegend(0.2,0.6,0.4,0.8);
  mylegend->AddEntry(&myGraph1,"This is graph1","LP");
  mylegend->AddEntry(&myGraph2,"This is graph2","LP");
  mylegend->Draw();

  // Do plots

  TCanvas * myCanvas1 = new TCanvas();
  myCanvas1->cd();

  myGraph3.SetTitle("Place here the title");
  myGraph3.GetXaxis()->SetTitle("x axis []");
  myGraph3.GetYaxis()->SetTitle("y axis []");

  myGraph3.SetLineColor(4);
  myGraph3.SetMarkerStyle(22);

  // here we build a parametric function k * pow ( x, p 
  
  TF1* myFitFun = new TF1("myFitFun","[0]*pow(x,[1])",0,20);

  // assign to the function parameters resonable starting values 
  
  myFitFun->SetParameter(1,2);
  
  // run the fit at get the value of the best exponent. Notice   
  // that the line now is the fit !           
  
  myGraph3.Draw("AP");
  myGraph3.Fit(myFitFun);
  cout << "Esponente = " << myFitFun->GetParameter(1) << endl;

  // trick to force the program to wait before closing so that one  
  //can look at the plots before they disappear       
  
 // myApp.Run();



 //ISTOGRAMMI MONODIMENSIONALI

 //In questo breve esempio vogliamo mettere in evidenza come manipolare degli istogrammi monidimensionali 
 //utilizzando la classe TH1F di ROOT. Nell'esempio si prova anche a fare un fit dei dati con una funzione gaussiana.
 

#include "TCanvas.h"
#include "TApplication.h"
#include "TH1F.h"
#include "TF1.h"
  
#include <iostream>
#include <fstream>
 
using namespace std;
 
int main (int argc, char**argv)
{
   
  TApplication app("App",0,0);

// Definizione di un istogramma monodimensionale da riempire con float

  TH1F *h1 = new TH1F("h1","Istogramma 1D",100,-5.,5.);
  h1.StatOverflows( kTRUE ); // utilizza underflow e overflow per calcoli statistici
  
// assumo di avere dei numeri immagazzinati in un file gaus.dat

  fstream f ;
  f.open("gaus.dat",ios::in);
  double x ;
  while ( f >> x ) {
    h1->Fill(x);  // riempimento
  }
  f.close();
  
  // creazione del canvas

  TCanvas *c1 = new TCanvas("c1","Istogramma 1D",600,400);
  
  // disegno.

  h1->Draw();
  h1->GetXaxis()->SetTitle("Titolo sull'asse X");
  h1->GetYaxis()->SetTitle("Titolo sull'asse Y");
  h1->Draw();
  
  // Puoi prendere le informazioni che ti interessano
  
  cout << "La media dell'istrogramma h1 vale -->>  " << h1->GetMean() << endl;
  cout << "L'RMS dell'istogramma h1 vale     -->>  " << h1->GetRMS() << endl;
  
  // Puoi fittare l'istogramma con una funzione gaussiana (e' gia' predefinita in ROOT)

  h1->Fit("gaus");
  
  // e recuperare i risultati
  
  TF1 *FitFun = h1->GetFunction("gaus");
  cout << "Ecco i parametri del fit " << endl;
  cout << "Media della gaussiana --->>> " << FitFun->GetParameter(1) << endl;
  cout << "Sigma della gaussiana --->>> " << FitFun->GetParameter(2) << endl;
  cout << "Chi2 del fit          --->>> " << FitFun->GetChisquare() << endl;
  
  c1->Print("HistoFit.pdf");
  
  app.Run();
  return 0;
  
}

//GRAFICI TGRAPH

//In questo breve esempio vogliamo mettere in evidenza come manipolare dei grafici (x,y) 
//utilizzando la classe TGraph di ROOT. Si effettua anche un fit del grafico con una funzione sinusoidale.
 

#include "TCanvas.h"
#include "TApplication.h"
#include "TGraph.h"
#include "TF1.h"

#include <cmath>
#include <iostream>
#include <fstream>

using namespace std;

int main (int argc, char**argv)
{
  
  TApplication app("App",0,0);

  // construisco il grafico

  TGraph * g = new TGraph() ;
  
  // riempio il vettore con dei punti costruiti artificialmente

  for (int i = 0; i < 20; i++){   
    double x = 0.5 * i;
    double y = 10.*sin( x+0.2 );		      
    g->SetPoint(i, x , y);
  }
    
  // Disegnare il grafico (N.B.: viene creato automaticamente un TCanvas di nome c1)
  
  TCanvas *mygraph = new TCanvas("mygraph","Grafico") ;
  g->Draw("AL*");   // A = Disegna gli assi
                    // L = Congiunge i punti con una linea spezzata
                    // * = Segna i punti con un *
  

  TF1 *f1 = new TF1("f1","[0]*sin([1]*x+[2])",0.,10.);

  f1->SetParameter(0,10.);     // Definire il parametro [0] a 10.
  f1->SetParameters(5.,1.,0.1);   // Definire [0] -> 10. e [1] a 5.

  g->Fit(f1);
  cout << "Parametro 0 del fit : " << f1->GetParameter(0) << endl;
  cout << "Parametro 1 del fit : " << f1->GetParameter(1) << endl;
  cout << "Parametro 2 del fit : " << f1->GetParameter(2) << endl;

  // Cambiare il tipo di punto
  
  TCanvas *mygraph1 = new TCanvas("mygraph1","Grafico") ;
  g->SetMarkerStyle(21);
  g->SetMarkerColor(9);
  g->Draw("ALP");   // P = Segna i punti con il punto corrente
  
  // Sovrapporre un altro grafico

  TGraph *h = new TGraph("punti.dat");
  h->SetMarkerStyle(20);
  h->SetMarkerColor(3);
  h->Draw("CP");    // C = Congiunge i punti con una linea continua  

  app.Run();

  return 0;

}
