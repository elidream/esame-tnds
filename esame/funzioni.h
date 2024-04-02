#include <iostream>
#include <fstream>
#include <cmath>
#include <assert.h>
#include <vector>   //container
#include <algorithm> //functions

using namespace std;

template <typename T> vector<T> Read(int ntot, const char* filename) {

    vector <T> v;

    ifstream in(filename);

    if(!in){
        cout << "cannot open file" << filename << endl;
        exit(-1);
    }else {
        while(!in.eof()){
            T val = 0;
            in >> val;
            v.push_back(val);
        }
    }

    in.close();

    return v;
}


template <typename T> double Media (const vector <T> &v)  {
    double sum = 0;
    for (int i = 0; i < v.size(); i++){
        sum += v[i];
    }
    return sum/(v.size());
}

template <typename T> double Varianza (const vector <T> &v) {
    double mean = Media(v);
    double scarti = 0;
    for(int i = 0; i < v.size(); i++){
        scarti += pow((v[i] - mean),2);
    }
    return (scarti/(v.size()-1));
}

template <typename T> double StDev (const vector <T> &v) {
    double mean = Media(v);
    double scarti = 0;
    for(int i = 0; i < v.size(); i++){
        scarti += pow((v[i] - mean),2);
    }
    return sqrt(scarti/(v.size()-1));
}



template <typename T> double Mediana (vector <T> & v) {
   sort(v.begin(),v.end());

    double Mediana;
        if(v.size()%2==0) {
            double num1 = v[v.size()/2-1];
            double num2 = v[v.size()/2];
            Mediana = (num1 + num2)/2;
        } else {
            Mediana = v[(v.size())/2];
        }
    return Mediana;
}

template <typename T> void Print(vector <T> & v, const char* filename) {
    fstream outfile(filename, ios::out);
    for(int i=0; i<v.size(); i++) {
        outfile << v[i] << endl;
    }
    outfile.close();
}

template <typename T> void Print(vector <T> & v) {
    cout << "Elementi dell'array:" << endl;
    for(int i=0; i<v.size(); i++) {
        cout << "Elemento " << i+1 << ": " << v[i] << endl;
    }
}