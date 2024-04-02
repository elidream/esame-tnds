#ifndef __FunzioneBase__
#define __FunzioneBase__
#include <cmath>

// classe astratta che verrà implementata dalle classi figlie

class FunzioneBase {

public:
  virtual double Eval(double) const = 0;       //classi figlie OBBLIGATE a implementarlo
  virtual ~FunzioneBase() {}     //buona prassi definire il distruttore di una classe madre astratta come virtual, in modo che il distruttoredella figlia venga invocato correttamente.
};

class Fun : public FunzioneBase {

public:
  double Eval(double x) const override { return sin(x) - x * cos(x); }
};

#endif // __FunzioneBase__