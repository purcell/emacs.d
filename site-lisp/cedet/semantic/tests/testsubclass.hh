/* Special test file for Semantic Analyzer and complex C++ inheritance.
 *
 * Header file for testsubclass.cpp with class defns but no
 * implementations.
 */

#include <cmath>

#ifndef TESTSUBCLASS_HH
#define TESTSUBCLASS_HH

namespace animal {

  class moose {
  public:
    moose() : fFeet(0),
	      fIsValid(false)
    { }

    void setFeet(int);
    int getFeet();

    void doNothing();

  protected:

    bool fIsValid;

  private:
    int fFeet; // Usually 2 or 4.
  };

} // moose


namespace deer {

  class moose : public animal::moose {
  public:
    moose() : fAntlers(false)
    { }

    void setAntlers(bool);
    bool getAntlers();

    void doSomething();

  protected:

    bool fSomeField;

  private:
    bool fAntlers;

  };

} // deer

// A second namespace of the same name will test the
// namespace merging needed to resolve deer::alces
namespace deer {

  class alces : public animal::moose {
  public:
    alces() : fLatin(true)
    { }

    void setLatin(bool);
    bool getLatin();

    void doLatinStuff(moose moosein); // for completion testing

    moose createMoose(); // for completion testing.

  private:
    bool fLatin;
  };

};
#endif
