/* Special test file for Semantic Analyzer and complex C++ inheritance.
 */

//#include <iostream>
#include "testsubclass.hh"

void animal::moose::setFeet(int numfeet) //^1^
{
  if (numfeet > 4) {
    std::cerr << "Why would a moose have more than 4 feet?" << std::endl;
    return;
  }

  fFeet = numfeet;
}

int animal::moose::getFeet() //^2^
{
  return fFeet;
}

void animal::moose::doNothing() //^3^
{
  animal::moose foo();

  fFeet = 3;
}


void deer::moose::setAntlers(bool have_antlers) //^4^
{
  fAntlers = have_antlers;
}

bool deer::moose::getAntlers() //^5^
// %1% ( ( "testsubclass.cpp" "testsubclass.hh" ) ( "deer::moose::getAntlers" "deer::moose::doSomething" "moose" ) )
{
  return fAntlers;
}

bool i_dont_have_symrefs()
// %2% ( ("testsubclass.cpp" ) ("i_dont_have_symrefs"))
{
}

void deer::moose::doSomething() //^6^
{
  // All these functions should be identified by semantic analyzer.
  getAntlers();
  setAntlers(true);

  getFeet();
  setFeet(true);

  doNothing();

  fSomeField = true;

  fIsValid = true;
}

void deer::alces::setLatin(bool l) {
  fLatin = l;
}

bool deer::alces::getLatin() {
  return fLatin;
}

void deer::alces::doLatinStuff(moose moosein) {
  // All these functions should be identified by semantic analyzer.
  getFeet();
  setFeet(true);

  getLatin();
  setLatin(true);

  doNothing();

  deer::moose foo();


}

moose deer::alces::createMoose()
{
  moose MooseVariableName;
  bool tmp;
  int itmp;
  bool fool;
  int fast;

  MooseVariableName = createMoose();

  doLatinStuff(MooseVariableName);
  
  tmp = this.f// -1-
    // #1# ( "fAlcesBool" "fIsValid" "fLatin" )
    ;

  itmp = this.f// -2-
    // #2# ( "fAlcesInt" "fGreek" "fIsProtectedInt" )
    ;

  tmp = f// -3-
    // #3# ( "fAlcesBool" "fIsValid" "fLatin" "fool" )
    ;

  itmp = f// -4-
    // #4# ( "fAlcesInt" "fGreek" "fIsProtectedInt" "fast" )
    ;

  MooseVariableName = m// -5-
    // #5# ( "moose" )

  return MooseVariableName;
}

/** Test Scope Changes
 *
 * This function is rigged to make sure the scope changes to account
 * for different locations in local variable parsing.
 */
int someFunction(int mPickle)
{
  moose mMoose = deer::alces::createMoose();
  
  if (mPickle == 1) {

    int mOption1 = 2;

    m// -5-
      // #5# ( "mMoose" "mOption1" "mPickle" )
      ;

  } else {

    int mOption2 = 2;

    m// -6-
      // #6# ( "mMoose" "mOption2" "mPickle" )
      ;
  }

}

// Thanks Ming-Wei Chang for this next example.

namespace pub_priv {

  class A{
  private:
    void private_a(){}
  public: 
    void public_a();
  };

  void A::public_a() {
    A other_a;

    other_a.p// -7-
      // #7# ( "private_a" "public_a" )
      ;
  }

  int some_regular_function(){
    A a;
    a.p// -8-
      // #8# ( "public_a" )
      ;
    return 0;
  }

}


/** Test Scope w/in a function (non-method) with classes using
 * different levels of inheritance.
 */
int otherFunction()
{
  sneaky::antelope Antelope(1);
  sneaky::jackalope Jackalope(1);
  sneaky::bugalope Bugalope(1);

  Antelope.// -9-
    // #9# ( "fAntyPublic" "fQuadPublic" "testAccess")
    ;

  Jackalope.// -10-
    // #10# ( "fBunnyPublic" "testAccess")
    ;

  Bugalope.// -11-
    // #11# ( "fBugPublic" "testAccess")
    ;
}

/** Test methods within each class for types of access to the baseclass.
 */

bool sneaky::antelope::testAccess() //^7^
{
  this.// -12-
    // #12# ( "fAntyPrivate" "fAntyProtected" "fAntyPublic" "fQuadProtected" "fQuadPublic" "testAccess" )
    ;
}

bool sneaky::jackalope::testAccess() //^8^
{
  this.// -13-
    // #13# ( "fBunnyPrivate" "fBunnyProtected" "fBunnyPublic" "fQuadProtected" "fQuadPublic" "testAccess" )
    ;
}

bool sneaky::bugalope::testAccess() //^9^
{
  this.// -14-
    // #14# ( "fBugPrivate" "fBugProtected" "fBugPublic" "fQuadPublic" "testAccess" )
    ;
}

