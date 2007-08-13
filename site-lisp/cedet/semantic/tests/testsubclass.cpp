/* Special test file for Semantic Analyzer and complex C++ inheritance.
 */

#include <iostream>
#include "testsubclass.hh"

void animal::moose::setFeet(int numfeet)
{
  if (numfeet > 4) {
    std::cerr << "Why would a moose have more than 4 feet?" << std::endl;
    return;
  }

  fFeet = numfeet;
}

int animal::moose::getFeet()
{
  return fFeet;
}

void animal::moose::doNothing()
{
  animal::moose foo();

  foo

  fFeet = 3;
}


void deer::moose::setAntlers(bool have_antlers)
{
  fAntlers = have_antlers;
}

bool deer::moose::getAntlers()
{
  return fAntlers;
}

void deer::moose::doSomething()
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

  MooseVariableName = createMoose();

  doLatinStuff(MooseVariableName);
  
  fIsValid = createMoose().

  return MooseVariableName;
}
