// Test parsing of friends and how they are used in completion.
/*
  >>  Thanks Damien Profeta for the nice example.
  >
  >  I paste a small example.
  >  It would be great if friend can be well parsed and even greater if
  >  class B can access to all the members of A.
*/

class Af // %2% ( ( "testfriends.cpp" ) ( "Af" "B::testB" ) )
{
public:
  int pubVar;
private:
  int privateVar;

  friend class B;

};

class B
{
public:
  int testB();
  int testAB();

};


int B::testB() {
  Af classA;
  classA.//-1-
    ; //#1# ( "privateVar" "pubVar" )
}

int B::testAB() { // %1% ( ( "testfriends.cpp" ) ( "B" "B::testAB" ) )
}
