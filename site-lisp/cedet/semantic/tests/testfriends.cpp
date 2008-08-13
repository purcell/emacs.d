// Test parsing of friends and how they are used in completion.
/*
  >>  Thanks Damien Profeta for the nice example.
  >
  >  I paste a small example.
  >  It would be great if friend can be well parsed and even greater if
  >  class B can access to all the members of A.
*/

class A
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
  int test();

};


int B::test(){
  A classA;
  classA.//-1-
    ; //#1# ( "privateVar" "pubVar" )
}
