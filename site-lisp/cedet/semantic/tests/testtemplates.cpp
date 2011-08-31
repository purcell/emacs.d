// Templates Test file:
// Written by 'Raf'

template <class T, int U, class V>
class read_ref {
public:
  const T* read_ref_member_one( T);
  const V* read_ref_member_two();
};

namespace NS {
  template <class T, int U, class V>
  class ref {
  public:  
    read_ref<T,10,V> operator->() {
      m_// -1-
	;
      // #1# ( "m_datas" )
    }  

  private:
    T m_datas[U];
  };

}

class FooOne {
public:
  int fooOneMember();
};

class FooTwo {
public:
  int fooTwoMember();
};

class FooThree {
public:
  int fooThreeMember();

  FooOne * operator->();
};

typedef ref<FooOne, 10,FooTwo> Test;

using NS;

void
main(void) {
  ref<FooOne, 10, FooTwo> v;
  
  v->read_ref_member_one()-> // -2-
    ;
  // #2# ( "fooOneMember" )

  v->read_ref_member_two()-> // -3-
    ;
  // #3# ( "fooTwoMember" )

  v-> // -4-
    ;
  // #4# ( "read_ref_member_one" "read_ref_member_two" )

  Test t;

  t->read_ref_member_two()-> // -5-
    ;
  // #5# ( "fooTwoMember" )

  ref<FooOne, 10, FooThree> v2;
  
  v2->read_ref_member_two()-> // -6- 
    ;
  // #6# ( "fooOneMember" )

  /* Try all these things by also specifying the namespace in the name. */
  NS::ref<FooOne, 10, FooTwo> v3;
  
  v3->read_ref_member_one()-> // -7-
    ;
  // #7# ( "fooOneMember" )

  v3->read_ref_member_two()-> // -8-
    ;
  // #8# ( "fooTwoMember" )

}
