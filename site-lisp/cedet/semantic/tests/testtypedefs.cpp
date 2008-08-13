// Sample with some fake bits out of std::string
//
// Thanks Ming-Wei Chang for these examples.

namespace std {

  template <T>class basic_string {

  public:
    void resize(int);
  
  };

}

typedef std::basic_string<char> mstring;

using namespace std;
typedef basic_string<char> bstring;


int main(){
  mstring a;

  a.// -1-
    ;
  // #1# ( "resize" )
  
  bstring b;
  // It doesn't work here.
  b.// -2-
    ;
  // #2# ( "resize" )
  
  return 0;
}


// ------------------

class Bar 
{
public:
     void someFunc() {}

};

typedef Bar new_Bar;

template <class mytype>
class TBar 
{
public:
     void otherFunc() {}

};

typedef TBar<char> new_TBar;

int main()
{
  new_Bar nb;
  new_TBar ntb;

  nb.// -3-
    ;
  // #3# ("someFunc")

  ntb.// -4-
    ;
  // #4# ("otherFunc")

  return 0;
}
