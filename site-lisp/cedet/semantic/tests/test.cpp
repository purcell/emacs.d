/* Test file for C++ language.
 * Attempt to include as many aspects of the C++ language as possible.
 * Do not include things tested in test.c since that shares the
 * same language.
 *
 * $Id: test.cpp,v 1.21 2007/02/03 03:07:20 zappo Exp $
 *
 */

/* An include test */
#include <cmath>

#include "c++-test.hh"

#include <c++-test.hh>

#include <stdio.h>

double var1 = 1.2;

int simple1(int a) {

}

struct foo1 {
  int test;
};

struct foo2 : public foo1 {
  const int foo21(int a, int b);
  const int foo22(int a, int b) { return 1 }
};

/* Classes */
class class1 {
private:
  int var11;
  struct foo1 var12;
public:
  int p_var11;
  struct foo p_var12;
};

class i_class1 : public class1 {
private:
  int var11;
  struct foo var12;
public:
  int p_var11;
  struct foo p_var12;
};

class class2 {
private:
  int var21;
  struct foo var22;
public:
  int p_var21;
  struct foo p_var22;
};

class i_class2 : public class1, public class2 {
private:
  int var21;
  struct foo var22;
protected:
  int pt_var21;
public:
  int p_var21;
  struct foo p_var22;
};

class class3 {
  /* A class with strange things in it */
public:
  class3(); /* A constructor */
  enum embedded_foo_enum {
    a, b, c
  } embed1;
  struct embedded_bar_struct {
    int a;
    int b;
  } embed2;
  class embedded_baz_class {
    embedded_baz_class();
    ~embedded_baz_class();
  } embed3;
  ~class3(); /* destructor */
  
  /* Methods */
  int method_for_class3(int a, char b);

  int inline_method(int c) { return c; }

  /* Operators */
  class3& operator^= (const class3& something);

  /* Funny declmods */
  const class3 * const method_const_ptr_ptr(const int * const argconst) const = 0;
};

class3::class3()
{
  /* Constructor outside the definition. */
}

int class3::method_for_class3(int a, char b)
{
}

int class3::method1_for_class3( int a, int &b)
{
  int cvariablename;
  class3 fooy[];

  // Complktion testing line should find external members.
  a = fooy[1].me ;
  b = cv ;

  if (fooy.emb) {
    simple1(c);
  }

  cos(10);
  abs(10);

  return 1;
}

char class3::method2_for_class3( int a, int b) throw ( exception1 )
{
  return 'a';
}

void *class3::method3_for_class3( int a, int b) throw ( exception1, exception2 )
{
  int q = a;
  return "Moose";
}

void *class3::method31_for_class3( int a, int b) throw ( )
{
  int q = a;
  return "Moose";
}

void *class3::method4_for_class3( int a, int b) reentrant
{
  class3 ct;

  ct.method5_for_class3(1,a)
}

/*
 * A method on class3.
 */
void *class3::method5_for_class3( int a, int b) const
{
}

// Stuff Klaus found.
// Inheritance w/out a specifying for public.
class class4 : class1 {
  // Pure virtual methods.
  void virtual print () const = 0;

public:
  // The whacky constructor type
  class4()
    try : class1(args)
  {
    // constructor body	
  }
  catch ()
    {
      
    }
  

};

class class5 : public virtual class4 {
  // Virtual inheritance
};

class class6 : class1 {
  // Mutable
  mutable int i;
};

/* Namespaces */
namespace namespace1 {
  void ns_method1() { }

  class n_class1 {
  public:
    void method11(int a) { }
  };

  /* This shouldn't parse due to missing semicolon. */
  class _n_class2 : public n_class1 {
    void n_c2_method1(int a, int b) { }
  };

  // Macros in the namespace
#define NSMACRO 1

  // Template in the namespace
  template<class T> T nsti1(const Foo& foo);
  template<> int nsti1<int>(const Foo& foo);
    
}

namespace namespace2 {

  using namespace1::n_class1;

}

/* Initializers */
void tinitializers1(): inita1(False),
		       inita2(False)
{
  inita1= 1;
}

/* How about Extern C type things. */
int funny_prototype(int ,int b,float c)
{

}

extern "C"
int extern_c_1(int a, int b)
{

  funny_prototype(1,2,3.4);

  return 1;
}

extern "C" {

  int extern_c_2(int a, int b)
  {
    return 1;
  }

}

// Some operator stuff
class Action
{
  // Problems!! operator() and operator[] can not be parsed with semantic
  // 1.4.2 but with latest c.bnf
  virtual void operator()(int i, char *p ) = 0;
  virtual String& operator[]() = 0;
  virtual void operator!() = 0;
  virtual void operator->() = 0;
  virtual T& operator+=();
};

// class with namespace qualified parents
class Multiinherit : public virtual POA::Parent,
                     public virtual POA::Parent1,
                     Parent
{
private:
  int i;

public:
  Multiinherit();
  ~Multiinherit();

  // method with a list of qualified exceptions
  void* throwtest()
    throw(Exception0,
          Testnamespace::Exception1,
          Testnamespace::Excpetion2,
          Testnamespace::testnamespace1::Exception3);
  
};

void*
Multiinherit::throwtest()
  throw (Exception0,
         Testnamespace::Exception1,
         Testnamespace::Excpetion2,
         Testnamespace::testnamespace1::Exception3)
{
  return;
}

// Jens Rock <jens.rock@asamnet.de>: Nested classes or structs defined
// outside of the containing class/struct.
class container
{
 public:
  struct contained;
  container();
  ~container();
};

struct container::contained
{
  public:
  contained();
  ~contained();
};

/*
 * Ok, how about some template stuff.
 */
template <class CT, class container = vector<CT> >
const CT& max (const CT& a, const CT& b)
{
  return a < b ? b : a;
}

class TemplateUsingClass
{
  typedef TestClassMap::iterator iterator;
  typedef map<long, long> TestClassMap;

  // typedefs with const and volatile
  typedef const map<long, long> const_TestClassMap;
  typedef TestClassMap<string>::iterator volatile volatile_iterator;

  map<int, int> mapclassvarthingy;
};

template<class T> T ti1(const Foo& foo);
template<> int ti1<int>(const Foo& foo);


// -----------------------------------
// Now some namespace and related stuff
// -----------------------------------

using CORBA::LEX::get_token;
using Namespace1;

using namespace POA::std;
using namespace Test;



namespace Parser
{
  namespace
  {
    using Lexer::get_test;
    string str = "";
  }
  
  namespace XXX
  {
    
    class Foobar : public virtual POA::Parent,
                   public virtual POA::Parent1,
                   private POA::list<fact>,
                   private map<string>
    {
      ini i;
      list <shared_ptr<item> >::const_iterator l;
    public:
      
      Foobar();
      ~Foobar();
    };
  }
  

  void test_function(int i);
    
};

// unnamed namespaces - even nested
namespace
{
  namespace
  {
    using Lexer::get_test;
    string str = "";
  }

  // some builtin types
  long long ll = 0;
  long double d = 0.0;
  unsigned test;
  unsigned long int **uli = 0;
  signed si = 0;
  signed short ss = 0;
  short int i = 0;
  long int li = 0;
  
  // expressions with namespace/class-qualifyiers
  ORB_var cGlobalOrb = ORB::_nil();
  ORB_var1 cGlobalOrb1 = ORB::_test;

  class Testclass
  {
    #define TEST 0
    ini i;

  public:

    Testclass();
    ~Testclass();
  };

  static void test_function(unsigned int i);

};


// outside method implementations which should be grouped to type Test
XXX&
Test::waiting()
{
  return;
}

void
Test::print()
{
  return;
}

// outside method implementations with namespaces which should be grouped to
// their complete (incl. namespace) types
void*
Parser::XXX::Foobar::wait(int i, const char const * const * p)
{
  return;
}

void*
Namespace1::Test::wait1(int i)
{
  return;
}

int
Namespace1::Test::waiting(int i)
{
  return;
}

// a class with some outside implementations which should all be grouped to
// this class declaration
class ClassWithExternals
{
private:
  int i;

public:
  ClassWithExternals();
  ~ClassWithExternals();
  void non_nil();
};


// Foobar is not displayed; seems that semantic tries to add this to the class
// Foobar but can not find/display it, because contained in the namespace above.
void
Foobar::non_nil()
{
  return;
}

// are correctly grouped to the ClassWithExternals class
void
ClassWithExternals::non_nil()
{
  String s = "lödfjg dlfgkdlfkgjdl";
  return;
}

ClassWithExternals::ClassWithExternals()
{
  return;
}

void
ClassWithExternals::~ClassWithExternals()
{
  return;
}


// -------------------------------
// Now some macro and define stuff
// -------------------------------

#define TEST 0
#define TEST1 "String"

// The first backslash makes this macro unmatched syntax with semantic 1.4.2!
// With flexing \+newline as nothing all is working fine!
#define MZK_ENTER(METHOD) \
{ \
  CzkMethodLog lMethodLog(METHOD,"Framework");\
}

#define ZK_ASSERTM(METHOD,ASSERTION,MESSAGE) \
   { if(!(ASSERTION))\
      {\
	std::ostringstream lMesgStream; \
        lMesgStream << "Assertion failed: " \
	<< MESSAGE; \
        CzkLogManager::doLog(CzkLogManager::FATAL,"",METHOD, \
        "Assert",lMesgStream); \
        assert(ASSERTION);\
      }\
   }

// Test if not newline-backslashes are handled correctly
string s = "My \"quoted\" string";

// parsed fine as macro
#define FOO (arg) method(arg, "foo");

// With semantic 1.4.2 this parsed as macro BAR *and* function method.
// With latest c.bnf at least one-liner macros can be parsed correctly.
#define BAR (arg) CzkMessageLog method(arg, "bar");

// some const and volatile stuff
char * p1 = "Hello"; // 1. variable Pointer, variable Data
const char * p2 = "Hello"; // 2. variable pointer, constant data
char * const p3 = "Hello"; // 3. constant pointer, variable data
const char * const p4 = "Hello"; // 4. constant pointer, constant data

// Case 2 and 4 can exchange first "const" and "char"
char const * p21 = "Hello"; // variable pointer, constant data
char const * const p41 = "Hello"; // constant pointer, constant data

char volatile a = 0; // a volatile char
void foo(bar const &arg); // a reference to a const bar
int foobar(bar const * const p); // a const pointer to a const bar
int foobar(bar const volatile * const p); // a const pointer to a const bar
int foobar3(char* p); // a const pointer to a const bar

// Should not be parsed because this is invalid code
int const & const r3 = i;

boolean i = 0;
boolean & r1 = i;
boolean const & r2 = i;

// const * sequences can be very long in C++ ;-)
char const * const * const * const * ppp;

// complex function declarationen with named pointer-arguments
const char** foobar1(volatile char const * const **p);
const char** foobar11(volatile Test::Namespace::Char<char*> const * const **p);

// complex function declarationen with unnamed pointer-arguments
const char* foobar2(const char***);
const char* foobar21(const Test::Namespace::Char<char>***);

// string literal parsing even with wchar_t
char const *p = "string1";
char const *q = "string1" "str\"ing2" "string3";
wchar_t testc = L'a';

wchar_t const *wp = L"string with a \" in it";
wchar_t const *wq = L"string \n\t\"test" L"string2";
wchar_t const *wr = L"string L";
