// C++ examples and requests from Klaus Berndl

// template in a unnamed namespace
namespace
{
  template<typename Target, typename Source>
  Target lexical_cast(Source arg)
  {
    std::stringstream interpreter;
    Target result;

    if(!(interpreter << arg) || !(interpreter >> result) ||
       !(interpreter >> std::ws).eof())
      throw bad_lexical_cast();
    
    return result;
  }
}


template <class T, FOO> class Vector
{
private:
  static T* v;
  int sz;

public:
  T& elem(int i) {return v[i];}
  virtual ~Vector ();

protected:
  Vector ();
};

template <> class Vector <void*>
{
private:
  void** v;
  int sz;

public:
  Vector ();
  virtual int func1(int i);
  virtual int func2(int i) = 0;
  static virtual int func3(int i) = 0;
  void*& elem(int i) {return v[i];}
  //...
};

// template contains namespace
typedef vector<CzkUtilities::Action*> ActionList;

// declaration of some template-types
map<XXX, Test::YYY>** map_var;

map_with_size<XXX, Test::YYY, size> map_size_var;
typedef map_with_size<XXX, Test::YYY, size> SizedMap;

map_with_10_size<XXX, Test::YYY, 10>* pMap_size10_var;
typedef map_with_10_size<XXX, Test::YYY, 10> Size10Map;

// a function which such a template-argument
void* test_function(map<ClassX, Test::ClassY, 10>* pMap);


template <class T> class Vector <T*> : private Vector <void*>
{
public:
	typedef Vector <void*> Base;

	Vector () : Base() {}

	T*& elem(int i) {return static_cast<T*&>(Base::elem(i));}
	//...
};

// outside method implementation of a template-class
template<class T> T& Vector<T*>::elem(int i)
{
  return C;
}

// same but qualified with a namespace Testnamespace
template<class T> T& Testnamespace::Vector<T*>::elem(int i)
{
  return C;
}

// function templates with keyword typename
template<typename Target, typename Source>
Target lexical_cast(Source arg)
{
  std::stringstream interpreter;
  Target result;

  if(!(interpreter << arg) || !(interpreter >> result) ||
     !(interpreter >> std::ws).eof())
    throw bad_lexical_cast();
    
  return result;
}

template<class T>
static
typename T::_ptr_type
getService(const std::string& pServiceName, const int pRetries=20)
{
  return T::_narrow(getServiceObject(pServiceName, pRetries));
}

// function template declaration
template<class T> void sort(vector<T>&);
// complex function template definition
template<class T, class S, const NamespaceY::ClassX<TestClass, &s> volatile ***&i>
map<ClassY, ClassX, 10>
sort(const vector<T>& v)
{
  return;
}

// variable declarations of template-types
foo<TClass, Testnamespace::XClass, i> *bar1;
foo<TClass, Testnamespace::XClass, **&i> *bar2;
foo<TClass, Testnamespace::XClass, *Namespace::ClassX::i> bar3;
foo<0> bar0;

class SomeName;
class OtherName;

