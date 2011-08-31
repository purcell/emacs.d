// test usings header file.

namespace moose {

  class Point;

  class MyClass;

}


namespace moose {

  class Point;

  class MyClass {
  public:
    MyClass() : fVal(0) {
    }

    ~MyClass() {};

    /**
     * fVal Accessors
     * @{
     */
    int getVal() const {
      return fVal;
    }
    void setVal(int Val) const {
      fVal = Val;
    }
    /**
     * @}
     */
  private:
    int fVal;
  };

}

namespace deer {

  class Pickle;

};

// Code from Zhiqiu Kong

#ifndef BREAD_H
#define BREAD_H

namespace bread_name {
  class bread
  {
  public:
    void geta();
  private:
    int m_a;
    int m_b;
  };
}

#endif

