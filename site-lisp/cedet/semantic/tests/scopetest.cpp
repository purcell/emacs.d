/* Test scoping rules for c++ in the analyzer. */

namespace {

  class aclass2;  // Forward reference.

  class aclass1 {
  private:
    int attrp1;
    int amethodp1(int argp1);
    aclass2 areference;
  protected:
    int attrpr1;
  public:
    int attr1;
    int amethod1(int arg1);
  };
}

namespace {
  class aclass2 : public aclass1
  {
  private:
    int attrp2;
    int amethodp2(int argp1);
  public:
    aclass2(int aninitvalue);
    ~aclass2();
    int attr2;
    int amethod2(int arg2);
  };
}

aclass2::aclass2(init aninitvalue)
{
  /* This is a comment that talks about aclass1 */
  aclass1;
}

int aclass2::amethod2(int arg2)
{
  int alocalvar;
  float q;

  ac;
}

