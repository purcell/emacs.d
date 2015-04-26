var obj = {
  // This is foo
  get foo() { return "hi"; },
  set foo(x) { return 55; }
};

obj.foo; //: string

obj.foo; //doc: This is foo
