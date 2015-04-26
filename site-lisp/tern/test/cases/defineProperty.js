var o = {};

// Docstring for prop1
Object.defineProperty(o, "prop1", {
  get: function() { return "hi"; }
});

Object.defineProperty(o, "prop2", {
  value: 100
});

o.prop1; //: string
o.prop2; //: number

o.prop1; //doc: Docstring for prop1
