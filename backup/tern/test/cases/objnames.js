function Ctor1() { this.x = 10; }
Ctor1.prototype = {a: 1};

function Ctor2() {}

var singleton = {a: 10, b: 20}; //: singleton

new Ctor1(); //: Ctor1
new Ctor2(); //: Ctor2
