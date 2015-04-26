function Base() {}
Base.prototype = {};

Base.prototype; //: Base.prototype
new Base; //: Base

function Sub1() {}
Sub1.prototype = new Base();
new Sub1(); //: Sub1

function Sub2() {}
Sub2.prototype = Object.create(Base.prototype);
new Sub2(); //: Sub2

function Base2() {}

function Sub3() {}
Sub3.prototype = new Base2();

new Sub3(); //: Sub3
