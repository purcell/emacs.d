function A() {}
A.prototype.prop_A = 1;
function B() {}
B.prototype = new A;
B.prototype.prop_B = 2;
function C() {}
C.prototype = new A;
C.prototype.prop_C = 3;

(new A).prop_ //+ prop_A
;
(new B).prop_ //+ prop_A, prop_B
;
(new C).prop_ //+ prop_A, prop_C
