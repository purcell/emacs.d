// number tests

var x = Math.PI;
x. //+ toExponential, toString, toFixed

Math.cos(x). //+ toExponential, ...

var a = [1, 2, 3]; 
a[0]. //+ toExponential, toString, toFixed

var y = a.slice(2);
y[0]. //+ toExponential, toString, toFixed

var z = [].reduce(function(a, b) { return a - 2; }, 0);
z. //+ toExponential, toString, toFixed, ...

var n = Math.cos.call(null, 10); 
n. //+ toExponential, toString, toFixed

var m = 10 - "1";
m. //+ toExponential, toString, toFixed

var k = m;
k. //+ toExponential, toString, toFixed

var l = (function () { return { x: 10, y: 100 }; })().x;
l. //+ toExponential, toString, toFixed

var a2 = parseInt("2");
a2. //+ toExponential, toString, toFixed

var a3 = parseFloat("1.5");
a3. //+ toExponential, toString, toFixed

var a4 = Number("10");
a4. //+ toExponential, toString, toFixed

var a5 = (2 > 1) ? 10: 200;
a5. //+ toExponential, toString, toFixed

var b1 = Number.POSITIVE_INFINITY;
b1. //+ toExponential, toString, toFixed

Math.acos(-1 * Math.abs(-1)). //+ toExponential, toString, toFixed

Math. //+ abs, min, max, ...
Math.p //+ pow, propertyIsEnumerable

// regular expression tests

var reg = new RegExp(/test/i, "test Test");
reg. //+ compile, exec, test, ...

var pattern1 = /is/g;
pattern1. //+ global, ignoreCase, multiline, ...

// String tests

"x". //+ charAt, concat, ...  @5

"x".ch //+ charAt, charCodeAt @7

"x".charA //+ charAt

"1". //+ length,lastIndexOf,...

"1".toString(). // charAt, concat, ...

var car = "Volvo";
car[1]. //+ charAt, indexOf, lastIndexOf,...

var s1 = 10 +"000";
s1. //+ match, slice, replace, search, ...

var s2 = new String("hello, world");
s2. //+ substring, toLowerCase, trim, substr, toUpperCase, ...

var s3 = "test";
s3. //+ split, trimLeft, trimRight, ...

var s4 = ["Strawberry", "Blueberry"][0];
s4. //+ toLocaleLowerCase, toLocaleUpperCase, ...

// object tests

var person = new Object();
person.firstname = "John";
person.lastname = "Doe";
person.age = 50;
person.eyecolor = "blue";
person. //+ age, eyecolor, firstname, lastname
person.age. //+ toExponential, toFixed, toString
person.lastname. //+ charAt, charCodeAt, concat, ...

function personType(firstname, lastname, age, eyecolor) {
  this.firstname = firstname;
  this.lastname = lastname;
  this.age = age;
  this.eyecolor = eyecolor;
  this.changeName = changeName;
  this. //+ age, changeName, eyecolor, firstname, lastname
  this.changeName     //+ changeName
  this.changeName()   
  function changeName(name) {
    this.lastname = name;
    return this.lastname;
  }
}

var friend = new personType("Sally", "Rally", 48, "green");
friend. //+ age, changeName, eyecolor, firstname, lastname
friend.firs //+ firstname
friend.firstname. //+ charAt, charCodeAt, concat, ...
friend.ch //+ changeName
friend.changeName() //+ parseInt, Function, Math, ...
friend.changeName(). //+ charAt, charCodeAt, concat, ...


var base = {foo: 10, bar: 20, foo2: { a: 10, b: "test" } };
base. //+ bar, baz, foo, foo2
base.f //+ foo, foo2
base.foo //+ foo, foo2
base.foo. //+ toExponential, toFixed, toString
base.foo2 //+ foo2
base.foo2. //+ a, b
base.foo2.a. //+ toExponential, toFixed, toString
base.foo2.b. //+ charAt, charCodeAt, concat, ...

base.baz = 30;
base. //+ bar, baz, foo, foo2

var gen1 = Object.create(base);
var gen2 = Object.create(gen1);
gen1.quux = 50;
gen2.kaka = 10;
gen1.  //+ bar, baz, foo, foo2, quux
gen1.foo. //+ toExponential, toFixed, toString
gen1.kaka. //+ 
gen1.quux. //+ toExponential, toFixed, toString
gen2. //+ bar, baz, foo, foo2, quux, kaka
gen2.kaka. //+ toExponential, toFixed, toString
gen2.quux. //+ toExponential, toFixed, toString

var extend = Object.create(base, { prop1: { value: "hi" }, prop2: { value: 10 } });
extend. //+ bar, baz, foo, foo2, prop1, prop2
extend.prop1. //+ charAt, charCodeAt, concat, ... @14
extend.prop2. //+ toExponential, toFixed, toString
extend.bar. //+ toExponential, toFixed, toString

var empty = Object.create(null);
empty.prop1 = "hi";
empty.hasOwnProperty. //+ 
empty.prop1. //+ charAt, charCodeAt, concat, ...

function Ctor1() { this.x = 10; }
Ctor1.prototype = {a: 1};
Ctor1. //+ apply, bind, call, prototype

function Ctor2() {}

new Ctor1(). //+ a, x
new Ctor2(). //+ 

var singleton = {a: 10, b: 20}.  //+ a, b

function Foo(x) {
  this.x = x;
  this.y = [1];
}

Foo.prototype = {
  makeString: function() { return "hi"; },
  bar: 13
};

var foo = new Foo(true); 

foo. //+ bar, makeString, x, y, ...
foo.makeString(). //+ charAt, charCodeAt, concat, ...
foo.bar. //+ toExponential, toFixed, toString

config = {
  CSS: {
    IDs: {
      container: 'eytp-maincontainer',
      canvas: 'eytp-playercanvas',
      player: 'eytp-player',
      controls: 'eytp-controls' 
     }
   }
}
config. //+ CSS
config.CSS. //+ IDs
config.CSS.IDs. //+ container, canvas, player, controls

// array tests

var a = [1, 2, 3]; 
a. //+ pop, push, reverse, shift, sort, ...

var years = [1950, 1960, 1970, 1980, 1990, 2000, 2010];
years.slice(2). //+ splice, unshift, concat, ...

["x"].concat(["hi"]). //+  pop, push, reverse, ...

var arr1 = [true, false, true].filter(function(x){return x;});
arr1. //+  pop, push, reverse, ...

[].map(function() { return "x"; }). //+ pop, push, reverse, ...

"foo bar baz".split(" "). //+ pop, shift, sort, ...

var arr2 = new Array(1, 3, 5, 7);
arr2. //+ pop, shift, sort, ...

function scope1() {arr2. } //+ pop, shift, sort, ...  @25

var f = function (arr1, arr2) {}(arr2. ); //+ pop, shift, sort, ... @39

var f2 = function (arr3) {arr3. }("foo bar baz".split(" ")); //+ pop, ... @32

var f3 = function (arr4) {arr4. }("test string"); //+ charAt, ... @32

(function() {
  var innerArr1 = arr2;
  var refArr = innerArr1; 
  innerArr1. //+ pop, shift, sort, ...

  var ref = innerArr2;
  ref. //+ pop, shift, sort, ...

  var innerArr2 = [1, 2, 3]; 
  var self = this;
  function nestedFun(){
    innerArr3. //+ pop, shift, sort, ...
    innerArr2. //+ pop, shift, sort, ...
    refArr. //+ pop, shift, ...

    return [5, 7, 9];
  }
  var innerArr3 = [];
  var result = nestedFun();
  result. //+ pop, shift, sort, ...
  nestedFun(). //+ pop, shift, sort, ...
});

//arguments is an array
function(){
  arguments. //+ pop, shift, sort, ...
}

// scope tests

function one(){
  person. //+ charAt, charCodeAt, concat, ...
  var person = "test";
  person. //+ charAt, charCodeAt, concat, ...
  var localVar1InOne = "test"; 
  globalVar1 = 10; 
  function innerFunc1() {
    globalVar1. //+ toExponential, toFixed, toString
    localVar1InOne. //+ charAt, charCodeAt, concat, ...
    var localVar1InInnerFunc = "hello";
  }
  localVar1InInnerFunc  //+ 
}
person. //+ firstname, lastname, age, eyecolor

globalVar1. //+ toExponential, toFixed, toString
localVar1InOne. //+ 

function timeout(f, timeout) {}
var obj = {
  name: 'foo',
  func: function() {
    var that = this;
    timeout(function() {
      console.log(that.name.); //+ charAt, charCodeAt, concat, ... @29
    }, 3000);
  }
};

(function () {
  var dog = "German Shepherd";
  localVarBecomeGlobal = [1, 2, 3];
})();

localVarBecomeGlobal. //+ pop, shift, sort,...

// if control flow
function testIfControlFlow1() {
  inNestedIf. //+ charAt, charCodeAt, concat, ...
  if (true) {
    var varInIf = "in if";
    varInIf. //+ charAt, charCodeAt, concat, ...
    var f = (function() {
          var name = "inner func"; 
          varInIf. //+ charAt, charCodeAt, concat, ...
          return { name: name }; })();
      
    f.name. //+ charAt, charCodeAt, concat, ...
    if (true) {
      var inNestedIf = "in nested if";
    } else {
      var t = varInIf. //+ charAt, charCodeAt, concat, ...
      f.name. //+ charAt, charCodeAt, concat, ...
      inNestedIf. //+ charAt, charCodeAt, concat, ...
    }
  }
  t. //+ 
  f. //+ name
}
inNestedIf.        //+ 
varInIf.           //+ 

// for control flow
function testForControlFlow1() {
  varInForLoop. //+ charAt, charCodeAt, concat, ...
  varInNestedForLoop. //+ toExponential, toString, toFixed

  var m = "test";
  for (var n = 0; n < 10; n++) {
    var varInForLoop = "test";
    for (var j = 0; j < 5; j++ ) {
      var varInNestedForLoop = 10;
    }
    varInNestedForLoop. //+ toExponential, toString, toFixed
  }
}
varInForLoop. //+ 
varInNestedForLoop. //+  

// for/in loop
function testForInLoopFlow1() {
  for (x in person) {     
    person[x]. //+ 
  }
}

// while loop
function testWhileLoopFlow1() {
  var n = 5; 
  while (n-- > 0) {
    var varInWhileLoop = "test";
  }
}
varInWhileLoop. //+ 

function testDoWhileLoopFlow1() {
  n = 5; 
  do {
    var varInDoWhileLoop = "test";
  } while(n-- > 0);
}
varInDoWhileLoop. //+ 


// break
function testBreakFlow1() {
  for (i = 0;i < 10; i++) {
    if (i == 3) {
      var testBreak = "test";
      break;
    } else {
      var testContinue = "test"
      continue;
    }
    testBreak. //+ charAt, charCodeAt, concat, ...
    testContinue. //+ charAt, charCodeAt, concat, ...
  }
}
testBreak. //+ 
testContinue. //+ 

// switch/case

function testSwitchFlow1(n) {

  switch(n) {
    case 1:
      var inCase1 = "case1";
      break;
    case 2:
      inCase1. //+ charAt, charCodeAt, concat, ...
      var inCase2 = " case2";
      break;
    default:
  }

  inCase1. //+ charAt, charCodeAt, concat, ...
  inCase2. //+ charAt, charCodeAt, concat, ...
}

inCase1. //+ 

// try/catch/finally

function testTryCatchFlow1() {
  try {
    var varInTry = " test";
  } catch(err) {
    varInTry. //+ charAt, charCodeAt, concat, ...
    var varInCatch = "test";
  } finally {
    varInTry. //+ charAt, charCodeAt, concat, ...
    varInCatch. //+ charAt, charCodeAt, concat, ...
  }
}

varInTry. //+ 
varInCatch. //+ 
