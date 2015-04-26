function out(a) { return a.bar + 10; }

(function() {
  function Foo() { this.bar = 10; }
  out(new Foo);
})();
