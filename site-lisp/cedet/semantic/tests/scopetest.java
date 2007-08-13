class Foo {

   public void a1() {}

   class Bar {

     public int a2() {}

     public void b() {
	 a   // -1-
     }

     class Baz {

       public int a3() {}

       public Baz(int a4) {
	   a  // -2-
       }

     }

   }

}