// Issue #267

// plugin=angular
// environment=browser

var foodMeApp = angular.module('foodMeApp');

foodMeApp.service('cart', function Cart(localStorage, customer, $rootScope, $http, alert) {
  this.length = "4";
});

foodMeApp.controller('CheckoutController', function CheckoutController($scope, cart) {
  cart; //: Cart
  cart.length; //: string
});
