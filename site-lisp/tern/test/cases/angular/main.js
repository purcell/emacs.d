// plugin=angular
// environment=browser
// environment=jquery
// loadfiles=config.js, filters.js

angular.module('sample', ['ngResource', 'sample.config', 'sample.filters'])

.run(function($rootScope, someNumber) {
  someNumber; //: number
  $rootScope.rootName; //: string
})

.controller('GreetingCtrl', ['$rootScope', '$scope', 'User', 'myConfig', 'version', function($rootScope, $scope, User, myConfig, version) {
  $rootScope.rootName = $scope.myName = 'John';
  $scope.myName; //: string

  $scope.myConfig = myConfig; //: {myColor}
  $scope.myConfig; //: {myColor}

  $scope.version = version; //: string
  $scope.version; //: string

  User; //doc: doc for User

  $scope.user = User.get({login: 'sqs'});
  $scope.user.$promise.finally; //: fn(callback: fn()) -> Promise
}])

.controller('OtherCtrl', ['$scope', function($scope) {
  // Test that controllers' $scope objects are distinct.
  $scope.myName; //: ?
}])

.constant('version', 'v1.2.3')

// doc for User
.factory('User', ['$resource', function($resource) {
  return $resource('https://api.github.com/users/:login');
}])

;

angular.module('docsScopeProblemExample', [])
.directive('myCustomer', function() {
  return {
    //+ bindToController, compile, controller, controllerAs, template, templateUrl, ...
  };
}).directive('myCustomer', function() {
  return {
    t //+ template, templateNamespace, templateUrl, ...
  };
});
