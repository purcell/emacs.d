var fs = require("fs"), path = require("path");

var projectDir = path.resolve(__dirname, "..");
exports.resolve = function(pth) { return path.resolve(projectDir, pth); };

exports.ecma5 = JSON.parse(fs.readFileSync(exports.resolve("defs/ecma5.json")), "utf8");
exports.ecma6 = JSON.parse(fs.readFileSync(exports.resolve("defs/ecma6.json")), "utf8");
exports.browser = JSON.parse(fs.readFileSync(exports.resolve("defs/browser.json")), "utf8");
exports.jquery = JSON.parse(fs.readFileSync(exports.resolve("defs/jquery.json")), "utf8");
exports.underscore = JSON.parse(fs.readFileSync(exports.resolve("defs/underscore.json")), "utf8");

var files = 0, tests = 0, failed = 0;

exports.addFile = function() { ++files; };
exports.addTest = function() { ++tests; };

exports.failure = function(message) {
  console.log(message);
  ++failed;
};

exports.hasFailed = function() { return failed > 0; };

process.on("exit", function() {
  console.log("Ran " + tests + " tests from " + files + " files.");
  console.log(failed ? failed + " failures!" : "All passed.");
});
