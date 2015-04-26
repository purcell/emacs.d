// plugin=node

var fs = require("fs"), crypto = require("crypto"), tls = require("tls");

fs.readFileSync; //: fn(filename: string, encoding: string) -> Buffer

fs.stat("foobar", function(err, stats) {
  err; //: Error
  stats.isFile(); //: bool
});

var module = {};

crypto.getCiphers()[3]; //: string
crypto.createHash("sha1").digest().readUInt16BE(0); //: number

tls.createServer({}, function(stream) {
  // Has event emitter props
  stream.once; //: fn(event: string, listener: fn())
  // Writable stream props
  stream.write; //: fn(chunk: Buffer, encoding?: string, callback?: fn()) -> bool
  // Readable stream
  stream.read; //: fn(size?: number) -> Buffer
  // ClearTextStream
  stream.authorized; //: bool
});

require("timers").setInterval; //: fn(callback: fn(), ms: number) -> timers.Timer
setInterval; //: fn(callback: fn(), ms: number) -> timers.Timer
setTimeout(function(){}, 10).ref; //: fn()


require("module");

// don't attempt to handle .node binary modules
require("./binary.node").binary; //: ?

var mymod = require("mymod");

require("_stream_readable");

mymod.foo; //: number
mymod.bar; //: string

require("./localfile").hello; //: fn() -> number

require("./foo/../exportfunc.js"); //: fn(a: number, b: number) -> number

require("./dir"); //:: {foo: string, rel: {abc: number, def: {xyz: string}}}

var mod1 = require("mod1");
var mod2 = require("mod1/mainfile.js");
mod1.mainExport.x; //: number
mod2.mainExport.x; //: number
mod1.fromSubdep; //: fn()

require("mod1/secondfile").secondExport.u; //: number
require("mod1/dir1").foo.a; //: number

require("mod1/reassign_exports").funcPropExport; //loc: 2, 15

require("mod1/reassign_exports_to_required"); //:: {A: number}

// inference should continue even if a module is not found
require("mod_not_found"); //: ?

var doc = require("mod1/doc");
doc.f1; //doc: doc for f1
doc.f2; //doc: doc for f2

module.exports. //+

// completion on known require module
require('f //+ 'fs'
// completion on custom require module
require("my //+ "mymod"

// go to definition for modulest
require('./localfile'//loc: 1, 0, localfile.js
