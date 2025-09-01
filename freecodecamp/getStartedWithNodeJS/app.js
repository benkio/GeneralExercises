// Global Variable ////////////////////////////////////////////////////////////

console.log(__dirname);
console.log(__filename);
global.globalVar = `Custom Global Variable`;
console.log(globalVar);

// Module

const sayHelloModule = require(`./hello.js`);

sayHelloModule.sayHello("Enrico");
sayHelloModule.sayHello("Benkio");
sayHelloModule.sayHello("Minsi");
sayHelloModule.sayGoodbye("Enrico");
sayHelloModule.sayGoodbye("Benkio");
sayHelloModule.sayGoodbye("Minsi");

// Os Module

const os = require("os");

const uptime = os.uptime();
const userInfo = os.userInfo();

console.log("uptime: ", uptime);
console.log("userInfo: ", userInfo);

const otherInfo = {
  name: os.type(),
  release: os.release(),
  totalMem: os.totalmem(),
  freeMem: os.freemem(),
};

console.log('otherInfo: ', otherInfo);

// Path Module ////////////////////////////////////////////////////////////////

const path = require('path');

const thisScriptPath = __filename;

const pathInfo = {
    fileName: path.basename(thisScriptPath),
    folderName: path.dirname(thisScriptPath),
    fileExtension: path.extname(thisScriptPath),
    isAbsolute: path.isAbsolute(thisScriptPath),
    detailInfo: path.parse(thisScriptPath)
};

console.log('pathInfo: ', pathInfo);
console.log('OS path.sep: ', path.sep);
console.log('path.join: ', path.join(`granParent`, `parent`, `child`));
console.log('path.resolve: ', path.resolve(`granParent`, `parent`, `child`));

// FS Module //////////////////////////////////////////////////////////////////

const fs = require('fs');

// TBD
