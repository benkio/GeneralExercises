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

console.log("otherInfo: ", otherInfo);

// path Module ////////////////////////////////////////////////////////////////

const path = require("path");

const thisScriptPath = __filename;

const pathInfo = {
  fileName: path.basename(thisScriptPath),
  folderName: path.dirname(thisScriptPath),
  fileExtension: path.extname(thisScriptPath),
  isAbsolute: path.isAbsolute(thisScriptPath),
  detailInfo: path.parse(thisScriptPath),
};

console.log("pathInfo: ", pathInfo);
console.log("OS path.sep: ", path.sep);
console.log("path.join: ", path.join(`granParent`, `parent`, `child`));
console.log("path.resolve: ", path.resolve(`granParent`, `parent`, `child`));

// FS Module //////////////////////////////////////////////////////////////////

const fs = require("fs");
let tempFolder = path.resolve(`myfolder`);
let tempFileAsync = path.join(tempFolder, `tempFileAsync.txt`);
let tempFileSync = path.join(tempFolder, `tempFileSync.txt`);

// Async folder creation
//
//   fs.mkdir(tempFolder, function (err) {
//     if (err) {
//       console.log(err);
//     } else {
//       console.log(`directory created successfully`);
//     }
//   });

try {
  !fs.existsSync(tempFolder) && fs.mkdirSync(tempFolder);
} catch (err) {
  console.log("err creating sync folder: ", err);
}

fs.writeFile(
  tempFileAsync,
  `test file content Async `,
  { flag: "a" },
  (err) => {
    if (err) {
      console.log(`error while writing to ${tempFileAsync}: ${err}`);
    } else {
      console.log(`file ${tempFileAsync} written successfully`);
    }
  },
);

fs.readFile(tempFileAsync, { encoding: "utf8" }, (err, data) => {
  if (err) {
    console.log(
      `error while reading to ${path.basename(tempFileAsync)}: ${err}`,
    );
  } else {
    console.log(
      `file ${path.basename(tempFileAsync)} read successfully: ${data}`,
    );
  }
});

try {
  fs.writeFileSync(tempFileSync, `test file content Sync `);
  console.log(`file ${tempFileSync} written successfully`);
} catch (err) {
  console.log(`error while writing to ${tempFileSync}: ${err}`);
}

try {
  let syncTempFileContent = fs.readFileSync(tempFileSync, { encoding: "utf8" });
  console.log("syncTempFileContent: ", syncTempFileContent);
} catch (err) {
  console.log(`error while reading to ${tempFileSync}: ${err}`);
}

let newTempFileAsync = path.join(tempFolder, "newTempFileAsync.txt");

fs.rename(tempFileAsync, newTempFileAsync, (err) => {
  if (err) {
    console.log("rename err: ", err);
  } else {
    console.log(
      `${path.basename(tempFileAsync)} successfully renamed to ${newTempFileAsync}`,
    );
  }
});

fs.readdir(tempFolder, (err, files) => {
  if (err) {
    console.log("err: ", err);
    return;
  }
  console.log(`Files readed successfully: `);
  console.log("files: ", files);
  deleteFiles(files);
});

function deleteFiles(files) {
  return Promise.all(
    files.map((f) => {
      return new Promise((resolve, reject) => {
        fs.unlink(path.join(tempFolder, f), (err) => {
          if (err) {
            console.log(`${err} deleting file ${f}: `);
            reject(`${err} deleting file ${f}: `);
          } else {
            resolve(`${f} successfully deleted`);
          }
        });
      });
    }),
  )
    .then((x) => console.log(`all files deleted`))
    .then(
      new Promise((resolve, reject) => {
        fs.rmdir(tempFolder, (err) => {
          if (err) {
            reject(`error removing directory ${tempFolder}`);
          } else {
            resolve(`folder deletion successful for ${tempFolder}`);
          }
        });
      }),
    )
    .catch((err) => console.log("err deleting all files: ", err));
}
