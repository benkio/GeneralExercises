"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const http = require("node:http");
const node_fs_1 = require("node:fs");
const node_util_1 = require("node:util");
function fileToResponse(res, filePromise, statusCode = 200) {
    return filePromise
        .then(data => {
        res.writeHead(statusCode, { 'Content-Type': 'text/html; charset=utf8' });
        res.end(data);
        return res;
    })
        .catch(err => {
        res.writeHead(500);
        res.end(err);
        return res;
    });
}
const index = (0, node_util_1.promisify)(node_fs_1.readFile)('./pages/index.html', {
    encoding: 'utf8',
});
const about = (0, node_util_1.promisify)(node_fs_1.readFile)('./pages/about.html', {
    encoding: 'utf8',
});
const contactMe = (0, node_util_1.promisify)(node_fs_1.readFile)('./pages/contact-me.html', { encoding: 'utf8' });
const notFound = (0, node_util_1.promisify)(node_fs_1.readFile)('./pages/404.html', {
    encoding: 'utf8',
});
// Create a local server to receive data from
const server = http.createServer(async (req, res) => {
    let promiseResponse = Promise.resolve(res);
    switch (req.url) {
        case '/':
            promiseResponse = fileToResponse(res, index);
            break;
        case '/about':
            promiseResponse = fileToResponse(res, about);
            break;
        case '/contact-me':
            promiseResponse = fileToResponse(res, contactMe);
            break;
        default:
            promiseResponse = fileToResponse(res, notFound, 404);
    }
    try {
        return await promiseResponse;
    }
    catch (err) {
        res.writeHead(500);
        res.end(`An error occurred when fetching the files ${err}`);
    }
    finally {
        return res;
    }
});
const port = 8000;
console.log(`Running Server at ${port}`);
server.listen(port);
//# sourceMappingURL=index.js.map