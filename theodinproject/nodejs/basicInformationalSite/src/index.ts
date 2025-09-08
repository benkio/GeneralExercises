import {ServerResponse} from 'node:http';
import * as http from 'node:http';
import {readFile} from 'node:fs';
import {promisify} from 'node:util';

function fileToResponse(
  res: ServerResponse,
  filePromise: Promise<string>,
  statusCode = 200,
): Promise<ServerResponse> {
  return filePromise
    .then(data => {
      res.writeHead(statusCode, {'Content-Type': 'text/html; charset=utf8'});
      res.end(data);
      return res;
    })
    .catch(err => {
      res.writeHead(500);
      res.end(err);
      return res;
    });
}

const index: Promise<string> = promisify(readFile)('./pages/index.html', {
  encoding: 'utf8',
});
const about: Promise<string> = promisify(readFile)('./pages/about.html', {
  encoding: 'utf8',
});
const contactMe: Promise<string> = promisify(readFile)(
  './pages/contact-me.html',
  {encoding: 'utf8'},
);
const notFound: Promise<string> = promisify(readFile)('./pages/404.html', {
  encoding: 'utf8',
});
// Create a local server to receive data from
const server = http.createServer(async (req, res) => {
  let promiseResponse: Promise<ServerResponse> = Promise.resolve(res);
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
  } catch (err) {
    res.writeHead(500);
    res.end(`An error occurred when fetching the files ${err}`);
  }
  return res;
});

const port = 8000;
console.log(`Running Server at ${port}`);
server.listen(port);
