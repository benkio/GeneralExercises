import fetch from "node-fetch";
import http from "http";

const [homePage, style, logo, logic] = await Promise.all([
  fetch(
    "https://raw.githubusercontent.com/john-smilga/node-express-course/refs/heads/main/02-express-tutorial/navbar-app/index.html",
  ),
  fetch(
    "https://raw.githubusercontent.com/john-smilga/node-express-course/refs/heads/main/02-express-tutorial/navbar-app/styles.css",
  ),
  fetch(
    "https://raw.githubusercontent.com/john-smilga/node-express-course/refs/heads/main/02-express-tutorial/navbar-app/logo.svg",
  ),
  fetch(
    "https://raw.githubusercontent.com/john-smilga/node-express-course/refs/heads/main/02-express-tutorial/navbar-app/browser-app.js",
  ),
])
  .then((responses) => {
    const promises = responses.map((response) => {
      if (!response.ok) {
        throw new Error(`HTTP error, status = ${response.status}`);
      }
      return response.text();
    });
    return Promise.all(promises);
  })
  .catch((error) => {
    console.error(`Error while retrieving file from github: ${error}`);
  });

const server = http.createServer((req, res) => {
  switch (req.url) {
    case "/":
      res.writeHead(200, { "content-type": "text/html" });
      res.write(homePage);
      res.end();
      break;
    case "/styles.css":
      res.writeHead(200, { "content-type": "text/css" });
      res.write(style);
      res.end();
      break;
    case "/browser-app.js":
      res.writeHead(200, { "content-type": "text/javascript" });
      res.write(logic);
      res.end();
      break;
    case "/logo.svg":
      res.writeHead(200, { "content-type": "image/svg+xml" });
      res.write(logo);
      res.end();
      break;
    case "/about":
      res.writeHead(200, { "content-type": "text/html" });
      res.write("<h1>About Page</h1>");
      res.end();
      break;
    default:
      res.writeHead(200, { "content-type": "text/html" });
      res.write("<h1>404, Resource Not Found</h1>");
      res.end();
  }
});

server.listen(5000, () => {
  console.log("Server listening at port 5000");
});
