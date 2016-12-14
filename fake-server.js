
var jsonServer = require('json-server')
var server = jsonServer.create()
var router = jsonServer.router('db.json')
var middlewares = jsonServer.defaults()

server.use(jsonServer.bodyParser)
server.use(function (req, res, next) {
  if (req.method === 'POST') {
    console.log('xxx POST ', req.url);
    console.log('xxx POST body ', req.body);

    const urlParts = req.url.split('/').filter(s => s.length != 0);

    if (urlParts[0] === 'dojos' && urlParts[2] === 'teams') {
      console.log('xxx Create new team ');
      req.body = {
        "name": req.body.teamName,
        "descr": "ver 0.100",
        "points": 0,
        "members": [
          {
            "name": req.body.captainName,
            "status": "captain"
          }
        ]
      };
    }
  }
  next();
})


server.use(middlewares)
server.use(router)
server.listen(3000, function () {
  console.log('JSON Server is running on port 3000')
})
