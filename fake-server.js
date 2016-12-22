
var jsonServer = require('json-server')
var server = jsonServer.create()
var router = jsonServer.router('db.json')
var middlewares = jsonServer.defaults()

server.use(jsonServer.bodyParser)
server.use(function (req, res, next) {
  if (req.method === 'POST') {
    const urlParts = req.url.split('/').filter(s => s.length != 0);

    if (urlParts[0] === 'dojos' && urlParts[2] === 'teams') {
      console.log('xxx Create new team ');
      req.body = {
        "name": req.body.teamName,
        "captain":  {
          "name": req.body.captainName,
          "fullname": req.body.captainFullname,
          "picture": req.body.captainPicture,
          "status": "crew"
        },
        "descr": "ver 0.100",
        "points": 0,
        "joinUrl": "http://localhost:3000/members"
      };
    }
  } // if req.method === 'POST'
  next();
})

router.render = (req, res) => {

  var handled = false;
  if (req.method === 'GET') {
    const urlParts = req.url.split('/').filter(s => s.length != 0);

    if (urlParts[0] === 'teams') {
      var teams = res.locals.data;
      teams.forEach(team => {
        team.objectId = team.id.toString();
        if (team.members) {
          team.members.forEach(teamMember => {
            teamMember.selfUrl = "http://localhost:3000/members/" + teamMember.id;
          })
        }
      });

      res.jsonp(teams)
      handled = true;
    }

    if (urlParts[0] === 'dojos') {
      var dojos = res.locals.data;
      dojos.forEach(dojo => {
        dojo.objectId = dojo.id.toString();
      });

      res.jsonp(dojos)
      handled = true;
    }




  } //if (req.method === 'GET')

  if (!handled) {
    res.jsonp(res.locals.data)
  }

}


server.use(middlewares)
server.use(router)
server.listen(3000, function () {
  console.log('JSON Server is running on port 3000')
})
