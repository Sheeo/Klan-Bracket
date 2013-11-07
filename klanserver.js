var app   = require('http').createServer(handler)
   , io   = require('socket.io').listen(app)
   , fs   = require('fs')
   , repl = require('repl');

app.listen(8080);

function handler (req, res) {
  var url = req.url;
  if(url == "/") url = "/index.html";
  fs.readFile(__dirname + url,
    function (err, data) {
      if (err) {
        res.writeHead(500);
        return res.end('Error loading ' + url);
      }
      res.writeHead(200);
      res.end(data);
    });
}

global.sockets = [];
global.io = io;
io.set('log level', 1);
io.sockets.on('connection', function (socket) {
  global.sockets.push(socket);
  socket.on('bracketLog', function (data) {
    console.log(data);
  });
  socket.on('disconnect', function() {
    global.sockets.pop(socket);
  });
});

global.sendAll = function(evt, data) {
  for(var i = 0; i < global.sockets.length; i++) {
    global.sockets[i].emit(evt,data);
  }
}

global.reload = function() {
  var res = "Reloading: ";
  for(var i = 0; i < global.sockets.length; i++) {
    res += i + " ";
    global.sockets[i].emit('reload');
  }
  return res;
};

global.newBracket = function() {
  var bracket = {};
  global.sendAll('addBracket', bracket);
  return bracket;
}

repl.start({input:process.stdin,
            output:process.stdout,
            useGlobal:true});
