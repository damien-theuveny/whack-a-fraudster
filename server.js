"use strict"

process.title = 'whack-a-fraudster';

var webSocketsServerPort = 1337;
var WebSocketServer = require('websocket').server;
var http = require('http');
var express = require('express');
var app = express();

var serverPort = process.env.PORT || 3000;

app.set('port', serverPort);

app.get('/', function (req,res) {
  res.sendFile(__dirname + '/index.html');
});

app.get('/elm.js', function (req,res) {
  res.sendFile(__dirname + '/elm.js');
});

app.get('/styles.css', function (req,res) {
  res.sendFile(__dirname + '/styles.css');
});

app.get('/hack-cambridge-dist-webtag.js', function (req,res) {
  res.sendFile(__dirname + '/hack-cambridge-dist-webtag.js');
});

app.get('/fonts/TitilliumWeb-Regular.ttf', function (req,res) {
  res.sendFile(__dirname + '/fonts/TitilliumWeb-Regular.ttf');
});

app.get('/fonts/TitilliumWeb-Bold.ttf', function (req,res) {
  res.sendFile(__dirname + '/fonts/TitilliumWeb-Bold.ttf');
});

app.get('/bg.jpg', function (req,res) {
  res.sendFile(__dirname + '/bg.jpg');
});

app.get('/favicon.png', function (req,res) {
  res.sendFile(__dirname + '/favicon.png');
});

app.get('/favicon.ico', function (req,res) {
  res.sendFile(__dirname + '/favicon.png');
});

app.get('/373611.png', function (req,res) {
  res.sendFile(__dirname + '/373611.png');
});

app.get('/373596.png', function (req,res) {
  res.sendFile(__dirname + '/373596.png');
});

app.get('/373720.png', function (req,res) {
  res.sendFile(__dirname + '/373720.png');
});


var history = [];
var clients = [];
var clientNames = [];
var readyList = [];
var history = {};

var colours = ['red', 'green', 'blue', 'magenta', 'purple', 'plum', 'orange'];
colours.sort(function(a,b) {
  return Math.random() > 0.5;
});

var server = http.createServer(function(request, response) {
  // process HTTP request. Since we're writing just WebSockets
  // server we don't have to implement anything.
});
server.listen(webSocketsServerPort, function() {
  console.log((new Date()) + " Server is listening on port " + webSocketsServerPort);
});

// create the server
var wsServer = new WebSocketServer({
  httpServer: server
});

// WebSocket server
wsServer.on('request', function(request) {
  console.log((new Date()) + ' Connection from origin ' + request.origin + '.');

  // if(request.origin === "http://")
  var connection = request.accept(null, request.origin);

  // we need to know client index to remove them on 'close' event
  var index = clients.push(connection) - 1;
  var userName = false;
  var userColor = false;
  var screensize = false;
  var lastLocation;
  clientNames.push({name: "anonymous", index: index});
  console.log((new Date()) + ' Connection accepted.');

  // for (var i=0; i < clients.length; i++) {
  //   clients[i].sendUTF((JSON.stringify({
  //     type: 'connections',
  //     data: clientNames
  //   })));
  // }

  // all messages from users here.
  connection.on('message', function(data) {
    var parsedData = JSON.parse(data.utf8Data);
    // first message sent by user is their name
    if(parsedData.type === "name") {
      var alreadyTaken = clientNames.find(function(element) {
        return element.name === parsedData.name;
      });
      if(alreadyTaken || parsedData.name.length === 0) {
        connection.sendUTF(JSON.stringify({ type: 'invalidName' }));
      } else {
        userName = parsedData.name;
        userColor = colours.shift();
        clientNames[index] = { name: parsedData.name, colour: userColor, screensize: screensize, lastLocation: lastLocation };
        connection.sendUTF(JSON.stringify({
          type: 'registration',
          data: {
            lead: index === 0,
            colour: userColor
          }
        }));
        for (var i=0; i < clients.length; i++) {
          clients[i].sendUTF(JSON.stringify({
            type: 'clientNames',
            data: clientNames
          }));
        };
        console.log("registration", userName, index, clients.length, clientNames);
      }
    }

    if(parsedData.type === "fingerprint") {
      var fingerprint = JSON.parse(parsedData.data)["9"]["3"].split("x");
      screensize = { width: parseInt(fingerprint[0]), height: parseInt(fingerprint[1])};
      // console.log(clientNames);
    }

    if(parsedData.type === "resetServer") {
      clients = [];
      clientNames = [];
      readyList = [];

      colours = ['red', 'green', 'blue', 'magenta', 'purple', 'plum', 'orange'];
    }

    if(parsedData.type === "mouseMove") {
      var movementData = JSON.parse(parsedData.data)[0];
      if(typeof lastLocation === "undefined") {
        lastLocation = { x: movementData.x, y: movementData.y };
      } else {
        lastLocation = { x: (lastLocation.x + movementData.x), y: (lastLocation.y + movementData.y)}
      }
      if(clientNames[index] && typeof clientNames[index].lastLocation !== "undefined") {
        clientNames[index].lastLocation = lastLocation;
        // console.log(clientNames[index], lastLocation);
        for (var i=0; i < clients.length; i++) {
          if(i !== index) {
            clients[i].sendUTF(JSON.stringify({
              type: 'clientNames',
              data: clientNames
            }));
          }
        };
      }
    }

    if(parsedData.type === "playerReady") {
      var alreadyTaken = readyList.find(function(element) {
        return element === userName;
      });
      if(!alreadyTaken) {
        readyList.push(userName);
        for (var i=0; i < clients.length; i++) {
          clients[i].sendUTF(JSON.stringify({
            type: 'updateReadyList',
            data: readyList.length
          }));
        };
        console.log("ready", userName, index, clients.length);
      }
    }

    if(parsedData.type === "notifyAllStart") {
      for (var i=0; i < clients.length; i++) {
        clients[i].sendUTF(JSON.stringify({
          type: 'startGame',
          data: readyList.length
        }));
      };
      console.log("start", userName, index, clients.length);
    }

    if(parsedData.type === "gridContents") {
      for (var i=0; i < clients.length; i++) {
        clients[i].sendUTF(JSON.stringify({
          type: 'sharingGridContents',
          data: parsedData.data
        }));
      };
      // console.log("gridContents", userName, index, clients.length);
    }

    if(parsedData.type === "clickBox") {
      for (var i=0; i < clients.length; i++) {
        clients[i].sendUTF(JSON.stringify({
          type: 'sharingClickBox',
          data: parsedData.data
        }));
      };
    }

    if(parsedData.type === "notifyAllLevel") {
      for (var i=0; i < clients.length; i++) {
        clients[i].sendUTF(JSON.stringify({
          type: 'sharingLevel',
          data: parsedData.data
        }));
      };
    }

    if(parsedData.type === "endGameSignal") {
      for (var i=0; i < clients.length; i++) {
        clients[i].sendUTF(JSON.stringify({
          type: 'sharingEndGame'
        }));
      };
    }

  });

  // user disconnected
  connection.on('close', function(connection) {
    console.log((new Date()) + " Peer " + (JSON.stringify(clientNames[index])) + " disconnected.");
    // remove user from the list of connected clients
    //replace this if else with single if using an OR
    if(clientNames[index] && clientNames[index].name === userName) {
      clients.splice(index, 1);
      clientNames.splice(index, 1);
    } else if(clientNames[index] && clientNames[index].name === "anonymous" && clientNames[index].index === index) {
      clients.splice(index, 1);
      clientNames.splice(index, 1);
    }
    for (var i=0; i < clients.length; i++) {
      clients[i].sendUTF(JSON.stringify({
        type: 'connections',
        data: clientNames
      }));
    };
    console.log(clientNames);
  });
});

app.listen(serverPort, function() {
  console.log("Listening on port " + serverPort);
})
