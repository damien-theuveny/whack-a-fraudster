"use strict"

process.title = 'node-chat';

var webSocketsServerPort = 1337;
var WebSocketServer = require('websocket').server;
var http = require('http');

var history = [];
var clients = [];
var readyList = [];


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
  console.log((new Date()) + ' Connection accepted.');

  for (var i=0; i < clients.length; i++) {
    clients[i].sendUTF((JSON.stringify({
      type: 'connections',
      data: clients.length
    })));
  }

  // var json = JSON.stringify({ type:'successfulConnection', data: {} });
  // connection.send(json);

  // This is the most important callback for us, we'll handle
  // all messages from users here.
  connection.on('message', function(data) {
    var parsedData = JSON.parse(data.utf8Data);
    // first message sent by user is their name
    if(parsedData.type === "name") {
      userName = parsedData.name
      connection.sendUTF(JSON.stringify({
        type: 'registration',
        data: index === 0
      }));
      console.log("registration", userName, index, clients.length);
    }

    if(parsedData.type === "playerReady") {
      readyList.push(userName);
      for (var i=0; i < clients.length; i++) {
        clients[i].sendUTF(JSON.stringify({
          type: 'updateReadyList',
          data: readyList.length
        }));
      };
      console.log("ready", userName, index, clients.length);
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

  });

  // user disconnected
  connection.on('close', function(connection) {
    console.log((new Date()) + " Peer " + connection.remoteAddress + " disconnected.");
    // remove user from the list of connected clients
    clients.splice(index, 1);
    for (var i=0; i < clients.length; i++) {
      clients[i].sendUTF(JSON.stringify({
        type: 'connections',
        data: clients.length
      }));
    };
  });
});
