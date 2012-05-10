function ws_perf(update, onload) {
  var host = "ws://" + document.location.host + "/http_api.yaws?api=ws_perf";
  try {
    var socket = new WebSocket(host);
    var wsid = undefined;
    socket.onopen = function(msg) {
      onload();
      wsid = setInterval(function() { socket.send(''); }, 2000);
    };
    socket.onmessage = function(msg) { update(msg); };
    socket.onclose = function(msg) { clearInterval(wsid); };
    return socket;
  } catch(ex) { console.log(ex); }
  return undefined;
};
