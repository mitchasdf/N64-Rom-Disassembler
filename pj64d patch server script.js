
socket = new Socket();
server = new Server({'port': 8329});
settings = new Object();
settings['host'] = '127.0.0.1';
settings['port'] = 8319;
ints = new Object();
logging = false;

// try {
socket.connect(settings, function() {
    console.log('Successfully connected.');
    console.log('Use log() to toggle task logging. Disabled by default.');
});

server.on('connection', function(sock) {
    sock.on('data', function(data) {
        data = String(data);
        var cut_param = function() {
            ind = data.indexOf('\n');
            if (ind === -1) {
                ind = data.length;
            }
            param = data.substr(0, ind);
            data = data.substr(ind + 1, data.length);
            return param;
        };
        while (data) {
            address = cut_param();
            value = parseInt(cut_param());
            text = cut_param();
            ints[address] = value;
            address = parseInt(address);
            mem.u32[address] = value;
            if (logging) {
                console.log(text);
            }
        }
    });
});


// } catch(e) {
//     console.log('asdf');
// }
//
var log = function() {
    logging = !logging;
    console.log('logging = ' + logging);
};

var send = function(addr, val) {
    console.log('sending ' + addr + ' ' + val)
    socket.write(addr + '\n' + val, function(response) {
        if (logging) {
            console.log(response);
        }
    });
    // Will recieve data to print which instruction was written
};


// I think this method would stop a lot of unnecessary data transfer between programs
//   at the cost of a few MB of RAM.
//var v = 0;
//events.onexec(ADDR_ANY, function(addr) {
//    key = addr.toString();
//    this_val = mem.u32[addr];
//    if (key in ints) {
//       if (this_val != ints[key]) {
//           send(addr, this_val);
//       }
//    }
//    ints[key] = this_val;
//    if (v++ < 100) {
//        console.log(key);
//        console.log(ints[key]);
//    }
//});
