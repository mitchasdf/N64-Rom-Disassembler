
// Put me into the PJ64d scripts directory please

server = new Server({'port': 8329});
ints = {};
logging = false;

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

var log = function() {
    logging = !logging;
    console.log('logging = ' + logging);
};

console.log('You can now simply apply patches with Ctrl+P in the disassembler.');
console.log('Use log() to toggle task logging (default: disabled)');
