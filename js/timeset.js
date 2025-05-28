var currentDate = new Date();

// load the wasm module
var script = document.createElement('script');
script.src = 'js/time.js';
document.head.appendChild(script);
var curtime;

var divt = document.getElementById('HaYiTime');
divt.innerHTML = "HaYi Time";

var loct = document.getElementById('LocalTime');
loct.innerHTML = "Local Time";

script.onload = () => {
    Module.onRuntimeInitialized = () => {
        
		curtime = Module.cwrap('curtime', null, ['number']);
    };
};

var i = 0;

var checkWasmReady = setInterval(() => {
    if (curtime) {
		currentDate = new Date();
		const year = currentDate.getFullYear();
		const month = currentDate.getMonth() + 1;
		const day = currentDate.getDate();
		const hours = currentDate.getHours();
		const minutes = currentDate.getMinutes();
		const seconds = currentDate.getSeconds();
		loct.innerHTML = "<p> Local Time: </p>" +
						 "<p>" + year + "/" + month  + "/" + day+
						 "<p>" + hours + ":" + minutes  + ":" + seconds;

		var oup = Module._malloc(50);
		curtime(oup);
		var outputString = Module.UTF8ToString(oup);
		if(outputString.charAt(outputString.length - 3) == ':')
			outputString = outputString.substring(0, outputString.length - 2) + '0' + outputString.charAt(outputString.length - 2);
		outputString = "<p> HaYi Time: </p>" +
					   "<p>" + outputString.substring(0, outputString.indexOf(' ')) + "</p>" + 
			           "<p>" + outputString.substring(outputString.indexOf(' ')+1) + "</p>";
		divt.innerHTML = outputString;
	        

        Module._free(oup);
    } else {
		if(i == 0) i = 1;
		else i*=16;
    }
}, 0);

