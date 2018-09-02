var updateHandlers = new Array();
var g_lastTime = 0;
var g_deltaTime = 0;
function update(){
	var curTime = new Date().getTime();
	if(g_lastTime != 0){
		g_deltaTime = curTime - g_lastTime;
	}
	g_lastTime = curTime;
	
	if(updateHandlers.length > 0){
		for (var i = 0; i < updateHandlers.length; i++) {
			updateHandlers[i](g_deltaTime);
		}
	}
}
setInterval(update, 20);

function getDeltaTime () {
	return g_deltaTime;
}

function scheduleUpdate (func) {
	updateHandlers.push(func);
}

function unscheduleUpdate(func){
	updateHandlers.remove(func);
}

Array.prototype.indexOf = function(val) {
	for (var i = 0; i < this.length; i++) {
		if (this[i] == val) return i;
		
	}
	return -1;
};
Array.prototype.remove = function(val) {
	var index = this.indexOf(val);
	if (index > -1) {
		this.splice(index, 1);
	}
};
