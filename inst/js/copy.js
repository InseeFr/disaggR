// the event handler listens to shiny for messages send by handler1
// if it receives a message, call the callback function doAwesomething and pass the message
Shiny.addCustomMessageHandler("copy", copy );

// this function is called by the handler, which passes the message
function copy(text) {
    var input = document.createElement('textarea');
    input.innerHTML = text;
    document.body.appendChild(input);
    input.select();
    document.execCommand('copy');
    document.body.removeChild(input);
	
    alert("Copied !")
}