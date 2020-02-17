// https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Your_first_WebExtension
// about:debugging 'this firefox' load temporary add on

browser.runtime.onMessage.addListener(wait_for_bg_task);

function wait_for_bg_task(message) {
    console.log('bg task notified content script');
}


var arr = [], l = document.links;
for(var i=0; i<l.length; i++) {
    if(l[i].href.endsWith('.txt')) {
	url = l[i].href;
	console.log(url)
	arr.push(url);
    }
}

console.log('send urls');
browser.runtime.sendMessage({"urls": arr});
