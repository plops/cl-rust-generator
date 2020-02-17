// https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Your_first_WebExtension
// about:debugging 'this firefox' load temporary add on
// https://stackoverflow.com/questions/48450230/firefox-webextension-api-downloads-not-working/48456109

browser.runtime.sendMessage({"bg_task":"started"});

console.log('start listening for urls');
browser.runtime.onMessage.addListener(notify);




function notify(message) {
    console.log('receive urls');
    urls = message.urls;
    for(var i=0; i<urls.length; i++) {
	a = urls[i];
	url = new URL(a);
	
	fn = url.pathname.replace(/\//g,"_");
	console.log(fn)
	var downloading = browser.downloads.download({
	    url : a,
	    filename : fn,
	    conflictAction : 'uniquify'
	});
    }
}
