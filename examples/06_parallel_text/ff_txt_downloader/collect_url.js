// https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Your_first_WebExtension
// about:debugging 'this firefox' load temporary add on

var arr = [], l = document.links;
for(var i=0; i<l.length; i++) {
    if(l[i].href.endsWith('.txt')) {
	arr.push(l[i].href);
    }
}

var downloading = browser.downloads.download({
    url : arr[0],
    filename : '/dev/shm/my-image-again.txt',
    conflictAction : 'uniquify'
});
