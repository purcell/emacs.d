// ==UserScript==
// @name         Skewer Everything
// @description  Add a toggle button to run Skewer on the current page
// @lastupdated  2015-09-14
// @version      1.3
// @license      Public Domain
// @include      /^https?:///
// @grant        none
// @run-at       document-start
// ==/UserScript==

window.skewerNativeXHR = XMLHttpRequest;
window.skewerInject = inject;

var host = 'http://localhost:8080';

var toggle = document.createElement('div');
toggle.onclick = inject;
toggle.style.width = '0px';
toggle.style.height = '0px';
toggle.style.borderStyle = 'solid';
toggle.style.borderWidth = '0 12px 12px 0';
toggle.style.borderColor = 'transparent #F00 transparent transparent';
toggle.style.position = 'absolute';
toggle.style.right = 0;
toggle.style.top = 0;
toggle.style.zIndex = 214748364;

var injected = false;

function inject() {
    if (!injected) {
        var script = document.createElement('script');
        script.src = host + '/skewer';
        document.body.appendChild(script);
        toggle.style.borderRightColor = '#0F0';
    } else {
        /* break skewer to disable it */
        skewer.fn = null;
        toggle.style.borderRightColor = '#F00';
    }
    injected = !injected;
    localStorage._autoskewered = JSON.stringify(injected);
}

document.addEventListener('DOMContentLoaded', function() {
    /* Don't use on iframes. */
    if (window.top === window.self) {
        document.body.appendChild(toggle);
        if (JSON.parse(localStorage._autoskewered || 'false')) {
            inject();
        }
    }
});
