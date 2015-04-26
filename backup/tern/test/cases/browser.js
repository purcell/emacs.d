// environment=browser

window.document.body; //: Element

var newElt = document.createElement("div"); //: Element

newElt.style.border; //: string

var e_which;
window.addEventListener("mousemove", function(e) { e_which = e.which; });
e_which; //: number
