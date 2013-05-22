var docFrame;
var logtextbox;
var destFile;
var embedFonts = false;
var finalMathJaxURL = null;

function log(text)
{
	logtextbox.setAttribute("value", logtextbox.getAttribute("value") + "\n" + text);
}

function init()
{
	try {
		docFrame = document.getElementById("docFrame");
		logtextbox = document.getElementById("logtextbox");

		// parse command line arguments
		var cmdLine = window.arguments[0];
		cmdLine = cmdLine.QueryInterface(Components.interfaces.nsICommandLine);
		
		embedFonts = cmdLine.handleFlag("embed-fonts", false);
		finalMathJaxURL = cmdLine.handleFlagWithParam("final-mathjax-url", false);
		
		if (!embedFonts && !finalMathJaxURL) {
			alert("You must eiher specify --embed-fonts or --final-mathjax-url");
			window.close();
			return;
		}
		
		sourceFilePath = cmdLine.getArgument(0);
		destFilePath = cmdLine.getArgument(1);
		if ( !sourceFilePath || !destFilePath ) {
			alert("Not enough parameters, expecting two arguments:\nInput file, output file");
			window.close();
			return;
		}
		
		sourceFile = cmdLine.resolveFile(sourceFilePath);
		if (! (sourceFile.exists() && sourceFile.isFile()) ) {
			alert("Invalid source file path.");
			window.close();
			return;
		}
		sourceURI = cmdLine.resolveURI(sourceFilePath);
		
		// create a nsIFile object for the output file
		try{
			destFile = cmdLine.resolveURI(destFilePath).QueryInterface(Components.interfaces.nsIFileURL).file;
		}catch(e){
			alert("Invalid destination file.\n\nException:\n" + e);
			window.close();
			return;
		}
		
		// add iframeLoaded() as an onload event handler, then navigate to the source file
		docFrame.addEventListener("DOMContentLoaded", iframeLoaded, true);
		docFrame.setAttribute("src", sourceURI.spec);

	} catch (e) {
		alert("Error in init():\n\n" + e);
		window.close();
		return;
	}
}

function iframeLoaded()
{
	/*
	// print every MathJax signal to the log
	docFrame.contentWindow.MathJax.Hub.Startup.signal.Interest(
	function (message) {log("Startup: "+message)}
	);
	docFrame.contentWindow.MathJax.Hub.signal.Interest(
	function (message) {log("Hub: "+message)}
	);
	*/

	// tell MathJax to call serialize() when finished
	docFrame.contentWindow.MathJax.Hub.Register.StartupHook("End", function() {serialize();});
}

function fileURLtoDataURI(url)
{
	var ios = Components.classes["@mozilla.org/network/io-service;1"]
		.getService(Components.interfaces.nsIIOService);
	var url_object = ios.newURI(url, "", null);
	var file = url_object.QueryInterface(Components.interfaces.nsIFileURL).file;
								
	var data = "";  
	var fstream = Components.classes["@mozilla.org/network/file-input-stream;1"].  
		createInstance(Components.interfaces.nsIFileInputStream);
	fstream.init(file, -1, -1, false);
	var bstream = Components.classes["@mozilla.org/binaryinputstream;1"].  
		createInstance(Components.interfaces.nsIBinaryInputStream);  
	bstream.setInputStream(fstream);  
				
	var bytes = bstream.readBytes(bstream.available());
	b64bytes = btoa(bytes);

	return "data:;base64," + b64bytes;

}

function serialize()
{
	var MathJaxURL = docFrame.contentWindow.MathJax.Hub.config.root;

	var searchURIList = new Array();
	var replacementURIList = new Array();
	
	log("serialize: preprocessing");

	// remove the MathJax status message window
	msgdiv = docFrame.contentDocument.getElementById("MathJax_Message");
	msgdiv.parentNode.removeChild(msgdiv);
	
	/* Loop through all CSS rules to find all @font-face rules.
	   At this point, they refer to local absolute paths using file:// URLs.
	   Replace them either with appropriate URLs relative to finalMathJaxURL
	   or with data URIs. */
	
	for (var i = 0; i<docFrame.contentDocument.styleSheets.length; i++) {
		var stylesheet = docFrame.contentDocument.styleSheets[i];
		
		for (var j=0; j< stylesheet.cssRules.length; j++) {
			var rule = stylesheet.cssRules[j];
			if (rule.cssText.match("font-face")) {

				url = rule.style.getPropertyValue("src");
				url = url.match(/url\(\"(.+)\"\)/)[1];
				
				// Since the properties seem read-only here, we populate
				// searchURIList and replacementURIList to do text substitution
				// after serialization
				searchURIList.push(url);
				if (embedFonts) {
					replacementURIList.push(fileURLtoDataURI(url));
				} else {
					replacementURIList.push(url.replace(MathJaxURL, finalMathJaxURL));
				}				
			}
		}
	}


	// find and remove the MathJax <script> tag
	try{
		var scriptTags = docFrame.contentDocument.getElementsByTagName("script");
		for (var i=0; i<scriptTags.length; i++) {
			if (scriptTags[i].getAttribute("src") && scriptTags[i].getAttribute("src").match(/MathJax.js/i))
				scriptTags[i].parentNode.removeChild(scriptTags[i]);
		}
	}catch(e){alert(e);}

	log("serialize: serializing");

	var serializer = new XMLSerializer();
	var xhtml = serializer.serializeToString(docFrame.contentDocument);
	
	log("serialize: postprocessing");
	// make the MathJax URL relative again
	//	xhtml = xhtml.replace(findMathJaxURL, "MathJax");
	
	try{
		r1 = RegExp("&lt;!--/\\*--&gt;&lt;!\\[CDATA\\[/\\*&gt;&lt;!--\\*/", "g");
		xhtml = xhtml.replace(r1, "");
		r2 = RegExp("/\\*\\]\\]&gt;\\*/--&gt;", "g");
		xhtml = xhtml.replace(r2, "");
		r3 = RegExp("/\\*\\]\\]&gt;\\*///--&gt;", "g");
		xhtml = xhtml.replace(r3, "");
	}catch(e){alert(e);}
	for (var i=0; i<searchURIList.length; i++)
		xhtml = xhtml.replace(searchURIList[i], replacementURIList[i]);
	
	save(xhtml);
	window.close();
}

function save(xhtml)
{
	try {
		var foStream = Components.classes["@mozilla.org/network/file-output-stream;1"].
			createInstance(Components.interfaces.nsIFileOutputStream);

		foStream.init(destFile, 0x02 | 0x08 | 0x20, 0666, 0); 
		// write, create, truncate

		// write in UTF-8 encoding
		var converter = Components.classes["@mozilla.org/intl/converter-output-stream;1"].
			createInstance(Components.interfaces.nsIConverterOutputStream);
		converter.init(foStream, "UTF-8", 0, 0);
		converter.writeString(xhtml);
		converter.close(); // this closes foStream
	} catch (e) {
		alert("Error in save():\n\n" + e);
	}
}
