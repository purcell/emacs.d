/**
 * @fileOverview Live browser interaction with Emacs
 * @version 1.4
 */

/**
 * Connects to Emacs and waits for a request. After handling the
 * request it sends back the results and queues itself for another
 * request.
 * @namespace Holds all of Skewer's functionality.
 */
function skewer() {
    function callback(request) {
        var result = skewer.fn[request.type](request);
        if (result) {
            result = skewer.extend({
                id: request.id,
                type: request.type,
                status: 'success',
                value: ''
            }, result);
            skewer.postJSON(skewer.host + "/skewer/post", result, callback);
        } else {
            skewer.getJSON(skewer.host + "/skewer/get", callback);
        }
    };
    skewer.getJSON(skewer.host + "/skewer/get", callback);
}

/**
 * Get a JSON-encoded object from a server.
 * @param {String} url The location of the remote server
 * @param {Function} [callback] The callback to receive a response object
 */
skewer.getJSON = function(url, callback) {
    var XHR = window.skewerNativeXHR || XMLHttpRequest;
    var xhr = new XHR();
    xhr.onreadystatechange = function() {
        if (xhr.readyState === 4 && xhr.status === 200) {
            callback(JSON.parse(xhr.responseText));
        }
    };
    xhr.open('GET', url, true);
    xhr.send();
};

/**
 * Send a JSON-encoded object to a server.
 * @param {String} url The location of the remote server
 * @param {Object} object The object to transmit to the server
 * @param {Function} [callback] The callback to receive a response object
 */
skewer.postJSON = function(url, object, callback) {
    var XHR = window.skewerNativeXHR || XMLHttpRequest;
    var xhr = new XHR();
    xhr.onreadystatechange = function() {
        if (callback && xhr.readyState === 4 && xhr.status === 200) {
            callback(JSON.parse(xhr.responseText));
        }
    };
    xhr.open('POST', url, true);
    xhr.setRequestHeader("Content-Type", "text/plain"); // CORS
    xhr.send(JSON.stringify(object));
};

/**
 * Add the properties other objects to a target object (jQuery.extend).
 * @param {Object} target The object to receive new properties
 * @param {...Object} objects Source objects for properties
 * @returns The target object
 */
skewer.extend = function(target) {
    for (var i = 1; i < arguments.length; i++) {
        var object = arguments[i];
        for (var key in object) {
            if (object.hasOwnProperty(key)) {
                target[key] = object[key];
            }
        }
    }
    return target;
};

/**
 * Globally evaluate an expression and return the result. This
 * <i>only</i> works when the implementation's indirect eval performs
 * a global eval. If not, there's no alternative, since a return value
 * is essential.
 *
 * @see http://perfectionkills.com/global-eval-what-are-the-options/
 *
 * @param expression A string containing an expression to evaluate
 * @returns The result of the evaluation
 */
skewer.globalEval = (function() {
    var eval0 = (function(original, Object) {
        try {
            return [eval][0]('Object') === original;
        } catch (e) {
            return false;
        }
    }(Object, false));
    if (eval0) {
        return function(expression) {
            return [eval][0](expression);
        };
    } else {
        return function(expression) { // Safari
            return eval.call(window, expression);
        };
    }
}());

/**
 * Same as Date.now(), supplied for pre-ES5 JS (<=IE8).
 * @returns {number} The epoch time in milliseconds
 */
skewer.now = function() {
    return new Date().valueOf();
};

/**
 * Handlers accept a request object from Emacs and return either a
 * logical false (no response) or an object to return to Emacs.
 * @namespace Request handlers.
 */
skewer.fn = {};

/**
 * Handles an code evaluation request from Emacs.
 * @param request The request object sent by Emacs
 * @returns The result object to be returned to Emacs
 */
skewer.fn.eval = function(request) {
    var result = {
        strict: request.strict
    };
    var start = skewer.now();
    try {
        var prefix = request.strict ? '"use strict";\n' : "";
        var value = skewer.globalEval(prefix + request.eval);
        result.value = skewer.safeStringify(value, request.verbose);
    } catch (error) {
        result = skewer.errorResult(error, result, request);
    }
    result.time = (skewer.now() - start) / 1000;
    return result;
};

/**
 * Load a hosted script named by the request.
 * @param request The request object sent by Emacs
 * @returns The result object to be returned to Emacs
 */
skewer.fn.script = function(request) {
    var script = document.createElement('script');
    script.src = skewer.host + request.eval;
    document.body.appendChild(script);
    return {value: JSON.stringify(request.eval)};
};

/**
 * A keep-alive and connecton testing handler.
 * @param request The request object sent by Emacs
 * @returns The result object to be returned to Emacs
 */
skewer.fn.ping = function(request) {
    return {
        type: 'pong',
        date: skewer.now() / 1000,
        value: request.eval
    };
};

/**
 * Establish a new stylesheet with the provided value.
 */
skewer.fn.css = function(request) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.className = 'skewer';
    if (style.styleSheet) { // < IE9
        style.styleSheet.cssText = request.eval;
    } else {
        style.appendChild(document.createTextNode(request.eval));
    }
    document.body.appendChild(style);
    return {};
};

/**
 * Remove all of Skewer's style tags from the document.
 */
skewer.fn.cssClearAll = function(request) {
    var styles = document.body.querySelectorAll('style.skewer');
    for (var i = 0; i < styles.length; i++) {
        styles[i].parentNode.removeChild(styles[i]);
    }
    return {};
};

/**
 * HTML evaluator, appends or replaces a selection with given HTML.
 */
skewer.fn.html = function(request) {
    function buildSelector(ancestry) {
        return ancestry.map(function(tag) {
            return tag[0] + ':nth-of-type(' + tag[1] + ')';
        }).join(' > ');
    }
    function query(ancestry) {
        return document.querySelector(buildSelector(ancestry));
    }
    function htmlToNode(html) {
        var wrapper = document.createElement('div');
        wrapper.innerHTML = html;
        return wrapper.firstChild;
    }

    var target = query(request.ancestry);

    if (target == null) {
        /* Determine missing part of the ancestry. */
        var path = request.ancestry.slice(0);  // copy
        var missing = [];
        while (query(path) == null) {
            missing.push(path.pop());
        }

        /* Build up the missing elements. */
        target = query(path);
        while (missing.length > 0) {
            var tag = missing.pop(),
                name = tag[0],
                nth = tag[1];
            var empty = null;
            var count = target.querySelectorAll(name).length;
            for (; count < nth; count++) {
                empty = document.createElement(tag[0]);
                target.appendChild(empty);
            }
            target = empty;
        }
    }

    target.parentNode.replaceChild(htmlToNode(request.eval), target);
    return {};
};

/**
 * Fetch the HTML contents of selector.
 */
skewer.fn.fetchselector = function(request) {
    var element = document.querySelector(request.eval);
    return { value: element.innerHTML };
};

/**
 * Return a list of completions for an object.
 */
skewer.fn.completions = function(request) {
    var object = skewer.globalEval(request.eval);
    var keys = new Set();
    var regex = new RegExp(request.regexp);
    for (var key in object) {
        if (regex.test(key)) {
            keys.add(key);
        }
    }
    var props = object != null ? Object.getOwnPropertyNames(object) : [];
    for (var i = 0; i < props.length; i++) {
        if (regex.test(props[i])) {
            keys.add(props[i]);
        }
    }
    return { value: Array.from(keys).sort() };
};

/**
 * Host of the skewer script (CORS support).
 * @type string
 */
(function() {
    var script = document.querySelector('script[src$="/skewer"]');
    if (script) {
        skewer.host = script.src.match(/\w+:\/\/[^/]+/)[0];
    } else {
        skewer.host = '';  // default to the current host
    }
}());

/**
 * Stringify a potentially circular object without throwing an exception.
 * @param object The object to be printed.
 * @param {boolean} verbose Enable more verbose output.
 * @returns {string} The printed object.
 */
skewer.safeStringify = function (object, verbose) {
    "use strict";
    var circular = "#<Circular>";
    var seen = [];

    var stringify = function(obj) {
        if (obj === true) {
            return "true";
        } else if (obj === false) {
            return "false";
        } else if (obj === undefined) {
            return "undefined";
        } else if (obj === null) {
            return "null";
        } else if (typeof obj === "number") {
            return obj.toString();
        } else if (obj instanceof Array) {
            if (seen.indexOf(obj) >= 0) {
                return circular;
            } else {
                seen.push(obj);
                return "[" + obj.map(function(e) {
                    return stringify(e);
                }).join(", ") + "]";
            }
        } else if (typeof obj === "string") {
            return JSON.stringify(obj);
        } else if (window.Node != null && obj instanceof Node) {
            return obj.toString();  // DOM elements can't stringify
        } else if (typeof obj === "function") {
            if (verbose)
                return obj.toString();
            else
                return "Function";
        } else if (Object.prototype.toString.call(obj) === '[object Date]') {
            if (verbose)
                return JSON.stringify(obj);
            else
                return obj.toString();
        } else {
            if (verbose) {
                if (seen.indexOf(obj) >= 0)
                    return circular;
                else
                    seen.push(obj);
                var pairs = [];
                for (var key in obj) {
                    if (obj.hasOwnProperty(key)) {
                        var pair = JSON.stringify(key) + ":";
                        pair += stringify(obj[key]);
                        pairs.push(pair);
                    }
                }
                return "{" + pairs.join(',') + "}";
            } else {
                try {
                    return obj.toString();
                } catch (error) {
                    return ({}).toString();
                }
            }
        }
    };

    try {
        return stringify(object);
    } catch (error) {
        return skewer.safeStringify(object, false);
    }
};

/**
 * Log an object to the Skewer REPL in Emacs (console.log).
 * @param message The object to be logged.
 */
skewer.log = function() {
    "use strict";
    for (var i = 0; i < arguments.length; i++) {
        var log = {
            type: "log",
            value: skewer.safeStringify(arguments[i], true)
        };
        skewer.postJSON(skewer.host + "/skewer/post", log);
    }
};

/**
 * Report an error event to the REPL.
 * @param event An error event object.
 */
skewer.error = function(event) {
    "use strict";
    var log = {
        type: "error",
        value: event.message,
        filename: event.filename,
        line: event.lineno,
        column: event.column
    };
    skewer.postJSON(skewer.host + "/skewer/post", log);
};

/**
 * Prepare a result when an error occurs evaluating Javascript code.
 * @param error The error object given by catch.
 * @param result The resutl object to return to Emacs.
 * @param request The request object from Emacs.
 * @return The result object to send back to Emacs.
 */
skewer.errorResult = function(error, result, request) {
    "use strict";
    return skewer.extend({}, result, {
        value: error.toString(),
        status: 'error',
        error: {
            name: error.name,
            stack: error.stack,
            type: error.type,
            message: error.message,
            eval: request.eval
        }
    });
};

if (window.addEventListener) {
    window.addEventListener('error', skewer.error);
    if (document.readyState === 'complete') {
        skewer();
    } else {
        window.addEventListener('load', skewer);
    }
} else { // < IE9
    window.attachEvent('onerror', skewer.error);
    if (document.readyState === 'complete') {
        skewer();
    } else {
        window.attachEvent('onload', skewer);
    }
}
