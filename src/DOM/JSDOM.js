"use strict";

// module DOM.JSDOM

exports._jsdom = function (html, config) {
    var JSDOM = require('jsdom').JSDOM;
    var jsdom = new JSDOM(html, config);
    return jsdom.window.document;
}
