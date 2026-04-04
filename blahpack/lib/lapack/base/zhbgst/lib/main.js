
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhbgst = require( './zhbgst.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhbgst, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhbgst;
