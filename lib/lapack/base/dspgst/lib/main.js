
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dspgst = require( './dspgst.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dspgst, 'ndarray', ndarray );


// EXPORTS //

module.exports = dspgst;
