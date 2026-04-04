
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgbsvx = require( './dgbsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbsvx;
