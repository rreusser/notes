'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgtsvx = require( './dgtsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgtsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgtsvx;
