'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dstevr = require( './dstevr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dstevr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dstevr;
