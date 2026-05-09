
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgemqrt = require( './dgemqrt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgemqrt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgemqrt;
