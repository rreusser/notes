
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgeqrt = require( './dgeqrt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeqrt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeqrt;
