
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgeqrt2 = require( './dgeqrt2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeqrt2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeqrt2;
