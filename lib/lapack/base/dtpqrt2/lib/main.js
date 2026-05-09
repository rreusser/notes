
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtpqrt2 = require( './dtpqrt2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtpqrt2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtpqrt2;
