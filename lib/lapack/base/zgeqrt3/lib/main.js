
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgeqrt3 = require( './zgeqrt3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeqrt3, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeqrt3;
