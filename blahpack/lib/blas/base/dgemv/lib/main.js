

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgemv = require( './dgemv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgemv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgemv;
