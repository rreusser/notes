

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgemm = require( './dgemm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgemm, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgemm;
