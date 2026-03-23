

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgeqr2 = require( './dgeqr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeqr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeqr2;
