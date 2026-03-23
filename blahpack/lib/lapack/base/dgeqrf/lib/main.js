

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgeqrf = require( './dgeqrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeqrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeqrf;
