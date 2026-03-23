

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsteqr = require( './dsteqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsteqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsteqr;
