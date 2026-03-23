

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorgqr = require( './dorgqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgqr;
