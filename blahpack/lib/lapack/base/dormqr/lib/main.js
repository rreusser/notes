

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dormqr = require( './dormqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormqr;
