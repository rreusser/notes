

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgbtrs = require( './zgbtrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbtrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbtrs;
