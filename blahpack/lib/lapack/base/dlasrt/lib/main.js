

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasrt = require( './dlasrt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasrt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasrt;
