
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsbgvx = require( './dsbgvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsbgvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsbgvx;
