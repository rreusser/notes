
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dswap = require( './dswap.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dswap, 'ndarray', ndarray );


// EXPORTS //

module.exports = dswap;
