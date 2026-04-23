'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dstevx = require( './dstevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dstevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dstevx;
