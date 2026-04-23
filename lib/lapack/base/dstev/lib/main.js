'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dstev = require( './dstev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dstev, 'ndarray', ndarray );


// EXPORTS //

module.exports = dstev;
