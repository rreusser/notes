'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_gbrpvgrw = require( './dla_gbrpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_gbrpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_gbrpvgrw;
