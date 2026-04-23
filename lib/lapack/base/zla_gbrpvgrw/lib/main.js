'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_gbrpvgrw = require( './zla_gbrpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_gbrpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_gbrpvgrw;
