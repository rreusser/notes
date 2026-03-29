'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_gerpvgrw = require( './dla_gerpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_gerpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_gerpvgrw;
