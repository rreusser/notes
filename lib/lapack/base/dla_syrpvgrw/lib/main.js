'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_syrpvgrw = require( './dla_syrpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_syrpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_syrpvgrw;
