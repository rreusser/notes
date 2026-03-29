'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_porpvgrw = require( './dla_porpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_porpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_porpvgrw;
