'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_syrpvgrw = require( './zla_syrpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_syrpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_syrpvgrw;
