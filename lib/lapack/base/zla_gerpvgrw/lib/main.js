'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_gerpvgrw = require( './zla_gerpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_gerpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_gerpvgrw;
