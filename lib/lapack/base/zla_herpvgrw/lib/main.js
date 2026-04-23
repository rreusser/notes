'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_herpvgrw = require( './zla_herpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_herpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_herpvgrw;
