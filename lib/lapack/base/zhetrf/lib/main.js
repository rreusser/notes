'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetrf = require( './zhetrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetrf;
