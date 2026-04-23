'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetrs2 = require( './zhetrs2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetrs2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetrs2;
