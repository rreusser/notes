'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetrs = require( './zhetrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetrs;
