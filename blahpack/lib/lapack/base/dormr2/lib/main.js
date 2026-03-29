'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dormr2 = require( './dormr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormr2;
