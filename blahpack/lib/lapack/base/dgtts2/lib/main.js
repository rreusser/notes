'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgtts2 = require( './dgtts2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgtts2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgtts2;
