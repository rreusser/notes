'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlasd2 = require( './dlasd2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasd2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasd2;
