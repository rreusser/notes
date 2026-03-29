'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaln2 = require( './dlaln2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaln2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaln2;
