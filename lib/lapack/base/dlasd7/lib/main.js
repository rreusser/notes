'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlasd7 = require( './dlasd7.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasd7, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasd7;
