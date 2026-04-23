'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dggqrf = require( './dggqrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dggqrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dggqrf;
