
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zggqrf = require( './zggqrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zggqrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zggqrf;
