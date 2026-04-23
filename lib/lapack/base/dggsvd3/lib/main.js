'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dggsvd3 = require( './dggsvd3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dggsvd3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dggsvd3;
