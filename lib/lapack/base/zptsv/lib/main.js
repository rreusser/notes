'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zptsv = require( './zptsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zptsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zptsv;
