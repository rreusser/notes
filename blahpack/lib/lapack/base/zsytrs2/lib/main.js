'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsytrs2 = require( './zsytrs2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytrs2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytrs2;
