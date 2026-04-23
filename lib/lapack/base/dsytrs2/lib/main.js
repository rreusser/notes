'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsytrs2 = require( './dsytrs2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytrs2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytrs2;
