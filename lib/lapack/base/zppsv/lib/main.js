
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zppsv = require( './zppsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zppsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zppsv;
