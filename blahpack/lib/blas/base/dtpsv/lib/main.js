'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtpsv = require( './dtpsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtpsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtpsv;
