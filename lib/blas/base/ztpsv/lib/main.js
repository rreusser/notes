'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztpsv = require( './ztpsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztpsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztpsv;
