'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhegvx = require( './zhegvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhegvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhegvx;
