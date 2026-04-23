'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsytf2rk = require( './zsytf2_rk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytf2rk, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytf2rk;
