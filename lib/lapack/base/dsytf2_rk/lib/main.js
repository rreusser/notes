
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsytf2rk = require( './dsytf2_rk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytf2rk, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytf2rk;
