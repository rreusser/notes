'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dptsv = require( './dptsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dptsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dptsv;
