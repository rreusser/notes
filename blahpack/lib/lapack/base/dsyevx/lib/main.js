'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsyevx = require( './dsyevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyevx;
