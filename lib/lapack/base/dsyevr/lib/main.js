'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsyevr = require( './dsyevr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyevr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyevr;
