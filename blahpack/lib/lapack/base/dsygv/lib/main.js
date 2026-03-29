'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsygv = require( './dsygv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsygv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsygv;
