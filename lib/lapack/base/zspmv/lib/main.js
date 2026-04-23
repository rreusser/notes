'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zspmv = require( './zspmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zspmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zspmv;
