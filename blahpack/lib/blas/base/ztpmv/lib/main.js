'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztpmv = require( './ztpmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztpmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztpmv;
