'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpttrs = require( './dpttrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpttrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpttrs;
