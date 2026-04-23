'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsyr2k = require( './zsyr2k.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyr2k, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyr2k;
