'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dstebz = require( './dstebz.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dstebz, 'ndarray', ndarray );


// EXPORTS //

module.exports = dstebz;
