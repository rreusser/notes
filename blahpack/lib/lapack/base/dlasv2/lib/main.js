

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasv2 = require( './dlasv2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasv2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasv2;
