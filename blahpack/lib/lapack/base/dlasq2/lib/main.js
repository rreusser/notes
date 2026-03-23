

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasq2 = require( './dlasq2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasq2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasq2;
