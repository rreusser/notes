

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasq3 = require( './dlasq3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasq3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasq3;
