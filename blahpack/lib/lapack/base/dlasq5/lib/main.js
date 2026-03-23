

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasq5 = require( './dlasq5.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasq5, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasq5;
