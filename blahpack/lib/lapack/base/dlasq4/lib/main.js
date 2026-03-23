

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasq4 = require( './dlasq4.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasq4, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasq4;
