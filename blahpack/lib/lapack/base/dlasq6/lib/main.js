

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasq6 = require( './dlasq6.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasq6, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasq6;
