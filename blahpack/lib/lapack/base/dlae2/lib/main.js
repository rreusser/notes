

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlae2 = require( './dlae2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlae2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlae2;
