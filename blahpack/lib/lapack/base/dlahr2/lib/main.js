

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlahr2 = require( './dlahr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlahr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlahr2;
