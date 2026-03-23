

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlacn2 = require( './dlacn2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlacn2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlacn2;
