
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqz2 = require( './dlaqz2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqz2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqz2;
