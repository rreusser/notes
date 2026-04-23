
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqz1 = require( './dlaqz1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqz1, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqz1;
