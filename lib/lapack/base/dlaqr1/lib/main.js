'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqr1 = require( './dlaqr1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqr1, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqr1;
