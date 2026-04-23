'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqr4 = require( './dlaqr4.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqr4, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqr4;
