'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqr2 = require( './dlaqr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqr2;
