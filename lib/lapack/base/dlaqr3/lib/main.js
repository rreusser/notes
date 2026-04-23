'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqr3 = require( './dlaqr3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqr3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqr3;
