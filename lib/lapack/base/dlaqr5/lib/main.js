'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqr5 = require( './dlaqr5.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqr5, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqr5;
