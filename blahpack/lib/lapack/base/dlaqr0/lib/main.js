'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqr0 = require( './dlaqr0.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqr0, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqr0;
