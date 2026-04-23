'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlasy2 = require( './dlasy2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasy2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasy2;
