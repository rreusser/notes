'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqge = require( './dlaqge.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqge, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqge;
