'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqps = require( './dlaqps.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqps, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqps;
