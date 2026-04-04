
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqsb = require( './dlaqsb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqsb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqsb;
