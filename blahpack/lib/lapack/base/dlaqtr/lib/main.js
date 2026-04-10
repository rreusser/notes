

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqtr = require( './dlaqtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqtr;
