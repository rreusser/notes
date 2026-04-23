
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaic1 = require( './dlaic1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaic1, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaic1;
