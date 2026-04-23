'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlabad = require( './dlabad.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlabad, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlabad;
