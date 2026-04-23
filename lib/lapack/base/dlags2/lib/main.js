'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlags2 = require( './dlags2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlags2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlags2;
